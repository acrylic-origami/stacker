import React from 'react'
import Q from 'q'
import { Map, Set, List, OrderedSet } from 'immutable'
import { id, map_intersect, assert, any, tuple, jsoneq } from './Util'
import { candidate, nk_span, repr_el_span, el2spk, span_contains, SPANTY, spaneq } from './Lang'
import MainContent from './MainContent'
import * as L from './Lang'
import { NUM_SNIP_DEPTH_COLORS } from './const'
import { mk_span_chars, slice_parsetree, mk_parsetree, TParseTree, ParseTree, MaybeKeyedSubSnip } from './parsetree'
import SnipItem from './SnipItem'
import CodeBlock from './CodeBlock'
import keycode from './keycode'
import parsePath from 'parse-filepath'

type TSpanTyd<T> = [ SPANTY, T ]
type SpanMeta = TSpanTyd<L.FwEdge>
// !!!!!!!!!!!!!!
// NOTE!!! MainSpanKey must remain serializable and unique. MainSpanKey is the main key that is used to identify things, and identification uses JSON equality for flexibility. Later we'll dedicate a MainSpanKey equality or use something hashable for use in a map, but for now obey these properties.
type MainSpanKey = L.SpanKey<SpanMeta>
type SplitSpanKeys = { ctxs: MainSpanKey[], nodes: MainSpanKey[][] }
export type SnipWrapper<Tk> = (txt: React.ReactNode, sp_ks: Tk) => React.ReactNode;
export type SpanKeySnipWrapper<Tk> = SnipWrapper<L.SpanKey<Tk>[]>

const wrap_snip: SpanKeySnipWrapper<TSpanTyd<unknown>> = (txt, sp_ks) => {
	const k_counts = sp_ks.reduce((acc, [sp, k]) => {
		if(k !== undefined) {
			const [spty, _el] = k;
			return acc.update(spty, 0, i => i + 1)
		}
		else return acc;
	}, Map<L.SPANTY, number>());
	return <span className={k_counts.map((cnt, k) => `snip-${k}-${Math.min(NUM_SNIP_DEPTH_COLORS, cnt)}`).join(' ')}>{txt}</span>
}

function ag2spks(ag: L.AppGroup): SplitSpanKeys {
	const ctxs: MainSpanKey[] = [];
	const nodes: MainSpanKey[][] = [];
	for(const fw_edge of ag) {
		const el = fw_edge[1];
		switch(el.tag) {
			case 'ArgEdge':
				nodes.push([
					[el.contents[0], [SPANTY.AG_TO_ARG, fw_edge]],
					[el.contents[1], [SPANTY.AG_TO_ARG, fw_edge]],
				]);
				ctxs.push([el.contents[1], [SPANTY.ARG, fw_edge]]);
				break;
			case 'AppEdge':
				nodes.push([
					[el.contents[0], [SPANTY.AG_TO_BIND, fw_edge]],
					[el.contents[1], [SPANTY.AG_TO_BIND, fw_edge]]
				]);
				ctxs.push([el.contents[1], [SPANTY.BIND_FROM_AG, fw_edge]]);
				break;
			default:
				throw new Error(`Unexpected EdgeLabel exiting AppGroup: ${el.tag}`);
		}
	}
	return { ctxs, nodes };
}
const split_spk_join = (l: SplitSpanKeys, r: SplitSpanKeys): SplitSpanKeys => ({
	ctxs: l.ctxs.concat(r.ctxs)
	, nodes: l.nodes.concat(r.nodes)
});

function next_spks(fw_edge: L.FwEdge, gr: L.NodeGraph): SplitSpanKeys {
	const [node, el] = fw_edge;
	const [elsp, elty] = el2spk(el);
	if(gr.has(node)) {
		const next = gr.get(node);
		if(next !== undefined) {
			const [[next_nk, next_cs_id], next_edges] = next;
			const here: SplitSpanKeys = {
				ctxs: [ // context spks
					[nk_span(next_nk), [L.NK2ENV[next_nk.tag], undefined as any]] // sorry. just too annoying to put an edge label there or figure out the semantics of not having one in the functions that expect it.
					, [elsp, [elty, fw_edge]]
				],
				nodes: []
			};
			const next_ = ((): SplitSpanKeys => { // node spks
				switch(el.tag) {
					case "ArgEdge":
						// acc.push([el.contents[0], [SPANTY.AG_TO_ARG, el]]);
						assert(next_nk.tag === 'NKBind');
						const nodes: MainSpanKey[][] = [];
						for(const fw_edge_ of next_edges) {
							const [targ, el_] = fw_edge_;
							switch(el_.tag) {
								case 'BindEdge':
									// const next__ = this.state.gr.get(targ); // kind of annoying but necessary: get the appgroup location based on the destination rather than using just the 
									nodes.push([
										[nk_span(next_nk), [SPANTY.BIND_CALLSITE, fw_edge_]],
										[el_.contents, [SPANTY.BIND_CALLSITE, fw_edge_]]
									]); // push the bind site too as a clickable: this is to support toggling
									break;
								case 'RevBindEdge':
									nodes.push([
										[nk_span(next_nk), [SPANTY.BIND_MATCHSITE, fw_edge_]],
										[el_.contents, [SPANTY.BIND_MATCHSITE, fw_edge_]]
									]);
									break;
							}
						}
						return {
							ctxs: [[nk_span(next_nk), [SPANTY.BIND_FROM_ARG, fw_edge]]]
							, nodes
						};
						break;
					case "BindEdge":
					case "RevBindEdge":
						assert(next_nk.tag === 'NKApp');
					case "AppEdge":
						// acc.push([el.contents[0], [SPANTY.AG_TO_BIND]])
						return ag2spks(next_edges);
						break;
				}
			})();
			return split_spk_join(here, next_);
		}
		else return { ctxs: [], nodes: [] };
	}
	else {
		return { ctxs: [], nodes: [] };
	}
}
function mk_snip_codeblock(parsetree: ParseTree<SpanMeta>) {
	return <section className="src-container"> {/* ref={e => this.setState({ root_container_el: e || undefined })} */ /* if I wanted this to scroll I should make a generalized scrollable code block with */}
		<pre>
			<code id="src_root" className="language-haskell hljs">
				&hellip;
				<CodeBlock<MainSpanKey>
					parsetree={parsetree}
					wrap_snip={wrap_snip}
					keycomp={(a, b) => false}
				/>
				&hellip;
			</code>
		</pre>
	</section>;
}

function ppr_loc(loc: L.Loc): string {
	return `(${loc[0]}, ${loc[1]})`;
}

/*
representations of at:

*/
interface TState {
	gr: L.NodeGraph,
	at_idx: number,
	at_history: List<L.FwEdge>,
	at_spks: SplitSpanKeys,
	at_spks_merged: MainSpanKey[], // even more egregious anti-single-source-of-truth, needed to avoid new objects made at render time
	at_spks_idx: number, // use this to check if at_spks is stale
	
	current_file: number, // implicit invariant that if `at` isn't undefined, this must be equal to the file location in the node that it references... or the one currently being shown... yeah. Hmm. Basically, this has to reflect the current file that wants to be pointed to with some degree of freedom by the user
	file_tabs: OrderedSet<string>,
	filelist: string[],
	src?: L.Src,
	src_req_idx: number,
	scroll_to?: L.Span,
	soft_selected?: MainSpanKey[], // the instance in the next nodes list that is soft-selected; note, this is distinct from the highlight on hover of a component within the code block // multiple instances because of toggling to tab
	snip_active?: MainSpanKey,
	
	num_toggles: number,
};

type TProps = {}

export default class extends React.Component<TProps, TState> {
	public readonly state: Readonly<TState> = {
		gr: Map(), // Map<node: int, (NodeKey, [(edge_target: Node, EdgeLabel)])> { <node>: { key: NodeKey, edges: [FWEdge] }
		at_idx: -1,
		at_history: List(), // List (?FWEdge)
		at_spks: { nodes: [], ctxs: [] },
		at_spks_merged: [], // memoize this to avoid deep comparison: do direct object comparison for the spks in MainContent
		at_spks_idx: -1, // synchronicity with at_idx: used to check if spks is up to date or not
		
		current_file: -1,
		file_tabs: OrderedSet(), //file_tabs <= filelist
		filelist: [], // [filename: String]
		
		src: undefined, // ?string
		src_req_idx: 0,
		scroll_to: undefined,
		soft_selected: undefined,
		snip_active: undefined,
		
		num_toggles: 0,
	}
	protected main_root_ref: React.RefObject<HTMLDivElement>
	constructor(props: TProps) {
		super(props);
		this.main_root_ref = React.createRef();
	}
	
	////////////////////////////////////////////////////////////////////////
	
	///////////////////////////
	///////  LIFECYCLE  ///////
	///////////////////////////
	
	componentDidMount(): void {
		window.addEventListener('keyup', this.keyPressHandler);
		fetch('/static/gr.json')
			.then(a => a.json())
			.then(([state_init_, filelist]) => {
				// for(const scc in state_init.sccs) {
				// 	if(state_init.sccs.hasOwnPropety(scc))
				// 		state_init.sccs.get(scc) = new Set(state_init.sccs.get(scc));
				// }
				this.setState(({ at_history, at_idx }) => {
					const gr: L.NodeGraph = Map(state_init_.gr);
					const at0 = state_init_.at[0];
					const next = gr.get(at0[0]);
					const next_span = next && nk_span(next[0][0]);
					const span_idx = next_span && parseInt(next_span[0]);
					return {
						at_idx: Math.min(at_idx + 1, at_history.size),
						at_history: at_history.push(at0),
						gr,
						filelist,
						current_file: span_idx !== undefined ? span_idx : -1,
						scroll_to: next_span
					};
				})
			});
	}
	componentDidUpdate(pprops: TProps, pstate: TState): void {
		const diff = {
			at_idx: pstate.at_idx !== this.state.at_idx,
			at_history: pstate.at_history !== this.state.at_history,
			at_spks: pstate.at_spks !== this.state.at_spks,
			gr: pstate.gr !== this.state.gr,
			current_file: pstate.current_file !== this.state.current_file,
			src:
				((this.state.src === undefined) !== (pstate.src === undefined))
				|| (
					this.state.src !== undefined
					&& pstate.src !== undefined
					&& pstate.src.path !== this.state.src.path
				),
			scroll_to: pstate.scroll_to !== this.state.scroll_to,
			num_toggles: pstate.num_toggles !== this.state.num_toggles,
		};
		if(diff.at_idx) {
			this.setState(({ at_idx }) => {
				const at = this.state.at_history.get(this.state.at_idx);
				if(at !== undefined) {
					const next = this.state.gr.get(at[0]);
					const at_spks_raw = next_spks(at, this.state.gr);
					const at_spks = {
						ctxs: at_spks_raw.ctxs.sort((a, b) => `${a[0]}`.localeCompare(`${b[0]}`)),
						nodes: at_spks_raw.nodes.sort((a, b) => `${a[0][0]}`.localeCompare(`${b[0][0]}`))
					};
					return {
						scroll_to: next && nk_span(next[0][0]),
						at_spks,
						at_spks_idx: at_idx
					};
				}
				else return null;
			});
		}
		if(diff.at_spks || diff.current_file) {
			// console.log(this.state.at_spks, this.state.current_file);
			this.setState(({ at_spks }) => ({
				at_spks_merged: at_spks.ctxs.concat(at_spks.nodes.flat()).filter(([sp, _k]) => parseInt(sp[0]) === this.state.current_file) // only show spankeys that are found in the current file
			}));
		}
		if(diff.scroll_to) {
			const scroll_to_file_idx = this.state.scroll_to && parseInt(this.state.scroll_to[0])
			if(scroll_to_file_idx !== undefined && scroll_to_file_idx !== this.state.current_file)
				this.setState({ current_file: scroll_to_file_idx });
		}
		if(diff.current_file) {
			// let at_file = at[1].contents[0];
			// if(typeof at_file !== 'string')
			// 	at_file = at_file[0]; // ArgEdge or AppEdge, a list of edges. need to go one further in
			
			// const at_path = this.state.filelist[parseInt(at_file)]; // this.state.gr.jsg_gr.get(this.state.at[0]).key.span.path;
			const current_file = this.state.filelist[this.state.current_file];
			// if(this.state.src === undefined && current_file !== undefined || this.state.src !== undefined && current_file !== this.state.src.path) {
				const stash_req_idx = this.state.src_req_idx;
				fetch(`/f?n=${encodeURIComponent(current_file.replace('lib/', '').replace('.hs', '.hie'))}`)
					.then(r => r.text())
					.then(t => this.setState(st => {
						if(this.state.src_req_idx === stash_req_idx) {
							return {
								file_tabs: st.file_tabs.add(current_file),
								src_req_idx: stash_req_idx + 1,
								src: { path: current_file, body: { raw: t, lines: t.split('\n') } }
							};
						}
						else return null;
					}))
			// }
		}
		
		if(diff.num_toggles) {
			const soft_selected = this.state.soft_selected;
			if(soft_selected !== undefined)
				this.setState({
					scroll_to: soft_selected[this.state.num_toggles % soft_selected.length][0] // TODO is this the right place to decide based on num toggles? dunno, it's a bit isolated. I wish I could decide at scroll time, instead of having it flow down
				})
		}
	}
	
	
	////////////////////////////////////////////////////////////////////////
	
	//////////////////////////////
	///////  VIEW-RELATED  ///////
	//////////////////////////////
	
	// protected should_scroll_to = (sp_ks: Array<MainSpanKey>): boolean => {
	// 	const scroll_to = this.state.scroll_to;
	// 	return scroll_to !== undefined
	// 	&& any(
	// 		k => span_contains(k, scroll_to)
	// 		, sp_ks.map(([sp, _k]) => sp)
	// 	)
	// }
	protected mk_snip_preview = <Tk extends any>(hljs_result: any, sp: L.Span, k: Tk): TParseTree<Tk | undefined> => {
		const src = this.state.src;
		if(src !== undefined) {
			const [isp] = mk_span_chars(src.body.lines, [[sp, k]]);
			const subt = slice_parsetree(hljs_result.emitter.root, [isp.key[0] - 50, Math.min(isp.key[1] + 50, isp.key[0] + 130)]);
			if(subt !== undefined) {
				const left = Math.min(isp.key[0], 50);
				const right = left + (isp.key[1] - isp.key[0]);
				const pt = mk_parsetree(subt, [
					[[0, left], undefined],
					[[left, right], isp.value[1]],
					[[right, Infinity], undefined]
				]);
				// console.log(pt);
				return pt;
			}
			else return [];
		}
		else return [];
	}
	protected spks2snips(named_spks: Array<[string, MainSpanKey]>, hljs_result: any, show: boolean = true): React.ReactNode {
		return named_spks.map(([name, [sp, [ty, [n, el]]]], i) => {
			const filename = this.state.filelist[parseInt(sp[0])];
			return <React.Fragment key={i}>
				<h3 className="ctx-head">
					<span className={`ctx-label ctx-label-${ty}`}>
						{name}
					</span>
					@&nbsp;&nbsp;
					{parsePath(filename).base}&nbsp;&nbsp;
					{ppr_loc(sp[1])}&nbsp;&mdash;&nbsp;{ppr_loc(sp[2])}
				</h3>
				<h4 className="ctx-head-fname">({filename})</h4>
				{show && mk_snip_codeblock(this.mk_snip_preview(hljs_result, sp, named_spks.map(([_name, spk]) => spk))) /* begs the question of whether I should promote them to bonified full-fledged classes rather than these ad-hoc arrays... probably. they're just so similar... i'll do this once all the dust settles */}
			</React.Fragment>
		});
	}
	protected render_ctx_bar = (hljs_result: any): React.ReactNode =>
		<React.Fragment>
			<section id="next_nodes_container">
				<div id="next_nodes_wrapper">
					<header>
						<h1>Next nodes</h1>
					</header>
					<ul id="next_nodes" className="ctx-list">
						{
							this.state.at_idx < this.state.at_history.size
							&& this.state.src !== undefined
							&& hljs_result !== undefined
							&& (() => {
								const at = this.state.at_history.get(this.state.at_idx);
								if(
									this.state.at_spks_idx === this.state.at_idx // => at_spks isn't stale
									&& this.state.at_spks !== undefined
									&& at !== undefined
								) {
									const [_n, el] = at;
									const { nodes } = this.state.at_spks;
									
									return nodes.map((spks, i) => {
										const active = any(sp_k =>
												this.state.soft_selected !== undefined
												&& any(soft_spk => jsoneq(soft_spk, sp_k), this.state.soft_selected)
											, spks
										);
										const soft_active = any(sp_k => jsoneq(this.state.snip_active, sp_k), spks);
										return <li className={
											active || soft_active ? 'active' : ''
										}>
											{ i + 1 < 10 ? <kbd>{i + 1}</kbd> : undefined}
											<SnipItem<L.SpanKey<SpanMeta>, MainSpanKey[]>
												onClick={this.nextNodeClickHandler}
												onDoubleClick={this.nextNodeDoubleClickHandler}
												onBlur={this.nextNodeBlurHandler}
												onFocus={this.nextNodeFocusHandler}
												click_key={spks}
												tabbable={true}
												active={active}
												// onSnipClick={this.snipClickHandler}
												key={`${JSON.stringify(spks)}` /* LOWPRI TODO come up with a better unique */}
												mk_children={show => this.spks2snips(
														spks.map((spk, i) =>
															[L.EDGELABEL2NAME[spk[1][1][1].tag][i] || '', spk]
														),
														hljs_result,
														show
													) } />
										</li>;
									})
								}
							})()
						}
					</ul>
				</div>
			</section>
			{ this.state.at_idx < this.state.at_history.size
			&& (() => {
				const at = this.state.at_history.get(this.state.at_idx);
				if(at !== undefined) {
					const [node, el] = at;
					const next = this.state.gr.get(node);
					if(next !== undefined) {
						const rarr = String.fromCharCode(0x2192);
						const names: Record<L.EdgeLabelTag, string> = {
							ArgEdge: "App group " + rarr + " Argument " + rarr + " Binding",
							AppEdge: "App group " + rarr + " Binding " + rarr + " RHS",
							BindEdge: "Binding " + rarr + " Callsite",
							RevBindEdge: "Binding " + rarr + " RHS",
						};
						return <React.Fragment>
							<section id="edge_ctx_container">
								<header>
									<h1>This node</h1>
									<h2>
										{ L.NK2NAME[next[0][0].tag] }
									</h2>
								</header>
								<ul id="edge_ctx" className="ctx-list">
									{
										this.state.src !== undefined && hljs_result !== undefined
										&& (() => {
											const spks = ((): Array<[string, MainSpanKey]> => {
												switch(el.tag) {
													case 'ArgEdge':
														return el.contents.map((sp, i): [string, MainSpanKey] => [['Use site', 'Bindsite'][i], [sp, [[SPANTY.AG_TO_ARG, SPANTY.ARG][i], at]]]);
														break;
													case 'AppEdge':
														return el.contents.map((sp, i): [string, MainSpanKey] => [['Callsite', 'Bindsite'][i], [sp, [[SPANTY.AG_TO_BIND, SPANTY.BIND_FROM_AG][i], at]]]);
														break;
													case 'BindEdge':
														return [['Bindsite', [el.contents, [SPANTY.BIND_CALLSITE, at]]]];
														break;
													case 'RevBindEdge':
														return [['Callsite', [el.contents, [SPANTY.BIND_MATCHSITE, at]]]];
														break;
												}
											})();
											return <li>
												<SnipItem<L.SpanKey<TSpanTyd<undefined>>>
													tabbable={false}
													key={`${JSON.stringify(spks)}` /* LOWPRI TODO come up with a better unique */}
													mk_children={show => this.spks2snips(spks, hljs_result, show) } />
											</li>;
										})()
									}
								</ul>
							</section>
							<section id="history_container">
								<div id="history_wrapper">
									<header>
										<h1>History</h1>
									</header>
									<ol className="ctx-list" id="history">
										{
											this.state.src && hljs_result
											&& this.state.at_history.map((at_, i) => {
													const [node_, el_] = at_; // TODO MainSpanKey lacks NodeKey which is a little too bad. I'll really have to firm up that type since I use it as the main key type everywhere.
													const next_ = this.state.gr.get(node_);
													if(next_ !== undefined) {
														const at_sp = nk_span(next_[0][0]);
														const next_nk_ = next_[0][0].tag;
														return <li>
															<SnipItem<number | undefined, number>
																onClick={this.historyClickHandler}
																active={false}
																click_key={i}
																tabbable={false}
																// onSnipClick={this.historyClickHandler}
																key={i}
																mk_children={show => this.spks2snips(
																	[[L.NK2NAME[next_nk_], [at_sp, [L.NK2ENV[next_nk_], [node_, el_]]]]], // artificially construct span_key. Muddies semantics a bit... in this case, I want it purely for presentation. especially that it's a singleton list. Eugh. TODO make this better.
																	hljs_result,
																	show
																) } />
														</li>
													}
											}).reverse()
										}
									</ol>
								</div>
							</section>
						</React.Fragment>
					}
				}
			})() }
		</React.Fragment>
	
	////////////////////////////////////////////////////////////////////////
	
	/////////////////////////////////
	///////  CONTROL-RELATED  ///////
	/////////////////////////////////
	
	protected cancel_mode = (): void => {
		// the only mode of this view is `soft_selected`, whose cancel state is undefined
		this.setState({ soft_selected: undefined });
	}
	protected advance_to([_, [__, c]]: MainSpanKey): void {
		const next = this.state.gr.get(c[0]);
		if(next !== undefined)
			this.setState(({ at_idx, at_history }) => ({
				at_idx: Math.min(at_idx + 1, at_history.size),
				at_history: at_history.take(at_idx + 1).push(c),
				soft_selected: undefined
			}));
	}
	
	
	////////////////////////////////////////////////////////////////////////
	
	//////////////////////////
	///////  HANDLERS  ///////
	//////////////////////////
	
	protected snipClickHandler = (e: React.SyntheticEvent, sp_ks: MainSpanKey[]): void => {
		e.stopPropagation();
		const mc = candidate(sp_ks);
		if(mc !== undefined) {
			this.advance_to(mc);
		}
	}
	
	protected ctxClickHandler = (e: React.SyntheticEvent, scroll_to: L.Span): void => {
		e.stopPropagation();
		this.setState({ scroll_to });
	}
	// next node click vs. focus is fairly subtle: without care (e.g. both handled by click handler, or without the event queue to avoid races within SnipItem), these might be possible:
	// 1. both handled by click handler: tabbing through a single-element list will result in it being clicked, because it was previously selected
	// 2. no event queue: since click also focuses and raises a focus event in SnipItem, it could possibly invoke the focus handler first (making it soft-selected), then clicking, which would advance it with a single click.
	protected nextNodeDoubleClickHandler = (e: Event, soft_selected: MainSpanKey[]): void => {
		this.advance_to(soft_selected[0]);
	}
	protected nextNodeClickHandler = (e: Event, soft_selected: MainSpanKey[]): void => {
		// console.log(soft_selected, this.state.soft_selected);
		if(soft_selected === this.state.soft_selected && this.state.soft_selected !== undefined) {
			this.advance_to(this.state.soft_selected[0]);
		}
		else {
			this.nextNodeFocusHandler(e, soft_selected); // risky re: event type, but expressive
		}
	}
	protected nextNodeFocusHandler = (e: Event, soft_selected: MainSpanKey[]): void => {
		this.setState({
			soft_selected,
			scroll_to: soft_selected[this.state.num_toggles % soft_selected.length][0] // TODO is this the right place to decide based on num toggles? dunno, it's a bit isolated. I wish I could decide at scroll time, instead of having it flow down
		});
	}
	protected nextNodeBlurHandler = (e: Event, soft_selected_: MainSpanKey[]): void => {
		this.setState(({ soft_selected }) => ({ soft_selected: soft_selected_ === soft_selected ? undefined : soft_selected }));
	}
	protected historyClickHandler = (e: Event | React.SyntheticEvent, at_idx: number): void => {
		const at_ = this.state.at_history.get(this.state.at_idx);
		if(at_ !== undefined) {
			const next = this.state.gr.get(at_[0]);
			if(next !== undefined) {
				this.setState({
					at_idx,
					scroll_to: nk_span(next[0][0])
				});
				e.stopPropagation();
			}
		}
	}
	protected snipHoverHandler = (e: React.SyntheticEvent, sp_ks: MainSpanKey[]): void => {
		const c = candidate(sp_ks);
		switch(e.type) {
			case 'mouseenter':
				if(c !== undefined)
					this.setState({ snip_active: c })
				break;
			case 'mouseleave':
				this.setState(({ snip_active }) => {
					if(jsoneq(snip_active, c))
						return { snip_active: undefined };
					else return null;
				});
				break;
		}
	}
	protected filetabClickHandler = (e: React.SyntheticEvent, fname: string): void => {
		const next_at_idx = this.state.at_history.reverse().map((at, i) => {
			const next = this.state.gr.get(at[0]);
			if(next !== undefined && this.state.filelist[parseInt(nk_span(next[0][0])[0])] === fname) {
				return this.state.at_history.size - i - 1;
			}
			else return undefined;
		}).filter(i => i !== undefined).first(undefined);
		this.setState(({ at_idx, current_file }) => ({
			at_idx: next_at_idx !== undefined ? next_at_idx : at_idx,
			current_file: this.state.filelist.indexOf(fname)
		}));
	}
	protected keyPressHandler = (e: KeyboardEvent): void => {
		console.log(e.key);
		switch(e.key) {
			case 'Escape':
				// for now not overloaded: just cancel the selected mode
				this.cancel_mode();
				break;
			case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
				this.setState(({ at_spks }) => ({
					soft_selected: at_spks.nodes[
						Math.min(
							at_spks.nodes.length - 1,
							parseInt(e.key) - 1
						)
					]
				}));
				break;
			case 'PageUp':
			case 'PageDown':
				if(e.altKey) {
					// go between files
					const dir = e.key === 'PageUp' ? -1 : 1;
					// TODO
					e.preventDefault();
				}
				break;
			case 'Backspace':
			case 'u':
			case 'U':
				this.setState(({ at_idx, at_history }) => ({ at_idx: Math.max(0, at_idx - 1) }));
				break;
			case 'R':
			case 'r':
				if(e.ctrlKey) {
					this.setState(({ at_idx, at_history }) => ({ at_idx: Math.min(at_history.size - 1, at_idx + 1) }));
				}
				break;
			case 'T':
			case 't':
				this.setState(({ num_toggles }) => ({ num_toggles: num_toggles + 1 }));
				break;
			case 'Z':
			case 'z':
				if(e.ctrlKey) {
				}
				else {
					// force scroll
					this.setState(({ scroll_to }) => ({ scroll_to: scroll_to && (scroll_to.slice() as L.Span) })); // since we use strict equality, just dupe `scroll_to` to force scroll
				}
		}
	}
	
	////////////////////////////////////////////////////////////////////////
	
	//////////////////////////
	///////  RENDERER  ///////
	//////////////////////////
	
	render = () => {
		const at = this.state.at_history.get(this.state.at_idx);
		const next = at && this.state.gr.get(at[0]);
		const next_fname = next && this.state.filelist[parseInt(nk_span(next[0][0])[0])];
		return <div
			onClick={this.cancel_mode}
			id="main_root"
			ref={this.main_root_ref}>
			<nav id="main_nav">
				<span id="logo_container">
					<div id="logo"></div>
				</span>
				<ul id="file_tabs" className="flatlist">
					{this.state.file_tabs.map((fname, i) =>
						<li className={`filetab ${fname === next_fname ? 'selected' : ''}`} key={fname}>
							<span onClick={e => this.filetabClickHandler(e, fname) /* technically there's a race condition between setting the new state and a new render, wonder if this is worth worrying about. */}>{parsePath(fname).base}</span>
							<i className="close fas fa-times"></i>
						</li>)}
				</ul>
			</nav>
			<MainContent<SpanMeta>
				ctx_renderer={this.render_ctx_bar}
				src={this.state.src}
				span_ks={this.state.at_spks_merged || []}
				onSnipHover={this.snipHoverHandler}
				scroll_to={this.state.scroll_to}
				soft_selected={this.state.soft_selected}
				wrap_snip={wrap_snip}
				onSnipClick={this.snipClickHandler}
			/>
		</div>;
	}
}