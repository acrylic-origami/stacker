import React from 'react'
import Q from 'q'
import { Map, Set, List } from 'immutable'
import { id, map_intersect, assert, any, tuple } from './Util'
import { candidate, nk_span, repr_el_span, el2spk, span_contains, SPANTY, spaneq } from './Lang'
import CodeBlock from './CodeBlock'
import * as L from './Lang'
import { NUM_SNIP_DEPTH_COLORS } from './const'
import { mk_span_chars, slice_parsetree, mk_parsetree } from './parsetree'
import CtxSnip, { TPreview } from './CtxSnip'

type SpanMeta = [ SPANTY, L.FwEdge ]
type MainSpanKey = L.SpanKey<SpanMeta>
type SplitSpanKeys = { ctxs: MainSpanKey[], nodes: MainSpanKey[] }

function wrap_snip(txt: React.ReactNode, sp_ks: MainSpanKey[]): React.ReactNode {
	const k_counts = sp_ks.reduce((acc, [sp, [spty, _el]]) => acc.update(spty, 0, i => i + 1), Map<L.SPANTY, number>());
	return <span className={k_counts.map((cnt, k) => `snip-${k}-${Math.min(NUM_SNIP_DEPTH_COLORS, cnt)}`).join(' ')}>{txt}</span>
}

function ag2spks(ag: L.AppGroup): SplitSpanKeys {
	const ctxs: MainSpanKey[] = [];
	const nodes: MainSpanKey[] = [];
	for(const fw_edge of ag) {
		const el = fw_edge[1];
		switch(el.tag) {
			case 'ArgEdge':
				nodes.push([el.contents[0], [SPANTY.AG_TO_ARG, fw_edge]]);
				ctxs.push([el.contents[1], [SPANTY.ARG, fw_edge]]);
				break;
			case 'AppEdge':
				nodes.push([el.contents[0], [SPANTY.AG_TO_BIND, fw_edge]]);
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

interface TState {
	gr: L.NodeGraph,
	at_idx: number,
	at_history: List<L.FwEdge>,
	filelist: string[],
	src?: L.Src,
	src_req_idx: number,
	scroll_to?: L.Span
};

type TProps = {}

export default class extends React.Component<TProps, TState> {
	public readonly state: Readonly<TState> = {
		gr: Map(), // Map<node: int, (NodeKey, [(edge_target: Node, EdgeLabel)])> { <node>: { key: NodeKey, edges: [FWEdge] }
		at_idx: 0,
		at_history: List(), // List (?FWEdge)
		filelist: [], // [filename: String]
		
		src: undefined, // ?string
		src_req_idx: 0,
		scroll_to: undefined
	}
	constructor(props: TProps) {
		super(props);
		
		// type Loc = (Int, Int)
		// type ISpan = (fileidx: String (show Int), start: Loc, end: Loc)
		// type NodeKey = { tag: "NKApp" | "NKBind", contents: [ISpan] | ISpan }
		// type EdgeLabel = { tag: "ArgEdge" | "AppEdge" | "BindEdge", contents: (ISpan, ISpan) | ISpan }
		// type FWEdge = (Int, EdgeLabel)
		// window.addEventListener('popstate', this.handle_uri_term);
	}
	componentDidMount(): void {
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
					return {
						at_idx: Math.min(at_idx + 1, at_history.size),
						at_history: at_history.push(at0),
						gr,
						filelist,
						scroll_to: next && nk_span(next[0][0])
					};
				})
			});
	}
	protected keyPressHandler = (e: React.SyntheticEvent): void => {
	}
	protected snipClickHandler = (e: React.SyntheticEvent, sp_ks: MainSpanKey[]): void => {
		// s :: (Span, Map Span (SPANTY, FWEdge)) // first span is the span of the mini region that was clicked
		
		// const sps = sp_ks.map((_k, sp) => sp); // TODO confirm that's the CS id, for my understanding
		// const which = ; // , which = sp_ks.filter((_k, sp) => spaneq(sp, which_sp)).first()[1];
		const mc = candidate(sp_ks);
		if(mc !== undefined) {
			const c = mc[1][1];
			const next = this.state.gr.get(c[0]);
			if(next !== undefined)
				this.setState(({ at_idx, at_history }) => ({
					at_idx: Math.min(at_idx + 1, at_history.size),
					at_history: at_history.take(at_idx + 1).push(c),
					scroll_to: nk_span(next[0][0])
				}));
		}
		
		// switch(which[0]) {
		// 	case SPANTY.AG_TO_ARG:
		// 		break;
		// }
	}
	protected ctxClickHandler = (e: React.SyntheticEvent, scroll_to: L.Span): void => {
		e.stopPropagation();
		this.setState({ scroll_to });
	}
	protected historyClickHandler = (e: React.SyntheticEvent, at_idx: number): void => {
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
	componentDidUpdate(pprops: TProps, pstate: TState): void {
		const diff = {
			at_idx: pstate.at_idx !== this.state.at_idx,
			at_history: pstate.at_history !== this.state.at_history,
			gr: pstate.gr !== this.state.gr,
			src:
				((this.state.src === undefined) !== (pstate.src === undefined))
				|| (
					this.state.src !== undefined
					&& pstate.src !== undefined
					&& pstate.src.path !== this.state.src.path
				)
		};
		if(this.state.at_idx < this.state.at_history.size) {
			const at = this.state.at_history.get(this.state.at_idx);
			if(at !== undefined) {
				let at_file = at[1].contents[0];
				if(typeof at_file !== 'string')
					at_file = at_file[0]; // ArgEdge or AppEdge, a list of edges. need to go one further in
				
				const at_path = this.state.filelist[parseInt(at_file)]; // this.state.gr.jsg_gr.get(this.state.at[0]).key.span.path;
				if(this.state.src === undefined && at_path !== undefined || this.state.src !== undefined && at_path !== this.state.src.path) {
					const stash_req_idx = this.state.src_req_idx;
					fetch(`/f?n=${encodeURIComponent(at_path.replace('lib/', '').replace('.hs', '.hie'))}`)
						.then(r => r.text())
						.then(t => this.setState(st => {
							if(this.state.src_req_idx === stash_req_idx) {
								return {
									src_req_idx: stash_req_idx + 1,
									src: { path: at_path, body: { raw: t, lines: t.split('\n') } }
								};
							}
							else return null;
						}))
				}
			}
		}
	}
	protected should_scroll_to = (sp_ks: Array<MainSpanKey>): boolean => {
		const scroll_to = this.state.scroll_to;
		return scroll_to !== undefined
		&& any(
			k => span_contains(k, scroll_to)
			, sp_ks.map(([sp, _k]) => sp)
		)
	}
	/*
		<section id="scc_section">
			<h1></h1>
			<ul id="scc_select">{
				this.state.at === undefined || this.state.gr === undefined ? undefined : this.state.gr.get(this.state.at).map(node =>
					<li key={node} className={ node === this.state.at[0] ? 'selected' : '' }>
						{node}
					</li>
				)
			}</ul>
		</section>
	*/
	protected mk_snip_preview = <Tk extends any>(hljs_result: any, sp: L.Span, k: [undefined | Tk] = [undefined]): Array<TPreview<Tk>> => {
		const src = this.state.src;
		if(src !== undefined) {
			const [isp] = mk_span_chars(src.body.lines, [[sp, k]]);
			const subt = slice_parsetree(hljs_result.emitter.root, [isp.key[0] - 50, isp.key[1] + 50]);
			if(subt !== undefined) {
				const left = Math.min(isp.key[0], 50);
				const right = left + (isp.key[1] - isp.key[0]);
				// debugger;
				return mk_parsetree(subt, [
					[[0, left], undefined],
					[[left, right], isp.value[1]],
					[[right, Infinity], undefined]
				]);
			}
			else return [];
		}
		else return [];
	}
	protected render_ctx_bar = (hljs_result: any): React.ReactNode => [
		this.state.at_idx < this.state.at_history.size
			&& (() => {
				const at = this.state.at_history.get(this.state.at_idx);
				if(at !== undefined) {
					const [node, el] = at;
					const rarr = String.fromCharCode(0x2192);
					const names: Record<L.EdgeLabelTag, string> = {
						ArgEdge: "App group " + rarr + " Argument " + rarr + " Binding",
						AppEdge: "App group " + rarr + " Binding " + rarr + " RHS",
						BindEdge: "Binding " + rarr + " Callsite",
						RevBindEdge: "Binding " + rarr + " RHS",
					};
					return <section>
						<section id="edge_ctx_container">
							<header>
								<h1>{ el.tag }</h1>
								<h2>
									{names[el.tag]}
								</h2>
							</header>
							<ul id="edge_ctx" className="flatlist">
								{
									this.state.src !== undefined && hljs_result !== undefined
									&& ((): Array<[string, L.Span]> => {
										switch(el.tag) {
											case 'ArgEdge':
												return el.contents.map((sp, i): [string, L.Span] => [['Use site', 'Bindsite'][i], sp]);
												break;
											case 'AppEdge':
												return el.contents.map((sp, i): [string, L.Span] => [['Callsite', 'Bindsite'][i], sp]);
												break;
											case 'BindEdge':
												return [['Callsite', el.contents]];
												break;
											case 'RevBindEdge':
												return [['Bindsite', el.contents]];
												break;
										}
									})().map(([name, sp]) => 
										<CtxSnip<MainSpanKey>
											name={name}
											filename={this.state.filelist[parseInt(sp[0])]}
											span={sp}
											preview={this.mk_snip_preview(hljs_result, sp)}
											key={sp.toString()}
										>
										</CtxSnip>
									)
								}
							</ul>
						</section>
						<section>
							<header>
								<h1>History</h1>
							</header>
							<ul className="flatlist" id="history">
								{
									this.state.src && hljs_result
									&& this.state.at_history.map((at_, i) => {
											const [node_, el_] = at_;
											const next = this.state.gr.get(node_);
											if(next !== undefined) {
												const at_sp = nk_span(next[0][0]);
												return <CtxSnip<number, number>
													onClick={this.historyClickHandler}
													click_key={i}
													onSnipClick={this.historyClickHandler}
													name={el_.tag}
													filename={this.state.filelist[parseInt(at_sp[0])]}
													span={at_sp}
													preview={this.mk_snip_preview<number>(hljs_result, at_sp, [i])}
													key={i}
												/>;
											}
									}).reverse()
								}
							</ul>
						</section>
					</section>
				}
			})()
		, <section id="next_nodes_container">
				<ul id="next_nodes" className="flatlist">
					{
						this.state.at_idx < this.state.at_history.size
						&& this.state.src !== undefined
						&& hljs_result !== undefined
						&& (() => {
							const at = this.state.at_history.get(this.state.at_idx);
							if(at !== undefined) {
								const [_n, el] = at;
								const { nodes } = this.fw_edge2spks(at);
								
								const nodes_ = ((): Array<[string, MainSpanKey]> => {
									switch(el.tag) {
										case 'ArgEdge':
											return nodes.map((spk): [string, MainSpanKey] => {
												const [_sp, [_ty, [_n, el_]]] = spk;
												assert(el_.tag === 'BindEdge' || el_.tag === 'RevBindEdge', `Unexpected ${el_.tag}`)
												const names : Partial<Record<L.EdgeLabelTag, string>> = {
													BindEdge: 'Callsite',
													RevBindEdge: 'Binding RHS'
												}
												return [
													names[el_.tag] || '',
													spk
												];
											});
											break;
										case 'AppEdge':
										case 'BindEdge':
										case 'RevBindEdge':
											return nodes.map((spk): [string, MainSpanKey] => {
												const [_sp, [_ty, [_n, el_]]] = spk;
												assert(el_.tag === 'AppEdge' || el_.tag === 'ArgEdge', `Unexpected ${el_.tag}`)
												const names : Partial<Record<L.EdgeLabelTag, string>> = {
													AppEdge: 'Value Callsite',
													ArgEdge: 'Arg Callsite'
												}
												return [
													names[el_.tag] || '',
													spk
												]
											});
											break;
									}
								})();
								return nodes_.map(([name, sp_k], i) => {
									const [sp, [_ty, [n, el]]] = sp_k;
									return <CtxSnip<MainSpanKey[], L.Span>
											onClick={this.ctxClickHandler}
											click_key={sp}
											onSnipClick={this.snipClickHandler}
											name={name}
											filename={this.state.filelist[parseInt(sp[0])]}
											span={sp}
											key={`${sp.toString()}-${n}=${i}`}
											preview={this.mk_snip_preview(hljs_result, sp, [[sp_k]])}
										/>
								})
							}
						})()
					}
				</ul>
			</section>
	]
	protected fw_edge2spks = (fw_edge: L.FwEdge): SplitSpanKeys => {
		
		const [node, el] = fw_edge;
		const [elsp, elty] = el2spk(el);
		if(this.state.gr.has(node)) {
			const next = this.state.gr.get(node);
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
							const nodes = new Array<MainSpanKey>();
							for(const fw_edge_ of next_edges) {
								const [targ, el_] = fw_edge_;
								switch(el_.tag) {
									case 'BindEdge':
										nodes.push([el_.contents, [SPANTY.BIND_CALLSITE, fw_edge_]]);
										break;
									case 'RevBindEdge':
										nodes.push([el_.contents, [SPANTY.BIND_MATCHSITE, fw_edge_]]);
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
	render = () => <div onKeyUp={this.keyPressHandler} id="main_root">
		<CodeBlock<SpanMeta>
			ctx_renderer={this.render_ctx_bar}
			src={this.state.src}
			span_ks={this.state.at_idx < this.state.at_history.size && this.state.gr && (() => {
					const at = this.state.at_history.get(this.state.at_idx);
					if(at !== undefined) {
						const { ctxs, nodes } = this.fw_edge2spks(at);
						return ctxs.concat(nodes);
					}
				})()
				|| []
			}
			should_scroll_to={this.should_scroll_to}
			wrap_snip={wrap_snip}
			onSnipClick={this.snipClickHandler}
		/>
	</div>
}