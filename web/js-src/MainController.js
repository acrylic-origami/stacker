import React from 'react'
import Q from 'q'
import { Map, Set, List } from 'immutable'
import { id, map_intersect, assert, any, list1eq } from './Util'
import { candidate, nk_span, repr_el_span, el2spk, span_contains } from './Lang'
import CodeBlock from './CodeBlock'
import { SPANTY, NK2ENV } from './Lang'
import { NUM_SNIP_DEPTH_COLORS } from './const.js'
import { mk_span_chars, slice_parsetree, mk_parsetree } from './parsetree'
import CtxSnip from './CtxSnip'

function span2key(x) {
	return x.toString();
	// return x.map(x_ => x_.toString()).join('_');
}

function wrap_snip(txt, sp_ks) {
	// console.log(sp_ks);
	const k_counts = sp_ks.reduce((acc, [sp, [spty, _el]]) => acc.update(spty, 0, i => i + 1), Map());
	return <span className={k_counts.map((cnt, k) => `snip-${k}-${Math.min(NUM_SNIP_DEPTH_COLORS, cnt)}`).join(' ')}>{txt}</span>
}

function ag2spks(ag) {
	const ctxs = [];
	const nodes = [];
	for(const fw_edge of ag) {
		// console.log(fwedge, ag)
		const el = fw_edge[1];
		switch(el.tag) {
			case 'ArgEdge':
				nodes.push([el.contents[0], [SPANTY.NODE.AG_TO_ARG, fw_edge]]);
				ctxs.push([el.contents[1], [SPANTY.CTX.ARG, fw_edge]]);
				break;
			case 'AppEdge':
				nodes.push([el.contents[0], [SPANTY.NODE.AG_TO_BIND, fw_edge]]);
				ctxs.push([el.contents[1], [SPANTY.CTX.BIND_FROM_AG, fw_edge]]);
				break;
			default:
				throw new Error(`Unexpected EdgeLabel exiting AppGroup: ${el.tag}`);
		}
	}
	return { ctxs, nodes };
}

export default class extends React.Component {
	constructor(props) {
		super(props);
		
		// type Loc = (Int, Int)
		// type ISpan = (fileidx: String (show Int), start: Loc, end: Loc)
		// type NodeKey = { tag: "NKApp" | "NKBind", contents: [ISpan] | ISpan }
		// type EdgeLabel = { tag: "ArgEdge" | "AppEdge" | "BindEdge", contents: (ISpan, ISpan) | ISpan }
		// type FWEdge = (Int, EdgeLabel)
		this.state = {
			gr: Map(), // Map<node: int, (NodeKey, [(edge_target: Node, EdgeLabel)])> { <node>: { key: NodeKey, edges: [FWEdge] }
			at_idx: 0,
			at_history: List(), // List (?FWEdge)
			filelist: [], // [filename: String]
			
			src: null, // ?string
			src_req_idx: 0
		}
		window.addEventListener('popstate', this.handle_uri_term);
	}
	componentDidMount() {
		fetch('/static/gr.json')
			.then(a => a.json())
			.then(([state_init_, filelist]) => {
				// for(const scc in state_init.sccs) {
				// 	if(state_init.sccs.hasOwnPropety(scc))
				// 		state_init.sccs.get(scc) = new Set(state_init.sccs.get(scc));
				// }
				this.setState(({ at_history, at_idx }) => ({
					at_idx: Math.min(at_idx + 1, at_history.size),
					at_history: at_history.push(state_init_.at[0]),
					gr: state_init_.gr.reduce((m, [k, a]) => m.set(k, a), Map()),
					filelist
				}))
			});
	}
	keyPressHandler = e => {
		console.log(e);
	}
	snipClickHandler = (e, sp_ks) => {
		e.stopPropagation();
		// s :: (Span, Map Span (SPANTY, FWEdge)) // first span is the span of the mini region that was clicked
		
		// const sps = sp_ks.map((_k, sp) => sp); // TODO confirm that's the CS id, for my understanding
		// const which = ; // , which = sp_ks.filter((_k, sp) => list1eq(sp, which_sp)).first()[1];
		const c = candidate(sp_ks)[1][1];
		// console.log(sp_ks, c);
		this.setState(({ at_idx, at_history }) => ({
			at_idx: Math.min(at_idx + 1, at_history.size),
			at_history: at_history.push(c),
			scroll_to: nk_span(this.state.gr.get(c[0])[0][0])
		}));
		
		// switch(which[0]) {
		// 	case SPANTY.NODE.AG_TO_ARG:
		// 		break;
		// }
	}
	ctxClickHandler = (e, scroll_to) => {
		e.preventPropagation();
		this.setState({ scroll_to });
	}
	historyClickHandler = (e, at_idx) => {
		e.preventPropagation();
		this.setState({ at_idx });
	}
	componentDidUpdate(pprops, pstate) {
		const diff = {
			at_idx: pstate.at_idx !== this.state.at_idx,
			at_history: pstate.at_history !== this.state.at_history,
			gr: pstate.gr !== this.state.gr,
			src:
				((this.state.src === null) !== (pstate.src === null))
				|| (
					this.state.src !== null
					&& pstate.src !== null
					&& pstate.src.path !== this.state.src.path
				)
		};
		if(this.state.at_idx < this.state.at_history.size) {
			let at_file = this.state.at_history.get(this.state.at_idx)[1].contents[0];
			if(typeof at_file !== 'string')
				at_file = at_file[0]; // ArgEdge or AppEdge, a list of edges. need to go one further in
			
			const at_path = this.state.filelist[parseInt(at_file)]; // this.state.gr.jsg_gr.get(this.state.at[0]).key.span.path;
			if(this.state.src === null && at_path != null || this.state.src !== null && at_path !== this.state.src.path) {
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
					}))
			}
		}
	}
	should_scroll_to = (sp_ks) =>
		this.state.scroll_to
		&& any(
			k => span_contains(k, this.state.scroll_to)
			, sp_ks.map(([sp, _k]) => sp)
		)
	/*
		<section id="scc_section">
			<h1></h1>
			<ul id="scc_select">{
				this.state.at === null || this.state.gr === null ? null : this.state.gr.get(this.state.at).map(node =>
					<li key={node} className={ node === this.state.at[0] ? 'selected' : '' }>
						{node}
					</li>
				)
			}</ul>
		</section>
	*/
	mk_snip_preview = (hljs_result, sp, k = [null]) => {
		const [isp] = mk_span_chars(this.state.src.body.lines, [[sp, k]]);
		const subt = slice_parsetree(hljs_result.emitter.root, [isp.key[0] - 50, isp.key[1] + 50]);
		const left = Math.min(isp.key[0], 50);
		const right = left + (isp.key[1] - isp.key[0]);
		// debugger;
		return mk_parsetree(subt, [
			[[0, left], null],
			[[left, right], isp.value[1]],
			[[right, Infinity], null]
		]);
	}
	render_ctx_bar = hljs_result => [
		this.state.at_idx < this.state.at_history.size
			&& (() => {
				const [node, el] = this.state.at_history.get(this.state.at_idx);
				const rarr = String.fromCharCode(0x2192);
				return <section>
					<section id="edge_ctx_container">
						<header>
							<h1>{ el.tag }</h1>
							<h2>
								{{
									ArgEdge: "App group " + rarr + " Argument " + rarr + " Binding",
									AppEdge: "App group " + rarr + " Binding " + rarr + " RHS",
									BindEdge: "Binding " + rarr + " Callsite",
									RevBindEdge: "Binding " + rarr + " RHS",
								}[el.tag]}
							</h2>
						</header>
						<ul id="edge_ctx" className="flatlist">
							{
								this.state.src !== null && hljs_result !== null
								&& (() => {
									switch(el.tag) {
										case 'ArgEdge':
											return el.contents.map((sp, i) => [['Use site', 'Bindsite'][i], sp]);
											break;
										case 'AppEdge':
											return el.contents.map((sp, i) => [['Callsite', 'Bindsite'][i], sp]);
											break;
										case 'BindEdge':
											return [['Callsite', el.contents]];
											break;
										case 'RevBindEdge':
											return [['Bindsite', el.contents]];
											break;
									}
								})().map(([name, sp]) => 
									<CtxSnip
										onClick={this.historyClickHandler}
										onSnipClick={this.historyClickHandler}
										name={name}
										filename={this.state.filelist[sp[0]]}
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
								&& this.state.at_history.map((at, i) => 
									this.state.gr.has(at[0])
									&& (() => {
										const at_sp = nk_span(this.state.gr.get(at[0])[0][0]);
										return <CtxSnip
											name={at[1][0]}
											filename={this.state.filelist[at_sp[0]]}
											span={at_sp}
											preview={this.mk_snip_preview(hljs_result, at_sp, at[0])}
											key={at_sp.toString()}
										/>;
									})()
								).reverse()
							}
						</ul>
					</section>
				</section>
			})()
		, <section id="next_nodes_container">
				<ul id="next_nodes" className="flatlist">
					{
						this.state.at_idx < this.state.at_history.size
						&& this.state.src !== null
						&& hljs_result !== null
						&& (() => {
							const at = this.state.at_history.get(this.state.at_idx);
							const [_n, el] = at;
							const { nodes } = this.fw_edge2spks(at);
							
							const nodes_ = (() => {
								switch(el.tag) {
									case 'ArgEdge':
										console.log(nodes, at)
										return nodes.map((spk) => {
											const [_sp, [_ty, [_n, el_]]] = spk;
											assert(el_.tag === 'BindEdge' || el_.tag === 'RevBindEdge', `Unexpected ${el_.tag}`)
											return [
												{
													BindEdge: 'Callsite',
													RevBindEdge: 'Binding RHS'
												}[el_.tag],
												spk
											]
										});
										break;
									case 'AppEdge':
									case 'BindEdge':
									case 'RevBindEdge':
										return nodes.map((spk) => {
											const [_sp, [_ty, [_n, el_]]] = spk;
											assert(el_.tag === 'AppEdge' || el_.tag === 'ArgEdge', `Unexpected ${el_.tag}`)
											return [
												{
													AppEdge: 'Value Callsite',
													ArgEdge: 'Arg Callsite'
												}[el_.tag],
												spk
											]
										});
										break;
								}
							})();
							console.log(nodes_);
							return nodes_.map(([name, sp_k], i) => {
								const [sp, [_ty, [n, el]]] = sp_k;
								return <CtxSnip
										onClick={this.ctxSnipClickHandler}
										click_key={sp}
										onSnipClick={this.snipClickHandler}
										name={name}
										filename={this.state.filelist[sp[0]]}
										span={sp}
										key={`${sp.toString()}-${n}=${i}`}
										preview={this.mk_snip_preview(hljs_result, sp, [sp_k])}
									/>
							})
						})()
					}
				</ul>
			</section>
	]
	fw_edge2spks = (fw_edge) => {
		const spk_join = (l, r) => ({
			ctxs: l.ctxs.concat(r.ctxs)
			, nodes: l.nodes.concat(r.nodes)
		});
		
		const [node, el] = fw_edge;
		const [elsp, elty] = el2spk(el);
		if(this.state.gr.has(node)) {
			const [[next_nk, next_cs_id], next_edges] = this.state.gr.get(node);
			const here = {
				ctxs: [ // context spks
					[nk_span(next_nk), [NK2ENV.get(next_nk.tag), null]]
					, [elsp, [elty, fw_edge]]
				],
				nodes: []
			};
			const next = (() => { // node spks
				switch(el.tag) {
					case "ArgEdge":
						// acc.push([el.contents[0], [SPANTY.AG_TO_ARG, el]]);
						console.log(next_nk, next_cs_id, next_edges);
						assert(next_nk.tag === 'NKBind');
						const nodes = []
						for(const fw_edge_ of next_edges) {
							const [targ, el_] = fw_edge_;
							switch(el_.tag) {
								case 'BindEdge':
									nodes.push([el_.contents, [SPANTY.NODE.BIND_CALLSITE, fw_edge_]]);
									break;
								case 'RevBindEdge':
									nodes.push([el_.contents, [SPANTY.NODE.BIND_MATCHSITE, fw_edge_]]);
									break;
							}
						}
						return {
							ctxs: [[nk_span(next_nk), [SPANTY.CTX.BIND_FROM_ARG, fw_edge]]]
							, nodes
						};
						// console.log(next_edges);
						// console.log(acc);
						break;
					case "BindEdge":
					case "RevBindEdge":
						assert(next_nk.tag === 'NKApp');
					case "AppEdge":
						// acc.push([el.contents[0], [SPANTY.NODE.AG_TO_BIND]])
						return ag2spks(next_edges);
						break;
				}
			})();
			return spk_join(here, next);
		}
		else {
			return { ctxs: [], nodes: [] };
		}
	}
	render = () => <div onKeyUp={this.keyPressHandler} id="main_root">
		<CodeBlock
			ctx_renderer={this.render_ctx_bar}
			src={this.state.src}
			span_ks={this.state.at_idx < this.state.at_history.size && this.state.gr && (() => {
				const at = this.state.at_history.get(this.state.at_idx);
				const { ctxs, nodes } = this.fw_edge2spks(at);
				console.log(nodes, ctxs);
				return ctxs.concat(nodes);
			})()}
			should_scroll_to={this.should_scroll_to}
			wrap_snip={wrap_snip}
			onSnipClick={this.snipClickHandler}
		/>
	</div>
}