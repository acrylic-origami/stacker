import React from 'react'
import Q from 'q'
import { Map, Set } from 'immutable'
import { id, map_intersect, candidate, assert, any, list1eq } from './Util'
import CodeBlock from './CodeBlock'
import { SPANTY, NK2ENV } from './Lang'
import { NUM_SNIP_DEPTH_COLORS } from './const.js'

function span2key(x) {
	return x.toString();
	// return x.map(x_ => x_.toString()).join('_');
}
function ag2spans(ag) {
	// ag represented as edges: [(Node, EdgeLabel)]
	const acc = [];
	for(const fw_edge of ag) {
		// console.log(fwedge, ag)
		const el = fw_edge[1];
		switch(el.tag) {
			case 'ArgEdge':
				acc.push([el.contents[0], [SPANTY.NODE.AG_TO_ARG, fw_edge]]);
				acc.push([el.contents[1], [SPANTY.NODE.ARG, fw_edge]]);
				break;
			case 'AppEdge':
				acc.push([el.contents[0], [SPANTY.NODE.AG_TO_BIND, fw_edge]]);
				acc.push([el.contents[1], [SPANTY.CTX.BIND_FROM_AG, fw_edge]]);
				break;
			default:
				throw new Exception(`Unexpected EdgeLabel exiting AppGroup: ${el.tag}`);
		}
	}
	return acc;
}

function wrap_snip(txt, sp_ks) {
	// console.log(sp_ks);
	const k_counts = sp_ks.reduce((acc, [spty, _el]) => acc.update(spty, 0, i => i + 1), Map());
	return <span className={k_counts.map((cnt, k) => `snip-${k}-${Math.min(NUM_SNIP_DEPTH_COLORS, cnt)}`).join(' ')}>{txt}</span>
}

export default class extends React.Component {
	constructor(props) {
		super(props);
		
		// type NodeKey = { tag: "NKApp" | "NKBind", contents: [Span] | Span }
		// type EdgeLabel = { tag: "ArgEdge" | "AppEdge" | "BindEdge", contents: (Span, Span) | Span }
		// type FWEdge = (Int, EdgeLabel)
		this.state = {
			gr: Map(), // Map<node: int, (NodeKey, [(edge_target: Node, EdgeLabel)])> { <node>: { key: NodeKey, edges: [FWEdge] }
			at: null, // ?FWEdge
			
			src: null, // ?string
			src_req_idx: 0
		}
		window.addEventListener('popstate', this.handle_uri_term);
	}
	componentDidMount() {
		fetch('/static/gr.json')
			.then(a => a.json())
			.then(state_init_ => {
				// for(const scc in state_init.sccs) {
				// 	if(state_init.sccs.hasOwnPropety(scc))
				// 		state_init.sccs.get(scc) = new Set(state_init.sccs.get(scc));
				// }
				const state_init = {
					at: state_init_.at[0],
					gr: state_init_.gr.reduce((m, [k, a]) => m.set(k, a), Map()),
				};
				this.setState({ ...state_init })
			});
	}
	keyPressHandler = e => {
		console.log(e);
	}
	snipClickHandler = (e, s) => {
		// s :: (Span, Map Span (SPANTY, FWEdge)) // first span is the span of the mini region that was clicked
		console.log(e, s);
		console.log('?');
		
		const [_spmini, sp_ks] = s;
		const sps = sp_ks.map(([sp, _ks]) => sp);
		const which_sp = candidate(sps), which = sp_ks.get(which_sp);
		this.setState({
			at: which
		});
		
		// switch(which[0]) {
		// 	case SPANTY.NODE.AG_TO_ARG:
		// 		break;
		// }
	}
	componentDidUpdate(pprops, pstate) {
		const diff = {
			at: pstate.at !== this.state.at,
			gr: pstate.gr !== this.state.gr,
			src: ((this.state.src === null) !== (pstate.src === null)) || (this.state.src !== null && pstate.src !== null && pstate.src.path !== this.state.src.path)
		};
		
		const at_path = 'Text/Regex/TDFA/CorePattern.hie'; // this.state.gr.jsg_gr.get(this.state.at[0]).key.span.path;
		if(this.state.src === null && at_path !== null || at_path !== this.state.src.path) {
			const stash_req_idx = this.state.src_req_idx;
			fetch(`/f?n=${encodeURIComponent(at_path)}`)
				.then(r => r.text())
				.then(t => this.setState(st => {
					if(this.state.src_req_idx === stash_req_idx) {
						return {
							src_req_idx: stash_req_idx + 1,
							src: { path: at_path, body: t }
						};
					}
				}))
		}
	}
	should_scroll_to = (sp_ks) =>
		this.state.at !== null
		&& any(
			k => list1eq(k, this.state.gr.get(this.state.at[0])[0][0].contents)
			, sp_ks.keySeq()
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
	render = () => <div onKeyUp={this.keyPressHandler}>
		<section>
			<CodeBlock
				body={this.state.src && this.state.src.body}
				spans={this.state.at && this.state.gr && (() => {
					const [node, el] = this.state.at;
					const acc = [];
					// spans :: [(Span, (SPANTY, FWEdge))]
					if(this.state.gr.has(node)) {
						const [[next_nk, next_cs_id], next_edges] = this.state.gr.get(node);
						acc.push([next_nk.contents, [NK2ENV.get(next_nk.tag), null]]);
						
						switch(el.tag) {
							case "ArgEdge":
								// acc.push([el.contents[0], [SPANTY.AG_TO_ARG, el]]);
								assert(next_nk.tag === 'NKBind');
								acc.push([el.contents[1], [SPANTY.CTX.ARG, this.state.at]]);
								acc.push([next_nk.contents, [SPANTY.CTX.BIND_FROM_ARG, this.state.at]]);
								for(const [targ, fw_edge_] of next_edges) {
									const el_ = fw_edge_[1];
									assert(el_.tag === 'BindEdge');
									acc.push([el_.contents, [SPANTY.NODE.BIND_CALLSITE, fw_edge_]]);
								}
								break;
							case "AppEdge":
								// acc.push([el.contents[0], [SPANTY.NODE.AG_TO_BIND]])
								acc.push([el.contents[1], [SPANTY.CTX.BIND_FROM_AG, this.state.at]]);
								acc.push.apply(acc, ag2spans(next_edges));
								break;
							case "BindEdge":
								assert(next_nk.tag === 'NKApp');
								acc.push([el.contents, [SPANTY.CTX.BIND_FROM_ARG, this.state.at]]);
								acc.push.apply(acc, ag2spans(next_edges));
								break;
						}
					}
					// debugger;
					return acc;
				})()}
				should_scroll_to={this.should_scroll_to}
				wrap_snip={wrap_snip}
				onSnipClick={this.snipClickHandler}
			/>
		</section>
	</div>
}