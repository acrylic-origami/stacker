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
				throw new Error(`Unexpected EdgeLabel exiting AppGroup: ${el.tag}`);
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
		
		// type Loc = (Int, Int)
		// type ISpan = (fileidx: String (show Int), start: Loc, end: Loc)
		// type NodeKey = { tag: "NKApp" | "NKBind", contents: [ISpan] | ISpan }
		// type EdgeLabel = { tag: "ArgEdge" | "AppEdge" | "BindEdge", contents: (ISpan, ISpan) | ISpan }
		// type FWEdge = (Int, EdgeLabel)
		this.state = {
			gr: Map(), // Map<node: int, (NodeKey, [(edge_target: Node, EdgeLabel)])> { <node>: { key: NodeKey, edges: [FWEdge] }
			at: null, // ?FWEdge
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
				const state_init = {
					at: state_init_.at[0],
					gr: state_init_.gr.reduce((m, [k, a]) => m.set(k, a), Map()),
					filelist
				};
				this.setState({ ...state_init })
			});
	}
	keyPressHandler = e => {
		console.log(e);
	}
	snipClickHandler = (e, sp_ks) => {
		// s :: (Span, Map Span (SPANTY, FWEdge)) // first span is the span of the mini region that was clicked
		
		// const sps = sp_ks.map((_k, sp) => sp); // TODO confirm that's the CS id, for my understanding
		// const which = ; // , which = sp_ks.filter((_k, sp) => list1eq(sp, which_sp)).first()[1];
		const c = candidate(sp_ks)[1][1];
		this.setState({
			at: c
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
		
		let at_file = this.state.at[1].contents[0];
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
						const nk_span = next_nk.tag === 'NKBind' ? next_nk.contents.contents : next_nk.contents;
						acc.push([nk_span, [NK2ENV.get(next_nk.tag), null]]);
						
						switch(el.tag) {
							case "ArgEdge":
								// acc.push([el.contents[0], [SPANTY.AG_TO_ARG, el]]);
								assert(next_nk.tag === 'NKBind');
								acc.push([el.contents[1], [SPANTY.CTX.ARG, this.state.at]]);
								acc.push([nk_span, [SPANTY.CTX.BIND_FROM_ARG, this.state.at]]);
								for(const fw_edge_ of next_edges) {
									const [targ, el_] = fw_edge_;
									switch(el_.tag) {
										case 'BindEdge':
											acc.push([el_.contents, [SPANTY.NODE.BIND_CALLSITE, fw_edge_]]);
											break;
										case 'RevBindEdge':
											acc.push([el_.contents, [SPANTY.NODE.BIND_MATCHSITE, fw_edge_]]);
											break;
									}
								}
								// console.log(next_edges);
								// console.log(acc);
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
							case "RevBindEdge":
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