import React from 'react'
import Q from 'q'
import { Map, Set } from 'immutable'
import { id, map_intersect } from './Util'
import CodeBlock from './CodeBlock'

const empty_gr = { jsg_gr: Map(), sccs: Set() };
function key(x) {
	return x.toString();
	// return x.map(x_ => x_.toString()).join('_');
}

export default class extends React.Component {
	constructor(props) {
		super(props);
		
		this.state = {
			st_gr: empty_gr, // { jsg_gr: { <node>: { key: NodeKey, edges: [(int, SrcSpan)] }, sccs: { <scc>: Set<node: int> } } // precalculated sccs are so that JS doesn't have to think too hard
			st_at: null, // ?(node: int)
			src: null, // ?string
			src_req_idx: 0
		}
		window.addEventListener('popstate', this.handle_uri_term);
	}
	componentDidMount() {
		fetch('/static/gr.json').then(a => a.json())
			.then(state_init_ => {
				// for(const scc in state_init.sccs) {
				// 	if(state_init.sccs.hasOwnPropety(scc))
				// 		state_init.sccs.get(scc) = new Set(state_init.sccs.get(scc));
				// }
				const state_init = { st_at: state_init_.st_at.map(key) };
				const sccs = state_init_.st_gr.jsg_sccs.map(Set);
				state_init.st_gr = {
					jsg_gr: state_init_.st_gr.jsg_gr.reduce((m, [k, a]) => m.set(key(k), a), Map()),
					jsg_sccs: sccs.reduce((m, scc) => scc.reduce((m, n) => m.set(key(n), scc), m), Map())
				};
				this.setState({ ...state_init })
			});
	}
	keyPressHandler = e => {
		console.log(e);
	}
	snipClickHandler = (e, s) => {
		console.log(e, s);
		console.log('?');
	}
	componentDidUpdate(pprops, pstate) {
		const diff = {
			at: pstate.st_at !== this.state.st_at,
			gr: pstate.st_gr !== this.state.st_gr,
			src: ((this.state.src === null) !== (pstate.src === null)) || (this.state.src !== null && pstate.src !== null && pstate.src.path !== this.state.src.path)
		};
		
		const at_path = 'Text/Regex/TDFA/CorePattern.hie'; // this.state.st_gr.jsg_gr.get(this.state.st_at[0]).key.span.path;
		if(this.state.src === null && at_path !== null || at_path !== this.state.src.path) {
			const stash_req_idx = this.state.src_req_idx;
			fetch(`/f?n=${encodeURIComponent(at_path)}`)
				.then(r => r.text())
				.then(t => this.setState(st => {
					if(this.state.src_req_idx === stash_req_idx) {
						return {
							src_req_idx: stash_req_idx + 1,
							src: { path: at_path, body: console.log(t.slice(0, 40)) || t }
						};
					}
				}))
		}
	}
	should_scroll_to = (ks) => this.state.st_at !== null && ks.indexOf(this.state.st_at) !== -1
	render = () => <div onKeyUp={this.keyPressHandler}>
		<section id="scc_section">
			<h1></h1>
			<ul id="scc_select">{
				this.state.st_at === null ? null : this.state.st_gr.jsg_sccs.get(this.state.st_at[0]).map(node =>
					<li key={node} className={ node === this.state.st_at[0] ? 'selected' : '' }>
						{node}
					</li>
				)
			}</ul>
		</section>
		<section>
			<CodeBlock
				body={this.state.src && this.state.src.body}
				spans={this.state.st_at && this.state.st_gr && map_intersect(this.state.st_gr.jsg_gr, this.state.st_at)}
				should_scroll_to={this.should_scroll_to}
				wrap_snip={id}
				onSnipClick={this.snipClickHandler}
			/>
		</section>
	</div>
}