import React from 'react'
import Q from 'q'
import hljs from 'highlight.js/lib/core'
import hs from 'highlight.js/lib/languages/haskell'
import IntervalTree from '@flatten-js/interval-tree'
import Snip from './Snip'
import { mk_span_chars, mk_parsetree } from './parsetree'
import { span_contains, map_intersect, candidate, compare } from './Util'
import { Set, Map } from 'immutable'
import { MinHeap } from 'mnemonist'

hljs.registerLanguage('haskell', hs);

// function snip(src, spans) {}

export default class extends React.Component {
	constructor(props) {
		super(props);
		// props: src text, list of snippets with keys, arbitrary tag wrapping logic based on keys
		// looks like state might have to be... hmm. This differentiation is tricky, because I want to be able to be responsible in the parent to decide the rendering, and not need too much specialization here
		// so a pre-req is thinking if I'm also going to render cliques, 
		// I think that a React render... hmm, if the props and state are identical, does it not render?
		// TIL no. Cool! PureComponent is used for that. Should I really inject a dependency on the top-level state? Really gutting this thing. 
		// I also need an optional focus on a specific snippet
		// It's just that I want to render and focus on groups. 
		// is that all? Focus is just because I need DOM manipulation to scroll places. Am I going to maintain a huge repository of refs? Yeah... probably as their own components with foci... accessing the global window object after being given focus. Eugh. At least the state transition only happens once, so the scroll event also only happens once. Am I going to repaint the whole thing just because the element re-renders though? Maybe I do need to use PureComponent, or implement my own `shouldComponentRender`.
		// that might not be necessary, because thanks to the reconciliation rules, the `render` keeps all the nodes unchanged unless we really get a source or highlights change, where we'd need to re-highlight anyways. in this simple model the updates and need for update are aligned.
		// i just need to write the right condition to make it only re-render the highlight in those instances.
		// i'd really like to make a pure function that determines how the keys are rendered, and there's no reason why I can't: the only one I can't really do is opacity based on a selected group because it's tied to state of what group is selected, so I'll just implement that later
		// I probably need to push a semantics to the keys somehow. Mainly I need to separate reasons from actual attributes, that's the least difference. It also makes a difference when it's bound to an argument or another binding, which leaks a bit of the abstraction into this module, which should be a dumb renderer, just needing to preserve keys
		// then that's it, just an interfacing contract to put the key first, then the span, in a tuple
		/*
		I need to know if the span is a node, or part of an edge. We can just enumerate them: the spans can be:
		- elements in appgroups
			- .. that point to arguments (arg edge meta)
			- .. that point to binds (app edge meta)
		- arguments (arg edge meta)
		- bindings arg-looking (bind nodes, arg edge targets)
		- bindings app-looking (app edge meta)
		
		these are all associated to 
		*/
		/*
		props :: {
			body: String,
			spans: [(Span, k)],
			should_scroll_to: [k] -> Bool,
			wrap_snip: String -> [k] -> React.Component
		}
		*/
		this.state = {
			// src_snips: [], // [(ColSpan, ?(Map Span k))] // intermediate state so that re-snipping and re-rendering is decoupled from the props upstairs changing inconsequentially
			root_container_el: null,
			parsetree: null, // 
			snip_focuses: null,
			hljs_result: null
		};
	}
	componentDidUpdate(pprops, pstate) {
		const diff = {
			body: pprops.body !== this.props.body,
			spans: pprops.spans !== this.props.spans,
			snip_focuses: pstate.snip_focuses !== this.state.snip_focuses,
			hljs_result: pstate.hljs_result !== this.state.hljs_result,
		};
		if(diff.body) {
			this.setState({ hljs_result: hljs.highlight('haskell', this.props.body.raw) });
		}
		if(diff.hljs_result || diff.spans) {
			this.update_parsetree();
		}
		// requestAnimationFrame(e => {
		// 	hljs.highlightBlock(this.src_ref.current);
		// 	// const a = hljs.highlight('haskell', 'module A where')
		// 	// debugger;
		// }); // assume synchronous
	}
	get_src_snips() {
		if(this.props.body != null && this.props.spans != null) {
			// if(this.props.spans.length > 100) return [];
			const span_chars = mk_span_chars(this.props.body.lines, this.props.spans);
			
			const I = []; // list of disjoint merged intervals
			Set().withMutations(vals => {
				const ends = new MinHeap((a, b) => compare(a.key[1], b.key[1]));
				const starts = span_chars.sort((a, b) => compare(a.key[0], b.key[0]));
				starts.push({ key: [Infinity, Infinity], value: null }); // infinity node to help tie all the ends at the... end
				
				let last_il = null;
				for(const start of starts) {
					while(ends.size > 0 && ends.peek().key[1] <= start.key[0]) {
						const end = ends.pop();
						if(last_il !== end.key[1])
							I.push({ key: [last_il, end.key[1]], value: vals.toArray() });
						
						vals.delete(end.value);
						last_il = end.key[1];
					}
					if(last_il !== null && last_il !== start.key[0] && vals.size > 0) {
						I.push({ key: [last_il, start.key[0]], value: vals.toArray() });
					}
					last_il = start.key[0];
					ends.push(start);
					vals.add(start.value);
				}
				// debugger;
				// console.log(I);
				// I.pop(); // remove the dummy between the last node and the infinity node
			});
			
			// const I = T.items; // [{ key: (Int, Int), value: [(Span, k)] }]
			// console.log(span_chars, I.map(({ key }) => key));
			if(I.length > 0) {
				// I.sort((l, r) => compare(l.key[0], r.key[0])); // sort intervals in ascending order
				const S = [[[0, I[0].key[0]], null]];
				for(let i = 0; i < I.length; i++) {
					S.push(
						[[I[i].key[0], I[i].key[1]], Map(I[i].value)],
					);
					if(i >= I.length - 1 || I[i].key[1] !== I[i + 1].key[0]) // filter zero-length spans
						S.push([[I[i].key[1], i >= I.length - 1 ? undefined : I[i + 1].key[0]], null])
				}
				return S;
			}
			else {
				return [];
			}
		}
		else return null;
	}
	
	update_parsetree() {
		const src_snips = this.get_src_snips();
		if(src_snips !== null && this.state.hljs_result !== null) {
			this.setState({
				parsetree: mk_parsetree(this.state.hljs_result.emitter.root, src_snips)
			});
		}
	}
	render_highlight() {
		if(this.state.parsetree === null)
			return null;
		else return <span>{this.state.parsetree.map(([txt, sp, sp_ks], k) => <span key={k}>
			{
				/* need to be very careful with <Snip />. As a PureComponent, none of the inputs can be closures (in any way that influences Snip rendering, that is) */
				sp_ks == null
					? txt
					: <Snip
						onClick={this.props.onSnipClick}
						onMouseEnter={this.snipHoverHandler}
						onMouseLeave={this.snipHoverHandler}
						ks={sp_ks}
						key={sp.toString()}
						className={
							sp_ks.has(this.state.snip_focuses)
							? 'focused'
							: ''
						}
						root={this.state.root_container_el}
						scroll_idx={this.props.should_scroll_to(sp_ks)} // just 0 or 1 for now: include re-focusing as needed
					>
						{ this.props.wrap_snip(txt, sp_ks) }
					</Snip>
			}
		</span>)}</span>;
	}
	snipHoverHandler = (e, sp_ks) => {
		// console.log(e, sp_ks);
		switch(e.type) {
			case 'mouseenter':
				const c = candidate(sp_ks);
				if(c != null) {
					const [sp, _k] = c;
					this.setState({ snip_focuses: sp })
				}
				break;
			case 'mouseleave':
				this.setState({ snip_focuses: null })
				break;
		}
	}
	rootRefChangeHandler = root_container_el => this.setState({ root_container_el });
	// snipClickHandler = (e, ks) => this.props.onSnipClick(this.candidate(ks));
	// the trickiest part is to figure out a way to distinguish the type of the span of the "reason" given that the list of elements coming in are completely agnostic to that. So find some way to mark it, plus add all the accompanying data in an elegant way. Otherwise, just pushing in keys of all the nodes that these are pointing to, hopefully without intersection (ah wait. There will be intersection with duplicated names. So... maybe not a map? As if I needed it to be unique anyways. multimap? eh. I expect them to be unique to each span. So no. No multimap. Really the spans should be their own keys. I just don't like to serialize and deserialize the data just for mapping efficiency. Although Immutable Maps can do better than that. So just key based on the straight spans. yeah.)
	
	render = () => <div>
		<section>
			{this.props.ctx_renderer(this.state.hljs_result)}
		</section>
		<section ref={this.rootRefChangeHandler} className="src-container">
			<pre>
				<code id="src_root" className="language-haskell hljs">
					{ this.render_highlight() }
				</code>
			</pre>
		</section>
	</div>
}