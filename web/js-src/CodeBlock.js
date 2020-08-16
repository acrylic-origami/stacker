import React from 'react'
import Q from 'q'
import hljs from 'highlight.js/lib/core'
import hs from 'highlight.js/lib/languages/haskell'
import IntervalTree from '@flatten-js/interval-tree'
import Snip from './Snip'
import { span_contains, map_intersect } from './Util'
import { Set, Map } from 'immutable'

hljs.registerLanguage('haskell', hs);

function ivl_split(t) {
	// create disjoint open intervals with pooled keys
	// accept list with two intervals, for fast identification of disjoint
	// please disregard how much this function sucks
	console.log(t);
	const [a, b] = t;
	const ck = a[2].concat(b[2]);
	if(a[0] === b[0] && a[1] === b[1]) {
		return [[a[0], a[1], ck]];
	}
	else if(a[0] === b[0]) {
		if(a[1] < b[1]) {
			return [
				[a[0], a[1], ck],
				[a[1], b[1], b[2]]
			];
		}
		else {
			return [
				[b[0], b[1], ck],
				[b[1], a[1], b[2]]
			];
		}
	}
	else if(a[1] === b[1]) {
		if(a[1] < b[1]) {
			return [
				a,
				[a[1], b[1], ck]
			];
		}
		else {
			return [
				b,
				[b[1], a[1], ck]
			];
		}
	}
	else if(a[0] > b[1] && a[1] > b[1] || a[0] < b[0] && a[1] < b[0]) {
		// disjoint
		return t;
	}
	else if(a[0] < b[0]) {
		return [
			a,
			[b[0], a[1], ck],
			b
		]
	}
	else {
		return [
			b,
			[a[0], b[1], ck],
			a
		]
	}
}

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
		/* props :: Hashable k => {
			body: String,
			spans: Map k ((Int, Int), (Int, Int)),
			should_scroll_to: [k] -> Bool,
			wrap_snip: String -> [k] -> React.Component
		}
		*/
		this.state = {
			src_snips: [] // intermediate state so that re-snipping and re-rendering is decoupled from the props upstairs changing inconsequentially
		};
		this.src_ref = React.createRef();
	}
	componentDidUpdate(pprops, pstate) {
		const diff = {
			body: pprops.body !== this.props.body,
			src_snips: pstate.src_snips !== this.state.src_snips,
			spans: pprops.spans !== this.props.spans
		};
		if(diff.body || diff.spans) {
			this.update_src_snips();
		}
		if(diff.src_snips) {
			requestAnimationFrame(e => {
				hljs.highlightBlock(this.src_ref.current)
			}); // assume synchronous
		}
	}
	update_src_snips() {
		if(this.props.body != null && this.props.spans != null) {
			const cum_line_chars = this.props.body.split('\n').reduce((acc, l) => acc.push(acc[acc.length - 1] + l.length + 1) === -1 || acc, [0]); // +1 for \n, faster push than concat
			// spans.sort(([la, ca], [lb, cb]) => la > lb || (la === lb && ca > cb));
			// const spans = this.state.st_at.map(e => this.state.st_gr.jsg_gr.get(e)[1])
			// 	.filter(l => !!l.length)[0]
			// 	.map(([_, sp]) => sp)
			const span_chars = this.props.spans.toArray().map(
				([k, [[ty, [[ll, lc], [rl, rc]]], _edges]]) =>
					[cum_line_chars[ll - 1] + lc - 1, cum_line_chars[rl - 1] + rc - 1, [k]]
			);
			const T = new IntervalTree();
			for(let i = 0; i < span_chars.length; i++) {
				const hits = T.search(span_chars[i].slice(0, 2), ((v, k) => ({ key: k, value: v })));
				const Q = [span_chars[i]];
				for(const hit of hits) {
					for(let _ = 0; _ < Q.length; _++) {
						Q.push.apply(Q, ivl_split([Q.shift(), [hit.key.low, hit.key.high, hit.value]]));
						// if(u !== t) {
						// }
						// else {
						// 	Q.push(t[0]); // optimize for the fact the tree should already be disjoint
						// }
					}
				}
				for(const hit of hits)
					T.remove(hit.key);
				for(const q of Q)
					T.insert(q.slice(0, 2), q[2]);
			}
			const I = T.items; // [((Int, Int), [Int])]
			const D = Q.defer();
			console.log(I);
			if(I.length > 0) {
				I.sort((l, r) => l.key[0] > r.key[0]); // sort intervals in ascending order
				const S = [[[0, I[0].key[0]], null]];
				for(let i = 0; i < I.length; i++) {
					S.push(
						[[I[i].key[0], I[i].key[1]], I[i].value],
						[[I[i].key[1], i >= I.length - 1 ? undefined : I[i + 1].key[0]], null]
					);
				}
				this.setState(st => {
					D.resolve(st);
					return { src_snips: S };
				});
			}
			else {
				this.setState(st => {
					D.resolve(st);
					return { src_snips: [] };
				});
			}
			return D.promise;
		}
	}
	
	candidate(ks) {
		const sps = map_intersect(this.props.spans, ks);
		const big_sps = sps.reduce((s, spa, ka) => sps.reduce((s_, spb, kb) => {
			if(!s_.has(ka) && spa !== spb && span_contains(spa[0], spb[0])) {
				return s_.add(ka); // eliminate all spans that contain other spans
			}
			else {
				return s_;
			}
		}, s), Set());
		return sps.deleteAll(big_sps).toArray[0]; // then pick one of the okay spans at random
	}
	snipHoverHandler = (e, ks) => {
		switch(e.type) {
			case 'mouseenter':
				const sp = this.candidate(ks);
				this.setState({ snip_focuses: sp[0] })
				break;
			case 'mouseleave':
				this.setState({ snip_focuses: null })
				break;
		}
	}
	snipClickHandler = (e, ks) => this.props.onSnipClick(this.candidate(ks));
	
	render = () => 
		<pre>
			<code ref={this.src_ref} id="src_root" className="language-haskell">
				{
					this.state.src_snips.map(([sp, ks]) =>
							ks === null
								? this.props.body.slice(sp[0], sp[1])
								: <Snip
									onClick={this.props.snipClickHandler}
									onMouseEnter={this.snipHoverHandler}
									onMouseLeave={this.snipHoverHandler}
									ks={ks}
									key={sp.toString()}
									className={
										ks.indexOf(this.state.snip_focuses) !== -1
										? 'focused'
										: ''
									}
									scroll_idx={this.props.should_scroll_to(ks)} // just 0 or 1 for now: include re-focusing as needed
								>
									{ this.props.wrap_snip(this.props.body.slice(sp[0], sp[1]), ks) }
								</Snip>
						)
				}
			</code>
		</pre>
}