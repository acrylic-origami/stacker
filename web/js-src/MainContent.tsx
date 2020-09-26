import React from 'react'
import Q from 'q'
import { offsetTo } from './Util'
import IntervalTree from '@flatten-js/interval-tree'
import Snip from './Snip'
import { mk_span_chars, mk_parsetree } from './parsetree'
import { map_intersect, compare, any } from './Util'
import { span_contains, candidate, spaneq, span_intersects } from './Lang'
import * as L from './Lang'
import CodeBlock from './CodeBlock'
import { Set, Map } from 'immutable'
import { TParseTree } from './MainController';


// function snip(src, span_ks) {}

// NOTE: keeping this props list  pure to only changing when the snips actually change visibly is VERY IMPORTANT TO PERFORMANCE.
export type SnipClickHandler<Tk> = (e: React.SyntheticEvent, k: Array<L.SpanKey<Tk>>) => void;
export interface PassthruProps<Tk> {
	onSnipClick: SnipClickHandler<Tk>,
}

interface TProps<Tk> extends PassthruProps<Tk> {
	src?: L.Src,
	parsetree?: TParseTree<Tk>,
	// should_scroll_to: (ks: Array<L.SpanKey<Tk>>) => boolean,
	wrap_snip: (t: React.ReactNode, ks: Array<L.SpanKey<Tk>>) => React.ReactNode,
	onSnipHover: SnipHoverHandler<Tk>,
	scroll_to?: L.Span
}

// type KeyedSubSnip<Tk> = L.ISpanKey<Array<L.SpanKey<Tk>>> // outer span for the sub-snippet location (mostly to supply unique React keys), inner span for the snippet location
// NOTE: _all_ of these are props-driven, NONE OF THESE ARE EVENT-DRIVEN. This means that all the work this component does is _totally driven by props_.
export type SnipHoverHandler<Tk> = (e: React.SyntheticEvent, sp_ks: Array<L.SpanKey<Tk>>) => void
interface TState<Tk> {
	root_container_el?: HTMLElement,
	snip_focuses?: L.Span,
	snip_refs: Array<React.RefObject<HTMLAnchorElement>>,
	parsetree?: TParseTree<Tk>,
}

export default class<Tk> extends React.PureComponent<TProps<Tk>, TState<Tk>> {
	public readonly state : Readonly<TState<Tk>> = {
		root_container_el: undefined,
		snip_focuses: undefined,
		parsetree: undefined,
		snip_refs: []
	}
	public constructor(props: TProps<Tk>) {
		super(props);
	}
	public componentDidUpdate(pprops: TProps<Tk>, pstate: TState<Tk>): void {
		const diff = {
			src: pprops.src !== this.props.src,
			scroll_to: pprops.scroll_to !== this.props.scroll_to,
			// span_ks: pprops.span_ks !== this.props.span_ks,
			snip_focuses: pstate.snip_focuses !== this.state.snip_focuses,
			// hljs_result: pstate.hljs_result !== this.state.hljs_result,
			props_parsetree: pprops.parsetree !== this.props.parsetree,
			state_parsetree: pstate.parsetree !== this.state.parsetree,
		};
		// if(diff.src || diff.span_ks) {
		// think a bit further on this diff condition: is this the only time we should check if we should scroll given all the races and dependencies that might force a re-scroll?
		if(diff.props_parsetree) {
			this.setState({
				parsetree: this.props.parsetree,
				snip_refs: (
					this.props.parsetree && this.props.parsetree
						.filter(([_n, [_isp, m_spk]]) => m_spk !== undefined)
						.map(_ => React.createRef())
				) || []
			});
		}
		if(diff.state_parsetree || diff.scroll_to) {
			const scroll_to = this.props.scroll_to;
			if(this.state.parsetree !== undefined && scroll_to !== undefined) {
				let i = 0;
				for(const [_n, [_isp, m_spk]] of this.state.parsetree) {
					// console.log(JSON.stringify(scroll_to), JSON.stringify(_isp), JSON.stringify(m_spk));
					if(m_spk !== undefined) {
						if(any(([sp, k]) => span_intersects(scroll_to, sp), m_spk)) {
							const ref = this.state.snip_refs[i];
							const root = this.state.root_container_el;
							if(ref !== undefined && ref.current !== null && root !== undefined) {
								const offset = offsetTo(ref.current, root);
								if(offset !== undefined) {
									// offset can be undefined in between renders
									const box = root.getBoundingClientRect();
									root.scroll({
										left: offset[0] - box.width / 2,
										top: offset[1] - box.height / 2,
										behavior: 'smooth'
									});
								}
							}
							break;
						}
						i++;
					}
				}
			}
		}
		// requestAnimationFrame(e => {
		// 	hljs.highlightBlock(this.src_ref.current);
		// 	// const a = hljs.highlight('haskell', 'module A where')
		// 	// debugger;
		// }); // assume synchronous
	}
	// snipHoverHandler: SnipHoverHandler<Tk> = (e, sp_ks) => {
	// 	switch(e.type) {
	// 		case 'mouseenter':
	// 			const c = candidate(sp_ks as any); // TODO: OOPS.
	// 			if(c !== undefined) {
	// 				const [sp, _k] = c;
	// 				this.setState({ snip_focuses: sp })
	// 			}
	// 			break;
	// 		case 'mouseleave':
	// 			this.setState({ snip_focuses: undefined })
	// 			break;
	// 	}
	// }
	rootRefChangeHandler = (root_container_el: HTMLElement): void => this.setState({ root_container_el });
	// snipClickHandler = (e, ks) => this.props.onSnipClick(this.candidate(ks));
	// the trickiest part is to figure out a way to distinguish the type of the span of the "reason" given that the list of elements coming in are completely agnostic to that. So find some way to mark it, plus add all the accompanying data in an elegant way. Otherwise, just pushing in keys of all the nodes that these are pointing to, hopefully without intersection (ah wait. There will be intersection with duplicated names. So... maybe not a map? As if I needed it to be unique anyways. multimap? eh. I expect them to be unique to each span. So no. No multimap. Really the span_ks should be their own keys. I just don't like to serialize and deserialize the data just for mapping efficiency. Although Immutable Maps can do better than that. So just key based on the straight span_ks. yeah.)
	
	render = () => 
		<section id="src_root_container">
			<header>
				<h1>{this.props.src && this.props.src.path}</h1>
			</header>
			<section ref={this.rootRefChangeHandler} className="src-container">
				<pre>
					<code id="src_root" className="language-haskell hljs">
						<CodeBlock<Tk>
							snip_refs={this.state.snip_refs}
							parsetree={this.state.parsetree}
							onSnipHover={this.props.onSnipHover}
							onSnipClick={this.props.onSnipClick}
							wrap_snip={this.props.wrap_snip}
							/>
					</code>
				</pre>
			</section>
		</section>
}