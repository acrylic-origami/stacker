import React from 'react'
import Q from 'q'
import { offsetTo } from './Util'
import hljs from 'highlight.js/lib/core'
import hs from 'highlight.js/lib/languages/haskell'
import IntervalTree from '@flatten-js/interval-tree'
import Snip from './Snip'
import { mk_span_chars, mk_parsetree } from './parsetree'
import { map_intersect, compare, any } from './Util'
import { span_contains, candidate, spaneq } from './Lang'
import * as L from './Lang'
import * as U from './Util'
import CodeBlock from './CodeBlock'
import { Set, Map } from 'immutable'
import { MinHeap } from 'mnemonist'

hljs.registerLanguage('haskell', hs);

// function snip(src, span_ks) {}

// NOTE: keeping this props list  pure to only changing when the snips actually change visibly is VERY IMPORTANT TO PERFORMANCE.
export type SnipClickHandler<Tk> = (e: React.SyntheticEvent, k: Array<L.SpanKey<Tk>>) => void;
export interface PassthruProps<Tk> {
	onSnipClick: SnipClickHandler<Tk>,
}

interface TProps<Tk> extends PassthruProps<Tk> {
	src?: L.Src,
	span_ks: Array<L.SpanKey<Tk>>,
	// should_scroll_to: (ks: Array<L.SpanKey<Tk>>) => boolean,
	wrap_snip: (t: React.ReactNode, ks: Array<L.SpanKey<Tk>>) => React.ReactNode,
	ctx_renderer: (hljs_result: any) => React.ReactNode,
	scroll_to?: L.Span
}

// type KeyedSubSnip<Tk> = L.ISpanKey<Array<L.SpanKey<Tk>>> // outer span for the sub-snippet location (mostly to supply unique React keys), inner span for the snippet location
export type MaybeKeyedSubSnip<Tk> = L.ISpanKey<Array<L.SpanKey<Tk>> | undefined> // outer span for the sub-snippet location (mostly to supply unique React keys), inner span for the snippet location
// NOTE: _all_ of these are props-driven, NONE OF THESE ARE EVENT-DRIVEN. This means that all the work this component does is _totally driven by props_.
export type TParseTree<Tk> = Array<[React.ReactNode, MaybeKeyedSubSnip<Tk>]>
export type SnipHoverHandler<Tk> = (e: React.SyntheticEvent, sp_ks: Array<L.SpanKey<Tk>>) => void
interface TState<Tk> {
	root_container_el?: HTMLElement,
	parsetree?: TParseTree<Tk>, 
	snip_focuses?: L.Span,
	hljs_result: any,
	snip_refs: Array<React.RefObject<HTMLAnchorElement>>
}

export default class<Tk> extends React.PureComponent<TProps<Tk>, TState<Tk>> {
	public readonly state : Readonly<TState<Tk>> = {
		root_container_el: undefined,
		parsetree: undefined,
		snip_focuses: undefined,
		hljs_result: undefined,
		snip_refs: []
	}
	public constructor(props: TProps<Tk>) {
		super(props);
	}
	public componentDidUpdate(pprops: TProps<Tk>, pstate: TState<Tk>): void {
		const diff = {
			src: pprops.src !== this.props.src,
			span_ks: pprops.span_ks !== this.props.span_ks,
			snip_focuses: pstate.snip_focuses !== this.state.snip_focuses,
			hljs_result: pstate.hljs_result !== this.state.hljs_result,
		};
		if(diff.src && this.props.src !== undefined) {
			this.setState({ hljs_result: hljs.highlight('haskell', this.props.src.body.raw) });
		}
		if(diff.hljs_result || diff.span_ks) {
			this.update_parsetree();
		}
		if(diff.src || diff.span_ks) {
			const scroll_to = this.props.scroll_to;
			if(this.state.parsetree !== undefined && scroll_to !== undefined) {
				let i = 0;
				for(const [_n, [_isp, m_spk]] of this.state.parsetree) {
					if(m_spk !== undefined) {
						console.log(scroll_to, m_spk[0][0]);
						if(any(([sp, k]) => span_contains(scroll_to, sp), m_spk)) {
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
	protected get_src_snips(): undefined | Array<MaybeKeyedSubSnip<Tk>> {
		if(this.props.src !== undefined && this.props.span_ks !== undefined) {
			// if(this.props.span_ks.length > 100) return [];
			const span_chars = mk_span_chars(this.props.src.body.lines, this.props.span_ks);
			
			const I = Array<U.DictDbl<L.ISpan, Array<L.SpanKey<Tk>>>>(); // list of disjoint merged intervals
			Set<L.SpanKey<Tk>>().withMutations(vals => {
				const ends = new MinHeap<U.DictDbl<L.ISpan, L.SpanKey<Tk>>>((a, b) => compare(a.key[1], b.key[1]));
				const starts = span_chars.sort((a, b) => compare(a.key[0], b.key[0]));
				starts.push({ key: [Infinity, Infinity], value: undefined as any }); // infinity node to help tie all the ends at the... end
				
				let last_il = undefined;
				for(const start of starts) {
					while(ends.size > 0 && (ends.peek()?.key[1] || Infinity) <= start.key[0]) {
						const end = ends.pop();
						if(end !== undefined) {
							if(last_il !== undefined && last_il !== end.key[1])
								I.push({ key: [last_il, end.key[1]], value: vals.toArray() });
							
							vals.delete(end.value);
							last_il = end.key[1];
						}
					}
					if(last_il !== undefined && last_il !== start.key[0] && vals.size > 0) {
						I.push({ key: [last_il, start.key[0]], value: vals.toArray() });
					}
					last_il = start.key[0];
					ends.push(start);
					vals.add(start.value);
				}
				// debugger;
				// I.pop(); // remove the dummy between the last node and the infinity node
			});
			
			// const I = T.items; // [{ key: (Int, Int), value: [(Span, k)] }]
			if(I.length > 0) {
				// I.sort((l, r) => compare(l.key[0], r.key[0])); // sort intervals in ascending order
				const S : Array<MaybeKeyedSubSnip<Tk>> = [[[0, I[0].key[0]], undefined]];
				for(let i = 0; i < I.length; i++) {
					S.push(
						[[I[i].key[0], I[i].key[1]], I[i].value],
					);
					if(i >= I.length - 1 || I[i].key[1] !== I[i + 1].key[0]) // filter zero-length span_ks
						S.push([
							[I[i].key[1], i >= I.length - 1 ? Infinity : I[i + 1].key[0]]
							, undefined
						])
				}
				return S;
			}
			else {
				return [];
			}
		}
		else return undefined;
	}
	
	update_parsetree(): void {
		const src_snips = this.get_src_snips();
		if(src_snips !== undefined && this.state.hljs_result !== undefined) {
			const parsetree = mk_parsetree(this.state.hljs_result.emitter.root, src_snips);
			this.setState({
				snip_refs: parsetree
					.filter(([_n, [_isp, m_spk]]) => m_spk !== undefined)
					.map(_ => React.createRef()),
				parsetree
			});
		}
	}
	snipHoverHandler: SnipHoverHandler<Tk> = (e, sp_ks) => {
		switch(e.type) {
			case 'mouseenter':
				const c = candidate(sp_ks as any); // TODO: OOPS.
				if(c !== undefined) {
					const [sp, _k] = c;
					this.setState({ snip_focuses: sp })
				}
				break;
			case 'mouseleave':
				this.setState({ snip_focuses: undefined })
				break;
		}
	}
	rootRefChangeHandler = (root_container_el: HTMLElement): void => this.setState({ root_container_el });
	// snipClickHandler = (e, ks) => this.props.onSnipClick(this.candidate(ks));
	// the trickiest part is to figure out a way to distinguish the type of the span of the "reason" given that the list of elements coming in are completely agnostic to that. So find some way to mark it, plus add all the accompanying data in an elegant way. Otherwise, just pushing in keys of all the nodes that these are pointing to, hopefully without intersection (ah wait. There will be intersection with duplicated names. So... maybe not a map? As if I needed it to be unique anyways. multimap? eh. I expect them to be unique to each span. So no. No multimap. Really the span_ks should be their own keys. I just don't like to serialize and deserialize the data just for mapping efficiency. Although Immutable Maps can do better than that. So just key based on the straight span_ks. yeah.)
	
	render = () => <section id="main_content">
		<section id="context_bar">
			{this.props.ctx_renderer(this.state.hljs_result)}
		</section>
		<section id="src_root_container">
			<header>
				<h1>{this.props.src && this.props.src.path}</h1>
			</header>
			<section ref={this.rootRefChangeHandler} className="src-container">
				<pre>
					<code id="src_root" className="language-haskell hljs">
						<CodeBlock<Tk>
							parsetree={this.state.parsetree}
							snipHoverHandler={this.snipHoverHandler}
							onSnipClick={this.props.onSnipClick}
							wrap_snip={this.props.wrap_snip}
							/>
					</code>
				</pre>
			</section>
		</section>
	</section>
}