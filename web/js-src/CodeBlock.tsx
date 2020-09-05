import React from 'react'
import Q from 'q'
import hljs from 'highlight.js/lib/core'
import hs from 'highlight.js/lib/languages/haskell'
import IntervalTree from '@flatten-js/interval-tree'
import Snip from './Snip'
import { mk_span_chars, mk_parsetree } from './parsetree'
import { map_intersect, compare, any } from './Util'
import { span_contains, candidate, spaneq } from './Lang'
import * as L from './Lang'
import * as U from './Util'
import { Set, Map } from 'immutable'
import { MinHeap } from 'mnemonist'

hljs.registerLanguage('haskell', hs);

// function snip(src, span_ks) {}
interface TProps<Tk> {
	src?: L.Src,
	span_ks: Array<L.SpanKey<Tk>>,
	onSnipClick: (e: React.SyntheticEvent, k: Array<L.SpanKey<Tk>>) => void,
	should_scroll_to: (ks: Array<L.SpanKey<Tk>>) => boolean,
	wrap_snip: (t: React.ReactNode, ks: Array<L.SpanKey<Tk>>) => React.ReactNode,
	ctx_renderer: (hljs_result: any) => React.ReactNode
}
// type KeyedSubSnip<Tk> = L.ISpanKey<Array<L.SpanKey<Tk>>> // outer span for the sub-snippet location (mostly to supply unique React keys), inner span for the snippet location
type MaybeKeyedSubSnip<Tk> = L.ISpanKey<Array<L.SpanKey<Tk>> | undefined> // outer span for the sub-snippet location (mostly to supply unique React keys), inner span for the snippet location
interface TState<Tk> {
	root_container_el?: HTMLElement,
	parsetree?: Array<[React.ReactNode, MaybeKeyedSubSnip<Tk>]>, 
	snip_focuses?: L.Span,
	hljs_result: any
}

export default class<Tk> extends React.Component<TProps<Tk>, TState<Tk>> {
	public readonly state : Readonly<TState<Tk>> = {
		root_container_el: undefined,
		parsetree: undefined,
		snip_focuses: undefined,
		hljs_result: undefined
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
			this.setState({
				parsetree: mk_parsetree(this.state.hljs_result.emitter.root, src_snips)
			});
		}
	}
	render_highlight(): React.ReactNode {
		if(this.state.parsetree === undefined)
			return undefined;
		else return <span>{this.state.parsetree.map(([txt, [sp, sp_ks]], k) => <span key={k}>
			{
				/* need to be very careful with <Snip />. As a PureComponent, none of the inputs can be closures (in any way that influences Snip rendering, that is) */
				sp_ks == undefined
					? txt
					: <Snip<L.SpanKey<Tk>>
						onClick={this.props.onSnipClick}
						onMouseEnter={this.snipHoverHandler}
						onMouseLeave={this.snipHoverHandler}
						ks={sp_ks}
						key={sp.toString()}
						className={
							this.state.snip_focuses !== undefined && any(([sp_, _ks]) => this.state.snip_focuses !== undefined && spaneq(sp_, this.state.snip_focuses), sp_ks)
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
	snipHoverHandler = (e: React.SyntheticEvent, sp_ks: Array<L.SpanKey<Tk>>): void => {
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
		<section ref={this.rootRefChangeHandler} id="src_root_container">
			<header>
				<h1>{this.props.src && this.props.src.path}</h1>
			</header>
			<section className="src-container">
				<pre>
					<code id="src_root" className="language-haskell hljs">
						{ this.render_highlight() }
					</code>
				</pre>
			</section>
		</section>
	</section>
}