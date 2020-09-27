import React from 'react'
import { SnipWrapper } from './MainController'
import { PassthruProps, SnipClickHandler, SnipHoverHandler, MaybeKeyedSubSnip, TParseTree } from './MainContent'
import { List } from 'immutable'
import { jsoneq, any } from './Util'
import * as L from './Lang'
import Snip from './Snip'

interface TProps<Tk> extends PassthruProps<Tk> {
	parsetree?: TParseTree<Tk>,
	snip_refs?: Array<React.RefObject<HTMLAnchorElement>>,
	onSnipHover: SnipHoverHandler<Tk>,
	soft_selected?: Array<L.SpanKey<Tk>>,
	// root_container_el?: HTMLElement,
	wrap_snip: SnipWrapper<Tk>,
};

type TReducerState = [List<React.ReactNode>, number];

export default class<Tk> extends React.PureComponent<TProps<Tk>, {}> {
	reducer: (a: TReducerState, b: [React.ReactNode, MaybeKeyedSubSnip<Tk>], c: number, d: any) => TReducerState = ([l, i], [txt, [sp, sp_ks]], k) => [
			l.push(
				<span key={k}>
					{
						/* need to be very careful with <Snip />. As a PureComponent, none of the inputs can be closures (in any way that influences Snip rendering, that is) */
						sp_ks === undefined
							? txt
							: <Snip<L.SpanKey<Tk>>
								onClick={this.props.onSnipClick}
								fwd_ref={this.props.snip_refs && this.props.snip_refs[i]}
								onMouseEnter={this.props.onSnipHover}
								onMouseLeave={this.props.onSnipHover}
								force_focus={any(spk => any(soft_spk => jsoneq(soft_spk, spk), this.props.soft_selected || []), sp_ks)}
								ks={sp_ks}
								key={sp.toString()}
								// root={this.props.root_container_el}
							>
								{ this.props.wrap_snip(txt, sp_ks) }
							</Snip>
					}
				</span>
			)
			, sp_ks === undefined ? i : i + 1
		]
	render() {
		if(this.props.parsetree === undefined)
			return null;
		else {
			const init: TReducerState = [List(), 0];
			return <span>{this.props.parsetree.reduce(this.reducer, init)}</span>;
		}
	}
}