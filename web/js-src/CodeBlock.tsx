import React from 'react'
import { SnipWrapper, SpanKeySnipWrapper } from './MainController'
import { SnipClickHandler, SnipHoverHandler } from './MainContent'
import { TParseTree, ParseTree, MaybeKeyedSubSnip } from './parsetree'
import { List } from 'immutable'
import { any } from './Util'
import * as L from './Lang'
import Snip from './Snip'

export type SnipEventHandler<Tk> = (e: React.SyntheticEvent, k: Array<Tk>) => void;

export interface TProps<Tk> {
	onSnipClick?: SnipEventHandler<Tk>,
	parsetree?: TParseTree<Tk[]>,
	snip_refs?: Array<React.RefObject<HTMLAnchorElement>>,
	onSnipHover?: SnipHoverHandler<Tk>,
	soft_selected?: Tk[],
	// root_container_el?: HTMLElement,
	wrap_snip: SnipWrapper<Tk[]>,
	keycomp: (a?: Tk[], b?: Tk[]) => boolean
};

type TReducerState = [List<React.ReactNode>, number];

export default class<Tk> extends React.PureComponent<TProps<Tk>, {}> {
	reducer: (a: TReducerState, b: [React.ReactNode, L.ISpanKey<Tk[] | undefined>], c: number, d: any) => TReducerState = ([l, i], [txt, [sp, sp_ks]], k) => [
			l.push(
				<span key={k}>
					{
						/* need to be very careful with <Snip />. As a PureComponent, none of the inputs can be closures (in any way that influences Snip rendering, that is) */
						sp_ks === undefined
							? txt
							: <Snip<Tk>
								onClick={this.props.onSnipClick}
								fwd_ref={this.props.snip_refs && this.props.snip_refs[i]}
								onMouseEnter={this.props.onSnipHover}
								onMouseLeave={this.props.onSnipHover}
								force_focus={this.props.keycomp(this.props.soft_selected, sp_ks)}
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
			return <span>{this.props.parsetree.reduce(this.reducer, init)[0]}</span>;
		}
	}
}