import React from 'react'
import Snip from './Snip'
import * as L from './Lang'

const MAX_LINES = 4;

function ppr_loc(loc: L.Loc): string {
	return `(${loc[0]}, ${loc[1]})`;
}

export type TPreview<Tk> = [React.ReactNode, L.ISpanKey<undefined | [undefined | Tk]>]

interface TProps<Tk, Tu> {
	onSnipClick?: (e: React.SyntheticEvent, k: Tk) => void,
	preview: Array<TPreview<Tk>>,
	onClick?: (e: React.SyntheticEvent, u: Tu) => void,
	click_key?: Tu,
	span: L.Span,
	name: string,
	filename: string,
}
interface TState {
	root_container_el?: HTMLElement,
	snip_hovered: boolean
}
export default class<Tk, Tu = undefined> extends React.PureComponent<TProps<Tk, Tu>, TState> {
	constructor(props: TProps<Tk, Tu>) {
		super(props);
		this.state = {
			root_container_el: undefined,
			snip_hovered: false
		};
	}
	protected clickHandler = (e: React.SyntheticEvent): void => {
		if(this.props.onClick !== undefined && this.props.click_key !== undefined)
			this.props.onClick(e, this.props.click_key);
	}
	protected snipHoverHandler = (e: React.SyntheticEvent, k: Array<Tk | undefined>): void => {
		switch(e.type) {
			case 'mouseenter':
				if(k !== undefined) {
					this.setState({ snip_hovered: true });
				}
				break;
			case 'mouseleave':
				this.setState({ snip_hovered: false });
		}
	}
	protected snipClickHandler = (e: React.SyntheticEvent, k: Array<Tk | undefined>) => {
		const k_ = k[0];
		if(k_ !== undefined && this.props.onSnipClick !== undefined) {
			this.props.onSnipClick(e, k_);
		}
	}
	render = () => <li className="ctx-snip" onClick={this.clickHandler}>
		<h3>{this.props.name}</h3>
		<h4>{this.props.filename}</h4>
		<h5>{ppr_loc(this.props.span[1])}&nbsp;&mdash;&nbsp;{ppr_loc(this.props.span[2])}</h5>
		<div className="src-container" ref={e => this.setState({ root_container_el: e || undefined })}>
			<pre>
				<code className="language-haskell hljs">
					&hellip;{this.props.preview.map(([txt, [sp, k]]) =>
						k === undefined
							? <span key={sp.toString()}>{txt}</span>
							: <Snip<Tk | undefined>
									onClick={this.snipClickHandler}
									onMouseLeave={this.snipHoverHandler}
									onMouseEnter={this.snipHoverHandler}
									ks={[k[0]]}
									key={sp.toString()}
									root={this.state.root_container_el}
									scroll_idx={0}
									className={
										(this.state.snip_hovered
											? 'focused'
											: '')
										+ (k[0] === undefined
											? 'unpointer'
											: '')
									}
								>
								{txt}
							</Snip>
					)}&hellip;
				</code>
			</pre>
		</div>
	</li>
}
