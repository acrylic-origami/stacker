import React from 'react'
import Snip from './Snip'
import { mk_span_chars, mk_parsetree } from './parsetree'

const MAX_LINES = 4;

function ppr_loc(loc) {
	return `(${loc[0]}, ${loc[1]})`;
}

export default class extends React.PureComponent {
	constructor(props) {
		super(props);
		this.state = {
			root_container_el: null,
			snip_hovered: false
		};
	}
	clickHandler = e => {
		if(this.props.onClick != null)
			this.props.onClick(this.props.click_key);
	}
	snipHoverHandler = (e, k) => {
		switch(e.type) {
			case 'mouseenter':
				if(k[0] !== null) {
					this.setState({ snip_hovered: true });
				}
				break;
			case 'mouseleave':
				this.setState({ snip_hovered: false });
		}
	}
	render = () => <li className="ctx-snip" onClick={this.clickHandler}>
		<h3>{this.props.name}</h3>
		<h4>{this.props.filename}</h4>
		<h5>{ppr_loc(this.props.span[1])}&nbsp;&mdash;&nbsp;{ppr_loc(this.props.span[2])}</h5>
		<div className="src-container" ref={e => this.setState({ root_container_el: e })}>
			<pre>
				<code className="language-haskell hljs">
					&hellip;{this.props.preview.map(([txt, sp, k]) =>
						k === null
							? <span key={sp.toString()}>{txt}</span>
							: <Snip
									onClick={this.props.onSnipClick}
									onMouseLeave={this.snipHoverHandler}
									onMouseEnter={this.snipHoverHandler}
									ks={k}
									key={sp.toString()}
									root={this.state.root_container_el}
									scroll_idx={0}
									className={
										(this.state.snip_hovered
											? 'focused'
											: '')
										+ (k[0] === null
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
