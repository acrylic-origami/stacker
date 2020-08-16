import React from 'react'
import { offsetTo } from './Util'

const NUM_SNIP_DEPTH_COLORS = 4;

export default class extends React.PureComponent {
	constructor(props) {
		super(props);
		this.aref = React.createRef();
	}
	componentDidUpdate(pprops) {
		if(+this.props.scroll_idx > 0 && this.props.scroll_idx !== pprops.scroll_idx) {
			const offset = offsetTo(this.aref.current, this.props.root);
			const box = this.props.root.getClientBoundingRect();
			this.props.root.scroll({
				left: offset[0] - box.width / 2,
				top: offset[1] - box.height / 2,
				behavior: 'smooth'
			});
		}
	}
	handleTaggedEvent = e => {
		const handler = {
			'click': this.props.onClick,
			'mouseenter': this.props.onMouseOver,
			'mouseleave': this.props.onMouseEnter
		}[e.type];
		handler(e, this.props.ks) // need to shuttle ks around so this function isn't regenerated on every re-render of the parent; I wish it wasn't this aware
	}
	render = () => <a
		className={`snip snip-${Math.min(NUM_SNIP_DEPTH_COLORS, this.props.ks.length)} ${this.props.className}`}
		href="#"
		onClick={this.handleTaggedEvent}
		onMouseEnter={this.handleTaggedEvent}
		onMouseLeave={this.handleTaggedEvent}
		ref={this.aref}
	>
		{this.props.children}
	</a>
}