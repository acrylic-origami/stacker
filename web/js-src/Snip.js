import React from 'react'
import { offsetTo } from './Util'
import { NUM_SNIP_DEPTH_COLORS } from './const.js'

export default class extends React.PureComponent {
	constructor(props) {
		super(props);
		this.aref = React.createRef();
	}
	componentDidMount() {
		if(+this.props.scroll_idx > 0) {
			this.scroll_here();
		}
	}
	componentDidUpdate(pprops) {
		if(+this.props.scroll_idx > 0 && (this.props.scroll_idx !== pprops.scroll_idx || pprops.root !== this.props.root)) {
			this.scroll_here();
		}
	}
	scroll_here = () => {
		if(this.aref.current != null && this.props.root != null) {
			const offset = offsetTo(this.aref.current, this.props.root);
			if(offset != null) {
				// offset can be null in between renders
				const box = this.props.root.getBoundingClientRect();
				this.props.root.scroll({
					left: offset[0] - box.width / 2,
					top: offset[1] - box.height / 2,
					behavior: 'smooth'
				});
			}
		}
	}
	handleTaggedEvent = e => {
		const handler = {
			'click': this.props.onClick,
			'mouseenter': this.props.onMouseEnter,
			'mouseleave': this.props.onMouseLeave
		}[e.type];
		if(handler != null)
			handler(e, this.props.ks) // need to shuttle ks around so this function isn't regenerated on every re-render of the parent; I wish it wasn't this aware
	}
	render = () => <a
		className={`snip snip-${Math.min(NUM_SNIP_DEPTH_COLORS, this.props.ks.length || 1)} ${this.props.className || ''}`}
		onClick={this.handleTaggedEvent}
		onMouseEnter={this.handleTaggedEvent}
		onMouseLeave={this.handleTaggedEvent}
	>
		{this.props.children}
	</a>
}