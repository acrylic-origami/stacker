import React from 'react'
import { offsetTo } from './Util'
import { NUM_SNIP_DEPTH_COLORS } from './const'

type Handler<Tk> = (e: React.SyntheticEvent, k: Tk) => void
type TProps<Tk> = {
	scroll_idx: number | boolean,
	root?: HTMLElement,
	onClick?: Handler<Tk>,
	onMouseEnter?: Handler<Tk>,
	onMouseLeave?: Handler<Tk>,
	ks: Tk,
	className?: string
}
type TState = {}
export default class<Tk> extends React.PureComponent<TProps<Tk[]>, TState> {
	private aref: React.RefObject<HTMLAnchorElement>;
	constructor(props: TProps<Tk[]>) {
		super(props);
		this.aref = React.createRef();
	}
	componentDidMount() {
		if(+this.props.scroll_idx > 0) {
			this.scroll_here();
		}
	}
	componentDidUpdate(pprops: TProps<Tk[]>) {
		if(+this.props.scroll_idx > 0 && (this.props.scroll_idx !== pprops.scroll_idx || pprops.root !== this.props.root)) {
			this.scroll_here();
		}
	}
	scroll_here = (): void => {
		const aref = this.aref?.current;
		if(aref !== null && this.props.root !== undefined) {
			const offset = offsetTo(aref, this.props.root);
			if(offset !== undefined) {
				// offset can be undefined in between renders
				const box = this.props.root.getBoundingClientRect();
				this.props.root.scroll({
					left: offset[0] - box.width / 2,
					top: offset[1] - box.height / 2,
					behavior: 'smooth'
				});
			}
		}
	}
	handleTaggedEvent = (e: React.SyntheticEvent): void => {
		const handler = ({
			'click': this.props.onClick,
			'mouseenter': this.props.onMouseEnter,
			'mouseleave': this.props.onMouseLeave
		} as Record<any, Handler<Tk[]>>)[e.type];
		if(handler !== undefined)
			handler(e, this.props.ks) // need to shuttle ks around so this function isn't regenerated on every re-render of the parent; I wish it wasn't this aware
	}
	render = () => <a
		className={`snip snip-${Math.min(NUM_SNIP_DEPTH_COLORS, this.props.ks.length || 1)} ${this.props.className || ''}`}
		ref={this.aref}
		onClick={this.handleTaggedEvent}
		onMouseEnter={this.handleTaggedEvent}
		onMouseLeave={this.handleTaggedEvent}
	>
		{this.props.children}
	</a>
}