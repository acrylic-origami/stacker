import React from 'react'
import { NUM_SNIP_DEPTH_COLORS } from './const'

type Handler<Tk> = (e: React.SyntheticEvent, k: Tk) => void
type TProps<Tk> = {
	root?: HTMLElement,
	onClick?: Handler<Tk>,
	onDoubleClick?: Handler<Tk>,
	onMouseEnter?: Handler<Tk>,
	onMouseLeave?: Handler<Tk>,
	fwd_ref?: React.RefObject<HTMLAnchorElement>,
	ks: Tk,
	force_focus: boolean,
	// hoverWith?: string,
	className?: string
}
type TState = {}
export default class<Tk> extends React.PureComponent<TProps<Tk[]>, TState> {
	// private aref: React.RefObject<HTMLAnchorElement>;
	public static defaultProps = {
		force_focus: false
	}
	constructor(props: TProps<Tk[]>) {
		super(props);
		// this.aref = React.createRef();
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
		className={`snip snip-${Math.min(NUM_SNIP_DEPTH_COLORS, this.props.ks.length || 1)} ${this.props.className || ''} ${this.props.force_focus ? 'focused' : ''}`}
		ref={this.props.fwd_ref}
		onClick={this.handleTaggedEvent}
		onDoubleClick={this.handleTaggedEvent}
		onMouseEnter={this.handleTaggedEvent}
		onMouseLeave={this.handleTaggedEvent}
	>
		{this.props.children}
	</a>
}
// screw ref forwarding, I can't make it preserve my generic component without jumping through hoops with exotic component or just coercing it. This is because you can't have consts with generics. The return from `forwardRef` is also so complicated that it's not worth even writing the type and bolting myself to the type definition in @types/react. Typescript sometimes makes me sad.

// const El = React.forwardRef(<Tk extends any>(props, ref) => <El<Tk> {...props} fwd_ref={ref} />);
// export default El;