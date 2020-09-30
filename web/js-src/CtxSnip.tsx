import React from 'react'
import Snip from './Snip'
import * as L from './Lang'
import { List } from 'immutable'
import { SnipWrapper } from './MainController';
import { TParseTree } from './parsetree'
import parsePath from 'parse-filepath'

const MAX_LINES = 4;

function ppr_loc(loc: L.Loc): string {
	return `(${loc[0]}, ${loc[1]})`;
}

// okay, the main problem is that this is now obsolete and thus hard to motivate: Tk used to be used for snip clicks, but we don't allow that anymore. The only reason we still have it is for snip wrapping, but that expects all of the keys to be packaged with a span, whereas mk_preview in MainController tries to be general and instead unions the undefined in the top-most level, allowing us to pass it that way.
interface TProps<Tk, Tu> {
	onSnipClick?: (e: React.SyntheticEvent, k: Tk | undefined) => void,
	preview: TParseTree<Tk | undefined>,
	onDoubleClick?: (e: Event, u: Tu) => void,
	onClick?: (e: Event, u: Tu) => void,
	onFocus?: (e: Event, u: Tu) => void,
	onBlur?: (e: Event, u: Tu) => void,
	click_key?: Tu,
	active: boolean,
	className?: string,
	wrap_snip: SnipWrapper<Tk>,
	span: L.Span,
	name: string,
	filename: string,
	tabbable: boolean,
}
const DEBOUNCE_FOCUS_CLICK = 150;
interface TState {
	root_container_el?: HTMLElement,
	snip_hovered: boolean,
	// clicked_for_focus: boolean,
	// event_queue: List<Event>,
	last_focus: number,
	last_click: number
}
export default class<Tk = undefined, Tu = undefined> extends React.PureComponent<TProps<Tk, Tu>, TState> {
	public static defaultProps = {
		active: false,
	}
	protected aref: React.RefObject<HTMLAnchorElement>;
	constructor(props: TProps<Tk, Tu>) {
		super(props);
		this.state = {
			root_container_el: undefined,
			snip_hovered: false,
			last_focus: -Infinity,
			last_click: -Infinity,
			// clicked_for_focus: false, // flag for whether the latest focus was caused by a click: fairly subtle (needed so that we don't "click" with tab)
			// event_queue: List()
		};
		this.aref = React.createRef();
	}
	public componentDidUpdate(pprops: TProps<Tk, Tu>, pstate: TState) {
		const diff = {
			active: pprops.active !== this.props.active
		}
		if(diff.active) {
			if(this.props.active === true && this.aref.current !== null) {
				this.aref.current.focus();
			}
		}
	}
	// public componentDidUpdate(pprops: TProps<Tk, Tu>, pstate: TState) {
	// 	if(this.props.active)
	// 		this.setState({ active: true });
	// }
	// protected flush_event_queue(): void {
	// 	let clicked_for_focus = this.state.clicked_for_focus;
	// 	this.state.event_queue.forEach(e => {
	// 		switch(e.type) {
	// 			case 'focus':
	// 				if(this.props.onFocus !== undefined && this.props.click_key !== undefined && !clicked_for_focus) {
	// 					this.props.onFocus(e, this.props.click_key);
	// 				}
	// 				clicked_for_focus = false;
	// 				break;
	// 			case 'click':
	// 				if(this.props.onClick !== undefined && this.props.click_key !== undefined)
	// 					this.props.onClick(e, this.props.click_key);
	// 				clicked_for_focus = true;
	// 				break;
	// 		}
	// 	});
	// 	const cut_length = this.state.event_queue.size;
	// 	this.setState(({ event_queue }) => ({ event_queue: event_queue.slice(cut_length), clicked_for_focus }));
	// }
	protected clickHandler = (e: React.SyntheticEvent): boolean => {
		e.stopPropagation();
		const e_ = e.nativeEvent;
		if(this.props.onClick !== undefined && this.props.click_key !== undefined && e.timeStamp > this.state.last_focus + DEBOUNCE_FOCUS_CLICK) {
			this.props.onClick(e_, this.props.click_key);
		}
		this.setState({ last_click: e.timeStamp });
		return false; // IMPORTANT to avoid adding `#` to history
		// this.setState(({ event_queue }) => ({ event_queue: event_queue.push(e_) }), this.flush_event_queue);
	}
	protected focusHandler = (e: React.SyntheticEvent): void => {
		const e_ = e.nativeEvent;
		if(this.props.onFocus !== undefined && this.props.click_key !== undefined && e.timeStamp > this.state.last_click + DEBOUNCE_FOCUS_CLICK) {
			this.props.onFocus(e_, this.props.click_key);
		}
		this.setState({ last_focus: e.timeStamp });
		// this.setState(({ event_queue }) => ({ event_queue: event_queue.push(e_) }), this.flush_event_queue);
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
	protected doubleClickHandler = (e: React.SyntheticEvent) => {
		const e_ = e.nativeEvent;
		if(this.props.onDoubleClick !== undefined && this.props.click_key !== undefined) {
			this.props.onDoubleClick(e_, this.props.click_key);
		}
	}
	protected snipClickHandler = (e: React.SyntheticEvent, k: Array<Tk | undefined>) => {
		const k_ = k[0];
		if(k_ !== undefined && this.props.onSnipClick !== undefined) {
			this.props.onSnipClick(e, k_);
		}
	}
	protected codeClickHandler = (e: React.SyntheticEvent) => e.stopPropagation()
	
	render() {
		const children = <React.Fragment>
			<h3 className="ctx-head">
				<span className={`ctx-label ctx-label-${this.props.name.toLowerCase()}`}>
					{this.props.name}
				</span>
				@&nbsp;&nbsp;
				{parsePath(this.props.filename).base}&nbsp;&nbsp;
				{ppr_loc(this.props.span[1])}&nbsp;&mdash;&nbsp;{ppr_loc(this.props.span[2])}
			</h3>
			<h4 className="ctx-head-fname">({this.props.filename})</h4>
			<div className="src-container" ref={e => this.setState({ root_container_el: e || undefined })}>
				<pre>
					<code className="language-haskell hljs" onClick={this.codeClickHandler}>
						&hellip;{this.props.preview.map(([txt, [isp, sp_ks]]) =>
							sp_ks === undefined
								? <span key={isp.toString()}>{txt}</span>
								: <Snip<undefined>
										// onClick={this.snipClickHandler}
										// onMouseLeave={this.snipHoverHandler}
										// onMouseEnter={this.snipHoverHandler}
										ks={[undefined]} // used to be an sp_ks for selection, but now it's obsolete since we don't click on these to move forward
										key={isp.toString()}
										root={this.state.root_container_el}
										className={
											(this.state.snip_hovered
												? 'focused'
												: '')
											+ (sp_ks === undefined
												? 'unpointer'
												: '')
										}
									>
									{this.props.wrap_snip(txt, sp_ks)}
								</Snip>
						)}&hellip;
					</code>
				</pre>
			</div>
		</React.Fragment>
		return this.props.tabbable
				? <a href="#"
						ref={this.aref}
						onClick={this.clickHandler}
						onDoubleClick={this.doubleClickHandler}
						onFocus={this.focusHandler}
						onBlur={e => this.props.onBlur && this.props.click_key && this.props.onBlur(e.nativeEvent, this.props.click_key)}
						tabIndex={this.props.tabbable ? 0 : undefined}>
						{children}
					</a>
				: children;
		
	}
}
