import React from 'react'
import CodeBlock, { TProps as TCodeProps } from './CodeBlock'
import { TParseTree } from './parsetree'
import { offsetTo } from './Util'

interface TProps<Tu, Tk> extends TCodeProps<Tk> { // Tu and Tk differentiated because Tu is usually more general (and hence less decorated) for scrolling to arbitrary things vs. Tk which are clickable things (TODO to strengthen/revise later)
	scroll_key_comp: (a: Tu, b: Tk[]) => boolean,
	scroll_to?: Tu,
	id?: string,
	className?: string
}

interface TState<Tk> {
	root_container_el?: HTMLElement,
	parsetree?: TParseTree<Tk[]>,
	snip_refs: Array<React.RefObject<HTMLAnchorElement>>,
}

export default class<Tu, Tk> extends React.PureComponent<TProps<Tu, Tk>, TState<Tk>> {
	public readonly state: TState<Tk> = {
		root_container_el: undefined,
		parsetree: undefined,
		snip_refs: []
	}
	public populate_sparsetree() {
		this.setState({
			parsetree: this.props.parsetree,
			snip_refs: this.props.parsetree && this.props.parsetree
				.filter(([_n, [_isp, m_spk]]) => m_spk !== undefined)
				.map(_ => React.createRef())
				|| [],
		});
	}
	public componentDidMount(): void {
		this.populate_sparsetree();
	}
	public componentDidUpdate(pprops: TProps<Tu, Tk>, pstate: TState<Tk>): void {
		const diff = {
			pparsetree: this.props.parsetree !== pprops.parsetree,
			sparsetree: this.state.parsetree !== pstate.parsetree,
			root_container_el: this.state.root_container_el !== pstate.root_container_el,
			scroll_to: this.props.scroll_to !== pprops.scroll_to
		};
		if(diff.pparsetree) {
			this.populate_sparsetree();
		}
		if(diff.sparsetree || diff.scroll_to || diff.root_container_el) {
			const scroll_to = this.props.scroll_to;
			if(this.state.parsetree !== undefined && scroll_to !== undefined) {
				let i = 0;
				for(const [_n, [_isp, m_spk]] of this.state.parsetree) {
					if(m_spk !== undefined) {
						if(this.props.scroll_key_comp(scroll_to, m_spk)) {
							const ref = this.state.snip_refs[i];
							const root = this.state.root_container_el;
							setTimeout(() => {
								if(ref !== undefined && ref.current !== null && root !== undefined) {
									const offset = offsetTo(ref.current, root);
									if(offset !== undefined) {
										// offset can be undefined in between renders
										const box = root.getBoundingClientRect();
										root.scroll({
											left: offset[0] - box.width / 2,
											top: offset[1] - box.height / 2,
											behavior: 'smooth'
										});
									}
								}
							}, 500); // give the render a little bit of breathing room: may help prevent scrolling to wrong places on first go
							break;
						}
						i++;
					}
				}
			}
		}
 	}
	render = () => 
		<div className={`src-wrapper scrollable ${this.props.className || ''}`} id={this.props.id || ''} ref={e => this.setState({ root_container_el: e || undefined })}>
			<section className="src-container">
				<pre>
					<code id="src_root" className="language-haskell hljs">
						&hellip;
						<CodeBlock<Tk>
							{...this.props /* oops, shadowing parsetree. Eh. */}
							snip_refs={this.state.snip_refs}
							parsetree={this.state.parsetree}
						/>
						&hellip;
					</code>
				</pre>
			</section>
		</div>
}