import React from 'react'

interface TProps {
	renderer: (onEvent: () => void, events: number) => React.ReactNode
};
interface TState {
	events: number
}
export default class extends React.Component<TProps, TState> {
	public readonly state = {
		events: 0
	};
	protected eventHandler = () => this.setState(({ events }) => ({ events: events + 1 }))
	render = () => this.props.renderer(this.eventHandler, this.state.events);
}