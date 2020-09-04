import { Map, Set } from 'immutable'
import { any } from './Util'

export interface Tagged<Tk, Tv> {
	tag: Tk,
	contents: Tv
}

export type Loc = [number, number]
export type Span = [string, Loc, Loc]
export type ISpan = [number, number]
export type ISpanKey<Tk> = [ISpan, Tk]
export type SpanKey<Tk> = [Span, Tk]

export type Node = number;

export type EdgeLabelTag = 'ArgEdge' | 'AppEdge' | 'BindEdge' | 'RevBindEdge';
export type EdgeLabelContents =
	[Span, Span] // ArgEdge, AppEdge
	| Span // BindEdge, RevBindEdge
export type EdgeLabel =
	{ tag: 'ArgEdge' | 'AppEdge', contents: [Span, Span] }
	| { tag: 'BindEdge' | 'RevBindEdge', contents: Span }
export type FwEdge = [Node, EdgeLabel]

export type BindKeyTag = 'BindNamed' | 
'BindLam'
export type BindKey = { tag: BindKeyTag, contents: Span }

export type NodeKeyTag = 'NKApp' | 'NKBind';
export type NodeKey = { tag: 'NKApp', contents: Span } | { tag: 'NKBind', contents: BindKey } // note: only restricts construction, not usage (where the union can distribute)

export enum SPANTY {
	AG_TO_ARG,
	AG_TO_BIND,
	BIND_CALLSITE,
	BIND_MATCHSITE,
	
	ARG,
	BIND_FROM_ARG,
	BIND_FROM_AG,
	
	BIND_ENV,
	APPGROUP_ENV,
};
export const SPANTY_NODES = Set([ SPANTY.AG_TO_ARG, SPANTY.AG_TO_BIND, SPANTY.BIND_CALLSITE, SPANTY.BIND_MATCHSITE ]);
export const SPANTY_CTXS = Set([ SPANTY.ARG, SPANTY.BIND_FROM_ARG, SPANTY.BIND_FROM_AG ]);
export const SPANTY_ENV = Set([ SPANTY.BIND_ENV, SPANTY.APPGROUP_ENV ]);
export const NK2ENV: Record<NodeKeyTag, SPANTY> = {
	NKBind: SPANTY.BIND_ENV,
	NKApp: SPANTY.APPGROUP_ENV
}

export type KTree =
	string
	| {
		kind?: string,
		children: KTree[]
	}

export type AppGroup = FwEdge[]

export type NodeGraphLabel = [[NodeKey, number], FwEdge[]]
export type NodeGraph = Map<Node, NodeGraphLabel>
export type Src = {
	path: string,
	body: {
		lines: string[],
		raw: string
	}
}

//////////////////////////////////////////////////////////
export function spaneq(l: Span, r: Span): boolean {
	return l[0] === r[0] && l.slice(1).toString() === r.slice(1).toString();
}

export function el2spk(el: EdgeLabel): [Span, SPANTY] {
	return [repr_el_span(el), {
		ArgEdge: SPANTY.ARG,
		AppEdge: SPANTY.BIND_FROM_AG,
		BindEdge: SPANTY.BIND_FROM_ARG,
		RevBindEdge: SPANTY.BIND_FROM_ARG
	}[el.tag]];
}

export function candidate<Tk>(sp_ks: SpanKey<[SPANTY, Tk]>[]): SpanKey<[SPANTY, Tk]> { // Map Span k -> (Span, k)
	const void_sps = sp_ks.reduce((s, [spa, [ka, _]]) => {
		// console.log(ka, spa, Map(SPANTY).keyOf(ka));
		if(SPANTY_CTXS.has(ka) || SPANTY_ENV.has(ka)) {
			return s.add(spa);
		}
		else {
			return sp_ks.reduce((s_, [spb, [_kb, _]]) => {
				if(!spaneq(spa, spb) && span_contains(spa, spb)) {
					return s_.add(spa); // eliminate all spans that contain other spans
				}
				else {
					return s_;
				}
			}, s);
		}
	}, Set<Span>());
	return sp_ks.filter(([sp, _]) => !any(void_sp => spaneq(void_sp, sp), void_sps.toArray()))[0];
}
export function nk_span(nk: NodeKey): Span {
	switch(nk.tag) {
		case 'NKApp':
			return nk.contents;
		case 'NKBind':
			return nk.contents.contents;
	}
}
export function span_contains(spa: Span, spb: Span): boolean {
	return (spa[0][0] >= spb[0][0] || spa[0][0] === spb[0][0] && spa[0][1] >= spb[0][1]) 
		&& (spa[1][0] <= spb[1][0] || spa[1][0] === spb[1][0] && spa[1][1] <= spb[1][1]) 
}
export function repr_el_span(el: EdgeLabel): Span {
	switch(el.tag) {
		case 'ArgEdge':
		case 'AppEdge':
			return el.contents[1];
		case 'BindEdge':
		case 'RevBindEdge':
			return el.contents;
	}
}