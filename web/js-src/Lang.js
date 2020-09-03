import { Map, Set } from 'immutable'
import { any, list1eq } from './Util'
export const SPANTY = Object.freeze({
	NODE: Object.freeze({
		AG_TO_ARG: 0,
		AG_TO_BIND: 1,
		BIND_CALLSITE: 2,
		BIND_MATCHSITE: 3,
	}),
	CTX: Object.freeze({
		ARG: 4,
		BIND_FROM_ARG: 5,
		BIND_FROM_AG: 6
	}),
	ENV: Object.freeze({
		BIND_ENV: 7,
		APPGROUP_ENV: 8
	})
});
export const NK2ENV = Map([
	['NKBind', SPANTY.ENV.BIND_ENV],
	['NKApp', SPANTY.ENV.APPGROUP_ENV]
]);

export function el2spk(el) {
	return [repr_el_span(el), {
		ArgEdge: SPANTY.CTX.ARG,
		AppEdge: SPANTY.CTX.BIND_FROM_AG,
		BindEdge: SPANTY.CTX.BIND_FROM_ARG,
		RevBindEdge: SPANTY.CTX.BIND_FROM_ARG
	}[el.tag]];
}

export function candidate(sp_ks) { // Map Span k -> (Span, k)
	const void_sps = sp_ks.reduce((s, [spa, [ka, _]]) => {
		// console.log(ka, spa, Map(SPANTY.ENV).keyOf(ka));
		if(Map(SPANTY.ENV).keyOf(ka) != undefined || Map(SPANTY.CTX).keyOf(ka) != undefined) {
			return s.add(spa);
		}
		else {
			return sp_ks.reduce((s_, [_kb, _], spb) => {
				if(spa !== spb && span_contains(spa[0], spb[0])) {
					return s_.add(spa); // eliminate all spans that contain other spans
				}
				else {
					return s_;
				}
			}, s);
		}
	}, Set());
	return sp_ks.filter(([sp, _]) => !any(void_sp => list1eq(void_sp, sp), void_sps))[0];
}
export function nk_span(nk) {
	switch(nk.tag) {
		case 'NKApp':
			return nk.contents;
		case 'NKBind':
			return nk.contents.contents;
	}
}
export function span_contains(spa, spb) {
	return
		(spa[0][0] >= spb[0][0] || spa[0][0] === spb[0][0] && spa[0][1] >= spb[0][1]) 
		&& (spa[1][0] <= spb[1][0] || spa[1][0] === spb[1][0] && spa[1][1] <= spb[1][1]) 
}
export function repr_el_span(el) {
	switch(el.tag) {
		case 'ArgEdge':
		case 'AppEdge':
			return el.contents[1];
		case 'BindEdge':
		case 'RevBindEdge':
			return el.contents;
	}
}