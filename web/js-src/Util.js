import { Map, Set } from 'immutable'
import { SPANTY } from './Lang'

class AssertionError extends Error {}
export function assert(p, q) {
	if(!p) throw new AssertionError(q);
}
function zipadd(x, y) {
	if(x === null || y === null)
		return null;
	
	let r = [];
	for(let i = 0; i < Math.min(x.length, y.length); i++)
		r.push(x[i] + y[i]);
	return r;
}
export function offsetTo(child, ancestor) {
	if(child !== ancestor) {
		if(child.offsetParent !== null && child.offsetParent !== child) {
			return zipadd([child.offsetLeft, child.offsetTop], offsetTo(child.offsetParent, ancestor));
		}
		else return null;
	}
	else {
		return [0, 0];
	}
}
export function span_contains(spa, spb) {
	return
		(spa[0][0] < spb[0][0] || spa[0][0] === spb[0][0] && spa[0][1] < spb[0][1]) 
		&& (spa[1][0] < spb[1][0] || spa[1][0] === spb[1][0] && spa[1][1] < spb[1][1]) 
}
export function list1eq(a, b) {
	if(a.length !== b.length) return false;
	for(let i = 0; i < a.length; i++) {
		if(a[i] !== b[i]) return false
	}
	return true;
}
export function id(x) { return x; }
export function map_intersect(M, K) {
	return K.reduce((m, k) => m.set(k, M.get(k)), Map());
}

export function candidate(sp_ks) { // Map Span k -> (Span, k)
	const void_sps = sp_ks.reduce((s, [ka, _], spa) => {
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
	return sp_ks.filter((_, sp) => !any(void_sp => list1eq(void_sp, sp), void_sps))
	            .toArray()[0];
}

export function any(f, t) {
	return t.reduce((b, a) => f(a) || b, false);
}