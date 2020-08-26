import { Map, Set } from 'immutable'

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

export function candidate(sps) { // Set Span -> Span
	const big_sps = sps.reduce((s, spa, ka) => sps.reduce((s_, spb, kb) => {
		if(!s_.has(ka) && spa !== spb && span_contains(spa[0], spb[0])) {
			return s_.add(ka); // eliminate all spans that contain other spans
		}
		else {
			return s_;
		}
	}, s), Set());
	console.log(sps.filter(sp => !any(big_sp => list1eq(big_sp, sp), big_sps)))
	return sps.filter(sp => !any(big_sp => list1eq(big_sp, sp), big_sps)).reduce((_, b) => b, null);
}

export function any(f, t) {
	return t.reduce((b, a) => f(a) || b, false);
}