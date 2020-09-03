import { Map, Set } from 'immutable'
import { SPANTY } from './Lang'

class AssertionError extends Error {}
export function assert(p, q = '') {
	if(!p) throw new AssertionError(q);
	return true;
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

export function any(f, t) {
	return t.reduce((b, a) => f(a) || b, false);
}

export function compare(a, b) {
	if(a > b) return 1;
	if(a === b) return 0;
	return -1;
}