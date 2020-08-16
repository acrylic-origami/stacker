import { Map } from 'immutable'

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
		if(child.offsetParent !== null) return zipadd([child.offsetX, child.offsetY], offsetTo(id, child.offsetParent));
		else return null;
	}
	else {
		return [0, 0];
	}
}
export function span_contins(spa, spb) {
	return
		(spa[0][0] < spb[0][0] || spa[0][0] === spb[0][0] && spa[0][1] < spb[0][1]) 
		&& (spa[1][0] < spb[1][0] || spa[1][0] === spb[1][0] && spa[1][1] < spb[1][1]) 
}
export function id(x) { return x; }
export function map_intersect(M, K) {
	return K.reduce((m, k) => m.set(k, M.get(k)), Map());
}