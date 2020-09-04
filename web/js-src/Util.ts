import { Map, Set, List } from 'immutable'
import { SPANTY } from './Lang'

export type DictDbl<Tk, Tv> = { key: Tk, value: Tv }

class AssertionError extends Error {}
export function assert(p: boolean, q: string = '') {
	if(!p) throw new AssertionError(q);
	return true;
}
function zipadd(x?: number[], y?: number[]): undefined | number[] {
	if(x === undefined || y === undefined)
		return undefined;
	
	let r = [];
	for(let i = 0; i < Math.min(x.length, y.length); i++)
		r.push(x[i] + y[i]);
	return r;
}
export function offsetTo(child: HTMLElement, ancestor: HTMLElement): undefined | [number, number] {
	if(child !== ancestor) {
		const offsetParent = child.offsetParent as HTMLElement;
		if(offsetParent !== null && child.offsetParent !== child) {
			return zipadd([child.offsetLeft, child.offsetTop], offsetTo(offsetParent, ancestor)) as [number, number];
		}
		else return undefined;
	}
	else {
		return [0, 0];
	}
}
export function list1eq<T>(a: T[], b: T[]): boolean {
	if(a.length !== b.length) return false;
	for(let i = 0; i < a.length; i++) {
		if(a[i] !== b[i]) return false
	}
	return true;
}
export function id<T>(x: T) { return x; }
export function map_intersect<Tk, Tv>(M: Map<Tk, Tv>, K: Tk[]): Map<Tk, Tv> { //  | Set<Tk> | List<Tk> // is there an Enumerable/Iterable/Traversable type in TS?
	return K.reduce((m, k) => {
		const v = M.get(k);
		return v !== undefined ? m.set(k, v) : m
	}, Map<Tk, Tv>());
}

export function any<T>(f: (a: T) => boolean, t: T[]): boolean {
	return t.reduce((b: boolean, a: T): boolean => f(a) || b, false);
}

export function compare<T>(a: T, b: T): number {
	if(a > b) return 1;
	if(a === b) return 0;
	return -1;
}
export function tuple<T extends any[]>(...elements: T) {
    return elements;
}