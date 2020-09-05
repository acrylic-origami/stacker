import React from 'react';
import { List } from 'immutable'
import * as L from './Lang'
import * as U from './Util'

type SpanChar<Tk> = U.DictDbl<L.ISpan, L.SpanKey<Tk>>;
export function mk_span_chars<Tk>(body_lines: string[], span_ks: Array<L.SpanKey<Tk>>): Array<SpanChar<Tk>> {
	const cum_line_chars = body_lines.reduce((acc, l) => acc.push((acc.last() as number) + l.length + 1), List<number>([0])); // +1 for \n, faster push than concat
	// span_ks.sort(([la, ca], [lb, cb]) => la > lb || (la === lb && ca > cb));
	// const span_ks = this.state.st_at.map(e => this.state.st_gr.jsg_gr.get(e)[1])
	// 	.filter(l => !!l.length)[0]
	// 	.map(([_, sp]) => sp)
	return span_ks.reduce(
		(l, [span, k]) => {
			const [_src, [ll, lc], [rl, rc]] = span;
			const lchar = cum_line_chars.get(ll - 1);
			const rchar = cum_line_chars.get(rl - 1);
			if(lchar !== undefined && rchar !== undefined) {
				const sp: L.ISpan = [lchar + lc - 1, rchar + rc - 1];
				const v : L.SpanKey<Tk> = [span, k];
				return l.push({
					key: sp,
					value: v
				});
			}
			else return l;
		}
	, List<SpanChar<Tk>>()).toArray(); // :: [{ key: ISpan, value: (Span, k))]
}

export function slice_parsetree(t: L.KTree, sp: L.ISpan): undefined | L.KTree { // , dir=false
	// dir = false => subtree, true = supertree
	function r(t_: L.KTree, n: number): [undefined | L.KTree, number]{
		if(typeof t_ === 'string') {
			const next_n = n + t_.length;
			if(
				// sp[0] - n < 0 && sp[1] - n > t_.length
				// || (sp[0] - n < 0 || sp[1] - n > t_.length) === dir
				sp[0] >= n && sp[0] < n + t_.length || sp[1] >= n && sp[1] < n + t_.length || sp[0] <= n && sp[1] >= n + t_.length
				// again, not sure about the boundary conditions, made it semiopen and consistent with the `slice` semantics and hoping for the best
			) {
				// debugger;
				return [
					// t_
					t_.slice(Math.max(0, sp[0] - n), Math.min(sp[1] - n, t_.length))
					, next_n
				];
			}
			else return [undefined, next_n];
		}
		else {
			const [children, n_] = t_.children.reduce(([acc, n_], t__) => {
				const [m_subt, n__] = r(t__, n_);
				return [m_subt !== undefined ? acc.push(m_subt) : acc, n__];
			}, [List<L.KTree>(), n]);
			if(children.size > 0) {
				return [{
					kind: t_.kind,
					children: children.toArray()
				}, n_];
			}
			else {
				return [undefined, n_];
			}
		}
	}
	return r(t, 0)[0];
}

export function mk_parsetree<T>(parsetree: L.KTree, src_snips: Array<L.ISpanKey<T>>): Array<[React.ReactNode, L.ISpanKey<T>]> {
	// KTree :: string | { kind: String, children: [KTree] }
	// type ParseTree = KTree
	// mk_parsetree :: KTree -> [(ISpan, k)] -> [<Span /> & ISpan]
	function rt(tree: L.KTree): React.ReactNode {
		function rt_(t: L.KTree, n: number): [React.ReactNode, number] {
			// t :: KTree
			if(typeof t === 'string')
				return [t, n + t.length];
			else {
				const z0: [List<React.ReactNode>, number] = [List<React.ReactNode>(), n];
				const [eles, n_] = t.children.reduce(([acc, n_], t_): [List<React.ReactNode>, number] => { // are you kidding? i think haskell has spoiled me.
					const [ele, n__] = rt_(t_, n_);
					return [acc.push(ele), n__];
				}, z0);
				return [<span className={t.kind && `hljs-${t.kind}`} key={n}>{eles.toArray()}</span>, n_];
			}
		}
		return rt_(tree, 0)[0];
	}
	
	const r = (hl_t: L.KTree, snip_idx: number, n: number): [L.KTree[], [number, number]] => {
		// KTree -> Int -> Int -> ([KTree], Int)
		if(typeof hl_t === 'string') {
			const rightdist = src_snips[snip_idx][0][1] - n;
			if(rightdist <= hl_t.length) {  // TODO check openness of span boundaries
				// console.log(snip_idx, rightdist, src_snips[snip_idx]);
				// need to cut this tag in half
				const next = r(hl_t.slice(rightdist), snip_idx + 1, src_snips[snip_idx][0][1]);
				// const partial_t_ = push_leaf(, partial_t);
				const here = hl_t.slice(0, rightdist); // mksnip(rt(partial_t_));
				next[0].unshift(here); // push new tag to start
				return next;
			}
			else {
				return [[hl_t], [snip_idx, n + hl_t.length]];
			}
		}
		else {
			let snip_idx_ = snip_idx;
			let n_ = n;
			const root_ctor = () => ({ kind: hl_t.kind, children: new Array<L.KTree>() });
			const roots = [root_ctor()];
			for(const c of hl_t.children) {
				const next = r(c, snip_idx_, n_);
				const next_trees = next[0];
				[snip_idx_, n_] = next[1];
				for(let t = 0; t < next_trees.length; t++) {
					if(t > 0)
						roots.push(root_ctor());
					
					roots[roots.length - 1].children.push(next_trees[t]);
				}
			}
			return [roots, [snip_idx_, n_]];
		}
	}
	const [ts, _] = r(parsetree, 0, 0);
	return ts.map((t, i) => [rt(t), src_snips[i]]) // OK, the tuple crap again
}