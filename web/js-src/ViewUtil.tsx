import React from 'react'
import * as L from './Lang'

export type SnipWrapper<Tk> = (txt: React.ReactNode, sp_ks: L.SpanKey<Tk>[]) => React.ReactNode;
const wrap_snip: SnipWrapper<SpanMeta> = (txt, sp_ks) => {
	const k_counts = sp_ks.reduce((acc, [sp, [spty, _el]]) => acc.update(spty, 0, i => i + 1), Map<L.SPANTY, number>());
	return <span className={k_counts.map((cnt, k) => `snip-${k}-${Math.min(NUM_SNIP_DEPTH_COLORS, cnt)}`).join(' ')}>{txt}</span>
}