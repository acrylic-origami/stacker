import React from 'react'
import { Map } from 'immutable'
import * as L from './Lang'
import { NUM_SNIP_DEPTH_COLORS } from './const'

export type SnipWrapper<Tk> = (txt: React.ReactNode, sp_ks: L.SpanKey<Tk>[]) => React.ReactNode;
const wrap_snip: SnipWrapper<L.SpanMeta> = (txt, sp_ks) => {
	const k_counts = sp_ks.reduce((acc, [sp, [spty, _el]]) => acc.update(spty, 0, (i: number) => i + 1), Map<L.SPANTY, number>());
	return <span className={k_counts.map((cnt: number, k: number) => `snip-${k}-${Math.min(NUM_SNIP_DEPTH_COLORS, cnt)}`).join(' ')}>{txt}</span>
}