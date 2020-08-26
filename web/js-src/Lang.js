import { Map } from 'immutable'
export const SPANTY = Object.freeze({
	NODE: Object.freeze({
		AG_TO_ARG: 0,
		AG_TO_BIND: 1,
		BIND_CALLSITE: 2,
	}),
	CTX: Object.freeze({
		ARG: 3,
		BIND_FROM_ARG: 4,
		BIND_FROM_AG: 5
	}),
	ENV: Object.freeze({
		BIND_ENV: 6,
		APPGROUP_ENV: 7
	})
});
export const NK2ENV = Map([
	['NKBind', SPANTY.ENV.BIND_ENV],
	['NKApp', SPANTY.ENV.APPGROUP_ENV]
]);