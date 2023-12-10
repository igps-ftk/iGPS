;+
;
; Name:
;		ECONGRID
;
; Purpose:
;		Resize image according to scale factor - for implementation of CONGRID.
;
; Description:
;		Enhanced CONGRID
;		Can only handle 2- or 3-D array.
;
; Calling Sequence:
;
;
; Called Routines:
;		CONGRID
;
;++++++++++++++++++++++++++++
;-
FUNCTION ECONGRID, IMG, par2, par3, par4, _EXTRA=_EXHELTRA_
	;
	; No input parameter:
	if n_params() eq 0 then begin
		print,' Usage: ECONGRID, data, scale_factor.'
		return,-1
	endif
	if n_params() eq 1 then return,img
	;
	if n_params() eq 2 then begin
		SZ=SIZE(IMG)
		if sz(0) eq 2 then begin
			SCALE_RATIO=par2
			RESULT_IMG=CONGRID(IMG, SZ(1)*scale_ratio, sz(2)*scale_ratio, _EXTRA=_EXTRA_)
		endif
		if sz(0) eq 3 then begin
			RESULT_IMG=CONGRID(IMG, SZ(1)*scale_ratio, sz(2)*scale_ratio, sz(0),  _EXTRA=_EXTRA_)
		endif
	endif
	if N_params() ge 3 then begin
		SZ=SIZE(IMG)
		if sz(0) eq 2 then begin
			SCALE_RATIO=par2
			RESULT_IMG=CONGRID(IMG, par2, par3, _EXTRA=_EXTRA_)
		endif
		if sz(0) eq 3 then begin
			RESULT_IMG=CONGRID(IMG, par2, par3, par4,  _EXTRA=_EXTRA_)
		endif
	endif
	;
	RETURN, RESULT_IMG
	;	
	
END
;//////////////////////////////////////////////////////////////////////////