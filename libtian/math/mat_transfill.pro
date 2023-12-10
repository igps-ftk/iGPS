;+
; :Name:
;   MAT_TRANSFILL
;
; :Description:
;   Fill the lower part of a upper triangular matrix.
; :Params:
;    MAT
;
; :Keywords:
;    UP
;
; :Examples:
;
; :Modifications:
;   Released on May 11, 2010
;
; :Author:
;   tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION MAT_TRANSFILL, MAT, UP=UP
	IF N_ELEMENTS(MAT) EQ 0 THEN RETURN, !VALUES.D_NAN
	IF N_ELEMENTS(MAT) EQ 1 THEN RETURN, MAT

	SZ = SIZE(MAT,/DIMENSIONS)
	IF SZ(0) NE SZ(1) THEN BEGIN
		RETURN,-1
	ENDIF

	IF KEYWORD_SET(UP) EQ 0 THEN UP = 1

	TMPMAT = MAT

	IF UP THEN BEGIN
		FOR I=0, SZ(0)-2 DO BEGIN
			TMPMAT[I,I:*] = MAT[I:*,I]
		ENDFOR
	ENDIF

	RETURN, TMPMAT

END

PRO MAT_TRANSFILL
	MAT = [[1,2,3], $
		   [0,4,5], $
		   [0,0,7] ]
	PRINT,MAT_TRANSFILL(MAT)
END