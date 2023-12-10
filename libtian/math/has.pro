;+
; Name:
;		has
;
; Purpose:
;		If b in one element of a, then return 1;
;
; Parameters:
;		a	-
;		b	-
;
;-
FUNCTION HAS, A, B
	IF N_PARAMS() LT 2 THEN RETURN, 0
	IF N_ELEMENTS(B) NE 1 THEN RETURN, 0
	;
	FOR I=0, N_ELEMENTS(A)-1 DO BEGIN
		IF A[I] EQ B THEN RETURN,1
	ENDFOR
	;
	RETURN,0
END

PRO HAS
	PRINT, HAS(INDGEN(3),12)
END
