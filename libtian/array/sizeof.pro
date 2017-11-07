;+
; Name:
;		sizeof
;
; Purpose:
;		Return the number of bytes of input type.
;
; Parameters:
;		x	-	type code (corresponding to IDL type code).
;
;-
FUNCTION SIZEOF, X
	CASE X OF
		1: RETURN,1	; BYTE
		2: RETURN,2	; INTEGER
		3: RETURN,4	; LONG
		4: RETURN,4	; FLOAT
		5: RETURN,8
		12: RETURN,2
		13: RETURN,4
		14: RETURN,8
		15: RETURN,8
		ELSE: RETURN,0
	ENDCASE
END
;
PRO SIZEOF
	PRINT,SIZEOF(2)
END
