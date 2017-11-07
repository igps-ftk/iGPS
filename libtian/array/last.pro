;+
; Name:
;		LAST
;
; Purpose:
;		Get the last element(s) of array.
;
; Calling Sequence:
;		x=last(arr [,n])
;
; Parameters:
;		arr	-	array for extraction
;
; Keywords:
;		n	-	number of extracted elements
;
; Modification History:
;		Written by Yunfeng, Tian. Jun 13 2003. 15:32 CST.
;
;-
FUNCTION LAST, ARRAY, N
	NN=N_ELEMENTS(ARRAY)
	IF N_PARAMS() EQ 1 THEN BEGIN
		RETURN,ARRAY(NN-1)
	ENDIF
	IF N_PARAMS() EQ 2 THEN BEGIN
		N=NN-N+1
		N=N > 1
		N=N < NN
		RETURN,ARRAY(N-1:NN-1)
	ENDIF
	;
	RETURN,-1
	;
END
;///]
PRO LAST
	PRINT,'LAST IS A FUNCTION.'
	PRINT,'USAGE: RET=LAST(ARRAY)'
	PRINT,'EXAMPLE:'
	PRINT,'	PRINT,LAST(INDGEN(3)) '
	PRINT,'YOU WILL GET:'
	PRINT,LAST(INDGEN(3))
	PRINT,''
END
