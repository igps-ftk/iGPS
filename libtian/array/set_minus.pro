

;+
; :Name:
;   SET_MINUS
;
; :Description:
;   Substract one group (set1) from another group (set0) of data.
;
; :Parameters:
;    SET0
;    SET1
;
; :Examples:
;   Run the routine to see an example. 
;    The original arrray:       1       2       3      65       7       8
;    Array to delete:       3       5       6       7
;    Original array after some elements deleted:       1       2      65       8
;
; :Modifications:
;   Released on May 11, 2010
;
; :Author:
;   tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION SET_MINUS, SET0, SET1
	SETS=MAKE_ARRAY(1,TYPE=SIZE(SET0,/TYPE) )
	FOR I=0, N_ELEMENTS(SET0)-1 DO BEGIN
		TMP = WHERE(SET1 EQ SET0[I])
		IF TMP[0] EQ -1 THEN BEGIN
			SETS = [SETS, SET0[I]]
		ENDIF
	ENDFOR
	IF N_ELEMENTS(SETS) GT 1 THEN RETURN, SETS[1:*] ELSE RETURN, SETS
END

PRO SET_MINUS
	SET0 = [1 ,2 ,3 ,65, 7, 8]
	SET1 = [3 ,5, 6, 7]

  PRINT,'The original arrray:',SET0
  PRINT,'Array to delete:',SET1
	PRINT,'Original array after some elements deleted:',SET_MINUS(SET0,SET1)
END
