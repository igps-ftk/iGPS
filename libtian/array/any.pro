;+
;ANY    True if any element of a vector is nonzero.
;   For vectors/matrices, ANY(V) returns 1 if any of the elements of the
;   vector are non-zero. 
;
;-
FUNCTION ANY, V
	INDEX=WHERE(V NE 0,COUNT)
	;PRINT,COUNT
	IF COUNT NE 0 THEN BEGIN
		RETURN, 0
	ENDIF ELSE BEGIN
		RETURN, 1
	ENDELSE
END

PRO ANY
	PRINT,'ANY(INDGEN(10)):',ANY(INDGEN(10))
	PRINT,'ANY(INTARR(10)):',ANY(INTARR(10))
END
