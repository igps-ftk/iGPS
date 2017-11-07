;+
; Name:
;		keep
; Purpose:
;		Get rid of the first element/character in an array/string.
; Parameters:
;		arr	-	array to be processed
; Keywords:
;		last	-	Get rid of the last element of an array or the last character of an string.
;		length	-	The number of elements to be deleted.
;-
FUNCTION KEEP, arr, last=last, length=length
  ;
  IF N_PARAMS() LT 1 THEN RETURN, ''
  ;
  IF N_ELEMENTS(length) EQ 0 THEN length=1
  ;
  IF SIZE(arr, /type) EQ 7 AND N_ELEMENTS(arr) EQ 1 THEN BEGIN	;; arr is string
    IF STRLEN(arr) LE length THEN RETURN, arr
    IF KEYWORD_SET(last) THEN BEGIN
      RETURN,STRMID(arr,0,STRLEN(arr)-length)
    ENDIF ELSE BEGIN
      RETURN, STRMID(arr,1,STRLEN(arr)-length)
    ENDELSE
  ENDIF ELSE BEGIN
    nn=N_ELEMENTS(arr)
    IF nn LE length THEN RETURN, arr
    IF KEYWORD_SET(last) THEN BEGIN
      RETURN, arr(0:nn-length-1)
    ENDIF ELSE BEGIN
      RETURN,arr(1:nn-length)
    ENDELSE
  ENDELSE
END

PRO KEEP
  PRINT,'[KEEP]Array Test:'
  PRINT,'[KEEP] Original:',INDGEN(3)
  PRINT,'[KEEP] After:',keep(INDGEN(3),/last,length=2)
  PRINT,'[KEEP]String Test:'
  PRINT,'[KEEP] Original:', 'Hello, the world!'
  PRINT,'[KEEP] After:',keep('Hello, the world!',length=3,/last)
END
