;+
; Name:
;		str2arr
;
; Purpose:
;		Convert a string to array in which each character becomes a element of the resulting array.
;
; Example:
;		str='Hello, the world'
;		res=str2arr(str)
;
;-
FUNCTION STR2ARR, str, sep
  IF SIZE(str,/type) NE 7 THEN str=STRING(str)
  IF N_ELEMENTS(str) GT 1 THEN str=STRJOIN(str)
  str=STRTRIM(str,2)
  IF str EQ '' THEN RETURN,['']
  tmp=''
  FOR i=0, STRLEN(str)-1 DO BEGIN
    tmp=[tmp, STRMID(str,i,1)]
  ENDFOR
  RETURN,tmp(1:i)
END

PRO STR2ARR
  str='Hello, the world'
  str=['Hello',' ,the world!']
  res=str2arr(str)
  PRINT,'[STR2ARR]The string:'+str
  HELP,str
  PRINT,'[STR2ARR]Dimensions of output array:'
  HELP,res
  
  FOR i=0, STRLEN(str)-1 DO PRINT,'[STR2ARR] Element '+strtrim(i,2)+' of output string array is:'+strtrim(res(i),2)
END
