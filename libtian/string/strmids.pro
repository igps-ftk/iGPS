;+
; :Name:
;   strmids
;
; :Description:
;   In the old IDL, STMID function can operate on only one string varialbe. Thus,
;     strmids is used for the string array case.
;   However, in the new IDL release, STRMID can deal with the string array case.
;
; :Params:
;    strings
;    posstart
;    poslen
;
;
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
FUNCTION STRMIDS, strings, posstart, poslen
  strtmp = STRARR(N_ELEMENTS(strings))
  
  FOR i=0ull, N_ELEMENTS(strings)-1 DO BEGIN
    IF N_ELEMENTS(poslen) EQ 0 THEN BEGIN
      strtmp[i]=STRMID(strings[i],posstart)
    ENDIF ELSE BEGIN
      strtmp[i]=STRMID(strings[i],posstart, poslen)
    ENDELSE
  ENDFOR
  RETURN, strtmp
END

PRO STRMIDS
  strs=['test1','test2','Test3','tEst4','tesT5',REPLICATE('test0',1000)]
  PRINT, strmids(strs,0,4)
END
