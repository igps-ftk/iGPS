;TITLE:
;	STRREP
;Purpose:
;	String replacement.
;Syntax:
;	str_new=strrep(str_old,str_to_be_replace,str_replacinig)
;Example:
;	str_new=strrep('What a u doing?',' ','-')
;For test run, just compile and run this file, the procedure "STRREP" will be called automatically.
;
FUNCTION STRREP, strin, stra,strb
  IF N_ELEMENTS(STRIN) GT 1 THEN BEGIN
    PRINT,'[STRREP]ERROR: the input string must be a scalar!'
    RETURN,''
  ENDIF
  
  strout=STRARR(N_ELEMENTS(strin))
  FOR si=0, N_ELEMENTS(strin)-1 DO BEGIN
    ind = STRPOS(strin[si],stra)
    IF ind[0] NE -1 THEN BEGIN
      strout[si]=STRMID(strin,0,ind)+strb+STRMID(strin,ind+STRLEN(stra))
      strout[si]=strrep(strout[si],stra,strb)
    ENDIF ELSE BEGIN
      strout[si]=strin[si]
    ENDELSE
  ENDFOR
  RETURN,strout
  
END

PRO STRREP
  PRINT,'[STRREP]Replace "h" in "hello hello" with "H", we got:'+strrep('hello hello','h','H')+'".'
  PRINT,'[STRREP]Replace "wo" in "world world" with "WOR", we got:'+strrep('world world','wo','WOR')+'".'
END