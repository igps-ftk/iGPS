FUNCTION STRING_SPLIT, str, sepa

  IF N_ELEMENTS(str) NE 1 THEN BEGIN
    PRINT,'[STRING_SPLIT]ERROR: input should be a string (NOT an array)!!!'
    RETURN, ''
  ENDIF
  
  
  IF N_ELEMENTS(sepa) EQ 0 THEN sepa=','
  str_arr=str2arr(str)
  pos=WHERE(str_arr EQ sepa)
  
  ;if not found, exit
  IF pos[0] EQ -1 THEN RETURN, str
  
  nparts=N_ELEMENTS(pos)+1
  oStr=STRARR(nParts)
  
  FOR pi=0, nParts-2 DO BEGIN
    IF pi EQ 0 THEN BEGIN
      cur_start=0
      cur_end=pos[pi]
    ENDIF ELSE BEGIN
      cur_start=pos[pi-1]+1
      cur_end=pos[pi]
    ENDELSE
    ;
    oStr[pi]=STRMID(str, cur_start, cur_end-cur_start)
  ;print, pi,cur_start, cur_end-cur_start, cur_end, '=',oStr[pi]
  ENDFOR
  oStr[pi]=STRMID(str, last(pos)+1)
  ;print,str
  ;stop
  RETURN, oStr
  
END

PRO STRING_SPLIT
  str='1911,12,7,,,,23.0000,88.0000,,,,,,,,5.0,,(5.4),"5,2",West Bengal,TS'
  tmp=string_split(str)
;PRINT,tmp,format='(a)'
END