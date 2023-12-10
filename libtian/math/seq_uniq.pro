; +Mod on Fri, Apr 15, 2016  3:36:39 PM by tianyf
;   A bug found for the case when the last element is unique.
;
FUNCTION SEQ_UNIQ, lines
  pos=-1  ;no
  nl=N_ELEMENTS(lines)
  IF nl LE 0 THEN BEGIN
    RETURN,-1
  ENDIF
  
  IF nl EQ 1 THEN BEGIN
    pos=0
    RETURN, pos
  ENDIF
  
  line_p=lines[0]
  pt_p=0
  FOR i=1ull,nl-1 DO BEGIN
    line=lines[i]
    IF line EQ line_p THEN BEGIN
      IF i EQ nl-1 THEN BEGIN
        IF pos[0] EQ -1 THEN BEGIN
          pos=i
        ENDIF ELSE BEGIN
          pos=[pos, i]
        ENDELSE
        RETURN, pos
      ENDIF
      pt_p=i
    ENDIF ELSE BEGIN
      ;stop
      IF pos[0] EQ -1 THEN BEGIN
        pos=pt_p
      ENDIF ELSE BEGIN
        pos=[pos, pt_p]
      ENDELSE
      
      IF i EQ nl-1 THEN BEGIN
        ;stop
        pos=[pos,i]
      ENDIF
      line_p=line
      pt_p=i
    ENDELSE
    
  ENDFOR
  
  ;STOP
  RETURN,pos
END

PRO SEQ_UNIQ
  x=[1,1,2,2,3]
  x=[1]
  x=[1,2]
  x=[1,1,2,2]
  x=[1,2,2]
  x=[1,1,2]
  tmp=seq_uniq(x)
  PRINT,'unique index:', tmp
  print,'unique element:',x[tmp]
END