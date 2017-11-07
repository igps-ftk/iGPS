FUNCTION RANDOM_UNIQ_INT, n
  ints=INTARR(n)
  ints[*]=-1
  np=n
  seed=100L
  FOR pi=0, n-1 DO BEGIN
    WHILE 1 DO BEGIN
      tmp=FIX(RANDOMU(seed,1)*n)
      pos=WHERE(ints EQ tmp[0])
      ;help, pos
      ;stop
      IF pos[0] EQ -1 THEN BEGIN
        GOTO, out_while
      ENDIF else begin
        ;stop
      endelse
    ENDWHILE
    out_while:
    ints[pi]=tmp
  ;print,ints[0:pi]
  ENDFOR
  HELP, ints
  ;PRINT, ints
  t1=ints[SORT(ints)]
  HELP,t1
  ;PRINT,t1
  t2=t1[UNIQ(t1)]
  HELP, t2
  ;PRINT,t2
  
  RETURN, ints
END

PRO RANDOM_UNIQ_INT, n
  n=102
  ints=random_uniq_int(n)
  ;HELP, ints
  ;PRINT, ints
  
END