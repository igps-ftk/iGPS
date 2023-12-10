FUNCTION SCATTER, x, szw
  xo=x
  FOR i=0,N_ELEMENTS(x)-1 DO BEGIN
    imin=(i-szw/2) > 0
    imax=(i+szw/2) < n_elements(x)-1
    ;print,imin,imax
    xt=x[imin:imax]
    xo[i]=rms(xt)
    ;STOP
  ENDFOR
  
  return,xo
END