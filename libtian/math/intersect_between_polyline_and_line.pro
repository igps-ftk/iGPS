;FUNCTION GET_INTERSECT_POINT_BETWEEN_FAULT_AND_PROFILE, xys_fvec, a1,b1
FUNCTION INTERSECT_BETWEEN_POLYLINE_AND_LINE, xys_fvec, a1,b1,beta=beta,x1=x1,y1=y1,is_debug=is_debug
  npt=N_ELEMENTS(xys_fvec[0,*])
  
  if n_elements(is_debug) eq 0 then is_debug=0
  
  ;!!!Note: input variables should be double.
  
  IF is_debug EQ 1 THEN BEGIN
    WINDOW,1
    !p.MULTI=-1
    PLOT,[a1[0],b1[0],reform(xys_fvec[0,*])],[a1[1],b1[1], reform(xys_fvec[1,*])],psym=-2,background='ffffff'x,color='0'x,/ynozero;,/iso
    OPLOT,[a1[0],b1[0]],[a1[1],b1[1]],color='0000ff'x
  ENDIF
  ;
  FOR pi=0, npt-2 DO BEGIN
    x1=xys_fvec[*,pi]
    y1=xys_fvec[*,pi+1]
    rate=(y1[1]-x1[1])/(y1[0]-x1[0])
    ;print,'rate:',rate
    ;stop
    
    ;    plot,[x1[0],y1[0],a1[0],b1[0]],[x1[1],y1[1],a1[1],b1[1]],psym=2,background='ffffff'x,color='0'x,/ynozero;,/iso
;    help,a1,b1,x1,rate,c1
    LINE_INTERSECT_LINE, a1,b1, x1, rate, c1
    IF N_ELEMENTS(WHERE(FINITE(c1) EQ 1)) NE 2 THEN BEGIN
      ;PRINT,'problem with c1'
      CONTINUE
    ENDIF
    IF is_debug EQ 1 THEN BEGIN
      OPLOT,[x1[0],y1[0]],[x1[1],y1[1]],color='ff0000'x
      OPLOT,[a1[0],b1[0]],[a1[1],b1[1]],color='00ff00'x
      OPLOT,[x1[0],c1[0]],[x1[1],c1[1]],color='0000ff'x,linestyle=0,thick=2
      ;stop
    ENDIF
    
    IF c1[0] GT MAX([x1[0],y1[0]]) || c1[0] LT MIN([x1[0],y1[0]]) || $
      (c1[1] - MAX([x1[1],y1[1]])) GT 1d-6 || (MIN([x1[1],y1[1]])-c1[1]) GT 1d-06 THEN BEGIN
      ;      print,c1[0] GT MAX([x1[0],y1[0]]) , c1[0] LT MIN([x1[0],y1[0]]) , $
      ;      c1[1] GT MAX([x1[1],y1[1]]), c1[1] LT MIN([x1[1],y1[1]])
      ;      print,(c1[0] - MAX([x1[0],y1[0]])) gt 1d-6 , c1[0] LT MIN([x1[0],y1[0]]) , $
      ;      (c1[1] - MAX([x1[1],y1[1]])) gt 1d-6, (MIN([x1[1],y1[1]])-c1[1]) gt 1d-06
      ;print,'problem with x1/y1'
      ;stop
      CONTINUE
    ENDIF
    tmp=(x1[1]-y1[1])/(x1[0]-y1[0])
    beta=ATAN(tmp)
    
    ;stop
    RETURN,c1
  ENDFOR
  RETURN,REPLICATE(!values.D_NAN,2)
END
