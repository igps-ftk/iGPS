;FUNCTION GET_INTERSECT_POINT_BETWEEN_FAULT_AND_PROFILE, xys_fvec, a1,b1
FUNCTION INTERSECT_BETWEEN_POLYLINE_AND_LINE, xys_fvec, a1,b1,beta=beta,x1=x1,y1=y1
  npt=N_ELEMENTS(xys_fvec[0,*])
  FOR pi=0, npt-2 DO BEGIN
    x1=xys_fvec[*,pi]
    y1=xys_fvec[*,pi+1]
    rate=(y1[1]-x1[1])/(y1[0]-x1[0])
    POINT_CROSS_LINE, a1,b1, x1, rate, c1
    ;OPLOT,[x1[0],y1[0]],[x1[1],y1[1]],color='ff0000'x
    ;OPLOT,[x1[0],c1[0]],[x1[1],c1[1]],color='ff0000'x,linestyle=2
    IF c1[0] GT MAX([x1[0],y1[0]]) || c1[0] LT MIN([x1[0],y1[0]]) || $
      c1[1] GT MAX([x1[1],y1[1]]) || c1[1] LT MIN([x1[1],y1[1]]) THEN CONTINUE
    tmp=(x1[1]-y1[1])/(x1[0]-y1[0])
    beta=atan(tmp)
    ;stop
    RETURN,c1
  ENDFOR
  RETURN,REPLICATE(!values.D_NAN,2)
END
