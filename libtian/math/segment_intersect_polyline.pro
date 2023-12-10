FUNCTION SEGMENT_INTERSECT_POLYLINE, xys_fvec, a1,b1, is_plot=is_plot

  IF N_ELEMENTS(is_plot) EQ 0 THEN is_plot=0
  
  npt=N_ELEMENTS(xys_fvec[0,*])
  FOR pi=0, npt-2 DO BEGIN
    x1=xys_fvec[*,pi]
    y1=xys_fvec[*,pi+1]
    rate=(y1[1]-x1[1])/(y1[0]-x1[0])
    LINE_INTERSECT_LINE, a1,b1, x1, rate, c1
    IF is_plot EQ 1 THEN BEGIN
      OPLOT,[x1[0],y1[0]],[x1[1],y1[1]],color='ff0000'x
      OPLOT,[x1[0],c1[0]],[x1[1],c1[1]],color='ff0000'x,linestyle=2
    ENDIF
    IF c1[0] GT MAX([x1[0],y1[0]]) || c1[0] LT MIN([x1[0],y1[0]]) || $
      c1[1] GT MAX([x1[1],y1[1]]) || c1[1] LT MIN([x1[1],y1[1]]) THEN CONTINUE
    RETURN,c1
  ENDFOR
  RETURN,REPLICATE(!values.D_NAN,2)
END



PRO SEGMENT_INTERSECT_POLYLINE

  xys_polyline=[[92.0861129999999970d0,      28.2706699999999990], $
    [89.5549620000000030,      28.6762920000000000], $
    [89.8731379999999970,      30.3064980000000010], $
    [92.4469069999999960,      29.9035450000000010] ]
  a1=[91,29d0]
  b1=[88,       30.556845]
  
  
  WINDOW,0,xsize=1024
  !p.MULTI=-1
  DEVICE, DECOMPOSED=1
  PLOT,[REFORM(xys_polyline[0,*]),a1[0],b1[0]],[REFORM(xys_polyline[1,*]),a1[1],b1[1]],background='ffffff'x,color='0'x,psym=-0, $
    /iso, $
    /yno, $
    ;xrange=rect[0:1],yrange=rect[2:3], $
    /nod
  OPLOT, xys_polyline[0,*],xys_polyline[1,*],color='0'x,psym=-2
  OPLOT,[a1[0],b1[0]],[a1[1],b1[1]],psym=-6,color='ff00ff'x
  
  c1=segment_intersect_polyline( xys_polyline, a1,b1, is_plot=1)
  PLOTS,c1[0],c1[1],psym=5,color='ff0000'x,symsize=3
  
END