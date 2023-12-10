PRO LINE_POINT_EXTEND,  xy,  $
    rate, $
    length_extend,   $ ;
    oxy=oxy, $
    dummy=dummy
    
  IF N_PARAMS() LT 3 THEN BEGIN
    xy=[98,35]
    rate=1.95
    length_extend=2
  ENDIF  
  
  y_intercept=xy[1]-rate*xy[0]
  ox1=-1d3
  ox1=xy[0]-length_extend*COS(ATAN(rate))
  oy1=rate*ox1+y_intercept
  ox2=1d3
  ox2=xy[0]+length_extend*COS(ATAN(rate))
  oy2=rate*ox2+y_intercept
  
  
  oxy=[[ox1,oy1],[ox2,oy2]]
  
  IF N_PARAMS() LT 3 THEN BEGIN
  
    WINDOW,1,xsize=600,ysize=800
    !p.MULTI=-1
    PLOT,[xy[0],ox1,ox2],[xy[1],oy1,oy2],background='ffffff'x,color='0'x,/nodata,/ynozero,  $
      title='Extend from point along line', $
      /iso  , $
      ;xstyle=1,ystyle=1,  $
      xtitle='Longitude',ytitle='Latitude'
    OPLOT,[xy[0]],[xy[1]],color='00ff00'x,psym=2
    OPLOT,[ox2,ox1],[oy2,oy1],color='00ff00'x,psym=-6
    PRINT,'xy:',xy
    print,'rate and intercept:',rate,y_intercept
    PRINT,'oxy:',oxy
    
    ;stop
  ENDIF
;stop
END