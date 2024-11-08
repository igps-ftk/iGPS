FUNCTION POINT_DIST_TO_POLYGON, poly_xys, pt_xy
  ;
  IF N_ELEMENTS(poly_xys) EQ 0 THEN BEGIN
    poly_xys=[[92.0861129999999970d0,      28.2706699999999990], $
      [89.5549620000000030,      28.6762920000000000], $
      [89.8731379999999970,      30.3064980000000010], $
      [92.4469069999999960,      29.9035450000000010] ]
    pt_xy=[91,29d0]
    
    is_plot=1
  ENDIF
  
  if n_elements(is_plot) eq 0 then is_plot=0
  
  IF N_ELEMENTS(rect) EQ 0 THEN BEGIN
    xmin=MIN(poly_xys[0,*],max=xmax)
    ymin=MIN(poly_xys[1,*],max=ymax)
    rect=[xmin,xmax,ymin,ymax]
  ENDIF
  
  
  IF is_plot EQ 1 THEN BEGIN
    WINDOW,0,xsize=512
    PLOT,poly_xys[0,[0,1,2,3,0]],poly_xys[1,[0,1,2,3,0]],background='ffffff'x,color='0'x,psym=-2,/iso,/yno,xrange=rect[0:1],yrange=rect[2:3]
    PLOTS,pt_xy[0],pt_xy[1],psym=4,color='ff00ff'x
  ENDIF
  ;stop
  
  counts=INTARR(2)
  odists=DBLARR(N_ELEMENTS(poly_xys[0,*]))
  ;
  FOR li=0,N_ELEMENTS(poly_xys[0,*])-1 DO BEGIN
    a1=poly_xys[*,li]
    IF li EQ N_ELEMENTS(poly_xys[0,*])-1 THEN li_next=0 ELSE li_next=li+1
    b1=poly_xys[*,li_next]
    
    xmin1=MIN([a1[0],b1[0]],max=xmax1)
    ymin1=MIN([a1[1],b1[1]],max=ymax1)
    
    IF is_plot EQ 1 THEN BEGIN
      OPLOT,[a1[0],b1[0]],[a1[1],b1[1]],psym=-1,color='00ffff'x
      XYOUTS,b1[0],b1[1],'b1',color='ff0000'x
      XYOUTS,a1[0],a1[1],'a1',color='ff0000'x
    ENDIF
    
    c1=pt_xy
    POINT_PERP_LINE, a1, b1, c1, d1
    OPLOT,[c1[0],d1[0]],[c1[1],d1[1]],psym=-1,color='00ffff'x
    XYOUTS,c1[0],c1[1],'c1',color='ff0000'x
    XYOUTS,d1[0],d1[1],'d1',color='ff0000'x
    
    e1=a1
    f1=b1
    if a1[0] gt b1[0] then begin
      e1=b1
      f1=a1
    endif
    
    g1=d1
    if d1[0] lt e1[0] then g1=e1
    if d1[0] gt f1[0] then g1=f1
    
      tmp=MAP_2POINTS(c1[0],c1[1],g1[0],g1[1],/meter)
      odists[li]=tmp*1d-3
  ;STOP
    
  ;    ;
  ;    CONTINUE
  ;    rate=(b1[1]-a1[1])/(b1[0]-a1[0])
  ;    y0=a1[1]-rate*a1[0]
  ;    px=(pt_xy[1]-y0)/rate
  ;    ;stop
  ;    IF px LT xmin1 || px GT xmax1 THEN BEGIN  ;no intersection
  ;      CONTINUE
  ;    ENDIF
  ;
  ;    IF is_plot EQ 1 THEN BEGIN
  ;      PLOTS,[px,pt_xy[0]],[pt_xy[1],pt_xy[1]],psym=-2,color='ff00ff'x
  ;    ENDIF
  ;    ;stop
  ;    IF px LT pt_xy[0] THEN BEGIN
  ;      counts[0]=counts[0]+1
  ;    ENDIF ELSE BEGIN
  ;      counts[1]=counts[1]+1
  ;    ENDELSE
  ENDFOR
  
  ;  rems=counts MOD 2
  ;  IF rems[0] EQ 1 && rems[1] EQ 1 THEN BEGIN
  ;    RETURN,1
  ;  ENDIF ELSE BEGIN
  ;    RETURN,0
  ;  ENDELSE
  ;  STOP
  RETURN,odists
  
END

PRO POINT_DIST_TO_POLYGON
  PRINT,point_dist_to_polygon()
END