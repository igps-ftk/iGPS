FUNCTION IS_POINT_INSIDE_POLYGON, poly_xys, pt_xy
  ;
  IF N_ELEMENTS(poly_xys) EQ 0 THEN BEGIN
    poly_xys=[[92.0861129999999970d0,      28.2706699999999990], $
      [89.5549620000000030,      28.6762920000000000], $
      [89.8731379999999970,      30.3064980000000010], $
      [92.4469069999999960,      29.9035450000000010] ]
    pt_xy=[91,29d0]
    
  ENDIF
  
  IF N_ELEMENTS(rect) EQ 0 THEN BEGIN
    xmin=MIN(poly_xys[0,*],max=xmax)
    ymin=MIN(poly_xys[1,*],max=ymax)
    rect=[xmin,xmax,ymin,ymax]
  ENDIF
  
  
  
;  WINDOW,0,xsize=512
;  PLOT,poly_xys[0,[0,1,2,3,0]],poly_xys[1,[0,1,2,3,0]],background='ffffff'x,color='0'x,psym=-2,/iso,/yno,xrange=rect[0:1],yrange=rect[2:3]
;  PLOTS,pt_xy[0],pt_xy[1],psym=4,color='ff00ff'x
  
  counts=INTARR(2)
  FOR li=0,N_ELEMENTS(poly_xys[0,*])-1 DO BEGIN
    a1=poly_xys[*,li]
    IF li EQ N_ELEMENTS(poly_xys[0,*])-1 THEN li_next=0 ELSE li_next=li+1
    b1=poly_xys[*,li_next]
    
    xmin1=MIN([a1[0],b1[0]],max=xmax1)
    ymin1=MIN([a1[1],b1[1]],max=ymax1)
    
;    OPLOT,[a1[0],b1[0]],[a1[1],b1[1]],psym=-1,color='00ffff'x
;    XYOUTS,b1[0],b1[1],'b1',color='ff0000'x
;    XYOUTS,a1[0],a1[1],'a1',color='ff0000'x
    rate=(b1[1]-a1[1])/(b1[0]-a1[0])
    y0=a1[1]-rate*a1[0]
    px=(pt_xy[1]-y0)/rate
    ;stop
    IF px LT xmin1 || px GT xmax1 THEN BEGIN  ;no intersection
      CONTINUE
    ENDIF
;    PLOTS,[px,pt_xy[0]],[pt_xy[1],pt_xy[1]],psym=-2,color='ff00ff'x
    ;stop
    IF px LT pt_xy[0] THEN BEGIN
      counts[0]=counts[0]+1
    ENDIF ELSE BEGIN
      counts[1]=counts[1]+1
    ENDELSE
  ENDFOR
  
  rems=counts MOD 2
  IF rems[0] EQ 1 && rems[1] EQ 1 THEN BEGIN
    RETURN,1
  ENDIF ELSE BEGIN
    RETURN,0
  ENDELSE
  STOP
  
END