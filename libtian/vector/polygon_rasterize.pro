PRO POLYGON_RASTERIZE, xys,rect=rect,xstep=xstep,ystep=ystep, $  ;,nx=nx,ny=ny
    odata=odata,oxs=oxs,oys=oys
  ;
  IF N_ELEMENTS(xys) EQ 0 THEN BEGIN
    xys=[[92.0861129999999970d0,      28.2706699999999990], $
      [89.5549620000000030,      28.6762920000000000], $
      [89.8731379999999970,      30.3064980000000010], $
      [92.4469069999999960,      29.9035450000000010] ]
    xstep=.1
    ystep=.1
    rect=[85,95,25,35]
  ENDIF
  
  IF N_ELEMENTS(rect) EQ 0 THEN BEGIN
    xmin=MIN(xys[0,*],max=xmax)
    ymin=MIN(xys[1,*],max=ymax)
    rect=[xmin,xmax,ymin,ymax]
  ENDIF
  
  ;  if n_elements(nx) eq 0 then nx=10
  ;  if n_elements(ny) eq 0 then ny=10
  
  IF N_ELEMENTS(xstep) EQ 0 THEN BEGIN
    nx=10
    xstep=(rect[1]-rect[0])/nx
  ENDIF
  IF N_ELEMENTS(ystep) EQ 0 THEN BEGIN
    ny=10
    ystep=(rect[3]-rect[2])/ny
  ENDIF
  
  nx=CEIL((rect[1]-rect[0])/xstep)
  ny=CEIL((rect[3]-rect[2])/ystep)
  ;PRINT,rect,nx,ny,xstep,ystep
  
  odata=bytarr(nx,ny)
  oxs=DBLARR(nx,ny)
  oys=DBLARR(nx,ny)
  
  ;WINDOW,0,xsize=512
  ;PLOT,xys[0,[0,1,2,3,0]],xys[1,[0,1,2,3,0]],background='ffffff'x,color='0'x,psym=-2,/iso,/yno,xrange=rect[0:1],yrange=rect[2:3]
  
  FOR i=0, nx-1 DO BEGIN
    oxs[i,*]=rect[0]+xstep*i
    FOR j=0, ny-1 DO BEGIN
      oys[i,j]=rect[2]+ystep*j
      ;is_in=IS_POINT_INSIDE_POLYGON(xys, [86,36])
      is_in=IS_POINT_INSIDE_POLYGON(xys, [oxs[i,j],oys[i,j]])
      IF is_in NE 1 THEN CONTINUE
      ;plots,oxs[i,j],oys[i,j],psym=2,color='ff0000'x
      ;stop
      odata[i,j]=1
    ;wait,.01
    ENDFOR
  ENDFOR
END