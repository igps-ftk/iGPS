FUNCTION POLYGON_CENTER, pxys
  IF N_ELEMENTS(is_plot) EQ 0 THEN is_plot=0
  IF is_plot EQ 1 THEN BEGIN
    WINDOW,1
    PLOT,pxys[0,*],pxys[1,*],psym=1, background='ffffff'x,color='0'x,/ynozero
  ENDIF
  
  TRIANGULATE, pxys[0,*],pxys[1,*], tris
  nt=N_ELEMENTS(tris[0,*]) ;number of triangles
  cxys=DBLARR(2,nt) ;center of triangles
  cxys[*]=-9999
  sxys=DBLARR(nt) ;area of triangles
  FOR i=0,nt-1 DO BEGIN
    IF is_plot EQ 1 THEN BEGIN
      OPLOT,pxys[0,tris[0:1,i]], pxys[1,tris[0:1,i]],color='0000ff'x
      OPLOT,pxys[0,tris[1:2,i]], pxys[1,tris[1:2,i]],color='00ffff'x
      OPLOT,pxys[0,tris[[0,2],i]], pxys[1,tris[[0,2],i]],color='ff0000'x
    ENDIF
    ;
    xy1=pxys[*,tris[0,i]]
    xy2=pxys[*,tris[1,i]]
    xy3=pxys[*,tris[2,i]]
    ;
    cxy=(xy1+xy2+xy3)/3
    ;
    ;s =  ( (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1) ) / 2；
    sxy=( (xy2[0]-xy1[0])*(xy3[1]-xy1[1])-(xy3[0]-xy1[0])*(xy2[1]-xy1[1]) )/2
    ;
    x1=xy1[0]
    y1=xy1[1]
    x2=xy2[0]
    y2=xy2[1]
    x3=xy3[0]
    y3=xy3[1]
    sxy2=x1*y2-x2*y1+x2*y3-x3*y2+x3*y1-x1*y3
    sxy2=sxy2/2
    ;PRINT,i+1,sxy,sxy2
    ;
    ;stop
    is_in=IS_POINT_INSIDE_POLYGON(pxys[*,0:N_ELEMENTS(pxys[0,*])-1], cxy)
    ;is_in=1
    IF is_in EQ 1 THEN BEGIN
      IF is_plot EQ 1 THEN BEGIN
        polyfill,[x1,x2,x3],[y1,y2,y3],color='00ffff'x
        PLOTS,cxy[0],cxy[1],psym=2,color='0'x
        XYOUTS,cxy[0],cxy[1],STRTRIM(i+1,2),color='0'x,/data,alignment=.05
      ENDIF
      cxys[*,i]=cxy
      sxys[i]=sxy
    ENDIF
  ;stop
  ENDFOR
  ind=WHERE(cxys[0,*] NE -9999)
  
  p_cx=TOTAL(REFORM(cxys[0,ind])*sxys[ind])/(1*TOTAL(sxys[ind]))
  p_cy=TOTAL(REFORM(cxys[1,ind])*sxys[ind])/(1*TOTAL(sxys[ind]))
  p_cxy=[p_cx,p_cy]
  ;PRINT,p_cxy
  
  tmp=max(sxys,indmax)
  p_cxy=cxys[*,indmax]
  
  IF is_plot EQ 1 THEN BEGIN
    PLOTS,p_cxy[0],p_cxy[1],psym=4,color='0'x,symsize=2
  ENDIF
  ;STOP
  RETURN,p_cxy
END

PRO POLYGON_CENTER, file
  IF N_ELEMENTS(file) EQ 0 THEN BEGIN
    file='D:\gpse\eq.sc08\block\defnode\Xianshuihe\test_center.psxy'
  ENDIF
  
  READ_PSXY,   $
    file,   $ ;input file
    region=regions,   $ ;x,y coorinates of each polygons (pointer type)
    nps=nps, $  ;number of point pairs for each polygon
    count=count,  $ ;number of polygons
    igpsmode=igpsmode,  $ ;whether skip comments lines (i.e., non-blank first column lines)
    names=names   ;region names (if exist)
    
  FOR i=0,count-1 DO BEGIN
    pxys=(*regions[i])
    c=polygon_center(pxys)
    PRINT,i,c
  ENDFOR
  FOR i=0,count-1 DO PTR_FREE,regions[i]
END