FUNCTION DOWNSAMPLE_POLYLINE, xys, dtol

  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  npt=N_ELEMENTS(xys[0,*])
  IF npt LE 2 THEN BEGIN
    ;HELP,xys
    RETURN, xys
  ENDIF
  
  dists=DBLARR(npt)
  a1=REFORM(xys[*,0])
  b1=REFORM(xys[*,npt-1])
  
  FOR i=1, npt-2 DO BEGIN
    c1=REFORM(xys[*,i])
    POINT_PERP_LINE, a1, b1,c1,d1
    dist_i=SQRT((c1[0]-d1[0])^2+(c1[1]-d1[1])^2)
    dists[i]=dist_i
  ENDFOR
  
  dist_max=MAX(dists, ind_max)
  ;print,moment(dists)
  ;stop
  IF dist_max LT dtol THEN BEGIN  ;discard all middle points
    oxys=xys[*,[0,npt-1]] ;only return the first and last points
  ENDIF ELSE BEGIN
    xys1=xys[*,0:ind_max] ;split apart
    xys2=xys[*,ind_max:*]
    IF N_ELEMENTS(xys1[0,*]) GE 3 THEN BEGIN
      oxys1=DOWNSAMPLE_POLYLINE(xys1,dtol)
    ENDIF ELSE BEGIN
      oxys1=xys1
    ;HELP,xys1
    ENDELSE
    IF N_ELEMENTS(xys2[0,*])  GE 3 THEN BEGIN
      oxys2=DOWNSAMPLE_POLYLINE(xys2,dtol)
    ENDIF ELSE BEGIN
      oxys2=xys2
    ;HELP,xys2
    ENDELSE
    oxys=[[oxys1[*,0:N_ELEMENTS(oxys1[0,*])-2]],[oxys2]]
  ENDELSE
  
  ;PRINT,oxys
  ;stop
  RETURN,oxys
END


PRO DOWNSAMPLE_POLYLINE, xys,   $
    dtol, $
    oxys=oxys,  $
    is_plot=is_plot
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
    
  IF N_PARAMS() LT 1 THEN BEGIN
    file='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy'
    file=FILEPATH('fa_mbt.psxy',root_dir=!igps_root,subdirectory=['tables'])
    
    READ_PSXY, file, region=regions
    
    xys=*regions
    is_plot=1
  ENDIF
  
  IF N_ELEMENTS(dtol) EQ 0 THEN BEGIN
    xs=SMOOTH(xys[0,*],N_ELEMENTS(xys[0,*])/5d0>5)
    ys=SMOOTH(xys[1,*],N_ELEMENTS(xys[0,*])/5d0>5)
    
    tmp=SQRT((xys[0,*]-xs)^2+(xys[1,*]-ys)^2)
    ;PRINT,MEAN(tmp)
    dtol=MEAN(tmp)/2d0
    PRINT,'['+PROG+']INFO: use tolerance ',dtol
  ENDIF
  
  
  ;STOP
  ;xys=xys[*,0:9]
  oxys=DOWNSAMPLE_POLYLINE(xys,dtol)
  
  ;print,oxys
  
  IF N_PARAMS() LT 1 THEN BEGIN
    WINDOW,2,xsize=1200,ysize=800
    !p.MULTI=[0,2,2]
    
    PLOT,xys[0,*],xys[1,*],background='ffffff'x,color='0'x,psym=-0,/yno,thick=2
    OPLOT,xys[0,*],xys[1,*],color='999999'x,psym=1
    
    PLOT,xys[0,*],background='ffffff'x,color='0'x,psym=-0,/yno,thick=2,ytitle='lon'
    OPLOT,xs,color='00ff00'x
    
    
    PLOT,xys[1,*],background='ffffff'x,color='0'x,psym=-0,/yno,thick=2,ytitle='lat'
    OPLOT,ys,color='00ff00'x
    ;WINDOW,2
    PLOT,xys[0,*],xys[1,*],background='ffffff'x,color='0'x,psym=-0,/yno,thick=2
    OPLOT,oxys[0,*],oxys[1,*],color='ff0000'x,psym=-6,thick=1
    HELP,xys,oxys
    
  ;STOP
  ENDIF
END