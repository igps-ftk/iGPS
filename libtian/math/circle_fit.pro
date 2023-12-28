FUNCTION CIRCLE_FIT, xys, $
    yfit=yfit,  $
    strikes=strikes,  $
    xys_fit=xys_fit
    
  x1=REFORM(xys[0,*])
  y1=REFORM(xys[1,*])
  
  B=x1^2+y1^2
  
  npt=N_ELEMENTS(x1)
  A=DBLARR(3,npt)
  FOR i=0,npt-1 DO BEGIN
    A[*,i]=[1,x1[i],y1[i]]
  ENDFOR
  
  ;HELP, A,B
  ;HELP,xys,oxys
  COEF = LA_LEAST_SQUARES( $
    A, $
    B , $
    /DOUBLE, $
    ;CORRELATION=CCOV, $
    RESIDUAL=RESIDUAL)
  yfit=B-residual
  
  A1=REFORM(A[1:*,*])
  COEF1a = REGRESS( $
    A1, $
    B , $
    CHISQ=CHISQ,SIGMA=SIGMA1,CONST=CONST, $
    /DOUBLE,STATUS=STATUS,yfit=yfit1)
    
  COEF1=[CONST,REFORM(COEF1a)]
  
  aa=coef[1]
  bb=coef[2]
  cc=coef[0]
  xc=aa/2d0
  yc=bb/2d0
  rr=SQRT(cc+xc^2+yc^2)
  ;print,xc,yc,rr
  ;  print,'coef:',coef
  ;  print,'coef1:',coef1
  
  ;yfit
  yfit_a=SQRT(rr^2-(x1-xc)^2)+yc
  yfit_b=-1*SQRT(rr^2-(x1-xc)^2)+yc
  yfit=yfit_a
  IF ABS(y1[0]-yfit_a[0]) GT ABS(y1[0]-yfit_b[0]) THEN yfit=yfit_b
  
  ;calculate strike direction of profiles
  strikes=DBLARR(npt)
  FOR i=0,npt-1 DO BEGIN
    xy=REFORM(xys[*,i])
    strikes[i]=ATAN(xy[1]-yc,xy[0]-xc)
  ENDFOR
  
  ;
  xys_fit=TRANSPOSE([[x1],[yfit]])
  ;stop
  
  RETURN,[xc,yc,rr]
END


PRO CIRCLE_FIT, xys, $
    yfit=yfit,  $
    strikes=strikes,  $
    xys_fit=xys_fit
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy'
    ;    file=FILEPATH('fa_mbt.psxy',root_dir=!igps_root,subdirectory=['tables'])
    
    READ_PSXY, file, region=regions
    
    xys0=*regions
    
    xs=SMOOTH(xys0[0,*],N_ELEMENTS(xys0[0,*])/5d0>5)
    ys=SMOOTH(xys0[1,*],N_ELEMENTS(xys0[0,*])/5d0>5)
    
    tmp=SQRT((xys0[0,*]-xs)^2+(xys0[1,*]-ys)^2)
    PRINT,MEAN(tmp)
    dtol=MEAN(tmp)/2d0
    
    WINDOW,2,xsize=1200,ysize=800
    !p.MULTI=[0,2,2]
    
    PLOT,xys0[0,*],xys0[1,*],background='ffffff'x,color='0'x,psym=-0,/yno,thick=2
    OPLOT,xs,ys,color='0000ff'x,psym=-0
    
    PLOT,xys0[0,*],background='ffffff'x,color='0'x,psym=-0,/yno,thick=2,ytitle='lon'
    OPLOT,xs,color='00ff00'x
    
    
    PLOT,xys0[1,*],background='ffffff'x,color='0'x,psym=-0,/yno,thick=2,ytitle='lat'
    OPLOT,ys,color='00ff00'x
    
    ;  ;STOP
    ;  ;xys=xys[*,0:9]
    xys=DOWNSAMPLE_POLYLINE(xys0,dtol)
    
    
  ENDIF
  
  param=CIRCLE_FIT(xys,yfit=yfit, strikes=strikes, xys_fit=xys_fit)
  xc=param[0]
  yc=param[1]
  rr=param[2]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,xc,yc,rr
    
    ;
    PLOT,[REFORM(xys0[0,*]),[xc]],[REFORM(xys0[1,*]),[yc]],background='ffffff'x,color='0'x,psym=-0,/yno,thick=2,/nodata,/iso
    OPLOT,[REFORM(xys0[0,*])],[REFORM(xys0[1,*])],color='0'x
    OPLOT,xys[0,*],xys[1,*],color='ff0000'x,psym=-1,thick=1
    ;  OPLOT,x2s,y2s,color='0000ff'x,psym=-0
    ;OPLOT,x2s,y2s-2*(y2s-yc),color='0000ff'x,psym=-0
    ;OPLOT,oxys[0,*],yfit,color='00ffff'x,thick=2,psym=-0
    OPLOT,xys_fit[0,*],xys_fit[1,*],color='00ffff'x,thick=2,psym=-0
    OPLOT,[xc],[yc],color='ff9900'x,psym=2
    FOR i=0,N_ELEMENTS(yfit)-1 DO BEGIN
      OPLOT,[xc,xys[0,i]],[yc,yfit[i]],color='aaaaaa'x,thick=1,psym=-0
      IF (i MOD 5) EQ 0 THEN XYOUTS,(xc+xys[0,i])/2,(yc+xys[1,i])/2,STRING(strikes[i]*180/!dpi+0,format='(f7.1)'),ORIENTATION=strikes[i]*180/!dpi+180,color='0'x
    ENDFOR
    
    PRINT,'maximum strike difference (degree):',ABS(strikes[0]-last(strikes))*180/!dpi,' between ',first(strikes)*180/!dpi,' - ',last(strikes)*180/!dpi
    ;
    HELP, xys_fit
  ;  HELP,coef
  ;STOP
  ENDIF
END