PRO SAR_FAULT_SLIP_RATE_SIMU, fa, vmax, locking_depth, rmargin=rmargin, ofile=ofile

  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 3 THEN BEGIN
    fa='fake_west_kunlun
    fa='test_altyntagh'
    vmax=10d0  ;in mm/a
    locking_depth=15d0 ;in km
    
    ofile='C:\tmp\sliplos\vel_ll.llen'
  ENDIF
  
  IF N_ELEMENTS(rmargin) EQ 0 THEN BEGIN
    rmargin=1d0
    rmargin=3d0
  ;rmargin=0d0
  ENDIF
  
  IF N_ELEMENTS(xstep) EQ 0 THEN xstep=.25d0
  IF N_ELEMENTS(ystep) EQ 0 THEN ystep=.25d0
  
  PROFILE_NAME2VECTORFILE,   $
    fa,   $ ;input, fault name
    ffile=ffile   ;output, fault file
    
    
  ;read fault vector (if specified)
  lines_fvec=read_txt(ffile)
  lines_fvec2=STRTRIM(lines_fvec,2)
  pos=WHERE(strmids(lines_fvec2,0,1) NE '>')
  IF N_ELEMENTS(pos) LT 2 THEN BEGIN
    PRINT,'['+prog+']ERROR: invalid fault line vector file <'+ffile+'>!!'
    RETURN
  ENDIF
  xys_fvec=DOUBLE(str_lines2arr(lines_fvec2[pos]))
  ;stop
  
  xmin=MIN(xys_fvec[0,*],max=xmax)
  ymin=MIN(xys_fvec[1,*],max=ymax)
  rect=[Xmin, Ymin, Xmax, Ymax]
  rect=[Xmin-rmargin, Ymin-rmargin, Xmax+rmargin, Ymax+rmargin] ;enlarge the rect range
  ;
  nx=CEIL( (rect[2]-rect[0])/xstep )
  ny=CEIL( (rect[3]-rect[1])/ystep )
  
  xs=rect[0]+INDGEN(nx)*xstep
  ys=rect[1]+INDGEN(ny)*ystep
  
  lons=DBLARR(nx,ny)
  lats=DBLARR(nx,ny)
  vels=DBLARR(nx,ny)
  dists=DBLARR(nx,ny)
  
  WINDOW,1,xsize=1000,ysize=1000
  !p.MULTI=-1
  PLOT,xys_fvec[0,*],xys_fvec[1,*],background='ffffff'x,color='0'x,/nodata,/ynozero,  $
    xrange=[rect[0],rect[2]],yrange=[rect[1],rect[3]],  $
    title='Fault and Profiles',/iso  ;,xstyle=1,ystyle=1
  FOR i=0,ny-1 DO BEGIN
    PLOTS,xs,REPLICATE(ys[i],nx),psym=4,color='0000ff'x
    lons[*,i]=xs
    lats[*,i]=ys[i]
  ENDFOR
  OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='ff0000'x
  
  x1=REFORM(xys_fvec[*,0])
  x2=REFORM(xys_fvec[*,1])
  ;calculate strike of fault segment
  rateoff=(x1[1]-x2[1])/(x1[0]-x2[0])
  sar_beta=!dpi/2-ATAN(rateoff);+!dpi
  PRINT,'['+prog+']fault strike:',sar_beta*180/!dpi
  PLOTS,[x1[0],x2[0]],[x1[1],x2[1]],color='ffff00'x,thick=2,psym=-2
  
  FOR i=0,nx-1 DO BEGIN
    FOR j=0,ny-1 DO BEGIN
      xi=lons[i,j]
      yi=lats[i,j]
      x3=[xi,yi]    ;
      POINT_PERP_LINE,  x1,x2, x3, x4
      ;OPLOT,[xi],[yi],psym=2,color='ff0000'x
      ;OPLOT,[x4[0]],[x4[1]],psym=5,color='ff0000'x
      ;OPLOT,[x3[0],x4[0]],[x3[1],x4[1]],color='00ff'x
      ;
      
      dist1=MAP_2POINTS(x3[0],x3[1],x4[0],x4[1],/meters)*1d-3 ;in km
      dist2=dist1*(ABS(x3[0]-x4[0])/(x3[0]-x4[0]))  ;in km
      dists[i,j]=dist2
      ;
      vel_along_fault=vmax/!dpi*ATAN(dist1/locking_depth)*dist2/abs(dist2)
      vels[i,j]=vel_along_fault
      ;STOP
    ENDFOR
    ;STOP
  ENDFOR
  sf=1d0
  velovect, vels*sin(sar_beta)*sf,vels*cos(sar_beta)*sf,lons[*,0],lats[0,*],/overplot,color='0'x
  ;STOP
  IF N_ELEMENTS(ofile) NE 0 THEN BEGIN
    OPENW,fid,ofile,/get_lun
    FOR i=0,nx-1 DO BEGIN
      FOR j=0,ny-1 DO BEGIN
        PRINTF,fid,lons[i,j],lats[i,j],vels[i,j]*SIN(sar_beta),vels[i,j]*COS(sar_beta), $
          vels[i,j],sar_beta, dists[i,j], $
          format='(1x,2(1x,f16.10),1x,2(1x,f20.12),4x,10(1x,f22.8))'
      ENDFOR
    ENDFOR
    FREE_LUN,fid
  ENDIF
;STOP
END