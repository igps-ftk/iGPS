PRO DEM_GRID_EXTRACT_PROFILE, demfile,  $  ;DEM file (ENVI raw image format; with map projection)
    pfile,  $  ;profile line (two vertices; extended GMT psxy format)
    opath,  $  ;output path
    ffile=ffile  ;fault polyline (extended GMT psxy format)
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 3 THEN BEGIN
    demfile='C:\dem.gmt\envi.dem.min\envi.dem.min.raw'
    demfile='C:\dem.gmt\ETOPO1_Bed_g_geotiff\ETOPO1_Bed_g_geotiff.tif'
    
    pfile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\bengco\vector\profile_gulu.psxy'
    opath='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\lys\20161124\profiles.gl\'
    ffile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\bengco\vector\fault_gulu2.psxy'
    
    ;    pfile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\bengco\vector\profile_bengco.psxy'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\lys\20161124\profiles.bc'
    ;    ffile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\bengco\vector\fault_beng_co_east.psxy'
    ;    ;ffile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\bengco\vector\fault_beng_co_west.psxy'
    
    ;gulu profiles
    ffile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\bengco\vector\fault_gulu2.psxy'
    ffile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\bengco\vector\fault_ydgl.psxy'
    pfile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\bengco\vector\profile_gulu.psxy'
    opath='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\lys\20161129\176\profiles.gl'
    
  ENDIF
  
  IF N_ELEMENTS(ffile) EQ 0 THEN ffile=''
  
  ;read profiles
  READ_PSXY,   $
    pfile,   $ ;input file
    region=pfs,   $ ;x,y coorinates of each polygons (pointer type)
    nps=nps, $  ;number of point pairs for each polygon
    count=np,  $ ;number of polygons
    names=names   ;region names (if exist)
  ;
  tmplons=0d0
  tmplats=0d0
  FOR i=0,np-1 DO BEGIN
    tmplons=[tmplons,REFORM((*pfs[i])[0,*])]
    tmplats=[tmplats,REFORM((*pfs[i])[1,*])]
  ENDFOR
  tmplons=tmplons[1:*]
  tmplats=tmplats[1:*]
  WINDOW,1
  !p.MULTI=-1
  PLOT,tmplons,tmplats,background='ffffff'x,color='0'x,/nodata,/ynozero;,xstyle=1,ystyle=1
  FOR i=0,np-1 DO BEGIN
    lons=REFORM((*pfs[i])[0,*])
    lats=REFORM((*pfs[i])[1,*])
    OPLOT,lons,lats,color='ff0000'x,psym=-5
    XYOUTS,lons[0],lats[0],STRTRIM(i+1,2),color='ff0000'x
  ENDFOR
  ;STOP
  
  
  ;read fault lines
  READ_PSXY,   $
    ffile,   $ ;input file
    region=pfs_fa,   $ ;x,y coorinates of each polygons (pointer type)
    nps=nps_fa, $  ;number of point pairs for each polygon
    count=np_fa,  $ ;number of polygons
    names=names_fa   ;region names (if exist)
  IF np_fa NE 1 THEN STOP
  lons_fa=REFORM((*pfs_fa[0])[0,*])
  lats_fa=REFORM((*pfs_fa[0])[1,*])
  OPLOT,lons_fa,lats_fa,color='0'x,psym=-4
  xys_fvec=TRANSPOSE([[lons_fa],[lats_fa]])
  ;stop
  
  
  ENVI_OPEN_FILE,demfile,r_fid=fid
  ENVI_FILE_QUERY,fid,nb=nb,ns=ns,nl=nl
  dims=[-1,0,ns-1,0,nl-1]
  pos=0
  dem_grid=ENVI_GET_DATA(fid=fid,dims=dims,pos=pos)
  
  ;stop
  ;loop for each profile
  FOR pi=0,np-1 DO BEGIN
    pf=REFORM(*pfs[pi])
    a1=REFORM(pf[*,0])
    b1=REFORM(pf[*,1])
    WSET,1
    OPLOT,pf[0,*],pf[1,*],color='0000ff'x,thick=2,linestyle=2
    ;print,pf
    ;stop
    ;if no intersect between profile and fault lines, skip current profile
    xy3=INTERSECT_BETWEEN_POLYLINE_AND_LINE(xys_fvec,a1,b1)
    ;stop
    IF FINITE(xy3[0]) NE 1 THEN BEGIN
      PRINT,'['+prog+']WARNING:no intersect between fault polyline and profile '+STRTRIM(pi+1,2)+'!'
      CONTINUE
    ENDIF
    PLOTS,xy3[0],xy3[1],psym=6,color='0'x,symsize=1
    ;
    ENVI_CONVERT_FILE_COORDINATES, fid, xf1, yf1, a1[0], a1[1]
    ENVI_CONVERT_FILE_COORDINATES, fid, xf2, yf2, b1[0], b1[1]
    xf1=FIX(xf1)
    xf2=FIX(xf2)
    yf1=FIX(yf1)
    yf2=FIX(yf2)
    nx=ABS(xf2-xf1)+1
    ny=ABS(yf2-yf1)+1
    ;    print,'nx:',nx,' ny:',ny
    ;    print,xf1,xf2,yf1,yf2
    ;STOP
    npt=1000
    npt=MAX([nx,ny])
    
    rate=(yf2*1d0-yf1*1d0)/(xf2*1d0-xf1*1d0)
    ;rate=(yf2*1d0-yf1)/(xf2-xf1)
    ;intercept=([yf1,yf2])[indmin]-rate*([xf1,xf2])[indmin]
    intercept=yf1-rate*xf1
    ;    print,rate,intercept
    ;    intercept=yf2-rate*xf2
    ;    print,rate,intercept
    
    IF nx GT ny THEN BEGIN
      xfs=LINDGEN(npt)+MIN([xf1,xf2],indmin)
      yfs=xfs*rate+intercept
    ENDIF ELSE BEGIN
      yfs=LINDGEN(npt)+MIN([yf1,yf2],indmin)
      xfs=(yfs-intercept)/rate
    ;STOP
    ENDELSE
    
    odata=DBLARR(4,npt)
    FOR pxi=0, npt-1 DO BEGIN
      xf=xfs[pxi]
      yf=yfs[pxi]
      ENVI_CONVERT_FILE_COORDINATES, fid, xf, yf, xmap, ymap,/to_map
      odata[0:2,pxi]=[xmap,ymap,dem_grid[xf,yf]]
      tmp=MAP_2POINTS(xy3[0],xy3[1],xmap,ymap,/meter)*1d-3
      odata[3,pxi]=tmp*(xmap-xy3[0])/ABS(xmap-xy3[0])
    ;stop
    ENDFOR
    
    WINDOW,2,ysize=700
    !p.MULTI=[0,1,2]
    ;
    
    PLOT,[xf1,xf2],[yf1,yf2],background='ffffff'x,color='0'x,psym=-5,/ynozero,/iso
    PLOTS,xfs,yfs,color='ff0000'x,psym=4
    PRINT,xf1,xf2,MIN(xfs),MAX(xfs),xf2-xf1
    PRINT,yf1,yf2,MIN(yfs),MAX(yfs),yf2-yf1
    ;
    ;
    PLOT,odata[0,*],odata[2,*],background='ffffff'x,color='0'x
    !p.MULTI=-1
    ;stop
    
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i02)')+'_vel_dem.txt'
    OPENW,fido,ofile,/get_lun
    WRITE_SYS_INFO,fido,prog=prog,src=[demfile,pfile],user=user
    PRINTF,fido,'longitude','latitude','height','distance',format='("#",2(1x,a12),1x,a10,1x,a10)'
    FOR pxi=0, npt-1 DO BEGIN
      PRINTF,fido,odata[0:3,pxi],format='(1x,2(1x,f12.5),1x,f10.3,1x,f10.2)'
    ENDFOR
    FREE_LUN,fido
    jfile=desuffix(ofile)+'.jpg'
    WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
  ;BREAK
  ;stop
  ENDFOR  ;end-of-loop for profiles
  ;STOP
  WDELETE,1
  WDELETE,2
  ;
  PRINT,'['+PROG+']Normal end.'
  
END
