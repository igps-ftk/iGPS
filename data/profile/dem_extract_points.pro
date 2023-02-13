PRO DEM_EXTRACT_POINTS, demfile,  $  ;DEM file (ENVI raw image format; with map projection)
    pfile,  $  ;profile line (two vertices; extended GMT psxy format)
    opath,  $  ;output path
    ffile=ffile  ;fault polyline (extended GMT psxy format)
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 3 THEN BEGIN
    demfile='C:\dem.gmt\envi.dem.min\envi.dem.min.raw'
    demfile='C:\dem.gmt\ETOPO1_Bed_g_geotiff\ETOPO1_Bed_g_geotiff.tif'
    demfile='C:\dem.gmt\ETOPO1_Bed_g_geotiff\ETOPO1_Bed_g_envi.img'
    
    cfile='sites.net'    
    
    ofile=desuffix(cfile)+'.llh'
    
  ENDIF
  
  
  READ_NET, cfile, site=sites,llh=llh
  nsit=N_ELEMENTS(sites)
  ;stop
  
  
  ENVI_OPEN_FILE,demfile,r_fid=fid
  ENVI_FILE_QUERY,fid,nb=nb,ns=ns,nl=nl
  dims=[-1,0,ns-1,0,nl-1]
  pos=0
  dem_grid=ENVI_GET_DATA(fid=fid,dims=dims,pos=pos)
  
  ;stop
  ;loop for each profile
  dems=DBLARR(nsit)
  FOR si=0,nsit-1 DO BEGIN
    ;
    lon=llh[0,si]
    lat=llh[1,si]
    ENVI_CONVERT_FILE_COORDINATES, fid, xf, yf, llh[0,si],llh[1,si]
    xf=FIX(xf)
    yf=FIX(yf)
    dems[si]=dem_grid[xf,yf]
  ;BREAK
  ;stop
  ENDFOR  ;end-of-loop for profiles
  
  ;STOP
  OPENW,fido,ofile,/get_lun
  WRITE_SYS_INFO,fido,prog=prog,src=[demfile,cfile],user=user
  PRINTF,fido,'site','longitude','latitude','height',format='("#",a4,1x,2(1x,a12),1x,a10,1x,a10)'
  FOR si=0, nsit-1 DO BEGIN
    PRINTF,fido,sites[si],llh[0:1,si],dems[si],format='(1x,a4,2(1x,f12.5),1x,f10.3,1x,f10.2)'
  ENDFOR
  FREE_LUN,fido
  PRINT,'['+PROG+']Normal end.'
  
END
