PRO SAR_MULTI_LOOK_PREP_SITE_INCIDENCE, files,  $ ;
    lfile,  $ ;
    ofile
  IF N_PARAMS() LT 3 THEN BEGIN
    files=['D:\gsar\asc\dangxiong.b\asc_F3\incidences\look.nc',$  ;ascending dangxiong.b
      'D:\gsar\asc\mila1\asc_F1\incidences\look.nc',  $ ;ascending mila1
      'D:\gsar\des\dangxiong2.b\des_F1\incidences\look.nc', $ ;descending dangxiong2.b
      'D:\gsar\des\mila2\des_F3\incidences\look.nc' ] ;descending mila2
      
    ;a priori coordiantes for all sites
    lfile='D:\gsar\des\mila2\des_F3\SBAS8\x5\raw\sites.net'
    
    ofile='C:\Papers.data\sse.mila\multi-look\ts\incidences.txt'
  ENDIF
  
  READ_NET, lfile, site=sites, llh=llhs
  
  nlook=N_ELEMENTS(files)
  nsite=N_ELEMENTS(sites)
  
  pDataLook=REPLICATE(PTR_NEW(),nlook)
  pLon=REPLICATE(PTR_NEW(),nlook)
  pLat=REPLICATE(PTR_NEW(),nlook)
  
  ;read incidence grids
  FOR i=0, nlook-1 DO BEGIN
    file=files[i]
    READ_NC_SBAS,FILE,DATA=DATA, $
      LAT=LAT, $
      LON=LON
    pDataLook[i]=PTR_NEW(data)
    pLon[i]=PTR_NEW(lon)
    plat[i]=PTR_NEW(lat)
  ENDFOR
  
  ;get incident angles for each site
  odata=DBLARR(nsite,nlook)
  FOR i=0, nsite-1 DO BEGIN
    site=sites[i]
    lon=llhs[0,i]
    lat=llhs[1,i]
    
    FOR j=0, nlook-1 DO BEGIN
      tmpx=min(abs(*pLon[j]-lon),indx)
      tmpy=min(abs(*pLat[j]-lat),indy)
      oData[i,j]=(*pDataLook[j])[indx,indy]
    ENDFOR
  ENDFOR
  
  openw,fid,ofile,/get_lun
  for i=0,nsite-1 do begin
    printf,fid,sites[i],llhs[0:1,i],odata[i,*],format='(1x,a4,2(1x,f12.6),1000(1x,f12.6))'
  endfor
  free_lun,fid
  ;STOP
  ;
  ;free pointers
  FOR i=0,nlook-1 DO BEGIN
    IF PTR_VALID(pDataLook[i]) THEN PTR_FREE,pDataLook[i]
    IF PTR_VALID(pLon[i]) THEN PTR_FREE,pLon[i]
    IF PTR_VALID(pLat[i]) THEN PTR_FREE,pLat[i]
  ENDFOR
  
END