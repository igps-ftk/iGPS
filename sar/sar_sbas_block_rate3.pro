PRO SAR_SBAS_BLOCK_RATE3, path,  $
    rect=rect ;[xmin,ymin,xmax,ymax]
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
;    ;dang-xiong 2008 earthquake (yangyi village)
;    path='D:\gsar\des\envisat.d.t405f3015.yzs\SBAS'
;    ;path='D:\gsar\des\envisat.d.t176f3015.ydgl\SBAS'
;    ;path='\\gpsac5\root\d1\gsar\nujiang4\des_F2\SBAS'
;    path='\\gpsac4\root\g4b\tianyf\jiali.b\asc_F1\SBAS.defo2'
;    path='\\gpsac4\root\g4b\tianyf\jiali.b\asc_F1.defo2\SBAS'
;    path='\\gpsac4\root\g4b\tianyf\dangxiong.b\asc_F3\SBAS'
;    path='D:\gsar\des\dangxiong2.b\des_F1\SBAS'
;    path='\\gpsac5\root\g5c\tianyf\m_jiali2\des_F3\SBAS'
;    path='D:\gsar\asc\dangxiong.b\asc_F3\SBAS'
;    path='D:\gsar\asc\jiali.b\asc_F1\SBAS'
;    PATH='D:\gsar\des\dangxiong2.b\des_F1\SBAS'
;    ;    path='D:\gsar\des\jiali2\des_F3\SBAS'
;    ;
;    ;    ;    ;rect=[90.3,29.6,90.45,29.8]
;    ;    ;    rect=[92.34d0,29.80,   92.36,29.83]
;    ;rect=[92.3d0,29.78,   92.36,29.88]  ;mila
;    ;rect=[92.2d0,29.5,   92.7,30.]
;    ;    ;path='D:\gsar\asc\bengco\asc_F3\SBAS'
;    ;    path='D:\gsar\asc\mila1\asc_F1\SBAS\'
;    ;    path='D:\gsar\asc\mila1\asc_F1\SBAS\nc.detrend'
;    
;    ;    path='D:\gsar\asc\dongqiao\asc_F2\SBAS\'
;    ;    path='D:\gsar\des\bengco.des\des_F2\SBAS'
;    ;    rect=[91.16d0,31.30,   91.32,31.36] ;to the north of Bengco Lake
;    ;
;    ;    path='D:\gsar\des\envisat.d.bengco.t405f2979\SBAS'
;    ;rect=[91.36d0,31.51,   91.39,31.55] ;to the north of Bengco Lake
;;    rect=[91.30d0,31.4,   91.43,31.58] ;larger than the above area
;    ;
;    ;    path='D:\gsar\des\envisat.d.bengco.t405f2979\SBAS'
;    
;    ;
;    ;    path='\\gpsac5\root\d1\gsar\alos.f620p498.bengco\SBAS'
;    ;    path='\\gpsac5\root\d1\gsar\multi_shuanghu2\des_F2\SBAS'
;    ;    path='\\gpsac5\root\g5c\tianyf\m_jiali2\des_F3\SBAS'
;    ;rect=[91.9d0,28.36,   92.0,28.42] ;
;    
;    ;subsidence of Yang-ba-jing electricitiy plant
;    ;path='D:\gsar\asc\dangxiong.b\asc_F1\SBAS'
;    ;rect=[90.46d0,30.07,   90.48,30.10]
;    
;    ;    path='\\gpsac5\root\g5d\gsar\m_dangxiong2\des_F2\SBAS.long'
;    ;path='\\gpsac5\root\d1\gsar\mila2\des_F3\SBAS'
;    ;path='\\gpsac4\root\g4c\gsar\envisat.d.bengco.t405f2979\SBAS6'
;    ;    path='D:\gsar\des\envisat.d.t405f2979.test\SBAS'
;    ;    path='\\gpsac5\root\d1\gsar\m_jiali2\des_F2\SBAS11.bad'
;    ;    path='D:\gsar\des\mila2\des_F3\SBAS'
;    ;    path='D:\gsar\asc\mila1\asc_F1\SBAS4\nc.detrend'
;    ;    path='\\gpsac4\root\g4c\gsar\envisat.d.t405f2979.test\SBAS'
;    path='D:\gsar\des\envisat.d.t405f2979.test\SBAS'
;    path='D:\gsar\des\envisat.d.t405f2979.test\tmp'
;    path='\\gpsac4\root\g4c\gsar\envisat.d.t405f2979.test\SBAS'
;    path='\\gpsac4\root\g4c\gsar\envisat.d.t405f2979.test\SBAS'
;    path='\\gpsac4\root\g4c\gsar\envisat.d.t405f2979.test\SBAS'
;    ;path='\\gpsac4\root\g4b\tianyf\m_jiali1\asc_F1\SBAS'
;    
;    path='D:\gsar\asc\mila1\asc_F1\SBAS4\nc.detrend'
;    path='D:\gsar\des\mila2\des_F3\SBAS'
;    rect=[92.27d0, 29.75,   92.43, 29.9] ;for Mila Tunnel
    rect=[92.32d0, 29.79d0,   92.37d0, 29.85d0] ;for Mila Tunnel most deformed zone
;    path='D:\gsar\des\mila2\des_F3\SBAS7'
;    path='D:\gsar\des\mila2\des_F3\SBAS8'
;    ;path='D:\gsar\asc\mila1\asc_F1\SBAS4\nc.detrend'
;    path='D:\gsar\asc\dangxiong.b\asc_F3\SBAS'
;    ;path='D:\gsar\des\dangxiong2.b\des_F1\SBAS'
;    path='\\gpsac4\root\g4b\tianyf\dangxiong.b\asc_F3\SBAS'
;    path='\\gpsac4\root\g4c\gsar\envisat.d.t405f2979.test\SBAS'
;    path='\\vmshare\root\data\FTP\user\tianyf\envisat.d.t405f2979.test\SBAS'
;    path='\\gpsac5\root\b1\gsar\envisat.a.t398f621.bengco\SBAS'
;    
;    path='\\gpsac5\root\b1\gsar\mila4\des_F1\SBAS'
;    path='\\gpsac4\root\g4b\tianyf\mila3\asc_F3\SBAS'
;    path='\\gpsac4\root\dcd0\gsar\mila4\des_F1\SBAS'
;    ;path='\\gpsac4\root\g4b\tianyf\mila3\asc_F3\SBAS'
;    path='D:\gsar\des\mila4\des_F1\SBAS7'
;    path='\\gpsac5\root\b1\gsar\mila4\des_F1\SBAS'
;    path='\\gpsac4\root\g4b\tianyf\mila3\asc_F3\SBAS'
;    ;path='\\gpsac5\root\b1\gsar\envisat.d.t448f2907.shuanghu\SBAS\'
;    ;path='\\vmshare\root\data\FTP\user\tianyf\envisat.a.t398f585.sangri\SBAS'
;    path='\\gpsac5\root\b1\gsar\envisat.d.t448f2907.shuanghu\SBAS'
;    path='D:\gsar\asc\mila3\asc_F3\SBAS6'
;    path='\\gpsac4\root\g4b\tianyf\mila3\asc_F3\SBAS7c'
;    path='\\gpsac4\root\g4\gsar\mila1\asc_F1\SBAS'
;    path='D:\gsar\des\mila2\des_F3\SBAS8'
    ;path='D:\gsar\asc\mila1\asc_F1\SBAS4\nc.detrend'
    path='D:\gsar\des\mila2_oct\des_F3\SBAS'
    path='D:\gsar\des\mila2s\des_F3\SBAS5'
    ;path='D:\gsar\asc\mila3s\asc_F3\SBAS'
    path='D:\gsar\des\mila2s\des_F3\full\SBAS'
    path='\\gpsac4\root\g4b\tianyf\mila3s\asc_F3\SBAS'
    
    
    out_plot=1
    
    ;test coaser grid
    sx=3
    sx=5
    sx=10
    ;sx=20
    ;sx=30
    ;sx=90
    ;out_plot=1
    opath=path+PATH_SEP()+'outp_eq_gbjd2'
    opath=path+PATH_SEP()+'x5/raw'
    opath=path+PATH_SEP()+'x'+strtrim(sx,2)+'/raw'
    ;opath=path+PATH_SEP()+'x'+strtrim(sx,2)+'_mila/raw'
  ;opath=path+PATH_SEP()+'outp_bengco_north3'
  ;opath=path+PATH_SEP()+'outp'
  ;opath=path+PATH_SEP()+'outp_mila2'
    
  ENDIF
  ;stop
  ;
  ;steps of x and y
  IF N_ELEMENTS(sx) EQ 0 THEN sx=10
  ;for even grid
  sy=sx ;the same step for X and Y
  IF N_ELEMENTS(sy) EQ 0 THEN sy=10
  
  ;
  ;block size
  IF N_ELEMENTS(sz) EQ 0 THEN BEGIN
    sz=5
    sz=3
    ;sz=0
    sz=sx/2 ;half the step size
    sz=sz<5
    sz=0
  ENDIF
  
  ;
  IF N_ELEMENTS(opath) EQ 0 THEN opath=path+PATH_SEP()+'out'
  IF FILE_TEST(opath,/directory) NE 1 THEN BEGIN
    FILE_MKDIR, opath
  ENDIF
  
  IF N_ELEMENTS(out_plot) EQ 0 THEN out_plot=0
  
  files=FILE_SEARCH(path+PATH_SEP()+'disp*.nc', count=nf)
  ;files=FILE_SEARCH(path+PATH_SEP()+'defo*.nc', count=nf)
  IF nf LE 0 THEN RETURN
  ;
  ;skip the first reference file (all zeros) if it is present.
  IF GETFILENAME(files[0]) EQ 'disp_001_ll.nc' || GETFILENAME(files[0]) EQ 'disp_001.nc'  THEN BEGIN
    files=files[1:*]
    nf=nf-1
  ENDIF
  ;stop
  
  ;r;ad time information from scene.tab file
  file_scene_tab=path+PATH_SEP()+'scene.tab'
  IF FILE_TEST(file_scene_tab,/regular) NE 1 THEN BEGIN
  
  ENDIF
  lines=read_txt(file_scene_tab)
  lines_p=str_lines2arr(lines)
  years=strmids(lines_p[0,*],0,4)
  doyrs=strmids(lines_p[0,*],4,3)
  
  dyrs=DBLARR(N_ELEMENTS(years))
  FOR i=0,N_ELEMENTS(years)-1 DO BEGIN
    DOY,years[i],1,doyrs[i],dyear=dyr
    dyrs[i]=dyr
  ENDFOR
  
  ;stop
  ;skip the first image (the reference one)
  ;
  ;read the first image
  file=files[0]
  READ_NC_SBAS,FILE,DATA=DATA, $
    LAT=LAT, $
    LON=LON
  ny=N_ELEMENTS(data[0,*])
  nx=N_ELEMENTS(data[*,0])
  
  ;
  IF N_ELEMENTS(rect) EQ 4 THEN BEGIN
    xmin=rect[0]
    xmax=rect[2]
    ymin=rect[1]
    ymax=rect[3]
  ENDIF ELSE BEGIN
    xmin=MIN(lon,max=xmax)
    ymin=MIN(lat,max=ymax)
  ENDELSE
  
  stepx=sx*abs(lon[1]-lon[0])
  stepy=sy*abs(lat[1]-lat[0])
  
  oNx=ceil((xmax-xmin)/stepx)
  oNy=ceil((ymax-ymin)/stepy)
  
  olon=INDGEN(oNx)*stepx+xmin
  olat=INDGEN(oNy)*stepy+ymin
  
  ;STOP
  
  data_all=DBLARR(nx,ny,nf)
  data_all[*,*,0]=data
  FOR fi=1,nf-1 DO BEGIN
    file=files[fi]
    PRINT,file
    READ_NC_SBAS,FILE,DATA=DATA, $
      LAT=LAT, $
      LON=LON
    data_all[*,*,fi]=data
    HELP,data
  ENDFOR
  
  ;lat=REVERSE(lat)
  
  ;  xo=14550
  ;  yo=7310
  ;  ;
  ;  xo=10290
  ;  yo=3380
  
  
  data=REFORM(data_all[*,*,1])
  
  IF out_plot EQ 1 THEN BEGIN
    sf=.65
    odata=ECONGRID(data,sf)
    WINDOW,0,xsize=N_ELEMENTS(odata[*,0]),ysize=N_ELEMENTS(odata[0,*])
    !p.MULTI=-1
    TVSCL,odata,/nan;,/order
  ;plot,[0,0],[nx,ny]*sf,/nodata,clip=[0,0,1,1]
  ENDIF
  
  oRatesXyz=DBLARR(4,(nx/sx+1)*(ny/sy+1))
  oRates=DBLARR(onx,ony)
  oRates[*]=!VALUES.D_NAN
  oRMSs=DBLARR(onx,ony)
  
  count=0ull
  site_names=STRARR(onx*ony)
  llhs=DBLARR(3,onx*ony)
  ;
  FOR i=0, oNx-1 DO BEGIN
    ;indx=i*sx+sx/2
    xo=olon[i]
    tmp=min(abs(lon-xo),indx)
    FOR j=0, oNy-1 DO BEGIN
      indy=j*sy+sy/2
      yo=olat[j]
      tmp=min(abs(lat-yo),indy)
      
      ;stop
      ;tmp=MIN(ABS(lat-yo),indy)
      ;tmp=MIN(ABS(lon-xo),indx)
      
      ;PRINT,xo,yo,indx,indy,[indx,indy]*sf
      
      ;PRINT,lon[indx],lat[indy]
      x1=(indx-sz)>0
      x2=(indx+sz)<(nx-1)
      y1=(indy-sz)>0
      y2=(indy+sz)<(ny-1)
      ;PRINT,x1,x2,y1,y2
      data_ijs=data_all[x1:x2,y1:y2,*]
      pos=WHERE(FINITE(data_ijs[*,*,0]) EQ 1)
      IF pos[0] EQ -1 THEN CONTINUE
      
      count=count+1
      ij=i*ony+j
      IF ij GE 9999 THEN STOP
      
      site_name=STRING(ij+1,format='(i04)')+  $
        STRING(xo,format='("_X",f08.3)')+ $
        STRING(yo,format='("_Y",f07.3)')
      site_name=STRING(ij+1,format='(i04)');+  $
      ;STRING(xo,format='("_X",f08.3)')+ $
      ;STRING(yo,format='("_Y",f07.3)')
      ;
      site_names[ij]=site_name
      
      IF out_plot EQ 1 THEN BEGIN
        WSET,0
        XYOUTS,[indx]*sf,[indy]*sf,'+',color='ffffff'x,/device
        XYOUTS,[indx]*sf,[indy]*sf,'  '+site_name,color='ffffff'x,/device,charsize=.8
      ENDIF
      
      
      ;PRINT,data_all[indx,indy,*]
      IF out_plot EQ 1 THEN BEGIN
        WINDOW,1,title=site_name,/pixmap
        !p.MULTI=-1
        ;stop
        PLOT,dyrs,[0,REFORM(data_all[indx,indy,*])],background='ffffff'x,color='0'x, $
          psym=-2,/nodata,  $
          ;yrange=[MIN(data_ijs,/nan),MAX(data_ijs,/nan)],$
          yrange=[-60,60]*.5,  $
          ;xrange=[2014,2018], $
          title=site_name,$
          xtitle='Time (a)', ytitle='LOS Displacements (mm)'
      ENDIF
      data_ijs_sum=DBLARR(N_ELEMENTS(data_ijs[0,0,*]))
      nij=0
      FOR k=0,x2-x1 DO BEGIN
        FOR l=0,y2-y1 DO BEGIN
          IF out_plot EQ 1 THEN OPLOT,dyrs,[0,REFORM(data_ijs[k,l,*])],color='aaaaaa'x,psym=-3
          pos2=WHERE(FINITE(data_ijs[k,l,*]) EQ 1)
          IF pos2[0] EQ -1 || N_ELEMENTS(pos2) NE N_ELEMENTS(data_ijs[k,l,*]) THEN CONTINUE
          data_ijs_sum=data_ijs_sum+data_ijs[k,l,*]
          nij=nij+1
        ENDFOR
      ENDFOR
      IF nij EQ 0 THEN CONTINUE
      data_ijs_avg=data_ijs_sum/nij
      y=[0,data_ijs_avg]
      x=dyrs[0:*]
      IF out_plot EQ 1 THEN OPLOT,dyrs,y,color='ff0000'x,psym=-5
      ;y=data_ijs_avg
      ;x=dyrs[1:*]
      ;fit an line
      ;      tmp=LADFIT(x,y,/double,ABSDEV=absdev)
      ;      stdv=sqrt(absdev)
      ;      yfit=x*tmp[1]+tmp[0]
      ;      yres=y-yfit
      ;      stdv=rms(yres)
      tmp=LINFIT(x,y,/double,sigma=stdv)
      oRatesXyz[*,ij]=[xo,yo,tmp[1],stdv[1]]
      oRates[i,j]=tmp[1]
      oRMSs[i,j]=stdv[1]
      xs=FINDGEN(100)*((last(dyrs)-dyrs[0])/100d0)+dyrs[0]
      ys=tmp[0]+tmp[1]*xs
      ;
      ;fit an linear trend and an annual term
      
      ;TS_MODEL,x,y,/annual,yfit=yfit,coef=coef,sigma=sig
      TS_MODEL,x,y,yfit=yfit,coef=coef,sigma=sig
      oRatesXyz[*,ij]=[xo,yo,coef[1],sig[1]]
      oRates[i,j]=coef[1]
      oRMSs[i,j]=sig[1]
      llhs[*,ij]=[xo,yo,0]
      ;stop
      
      ;
      IF out_plot EQ 1 THEN BEGIN
        jfile=opath+PATH_SEP()+site_name+'.jpg'
        OPLOT,xs,ys,color='0000ff'x
        OPLOT,x,yfit,color='0'x,psym=-6
        XYOUTS,MEAN(xs),30,site_name+STRING(oRatesXyz[2:3,ij],format='(1x,"rate=",f7.2,1x,"+/-",f7.2,1x,"mm/yr")'),/data,color='0'x,alignment=.5
        ;PRINT,jfile
        WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
        ;stop
        WDELETE,1
        odata=DBLARR(6,N_ELEMENTS(dyrs))
        odata[0,*]=dyrs
        odata[1,*]=years
        odata[2,*]=doyrs
        odata[3,*]=[0,data_ijs_avg]*1d-3
        odata[4,*]=[0,data_ijs_avg]*1d-3
        odata[5,*]=[0,data_ijs_avg]*1d-3
        ofile=desuffix(jfile)+'.neu'
        cmt=STRING(xo,yo,format='("lon=",1x,f12.6,1x,"lat=",1x,f12.6)')
        WRITE_SIO,ofile,data=odata,src=[files,cmt],prog=prog
      ENDIF
    ;STOP
    ENDFOR
  ENDFOR
  
  IF count EQ 0 THEN BEGIN
    PRINT,'WARNING: no data points found!'
    RETURN
  ENDIF
  epochs=REPLICATE(2017, count)
  ofile=opath+PATH_SEP()+'sites.net'
  pos=where(site_names ne '')
  WRITE_NET, $
    OFILE, $
    SITES=SITE_NAMES[pos], $
    LLHS=LLHS[*,pos], $
    USER=USER, $
    PROG=PROG, $
    SRC=path, $
    EPOCHS=EPOCHS
  ;stop  
  NET2KML,ofile
  ;stop
  ;Output XYZ file
  ofile=opath+PATH_SEP()+'rates.xyz'
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=files
  IF count GT 0 THEN BEGIN
    FOR i=0ull, count-1 DO BEGIN
      IF oRatesXyz[2,i] EQ 0d0 THEN CONTINUE
      PRINTF,fid,oRatesXyz[*,i],format='(1x,4(1x,f))'
    ENDFOR
  ENDIF
  FREE_LUN,fid
  ;
  ;Output Rate NetCDF file
  ofile=opath+PATH_SEP()+'rates.nc'
  id = NCDF_CREATE(ofile, /CLOBBER)
  ; Fill the file with default values:
  NCDF_CONTROL, id, /FILL
  xid = NCDF_DIMDEF(id, 'lon', onx)    ; Make dimensions.
  yid = NCDF_DIMDEF(id, 'lat', ony)    ; Make dimensions.
  zid = NCDF_DIMDEF(id, 'z', onx*ony)
  ; Define variables:
  aid = NCDF_VARDEF(id, 'lon', [xid], /double)
  bid = NCDF_VARDEF(id, 'lat', [yid], /double)
  vid = NCDF_VARDEF(id, 'LOS_Rate', [xid,yid], /double)
  NCDF_ATTPUT, id, aid, 'units', 'degree_east'
  NCDF_ATTPUT, id, aid, 'long_name', 'Longitude'
  NCDF_ATTPUT, id, bid, 'units', 'degree_north'
  NCDF_ATTPUT, id, bid, 'long_name', 'Latitude'
  NCDF_ATTPUT, id, vid, 'units', 'mm/a'
  NCDF_ATTPUT, id, vid, 'long_name', 'InSAR LOS Velocity'
  NCDF_ATTPUT, id, /GLOBAL, 'Title', 'LOS_RATE'
  ; Put file in data mode:
  NCDF_CONTROL, id, /ENDEF
  ; Input data:
  NCDF_VARPUT, id, aid, olon
  NCDF_VARPUT, id, bid, olat
  NCDF_VARPUT, id, vid, oRates
  NCDF_CLOSE, id ; Close the NetCDF file.
  ;GRD_PLOT_HISTOGRAM, ofile, VAR_TYPE='LOS_Rate'
  
  
  ;writing RMS array
  ofile=opath+PATH_SEP()+'rms.nc'
  id = NCDF_CREATE(ofile, /CLOBBER)
  ; Fill the file with default values:
  NCDF_CONTROL, id, /FILL
  xid = NCDF_DIMDEF(id, 'lon', onx)    ; Make dimensions.
  yid = NCDF_DIMDEF(id, 'lat', ony)    ; Make dimensions.
  zid = NCDF_DIMDEF(id, 'z', onx*ony)
  ; Define variables:
  aid = NCDF_VARDEF(id, 'lon', [xid], /double)
  bid = NCDF_VARDEF(id, 'lat', [yid], /double)
  vid = NCDF_VARDEF(id, 'LOS_Rate_RMS', [xid,yid], /double)
  NCDF_ATTPUT, id, aid, 'units', 'degree_east'
  NCDF_ATTPUT, id, aid, 'long_name', 'Longitude'
  NCDF_ATTPUT, id, bid, 'units', 'degree_north'
  NCDF_ATTPUT, id, bid, 'long_name', 'Latitude'
  NCDF_ATTPUT, id, vid, 'units', 'mm/a'
  NCDF_ATTPUT, id, vid, 'long_name', 'InSAR LOS Velocity RMS'
  NCDF_ATTPUT, id, /GLOBAL, 'Title', 'LOS_RATE_RMS'
  ; Put file in data mode:
  NCDF_CONTROL, id, /ENDEF
  ; Input data:
  NCDF_VARPUT, id, aid, olon
  NCDF_VARPUT, id, bid, olat
  NCDF_VARPUT, id, vid, oRMSs
  NCDF_CLOSE, id ; Close the NetCDF file.
  ;GRD_PLOT_HISTOGRAM, ofile, VAR_TYPE='LOS_Rate_RMS'
  
  
  IF out_plot EQ 1 THEN BEGIN
    jfile=opath+PATH_SEP()+'site_map.jpg'
    ;PRINT,jfile
    WSET,0
    WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
  ENDIF
  ;stop
  PRINT,'['+prog+']Normal end.'
END