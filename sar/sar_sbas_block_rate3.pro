PRO SAR_SBAS_BLOCK_RATE3, path,  $
    rect=rect,  $ ;[xmin,ymin,xmax,ymax]
    aoi=aoi,  $ ;area of interest
    sx=sx,  $
    sy=sy,  $
    time_offset=time_offset,  $
    out_plot=out_plot
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  RESOLVE_ROUTINE, 'TS_MODEL', /COMPILE_FULL_FILE
  
  IF N_PARAMS() LT 1 THEN BEGIN
  
    ;    path='D:\gsar\des\jinsha.landslide2\des_F3\SBAS4'
    ;    path='D:\gsar\des\jinsha.landslide2\des_F3\SBAS8'
    ;    path='D:\gsar\asc\jinsha.landslide5\asc_F1\SBAS2'
    ;    path='\\10.4.134.31\root\g7c\gsar\jinsha.landslide2\des_F3\SBAS10'
    ;    rect=[98.45,31.60, 98.49,31.64]
    ;
    ;    path='D:\gsar\interseismic\t41-f100-f110-a-m_wulan3_sewa2_dongqiao\f123.1\SBAS10.long.gt200\r4'
    ;    rect=[89.95, 34.75,  90.4, 35.06] ;west of wulanwula lake
    ;
    ;    path='D:\gsar\coseismic\136-d-m1-0455-wush\F2.cut\sbas.2.0.0001.9999.20141015.20210129.146.3117.01.___\r4'
    ;    ;path='D:\gsar\coseismic\056-a-m2-0128_0133-wush\F2.cut\sbas.2.0.0001.9999.20141114.20210129.147.3237.01.___\r4'
    ;    rect=[78.76,41.15, 78.87, 41.21] ;aseismic near WUSH
    ;    time_offset=2019.819804034d0
    ;    IS_ANNUAL=0
    ;    IS_SEMIANNUAL=0
    ;
    path='\\10.4.134.30\root\g10c\gsar\136-d-m4-0481_0486_0491_0496-mht\f123\sbas.4.0.0367.9999.20141015.20200305.113.1400.01.___\r4_ref'
    path='\\192.168.11.17\root\g17n\gsar\InSAR_dataset4linear_v20230414\sbas.3.5.0001.9999.20150113.20181211.120.1702.01.s1\nc'
    
    out_plot=1
    ;
    sx=1
    ;    sx=2
    ;    ;test coaser grid
    sx=3
    sx=5
    ;        sx=7
    sx=10
    ;    ;    sx=20
    ;    ;    sx=11
    ;            sx=15
    sx=30
  ;                            sx=50
;    sx=60
  ;                sx=90
    
  ;out_plot=1
    
  ENDIF
  ;top
  ;
  ;area of interest (aoi)
  aoi=-9999d0
  
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
  
  IF N_ELEMENTS(aoifile) GT 0 && aoifile NE '' THEN BEGIN
    lines_fvec=read_txt(aoifile)
    lines_fvec2=STRTRIM(lines_fvec,2)
    pos=WHERE(strmids(lines_fvec2,0,1) NE '>')
    IF N_ELEMENTS(pos) LT 2 THEN BEGIN
      PRINT,'['+prog+']ERROR: invalid fault line vector file <'+ffile+'>!!'
      RETURN
    ENDIF
    aoi=DOUBLE(str_lines2arr(lines_fvec2[pos]))
    aoi=aoi[*,1:*]
  ;STOP
  ENDIF
  
  
  ;
  IF N_ELEMENTS(opath) EQ 0 THEN BEGIN
    opath=path+PATH_SEP()+'out'
    opath=path+PATH_SEP()+'outp_eq_gbjd2'
    ;    ;opath=path+PATH_SEP()+'x5/raw'
    opath=path+PATH_SEP()+'x'+STRTRIM(sx,2)+'/raw'
  ;
  ;opath=path+PATH_SEP()+'x'+strtrim(sx,2)+'_mila/raw'
  ;opath=path+PATH_SEP()+'outp_bengco_north3'
  ;opath=path+PATH_SEP()+'outp'
  ;opath=path+PATH_SEP()+'outp_mila2'
  ENDIF
  IF FILE_TEST(opath,/directory) NE 1 THEN BEGIN
    FILE_MKDIR, opath
  ENDIF
  
  IF N_ELEMENTS(out_plot) EQ 0 THEN out_plot=0
  
  files=FILE_SEARCH(path+PATH_SEP()+'disp*.nc', count=nf)
  ;stop
  IF nf LE 0 THEN BEGIN
    files=FILE_SEARCH(path+PATH_SEP()+'disp*.grd', count=nf)
  ENDIF
  IF nf LE 0 THEN BEGIN
    files=FILE_SEARCH(path+PATH_SEP()+'defo*.nc', count=nf)
  ENDIF
  IF nf LE 0 THEN BEGIN
    PRINT,'['+PROG+']ERROR: no displacement files found in '+path+'!!"
    RETURN
  ENDIF
  ;
  
  ;read time information from scene.tab file
  file_scene_tab=path+PATH_SEP()+'scene.tab'
  IF FILE_TEST(file_scene_tab,/regular) NE 1 THEN BEGIN
    PRINT,'['+PROG+']ERROR: no scene.tab file found!!"
    RETURN
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
  tmp=grepi(GETFILENAME(files),lines_p[0])
  ;help,tmp
  ;stop
  ;skip the first reference file (all zeros) if it is present.
  ;IF GETFILENAME(files[0]) EQ 'disp_001_ll.nc' || GETFILENAME(files[0]) EQ 'disp_001.nc'  THEN BEGIN
  IF tmp NE '' THEN BEGIN
    ;!!NOTE: always skip the first file
    files=files[1:*]
    nf=nf-1
  ENDIF
  ;stop
  ;
  ;read the first image
  file=files[0]
  READ_NC_SBAS,FILE,DATA=DATA, $
    LAT=LAT, $
    LON=LON
  ny=N_ELEMENTS(data[0,*])
  nx=N_ELEMENTS(data[*,0])
  
  ;adjust longitude range to [-180,180]
  ind_lon=WHERE(lon GT 180)
  IF ind_lon[0] NE -1 THEN BEGIN
    lon[ind_lon]=lon[ind_lon]-360d0
  ENDIF
  ;
  ;stop
  IF N_ELEMENTS(rect) EQ 4 THEN BEGIN
    xmin=rect[0]
    xmax=rect[2]
    ymin=rect[1]
    ymax=rect[3]
  ENDIF ELSE BEGIN
    HELP, rect
    xmin=MIN(lon,max=xmax)
    ymin=MIN(lat,max=ymax)
  ENDELSE
  
  stepx=sx*ABS(lon[1]-lon[0])
  stepy=sy*ABS(lat[1]-lat[0])
  
  oNx=CEIL((xmax-xmin)/stepx)
  oNy=CEIL((ymax-ymin)/stepy)
  
  olon=INDGEN(oNx)*stepx+xmin
  olat=INDGEN(oNy)*stepy+ymin
  
  ;STOP
  
  data_all=DBLARR(nx,ny,nf)
  data_all[*,*,0]=data
  FOR fi=1,nf-1 DO BEGIN
    file=files[fi]
    ;PRINT,file
    READ_NC_SBAS,FILE,DATA=DATA, $
      ;LAT=LAT, $
      ;LON=LON, $
      _extra=dummy
    data_all[*,*,fi]=data
  ;HELP,data
  ENDFOR
  HELP,data
  
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
  oRatesXyz=DBLARR(4,onx*ony)
  oRates=DBLARR(onx,ony)
  oRates[*]=!VALUES.D_NAN
  oRMSs=DBLARR(onx,ony)
  
  IF N_ELEMENTS(time_offset) EQ 0 THEN time_offset=2010
  IF N_ELEMENTS(is_annual) EQ 0 THEN is_annual=0
  IF N_ELEMENTS(is_semiannual) EQ 0 THEN is_semiannual=0
  
  nOff=N_ELEMENTS(time_offset)
  oOffs=DBLARR(2,onx*ony,nOff)
  oOffsets=DBLARR(onx,ony,nOff)
  oOffsetErrs=DBLARR(onx,ony,nOff)
  oAnns=DBLARR(3,onx*ony)
  oAnnAmps=DBLARR(onx,ony)
  oAnnAmps[*]=!values.D_NAN
  oAnnPhas=DBLARR(onx,ony)
  oSumLines=STRARR(onx*ony)
  
  ;stop
  count=0ull
  site_names=STRARR(onx*ony)
  llhs=DBLARR(3,onx*ony)
  oLLHs=DBLARR(3,onx,ony)
  PRINT,onx,ony
  ;return
  ;
  FOR i=0, oNx-1 DO BEGIN
    ;indx=i*sx+sx/2
    xo=olon[i]
    tmp=MIN(ABS(lon-xo),indx)
    FOR j=0, oNy-1 DO BEGIN
      indy=j*sy+sy/2
      yo=olat[j]
      tmp=MIN(ABS(lat-yo),indy)
      
      offset=DBLARR(3,noff)
      offset[*]=-9999d0
      offset[2,*]=time_offset
      
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
      
      ;for derive uncertainity
      sz_unc=(sx/2-1) > 0
      ;sz_unc=2
      ;help,sz_unc
      x1unc=(indx-sz_unc)>0
      x2unc=(indx+sz_unc)<(nx-1)
      y1unc=(indy-sz_unc)>0
      y2unc=(indy+sz_unc)<(ny-1)
      ; print,x1unc,x2unc,y1unc,y2unc
      ; PRINT,x1,x2,y1,y2
      data_ijs_unc=data_all[x1unc:x2unc,y1unc:y2unc,*]
      ;remove the low-frequency part from the time series
      FOR ti=x1unc, x2unc DO BEGIN
        FOR tj=y1unc, y2unc DO BEGIN
          tmp_ij=REFORM(data_all[ti,tj,*])
          pos=WHERE(FINITE(tmp_ij) EQ 1)
          IF N_ELEMENTS(pos) NE N_ELEMENTS(tmp_ij) THEN CONTINUE
          ;tmp_ij2=SMOOTH(tmp_ij,9,/edge)
          y=[0,tmp_ij]
          x=dyrs[0:*]
          ;tmp=ladfit(x,y,/double)
          tmp=LINFIT(x,y,yfit=tmp_ij2)
          
          tmp_ij3=tmp_ij-tmp_ij2
          tmp_ij3=tmp_ij  ;not removing linear trend
          data_ijs_unc[ti-x1unc,tj-y1unc,*]=tmp_ij3
        ;stop
        ENDFOR
      ENDFOR
      pos_unc=WHERE(FINITE(data_ijs_unc[*,*,0]) EQ 1)
      odata_uncs=DBLARR(N_ELEMENTS(data_ijs_unc[0,0,*]))
      FOR ti=0, N_ELEMENTS(odata_uncs)-1 DO BEGIN
        tmp=data_ijs_unc[*,*,ti]
        odata_uncs[ti]=STDDEV(tmp[pos_unc])
      ;odata_uncs[ti]=VARIANCE(tmp[pos_unc])
      ENDFOR
      ;stop
      ;
      ;
      ;check if a aoi polygon is required!
      IF aoi[0] NE -9999d0 THEN BEGIN
        ;stop
        isin=IS_POINT_INSIDE_POLYGON(aoi, [xo,yo])
        ;stop
        IF isin EQ 0 THEN CONTINUE
      ENDIF
      
      count=count+1
      ij=i*ony+j
      ;ij2=i*1+j
      ;IF ij GE 9999 THEN STOP
      
      ;      ;;site_name=STRING(ij+1,format='(i04)')+  $
      ;      ;        STRING(xo,format='("_X",f08.3)')+ $
      ;      ;        STRING(yo,format='("_Y",f07.3)')
      ;      site_name=STRING(ij+1,format='(i04)');+  $
      ;      ;      ;STRING(xo,format='("_X",f08.3)')+ $
      ;      ;      ;STRING(yo,format='("_Y",f07.3)')
      ;
      ;      site_names[ij]=site_name
      
      INIT_GNSS_SITE_NAME, ij, site_name=site_name
      site_names[ij]=site_name
      
      IF out_plot EQ 1 THEN BEGIN
        WSET,0
        XYOUTS,[indx]*sf,[indy]*sf,'+',color='ffffff'x,/device
        XYOUTS,[indx]*sf,[indy]*sf,'  '+site_name,color='ffffff'x,/device,charsize=.8
      ENDIF
      
      
      ;      z0=data_dem[indx,indy]
      z0=0
      ;PRINT,data_all[indx,indy,*]
      IF out_plot EQ 1 THEN BEGIN
        WINDOW,1,title=site_name,/pixmap
        !p.MULTI=-1
        ;stop
        PLOT,dyrs,[0,REFORM(data_all[indx,indy,*])],background='ffffff'x,color='0'x, $
          psym=-2,/nodata,  $
          ;yrange=[MIN(data_ijs,/nan),MAX(data_ijs,/nan)],$
          yrange=[-80,60]*1,  $
          ;xrange=[2014,2018], $
          title=site_name,$
          xtitle='Time (a)', ytitle='LOS Displacements (mm)'
      ENDIF
      data_ijs_sum=DBLARR(N_ELEMENTS(data_ijs[0,0,*]))
      ;stop
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
      ;Ts_model,x,y,yfit=yfit,coef=coef,sigma=sig
      ;stop
      cmdstr='CALL_TS_MODEL,X,Y,YFIT=YFIT,COEF=COEF,SIGMA=SIG,OFFSET=OFFSET,ANN_AMP=ANN_AMP,ANN_EAMP=ANN_EAMP,ANN_PHASE=ANN_PHASE,SUM_LINE=SUM_LINE,site=site_name'
      IF is_annual EQ 1 THEN cmdstr=cmdstr+',/IS_ANNUAL'
      IF is_semiannual EQ 1 THEN cmdstr=cmdstr+',/IS_SEMIANNUAL'
      
      tmp=EXECUTE(cmdstr)
      
      
      oRatesXyz[*,ij]=[xo,yo,coef[1],sig[1]]
      oRates[i,j]=coef[1]
      oRMSs[i,j]=sig[1]
      FOR oi=0, nOff-1 DO BEGIN
        oOffs[*,ij, oi]=offset[0:1,oi]
        oOffsets[i,j, oi]=offset[0,oi]
        oOffsetErrs[i,j, oi]=offset[1,oi]
      ENDFOR
      llhs[*,ij]=[xo,yo,0]
      ollhs[*,i,j]=[xo,yo,z0]
      ;stop
      
      oSumLines[ij]=sum_line
      IF is_annual EQ 1 THEN BEGIN
        oAnns[*,ij]=[ANN_AMP,ANN_PHASE,ANN_EAMP]
        oAnnAmps[i,j]=ANN_AMP
        oAnnPhas[i,j]=ANN_PHASE
      ENDIF
      ;
      jfile=opath+PATH_SEP()+site_name+'.jpg'
      IF out_plot EQ 1 THEN BEGIN
        ;OPLOT,xs,ys,color='0000ff'x ;only the liear trend (not considering offset, ...)
        OPLOT,x,yfit,color='0'x,psym=-6
        XYOUTS,MEAN(xs),30,site_name+STRING(oRatesXyz[2:3,ij],format='(1x,"rate=",f7.2,1x,"+/-",f7.2,1x,"mm/yr")'),/data,color='0'x,alignment=.5
        ;PRINT,jfile
        WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
        ;stop
        WDELETE,1
      ENDIF
      
      odata=DBLARR(9,N_ELEMENTS(dyrs))
      odata[0,*]=dyrs
      odata[1,*]=years
      odata[2,*]=doyrs
      odata[3,*]=[0,data_ijs_avg]*1d-3
      odata[4,*]=[0,data_ijs_avg]*1d-3
      odata[5,*]=[0,data_ijs_avg]*1d-3
      odata[6,*]=[0,odata_uncs]*1d-3
      odata[7,*]=[0,odata_uncs]*1d-3
      odata[8,*]=[0,odata_uncs]*1d-3
      ofile=desuffix(jfile)+'.neu'
      cmt=STRING(xo,yo,format='("lon=",1x,f12.6,1x,"lat=",1x,f12.6)')
      WRITE_SIO,ofile,data=odata,src=[files,cmt],prog=prog
    ;STOP
    ENDFOR
  ENDFOR
  
  IF count EQ 0 THEN BEGIN
    PRINT,'WARNING: no data points found!'
    RETURN
  ENDIF
  epochs=REPLICATE(2017, count)
  ofile_net=opath+PATH_SEP()+'sites.net'
  pos=WHERE(site_names NE '')
  WRITE_NET, $
    ofile_net, $
    SITES=SITE_NAMES[pos], $
    LLHS=LLHS[*,pos], $
    USER=USER, $
    PROG=PROG, $
    SRC=path, $
    EPOCHS=EPOCHS
  ;stop
  NET2KML,ofile_net
  
  ;stop
  ofile=opath+PATH_SEP()+'rate_ann.xyz'
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=files
  PRINTF,fid,'longitude','latitude','height','rate','rate_sig','annamp','annpha','annsig', 'site',$
    format='("*",8(",",1x,A),",",1x,a4)'
  IF count GT 0 THEN BEGIN
    FOR i=0ull, count-1 DO BEGIN
      ;if site_names[pos[i]] eq '00UW' then stop
      IF oRatesXyz[2,pos[i]] EQ 0d0 THEN CONTINUE
      PRINTF,fid,llhs[*,pos[i]],oRatesXyz[2:3,pos[i]],oAnns[*,pos[i]],site_names[pos[i]],format='(1x,8(1x,f),1x,a4)'
    ;printf,fid,oSumLines[pos[i]],format='(1x,a)'
    ENDFOR
  ENDIF
  FREE_LUN,fid
  
  ;Output XYZ file
  ofile=opath+PATH_SEP()+'rates.xyz'
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=files
  fmtstr='("*",4(1x,a15),1x,'+STRTRIM(nOff*2,2)+'(1x,a12),1x,a4)' ;'("*",6(",",1x,A),",",1x,a4)'
  FOR oi=0,nOff-1 DO BEGIN
    IF oi EQ 0 THEN BEGIN
      offset_names=['offset','offse_sig']
      PRINTF,fid,offset[2,oi],format='("*Time_of_offset : ",1x,f)'
    ENDIF ELSE BEGIN
      offset_names=[offset_names, ['offset','offse_sig']+STRTRIM(oi+1,2)]
      PRINTF,fid,STRTRIM(oi+1,2),offset[2,oi],format='("*Time_of_offset",a," : ",1x,f)'
    ENDELSE
    
  ENDFOR
  PRINTF,fid,'*Displacement_Unit : m',format='(a)'
  PRINTF,fid,'*Is_annual_term_esimated : ',is_annual, format='(a,1x,i1)'
  PRINTF,fid,'*Is_semiannual_term_esimated : ',is_semiannual, format='(a,1x,i1)'
  PRINTF,fid,'longitude','latitude','rate','rate_sig',offset_names, 'site',$
    format=fmtstr
  fmtstr='(1x,4(1x,f15.8),1x,'+STRTRIM(nOff*2,2)+'(1x,f12.5),1x,a4)'
  IF count GT 0 THEN BEGIN
    FOR i=0ull, count-1 DO BEGIN
      ;if site_names[pos[i]] eq '00UW' then stop
      IF oRatesXyz[2,pos[i]] EQ 0d0 THEN CONTINUE
      PRINTF,fid,oRatesXyz[*,pos[i]],oOffs[*,pos[i],*],site_names[pos[i]],format=fmtstr ;'(1x,6(1x,f),1x,a4)'
    ;printf,fid,oSumLines[pos[i]],format='(1x,a)'
    ENDFOR
  ENDIF
  FREE_LUN,fid
  
  
  ;output coseismic offset (if present)
  IF time_offset[0] NE -9999d0 THEN BEGIN
    ofile=opath+PATH_SEP()+'co_disp.llde'
    OPENW,fid,ofile,/get_lun
    
    IF count GT 0 THEN BEGIN
      FOR i=0ull, count-1 DO BEGIN
        ;if site_names[pos[i]] eq '00UW' then stop
        IF oRatesXyz[2,pos[i]] EQ 0d0 THEN CONTINUE
        PRINTF,fid,oRatesXyz[[1,0],pos[i]],oOffs[*,pos[i],*],format='(f12.6,1x,f12.6,2(1x,f9.5))'
      ;printf,fid,oSumLines[pos[i]],format='(1x,a)'
      ENDFOR
    ENDIF
    FREE_LUN,fid
  ENDIF
  
  ofile_stat=opath+PATH_SEP()+'STAT.MODEL'
  OPENW,FID_STAT,ofile_stat,/get_lun
  WRITE_SYS_INFO,FID_STAT,prog=prog,src=files
  COL_NAMES=['SITE','NEU','RATE','SIGMA.RATE']
  IF IS_ANNUAL THEN COL_NAMES=[COL_NAMES,'AMP.ANN','PHA.ANN']
  IF IS_SEMIANNUAL THEN COL_NAMES=[COL_NAMES,'AMP.SEMI','PHA.SEMI']
  COL_NAMES=[COL_NAMES,'RMSE']
  IF IS_ANNUAL THEN COL_NAMES=[COL_NAMES,'SIG.ANN.AMP']
  PRINTF,FID_STAT,COL_NAMES,FORMAT='("*",A4,1X,A3,8(1X,A12))'
  IF count GT 0 THEN BEGIN
    FOR i=0ull, count-1 DO BEGIN
      ;if site_names[pos[i]] eq '00UW' then stop
      IF oRatesXyz[2,pos[i]] EQ 0d0 THEN CONTINUE
      PRINTF,FID_STAT,oSumLines[pos[i]],FORMAT='(A)'
    ENDFOR
  ENDIF
  FREE_LUN,FID_STAT
  
  ;  ;Output XYZ file
  ;  ofile=opath+PATH_SEP()+'rates.xyz'
  ;  OPENW,fid,ofile,/get_lun
  ;  Write_sys_info,fid,prog=prog,src=files
  ;  IF count GT 0 THEN BEGIN
  ;    FOR i=0ull, count-1 DO BEGIN
  ;      IF oRatesXyz[2,i] EQ 0d0 THEN CONTINUE
  ;      PRINTF,fid,oRatesXyz[*,i],format='(1x,4(1x,f))'
  ;    ENDFOR
  ;  ENDIF
  ;  FREE_LUN,fid
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
  
  
  
  ;Output Rate NetCDF file
  ofile=opath+PATH_SEP()+'offset.grd'
  id = NCDF_CREATE(ofile, /CLOBBER)
  ; Fill the file with default values:
  NCDF_CONTROL, id, /FILL
  xid = NCDF_DIMDEF(id, 'lon', onx)    ; Make dimensions.
  yid = NCDF_DIMDEF(id, 'lat', ony)    ; Make dimensions.
  zid = NCDF_DIMDEF(id, 'z', onx*ony)
  ;  zeid = NCDF_DIMDEF(id, 'ze', onx*ony)
  ; Define variables:
  aid = NCDF_VARDEF(id, 'lon', [xid], /double)
  bid = NCDF_VARDEF(id, 'lat', [yid], /double)
  vid = NCDF_VARDEF(id, 'LOS_Offset', [xid,yid], /double)
  ;  veid = NCDF_VARDEF(id, 'LOS_Offset_Err', [xid,yid], /double)
  NCDF_ATTPUT, id, aid, 'units', 'degree_east'
  NCDF_ATTPUT, id, aid, 'long_name', 'Longitude'
  NCDF_ATTPUT, id, bid, 'units', 'degree_north'
  NCDF_ATTPUT, id, bid, 'long_name', 'Latitude'
  NCDF_ATTPUT, id, vid, 'units', 'mm'
  NCDF_ATTPUT, id, vid, 'long_name', 'InSAR LOS Displacement'
  ;  NCDF_ATTPUT, id, veid, 'units', 'mm'
  ;  NCDF_ATTPUT, id, veid, 'long_name', 'InSAR LOS Displacement Error Estimate'
  NCDF_ATTPUT, id, /GLOBAL, 'Title', 'LOS_OFFSET'
  ; Put file in data mode:
  NCDF_CONTROL, id, /ENDEF
  ; Input data:
  NCDF_VARPUT, id, aid, olon
  NCDF_VARPUT, id, bid, olat
  ;NCDF_VARPUT, id, vid, oOffsets
  ;NCDF_VARPUT, id, vid, oAnnAmps
  ;NCDF_VARPUT, id, veid, oOffsetErrs
  NCDF_CLOSE, id ; Close the NetCDF file.
  
  IF out_plot EQ 1 THEN BEGIN
    jfile=opath+PATH_SEP()+'site_map.jpg'
    ;PRINT,jfile
    WSET,0
    WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
  ENDIF
  ;stop
  
;  STAT_MODEL_FOR_PLOT, $
;    ofile_stat, $ ; INPUT FILE, CREATED BY MODEL (WITH "Output Statistics" CHECKED).
;    ofile_stat+'_GMT', $  ;OUTPUT FILE, INPUT OF PSXY (GMT): LONGITUDE, LATITUDE, PHASE, AMPLITUDE
;    CFILE=ofile_net,  $
;    scale_factor=1d-3 ,$
;    neu='N'
    
  PRINT,'['+prog+']Normal end.'
END