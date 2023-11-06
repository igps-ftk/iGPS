PRO DEM_GRID_EXTRACT_PROFILE, demfile,  $  ;DEM file (ENVI raw image format; with map projection)
    pfile,  $  ;profile line (two vertices; extended GMT psxy format)
    opath,  $  ;output path
    fa_xys=fa_xys,  $ ;x/y of fault line
    ffile=ffile  ;fault polyline (extended GMT psxy format)
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 3 THEN BEGIN
    demfile='C:\dem.gmt\envi.dem.min\envi.dem.min.raw'
    demfile='C:\dem.gmt\ETOPO1_Bed_g_geotiff\ETOPO1_Bed_g_geotiff.tif'
    demfile='C:\GMT_pub\dem.gmt\ETOPO1_Bed_g_geotiff\ETOPO1_Bed_g_envi.img'
    
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
    
    
    pfile='D:\gsar\des\m_gulu1_dangxiong2\f123.1\SBAS7\p.b\profiles_auto.psxy'
    opath='D:\gsar\des\m_gulu1_dangxiong2\f123.1\SBAS7\p.b\dem'
    ffile='C:\GMT_pub\vector\profile\fa_bengco_jiali_ext.psxy'
    
    pfile='D:\gsar\des\m_jiali2_nujiang4\f123.1\SBAS4\p.b\profiles_auto.psxy'
    opath='D:\gsar\des\m_jiali2_nujiang4\f123.1\SBAS4\p.b\dem'
    
    ffile='C:\GMT_pub\vector\profile\fault_altyntagh.psxy'
    pfile='D:\gsar\asc\m_altyntagh11\f123.1\SBAS2\p\profiles_auto.psxy'
    opath='D:\gsar\asc\m_altyntagh11\f123.1\SBAS2\p\'
    
    ffile='C:\GMT_pub\vector\profile\iys1.psxy'
    pfile='\\10.4.134.31\root\g6e\gsar\m_yzs2_shenza-dingjie\f123.1\SBAS8\p\profiles_auto.psxy'
    opath='\\10.4.134.31\root\g6e\gsar\m_yzs2_shenza-dingjie\f123.1\SBAS8\p'
    
    ffile='C:\GMT_pub\vector\profile\pf_longmenshan1.psxy'
    pfile='C:\gsar\des\m_dayi2\f123.1\SBAS5\p\profiles_auto.psxy'
    opath='C:\gsar\des\m_dayi2\f123.1\SBAS5\p\'
    
    ffile='C:\GMT_pub\vector\profile\fa_kunlun_ext.psxy'
    pfile='D:\gsar\des\t106-f471-f481-d-m_eastkunlun2M3\f123.1\SBAS14.long\p\profiles_auto.psxy'
    opath='D:\gsar\des\t106-f471-f481-d-m_eastkunlun2M3\f123.1\SBAS14.long\p\'
    
    ffile='C:\GMT_pub\vector\profile\fa_xiaoerkule1.psxy'
    pfile='D:\gsar\interseismic\t158-f107-f117-a-m_gozhaco1M3\f123.1\SBAS13.b600-.with_gacos_add\p.fa_xiaoerkule1\profiles_auto.psxy'
    opath='D:\gsar\interseismic\t158-f107-f117-a-m_gozhaco1M3\f123.1\SBAS13.b600-.with_gacos_add\p.fa_xiaoerkule1\dem'
    
    ffile='C:\GMT_pub\vector\profile\fa_atf.psxy'
    pfile='D:\gsar\interseismic\012-a-m7-0088_0093_0098_0103_0108_0113_0118-tibet\f123\sbas.4.9.0001.9999.20150627.20220930.053.0393.01.___\p.fa_atf_9r\profiles_auto.psxy'
    opath='D:\gsar\interseismic\012-a-m7-0088_0093_0098_0103_0108_0113_0118-tibet\f123\sbas.4.9.0001.9999.20150627.20220930.053.0393.01.___\p.fa_atf_9r\dem'
    
    
    ffile='C:\GMT_pub\vector\profile\fa_Kunlun_Fault.psxy'
    pfile='D:\gsar\interseismic\033-d-m7-0463_0468_0473_0478_0483_0487_0493-kunlun\f123\sbas.4.0.0001.9999.20141031.20210515.039.0231.01.___\p.fa_Kunlun_Fault\profiles_auto.psxy'
    opath='D:\ICD\meeting\cugs2023\abstract\kunlun\figure\profiles\dem'
    
    ffile='C:\GMT_pub\vector\profile\fa_redriver.psxy'
    pfile='D:\gsar\interseismic\164-d-m3-0510_0515_0520-honghe8M3\f123\sbas.4.0.0367.9999.20150625.20210524.136.1524.01.___\p.fa_redriver\profiles_auto.psxy'
    opath='D:\ICD\Eighth\2019\20190613.Hezong.Zone2\Final\report\figure\1.profiles.des\dem'
    
    
    ffile='C:\GMT_pub\vector\profile\fa_xiaojiang_ext.psxy'
    pfile='D:\gsar\interseismic\099-a-m4-1245_1250_1255_1260-honghe\f123\sbas.4.0.0367.9999.20170318.20210707.127.1346.01.___\p.fa_xiaojiang_ext\profiles_auto.psxy'
    opath='D:\ICD\Eighth\2019\20190613.Hezong.Zone2\Final\report\figure\2.profiles.asc.xiaojiang\dem.fa_xiaojiang_ext'
    
     ffile='C:\GMT_pub\vector\profile\fa_yuanjiang_puer_proposed.psxy'
    pfile='D:\gsar\interseismic\099-a-m4-1245_1250_1255_1260-honghe\f123\sbas.4.0.0367.9999.20170318.20210707.127.1346.01.___\p.fa_yuanjiang_puer_proposed\profiles_auto.psxy'
    opath='D:\ICD\Eighth\2019\20190613.Hezong.Zone2\Final\report\figure\2.profiles.asc.xiaojiang\dem.fa_yuanjiang_puer_proposed'
    
         ffile='C:\GMT_pub\vector\profile\fa_nantinghe_east.psxy'
    pfile='D:\gsar\interseismic\099-a-m4-1245_1250_1255_1260-honghe\f123\sbas.4.0.0367.9999.20170318.20210707.127.1346.01.___\p.fa_nantinghe_east\profiles_auto.psxy'
    opath='D:\ICD\Eighth\2019\20190613.Hezong.Zone2\Final\report\figure\2.profiles.asc.xiaojiang\dem.fa_nantinghe_east'
    
    
  ENDIF
  
  IF N_ELEMENTS(ffile) EQ 0 THEN ffile=''
  
  ;read profiles
  READ_PSXY,   $
    pfile,   $ ;input file
    region=pfs,   $ ;x,y coorinates of each polygons (pointer type)
    nps=nps, $  ;number of point pairs for each polygon
    count=np,  $ ;number of polygons
    igpsmode=0, $
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
  
  IF N_ELEMENTS(fa_xys) LT 4 THEN BEGIN
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
  ENDIF ELSE BEGIN
    lons_fa=REFORM(fa_xys[0,*])
    lats_fa=REFORM(fa_xys[1,*])
  ENDELSE
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
      if xf lt 0 || xf gt ns-1 || yf lt 0 || yf gt nl-1 then begin
        odata[2,pxi]=-9999d0
        continue
      endif
      ENVI_CONVERT_FILE_COORDINATES, fid, xf, yf, xmap, ymap,/to_map
      odata[0:2,pxi]=[xmap,ymap,dem_grid[xf,yf]]
      tmp=MAP_2POINTS(xy3[0],xy3[1],xmap,ymap,/meter)*1d-3
      odata[3,pxi]=tmp*(xmap-xy3[0])/ABS(xmap-xy3[0])
    ;stop
    ENDFOR
    
    pos=where(odata[2,*] ne -9999d0)
    
    WINDOW,2,ysize=700
    !p.MULTI=[0,1,2]
    ;
    
    PLOT,[xf1,xf2],[yf1,yf2],background='ffffff'x,color='0'x,psym=-5,/ynozero,title='Image Map (column/row)';,/iso
    ;PLOTS,xfs,yfs,color='ff0000'x,psym=4
    PRINT,xf1,xf2,MIN(xfs),MAX(xfs),xf2-xf1
    PRINT,yf1,yf2,MIN(yfs),MAX(yfs),yf2-yf1
    ;
    ;
    PLOT,odata[0,pos],odata[2,pos],background='ffffff'x,color='0'x,   $
      title='Profile #'+STRING(pi+1,format='(i03)'),  $
      xtitle='X-axis',ytitle='DSM (m)'
    !p.MULTI=-1
    ;stop
    
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i03)')+'_vel_dem.txt'
    OPENW,fido,ofile,/get_lun
    WRITE_SYS_INFO,fido,prog=prog,src=[demfile,pfile],user=user
    PRINTF,fido,'longitude','latitude','height','distance',format='("#",2(1x,a12),1x,a10,1x,a10)'
    FOR pxi=0, n_elements(pos)-1 DO BEGIN
      PRINTF,fido,odata[0:3,pos[pxi]],format='(1x,2(1x,f20.5),1x,f10.3,1x,f10.2)'
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
