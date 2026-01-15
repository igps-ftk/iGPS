; +by tianyf on Wed Sep 10 14:04:34 CST 2025
;   Fixed a bug for calculating intersection of the profile and fault trace.
;
PRO DEM_GRID_EXTRACT_PROFILE, demfile,  $  ;DEM file (ENVI raw image format; with map projection)
    pfile,  $  ;profile line (two vertices; extended GMT psxy format)
    opath,  $  ;output path
    fa_xys=fa_xys,  $ ;x/y of fault line
    ffile=ffile  ;fault polyline (extended GMT psxy format)
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 3 THEN BEGIN
    demfile='C:\GMT_pub\dem.gmt\ETOPO1_Bed_g_geotiff\ETOPO1_Bed_g_envi.img'
    ;demfile='D:\Papers\xsh\vector\dem\1s\dem.tif'
    
    
    fa='tianshen_daban'
    fa='bns'
    fa='karakoram'
    fa='longmuco'
    fa='longmuco_to_atf
    fa='kunlun_jss_revised'
    fa='F12'
    fa='yzs'
    fa='jiali_cses_beng_co_ext_north'
    fa='tyr'
    ffile='C:\GMT_pub\vector\profile\fa_'+fa+'.psxy'
    
    ;    ffile='C:\GMT_pub\vector\profile\fa_'+fa+'.psxy'
    ;    ;pfile='D:\gsar\interseismic\063-d-m5-0462_0467_0472_0478_0483-aksaichin2_karakoram\f123\sbas.4.0.0001.9999.20141010.20230707.109.0787.01.___\p.fa_'+fa+'\profiles_auto.psxy'
    ;    pfile='D:\gsar\gic3dv\g219\asc_des\profiles\p.fa_'+fa+'\profiles_auto.psxy'
    ;    opath='D:\ICD\Eighth\2023\20230508.G219\figure\profile.'+fa+'\dem'
    ;
    ;    pfile='D:\gsar\gic3dv\g219\asc_des\profiles\p.fa_F12\profiles_auto.psxy'
    ;    opath='D:\ICD\Eighth\2023\20230508.G219\figure\profile.F12\dem'
    
    ffile='C:\GMT_pub\vector\profile\fa_yzs.psxy'
    pfile='D:\gsar\gic3dv\yzs\asc_des\profiles\p.fa_yzs\profiles_auto.psxy'
    opath='D:\Papers\sse.yzs\figure\3.profiles_3d_y15-19.yzs.gacos\dem'
    
    ffile='C:\GMT_pub\vector\profile\fa_xsh_b.psxy'
    pfile='D:\gsar\gic3dv\xsh\profiles\pg.fa_xsh_b\profiles_auto.psxy'
    opath='D:\Papers\xsh\figure\graphical_abstract\dem'
    
  ;    ffile='C:\GMT_pub\vector\profile\fa_kunlun_jss_revised3.psxy'
  ;    pfile='D:\gsar\gic3dv\atf.d019\asc_des\profiles\p.fa_kunlun_jss_revised3\profiles_auto.psxy'
  ;    opath='D:\gsar\gic3dv\atf.d019\asc_des\profiles\p.fa_kunlun_jss_revised3\dem'
  ;
  ;
  ;    ffile='C:\GMT_pub\vector\profile\fa_jiali_cses_beng_co_ext_north.psxy'
  ;    pfile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\profiles\p.fa_jiali_cses_beng_co_ext_north\profiles_auto.psxy'
  ;    opath='D:\Papers\jiali.insar\figure\insar.thiswork.vs.fang2024\dem'
  ;
  ;
  ;    ffile='C:\GMT_pub\vector\profile\fa_qixiangco_likang.psxy'
  ;    pfile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\profiles\p.fa_qixiangco_likang\profiles_auto.psxy'
  ;    opath='D:\Papers\jiali.insar\figure\profiles.qixiangco\dem'
  ;
  ;
  ;    ffile='C:\GMT_pub\vector\profile\fa_tyr.psxy'
  ;    pfile='D:\gsar\gic3dv\yzs\asc_des\profiles\p.fa_tyr\profiles_auto.psxy'
  ;    opath='D:\Papers\sse.yzs\figure\profiles_3d.tyr\dem'
  ;
  ;    ffile='C:\GMT_pub\vector\profile\fa_gozhaco.psxy'
  ;    pfile='D:\gsar\envisat.alos\envisat.d.t248f2907.gozhaco\sbas.3.0.0001.9999.20080405.20100828.018.0088.01.___\p.fa_gozhaco\profiles_auto.psxy'
  ;    opath='D:\Papers\creep.gozhaco\figure\4.profiles_s1_vs_envisat\dem'
  ;
  ;    ffile='C:\GMT_pub\vector\profile\fa_atf_ext.psxy'
  ;    pfile='D:\gsar\gic3dv\atf.d019\asc_des\profiles\p.fa_atf_ext\profiles_auto.psxy'
  ;    opath='D:\gsar\gic3dv\atf.d019\asc_des\profiles\p.fa_atf_ext\dem'
  ;
  ;
  ;    ffile='C:\GMT_pub\vector\profile\fa_jiali.psxy'
  ;    pfile='D:\Papers\jiali\figure\profiles.gps.comb_ref_kunlun\pg.fa_jiali_200km\profiles_auto.psxy'
  ;    opath='D:\Papers\jiali\figure\profiles.vertical.20250220\dem'
  ;    pfile='D:\Papers\jiali\figure\profiles.gps.comb20241005\pg.fa_jiali_250km\profiles_auto.psxy'
  ;    opath='D:\Papers\jiali\figure\profiles.gps.comb20241005\dem'
  ;
  ;    ffile='C:\GMT_pub\vector\profile\fa_jiali.psxy'
  ;    pfile='D:\Papers\jiali\figure\profiles.gps.comb_ref_kunlun\pg.fa_jiali2\profiles_auto.psxy'
  ;    opath='D:\Papers\jiali\figure\profiles.gps.comb_ref_kunlun\pg.fa_jiali2\dem'
  ;
  ;
  ;    ffile='C:\GMT_pub\vector\profile\fa_haiyuan.psxy'
  ;    pfile='D:\gsar\envisat.alos\envisat.d.t333f2853.haiyuan\sbas.3.0.0001.9999.20031114.20100903.037.0483.01.___\p.fa_haiyuan\profiles_auto.psxy'
  ;   opath='D:\Papers\creep.gozhaco\figure\laohushan\dem'
  ;
  ;   ffile='C:\GMT_pub\vector\profile\fa_yadong_gulu_shuanghu.psxy'
  ;   pfile='D:\gsar\gic3dv\jiali\interp\profiles\pg.fa_yadong_gulu_shuanghu_withGPSupExt\profiles_auto.psxy'
  ;   opath='D:\Papers\jiali.insar\figure\profiles.yadong-gulu\dem.yadong_gulu_shuanghu'
    
  ;   ffile='C:\GMT_pub\vector\profile\fa_xsh_b.psxy'
  ;   pfile='D:\gsar\gic3dv\xsh\profiles\pg.fa_xsh_b\profiles_auto.psxy'
  ;   opath=''
  
  
      ffile='C:\GMT_pub\vector\profile\fa_jiali.psxy'
      pfile='D:\Papers\jiali\figure\profiles.gps.comb_ref_kunlun\pg.fa_jiali_200km\profiles_auto.psxy'
  opath='D:\Papers\jiali\figure\profiles.vertical.20250220\dem/
    
  ENDIF
  
  IF N_ELEMENTS(ffile) EQ 0 THEN ffile=''
  IF FILE_TEST(opath,/directory) NE 1 THEN BEGIN
    FILE_MKDIR, opath
  ENDIF
  
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
  
  xys_fvec_ij=xys_fvec
  xys_fvec_ij[*]=-1
  FOR i=0,N_ELEMENTS(xys_fvec[0,*])-1 DO BEGIN
    ENVI_CONVERT_FILE_COORDINATES, fid, xf1, yf1, xys_fvec[0,i], xys_fvec[1,i]
    xys_fvec_ij[*,i]=[xf1,yf1]
  ENDFOR
  
  ;STOP
  ;loop for each profile
    FOR pi=0,np-1 DO BEGIN
;  FOR pi=156,156 DO BEGIN
    pf=REFORM(*pfs[pi])
    a1=REFORM(pf[*,0])
    b1=REFORM(pf[*,1])
    WSET,1
    OPLOT,pf[0,*],pf[1,*],color='0000ff'x,thick=2,linestyle=2
    ;print,pf
    ;stop
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
    ;
    ;if no intersect between profile and fault lines, skip current profile
    ;stop
    xy3=INTERSECT_BETWEEN_POLYLINE_AND_LINE(xys_fvec_ij,[xf1,yf1],[xf2,yf2],is_debug=0)
    ;help,xy3
    ;print,xy3
    ;stop
    ;window,2
    ;plot,[xf1,xf2,reform(xys_fvec_ij[0,58:70])],[yf1,yf2,reform(xys_fvec_ij[1,58:70])],background='ffffff'x,color=0,psym=-2,/ynozero
    ;oplot,[xf1,xf2],[yf1,yf2],psym=-4,color='ff0000'x
    ;stop
    IF FINITE(xy3[0]) NE 1 THEN BEGIN
      PRINT,'['+prog+']WARNING:no intersect between fault polyline and profile '+STRTRIM(pi+1,2)+'!'
      CONTINUE
    ENDIF
    PLOTS,xy3[0],xy3[1],psym=6,color='0'x,symsize=1
    ENVI_CONVERT_FILE_COORDINATES, fid, xy3[0], xy3[1], xy3_map_x, xy3_map_y,/to_map
    
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
      IF xf LT 0 || xf GT ns-1 || yf LT 0 || yf GT nl-1 THEN BEGIN
        odata[2,pxi]=-9999d0
        CONTINUE
      ENDIF
      ENVI_CONVERT_FILE_COORDINATES, fid, xf, yf, xmap, ymap,/to_map
      odata[0:2,pxi]=[xmap,ymap,dem_grid[xf,yf]]
      tmp=MAP_2POINTS(xy3_map_x,xy3_map_y,xmap,ymap,/meter)*1d-3
      odata[3,pxi]=tmp*(xmap-xy3_map_x)/ABS(xmap-xy3_map_x)
;      IF ABS(tmp) LT 27 THEN BEGIN
;        PRINT,xf, yf, xmap, ymap,xy3
;        x1s=xfs[170:200]
;        y1s=yfs[170:200]
;        ENVI_CONVERT_FILE_COORDINATES, fid, x0, y0, xy3[0], xy3[1]
;        WINDOW,1
;        PLOT,x1s,y1s,psym=2,background='ffffff'x,color=0,/ynozero
;        OPLOT,[x0],[y0],psym=4,color='0000ff'x
;        ;STOP
;      ENDIF
    ;stop
    ENDFOR
    
    pos=WHERE(odata[2,*] NE -9999d0)
    
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
    FOR pxi=0, N_ELEMENTS(pos)-1 DO BEGIN
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
