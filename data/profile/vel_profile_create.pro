

PRO VEL_PROFILE_CREATE, vfile, $  ;velocity file (in varied formats)
    opath, $   ;output path
    ffile=ffile,  $ ;(if exist) fault trace (only one polyline in GMT format)
    f2file=f2file,  $ ;(if exist) fault trace (only one polyline in GMT format)
    pfile=pfile,  $   ;profiles file (two-end-points lines)
    flon=flon, $  ;fault longitude (if ffile is not present && it it not given, use the middle point of the profile)
    flat=flat, $     ;fault latitude (if ffile is not present && it is not given, use the middle point of the profile)
    out_plot=out_plot,  $ ;whether output temporary plots
    inputfmt=inputfmt,  $ ;input velocity format (0-  ; 1-psvelo)
    ;0 (default):1x site lon lat vn sign ve sige cne
    ;1 (psvelo): 1x lon lat ve vn sige sign cne site
    ;2 (qoca map velocity field):
    ;*Station   Longitude   Latitude Ve_init Ve_incr    Ve     dVe   Vn_init Vn_incr    Vn     dVn   Cen
    ; ARTU_GPS   58.5583   56.4278     0.0     0.0    24.9     0.0     0.0     0.0     6.1     0.0   0.0000
    ;3 (free format 1): Long.    Lat.      Ve       Vn     Se     Sn    Cne  Sta.      Source
    ;  e.g.,
    ; 74.336  39.842   0.170   13.790  0.560  0.503  0.000  I089  This_study
    ;4 xyz or xyze:
    
    auto_strike=auto_strike,  $
    ;  1: calculate strikes for individual segments
    ;  2: use the average strike for all segments
    spacing_profile=spacing_profile,  $
    length_profile=length_profile,  $
    search_radius=search_radius,  $
    LBl_ID=LBl_ID,  $
    dummy=dummy
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 2 THEN BEGIN
    ;
    ;inputfmt=1
  
    ;
    ;    vfile='D:\iGPS\example\profile\Supp_Table_S1.psvelo'
    ;    opath='D:\iGPS\example\profile\p_auto'
    ;
    ;    ;
    ;    ;
    ;    ffile='D:\iGPS\example\profile\fa_honghe1.psxy'
    ;    ;pfile='D:\iGPS\example\profile\pf_honghe1.psxy'
    ;
    ;
    ;    fa='pf_longmenshan1'
    ;    IF N_ELEMENTS(ffile) EQ 0 THEN BEGIN
    ;      PROFILE_NAME2VECTORFILE,   $
    ;        fa,   $ ;input, fault name
    ;        ffile=ffile,  $ ;output, fault file
    ;        pfile=pfile ;output, profile file
    ;    ENDIF
    ;
    ;    ;for Prof. Zhang S. M.
    ;    ;ffile='D:\tmp\gps.profile\in.fault.line\fa_lijiang_xiaojinhe.psxy'
    ;    vfile='D:\tmp\gps.profile\in.gps.velocity\jgrb52327-sup-00012-017JB014465-Data%20Set%20SI-S01.txt'
    ;    ;opath='D:\tmp\gps.profile\out.fault.velocity.profiles'
    ;    inputfmt=3
    ;  ;stop
    ;  ;pfile='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\vector\profiles.psxy'
  
  
    ;        vfile='D:\iGPS\tables\wang_shen_2019JB018774_Table.S4S5.psvelo'
    ;    inputfmt=1
    ;    ffile='D:\iGPS\tables\fa_ganzi.psxy'
    ;    opath='D:\gsar\interseismic\004-d-m5-0476_0481_0486_0491_0496-jiali8\f123\sbas.4.0.0001.9999.20170311.20230831.060.0410.01.___\pg.fa_ganzi'
    ;
    ;
    ;    vfile='C:\tmp\gic3dv\kunlun\asc_des\gic3dv.out.psvelo'
    ;    ffile='C:\GMT_pub\vector\profile\fa_Kunlun_Fault.psxy'
    ;;    f2file='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy'
    ;    opath='C:\tmp\gic3dv\kunlun\pg.fa_ganzi_xianshuihe'
    ;    opath='C:\tmp\gic3dv\kunlun\asc_des\sbas.4.0.0367.9999.20150808.20210520.147.1355.01.___\p.fa_kunlun_Fault_gic3dv_out_horizontal_twofaults'
    ;    ;pfile='C:\tmp\gic3dv\kunlun\pg.fa_ganzi_xianshuihe\profiles_auto.psxy'
    ;    auto_strike=2
    ;    auto_strike_2nd=1
    ;    length_profile=1600
    ;    spacing_profile=10
    ;
    ;    vfile='D:\iGPS.addon\test\fault_motion\fa_elastic_two_faults.llen'
    ;
    ;    ffile='C:\GMT_pub\vector\profile\fa_Kunlun_Fault.psxy'
    ;    opath='D:\iGPS.addon\test\fault_motion\grid3'
    ;;    f2file=''
    ;;    ffile='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy'
    ;    f2file='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy';
    ;;
    ;        vfile='C:\tmp\gic3dv\kunlun\asc_des\gps_prd.psvelo'
    ;        ffile='D:\iGPS\tables\fa_Kunlun_Fault.psxy'
    ;        f2files='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy'
    ;        opath='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\asc_des\sbas.4.0.0367.9999.20150808.20210520.147.1355.01.___\p.fa_kunlun_Fault_gic3dv_prd_toGYXSH'
  
  
    ;    vfile='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\asc_des\gic3dv.out.psvelo'
    ;    ffile='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy'
    ;    opath='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\asc_des\sbas.4.0.0367.9999.20150808.20210520.147.1355.01.___\p.fa_ganzi_yushu_xianshuihe_gic3dv_out_horizontal_a1'
    ;    is_fault_trace_downsample=1
    ;    auto_strike=1
  
    ;    vfile='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\insar_los_2_3d_all_2km.psvelo'
    ;   vfile='Z:\g11j\D\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\insar_los_2_3d_all_2km.psvelo'
    ;    vfile='Z:\g11j\D\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\insar_los_2_3d_all_1km.psvelo'
  
    ;    ;    ;master: ganzi-yushu-xianshui
    ;    ;    ;slaves: others
    ;    ffile='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy'
    ;    f2files=['C:\GMT_pub\vector\profile\fa_wudaoliang_changshagongma.psxy' $
    ;      ,'C:\GMT_pub\vector\profile\fa_dari.psxy'  $
    ;      ,'C:\GMT_pub\vector\profile\fa_maduo_gande.psxy'  $
    ;      ,'C:\GMT_pub\vector\profile\fa_Kunlun_Fault.psxy' $
    ;      ]
    ;    ;    is_fault_trace_downsample=1
    ;    ;    auto_strike=1
    ;    ;      opath='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_ganzi_xianshuihe_dec3d_byN_all_2km_toKLF_a1'
    ;    ;
    ;    is_fault_trace_downsample=1
    ;    auto_strike=2
    ;    opath='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_ganzi_xianshuihe_dec3d_byN_all_2km_toKLF'
    ;    ;    opath='Z:\g11j\D\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_ganzi_xianshuihe_dec3d_byN_all_2km_toKLF'
    ;
    ;
    ;        ;master: Kunlun
    ;        ;slaves: others
    ;        ffile='C:\GMT_pub\vector\profile\fa_Kunlun_Fault.psxy'
    ;;        opath='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_Kunlun_Fault_dec3d_byN_all_2km'
    ;
    ;        f2files=[ $
    ;          'C:\GMT_pub\vector\profile\fa_maduo_gande.psxy'  $
    ;          ,'C:\GMT_pub\vector\profile\fa_dari.psxy'  $
    ;          ,'C:\GMT_pub\vector\profile\fa_wudaoliang_changshagongma.psxy' $
    ;          ,'C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy' $
    ;          ]
    ;        opath='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_Kunlun_Fault_dec3d_byN_all_2km_toGYXSH'
    ;
  
    ;        opath='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\asc_des\sbas.4.0.0367.9999.20150808.20210520.147.1355.01.___\p.fa_kunlun_Fault_gic3dv_prd_toGYXSH_etal'
  
    ;    ffile='C:\GMT_pub\vector\profile\fa_wudaoliang_changshagongma.psxy'
    ;    opath='Z:\g11j\D\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_wudaoliang_changshagongma_dec3d_byN_all_1km'
    ;    ffile='C:\GMT_pub\vector\profile\fa_dari.psxy'
    ;    opath='Z:\g11j\D\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_dari_dec3d_byN_all_1km'
  
    ;    fa='fa_haiyuan'
    ;    ;     vfile='D:\gsar\interseismic\033-d-m2-0463_0468-menyuan\f123\sbas.4.0.0001.9999.20220110.20231219.058.0795.01.___\vel_mask_ll3.xyze'
    ;    cmt='3d_1920'
    ;    ; vfile='D:\gsar\interseismic\005-d-m3-0460_0466_0471-karakul_lake_south\f123.1\sbas.3.0.0720.9999.20141018.20200320.121.0340.01.___\vel_mask_ll3.xyz'
    ;    vfile='D:\gsar\gic3dv\hyf\asc_des\insar_los_2_3d_1920.psvelou'
    ;
  
    ;fa='fa_eklf'
    ;vfile='D:\gsar\gic3dv\kunlun\asc_des\gps_prd'
    ;vfile='D:\gsar\gic3dv\kunlun\asc_des\gic3dv.out'
    ;cmt='gicout'
  
    ;    vfile='D:\gsar\gic3dv\tianshan\asc_des\insar_los_2_3d2.psvelou'
    ;    fa='fa_maidan_shayilamu'
    ;    cmt='3d'
  
    vfile='D:\gsar\gic3dv\jiali\asc_des\insar_3d.150000'
    fa='fa_sangri_cuona_east_ext'
    cmt='i3d'
    opath='D:\gsar\interseismic\077-d-m7-0475_0480_0485_0490_0495_0500_0505-jiali\f123\asc_des\p.fa_sangri_cuona_east_ext_i3d'
    inputfmt=81
    
    is_fault_trace_downsample=0
    auto_strike=3
    ;auto_strike_2nd=2
    length_profile=1200
    spacing_profile=10
  ;
  ;    spacing_profile=200
  ;      search_radius=5
  ENDIF
  
  IF N_ELEMENTS(cmt) EQ 0 THEN BEGIN
    cmt=''
  ENDIF
  
  IF N_ELEMENTS(opath) EQ 0 THEN BEGIN
    opath=getpathname(vfile)+PATH_SEP()+'p.'+fa
    IF cmt NE '' THEN BEGIN
      opath=opath+'_'+cmt
    ENDIF
  ENDIF
  
  IF FILE_TEST(opath,/directory) NE 1 THEN FILE_MKDIR,opath
  
  IF N_ELEMENTS(out_plot) EQ 0 THEN out_plot=0
  ;IF N_ELEMENTS(inputfmt) EQ 0 THEN inputfmt=0
  IF N_ELEMENTS(auto_strike) EQ 0 THEN auto_strike=2
  IF N_ELEMENTS(spacing_profile) EQ 0 THEN spacing_profile=30  ;in km (distance between two neighboring profiles)
  IF N_ELEMENTS(length_profile) EQ 0 THEN length_profile=600  ;in km (the total length of the profile generated automatically by iGPS)
  IF N_ELEMENTS(is_fault_trace_downsample) EQ 0 THEN is_fault_trace_downsample=0
  
  IF N_ELEMENTS(ffile) EQ 0 THEN BEGIN
    ;      PROFILE_NAME2VECTORFILE,   $
    ;        fa,   $ ;input, fault name
    ;        ffile=ffile   ;output, fault file
    ;
  
    path_profile='C:\GMT_pub\vector\profile'
    ;      path_profile=!igps_root+path_sep()+'tables'
    ffile=path_profile+PATH_SEP()+fa+'.psxy'
  ENDIF
  
  np=0
  pxys=-9999d0
  IF N_ELEMENTS(search_radius) EQ 0 THEN BEGIN
    search_radius=50d0 ;maximum searching distance beside the profile line, in kilometers
    search_radius=spacing_profile*.5d0
  ENDIF
  ;search_radius=100d0
  
  lbl_str='['+prog+']INFO:reading velocity ('+vfile+')...'
  PRINT,lbl_str
  IF N_ELEMENTS(lbl_id) NE 0 THEN WIDGET_CONTROL, lbl_id, set_value=lbl_str
  READ_DEFO_VELOCITY, vfile,   $
    data=data,  $
    sites=sites,  $
    fmt=inputfmt, $
    dummy=dummy
  lls=data[0:1,*]
  nsit=N_ELEMENTS(sites)
  
  lons=REFORM(lls[0,*])
  lats=REFORM(lls[1,*])
  QHULL, lons, lats, tris
  FOR i=0,N_ELEMENTS(tris[0,*])-2 DO BEGIN
    p1=tris[0,i]
    p2=tris[1,i]
    pos=WHERE(tris[0,*] EQ p2)
    IF pos NE i+1 THEN BEGIN
      tmp=tris[*,i+1]
      tris[*,i+1]=tris[*,pos]
      tris[*,pos]=tmp
    ;stop
    ENDIF
    
  ;plots,lons[[p1,p2]], lats[[p1,p2]],psym=-2, color='0000ff'x
  ;stop
  ENDFOR
  PLOTS,lons[tris[0,*]],lats[tris[0,*]],psym=-4,color='ff0000'x
  xys_vel_hull=[lons[tris[0,*]],lats[tris[0,*]]]
  ;stop
  
  
  xmin=MIN(lls[0,*],max=xmax)
  ymin=MIN(lls[1,*],max=ymax)
  rect=[Xmin, Ymin, Xmax, Ymax]
  ;  rect=[Xmin-1, Ymin-1, Xmax+1, Ymax+1] ;enlarge the rect range
  
  
  PRINT,'['+prog+']INFO:reading fault ('+ffile+')...'
  ;read fault vector (if specified)
  IF N_ELEMENTS(ffile) GT 0 && ffile NE '' THEN BEGIN
    lines_fvec=read_txt(ffile)
    lines_fvec2=STRTRIM(lines_fvec,2)
    pos=WHERE(strmids(lines_fvec2,0,1) NE '>')
    IF N_ELEMENTS(pos) LT 2 THEN BEGIN
      PRINT,'['+prog+']ERROR: invalid fault line vector file <'+ffile+'>!!'
      RETURN
    ENDIF
    xys_fvec=DOUBLE(str_lines2arr(lines_fvec2[pos]))
  ;OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='0000ff'x
  ;stop
  ENDIF
  
  IF is_fault_trace_downsample EQ 1 THEN BEGIN
    DOWNSAMPLE_POLYLINE, xys_fvec,   $
      dtol, $
      oxys=xys_fvec_downsampled,  $
      is_plot=0
    xys_fvec=xys_fvec_downsampled
  ENDIF
  ;stop
  
  
  ;read 2nd fault vector (if present)
  IF N_ELEMENTS(f2files) GT 0 && f2files[0] NE '' THEN BEGIN
    n_2nd_fault=N_ELEMENTS(f2files)
    p_xys_fvec_2nds=REPLICATE(PTR_NEW(),n_2nd_fault)
    p_strikes_fa_2nds=REPLICATE(PTR_NEW(),n_2nd_fault)
    p_strikes_pr_2nds=REPLICATE(PTR_NEW(),n_2nd_fault)
    p_xys_fp_2nds=REPLICATE(PTR_NEW(),n_2nd_fault)
    p_oxys_2nds=REPLICATE(PTR_NEW(),n_2nd_fault)
    
    n_profiles_2nds=INTARR(n_2nd_fault)
    
    FOR f2i=0, n_2nd_fault-1 DO BEGIN ;loop for each 2nd fault file
      f2file=f2files[f2i]
      PRINT,'f2file:',f2file
      lines2_fvec=read_txt(f2file)
      lines2_fvec2=STRTRIM(lines2_fvec,2)
      pos2=WHERE(strmids(lines2_fvec2,0,1) NE '>')
      IF N_ELEMENTS(pos2) LT 2 THEN BEGIN
        PRINT,'['+prog+']ERROR: invalid fault line vector file <'+f2file+'>!!'
        RETURN
      ENDIF
      xys_fvec_2nd=DOUBLE(str_lines2arr(lines2_fvec2[pos2]))
      p_xys_fvec_2nds[f2i]=PTR_NEW(xys_fvec_2nd)
      
      OPLOT,xys_fvec_2nd[0,*],xys_fvec_2nd[1,*],psym='-3',color='00ff00'x
      ;
      ;    DOWNSAMPLE_POLYLINE, xys_fvec_2nd,   $
      ;      dtol, $
      ;      oxys=xys_fvec_2nd_downsampled,  $
      ;      is_plot=0
      ;    ;
      ;    CIRCLE_FIT, xys_fvec_2nd_downsampled, $
      ;      yfit=yfit,  $
      ;      strikes=strikes,  $
      ;      xys_fit=xys_fvec_2nd_downsampled_circle
      
      ;stop
      
      ;calculate strikes for 2nd fault
      FA_STRIKE_CAL,  '', $   ;fault file (if given); otherwise fault coordinate (xys) should be given
        ;xys=xys_fvec_2nd,  $   ;fault coordinates (lon, lat) array (2*N)
        xys=xys_fvec_2nd,  $   ;fault coordinates (lon, lat) array (2*N)
        strikes=strikes_fa_2nd,  $ ;output strikes
        auto_strike=auto_strike2 ;strike mode
      p_strikes_fa_2nds[f2i]=PTR_NEW(strikes_fa_2nd)
      
      pfile_2nd=opath+PATH_SEP()+'profiles_auto_2nd.psxy'
      ; PROFILE_LINES_AUTO, xys_fvec_2nd,  oxys=oxys_2nd, spacing=spacing_profile,  $
      PROFILE_LINES_AUTO, xys_fvec_2nd,  oxys=oxys_2nd, spacing=spacing_profile,  $
        strikes_pr=strikes_pr_2nd, auto_strike=auto_strike_2nd,   $
        xys_fp_fault_profile_intersection=xys_fp_2nd,  $ ;intersection points of profile and fault lines
        length_profile=length_profile, ofile=pfile_2nd
      p_oxys_2nds[f2i]=PTR_NEW(oxys_2nd)
      p_strikes_pr_2nds[f2i]=PTR_NEW(strikes_pr_2nd)
      p_xys_fp_2nds[f2i]=PTR_NEW(xys_fp_2nd)
      
      n_profiles_2nds[f2i]=N_ELEMENTS(xys_fp_2nd[0,*])
      
    ENDFOR
  ;STOP
  ENDIF
  
  ;stop
  
  ;read profiles
  ;
  IF N_ELEMENTS(pfile) GT 0 && pfile NE '' THEN BEGIN
    READ_PSXY,   $
      pfile,   $ ;input file
      region=regions,   $ ;x,y coorinates of each polygons (pointer type)
      nps=nps, $  ;number of point pairs for each polygon
      count=count,  $ ;number of polygons
      igpsmode=igpsmode,  $ ;whether skip comments lines (i.e., non-blank first column lines)
      names=names   ;region names (if exist)
      
    np=count
    pxys=DBLARR(2,2,count)
    FOR i=0, count-1 DO BEGIN
      pxys[*,*,i]=*(regions[i])
    ENDFOR
    
    
  ;STOP
  ENDIF ELSE BEGIN
  
    ofile=opath+PATH_SEP()+'profiles__map.jpg'
    pfile=opath+PATH_SEP()+'profiles_auto.psxy'
    ;STOP
    ;generate profiles lines by calling PROFILE_LINES_AUTO program
    PROFILE_LINES_AUTO, xys_fvec,  oxys=oxys, spacing=spacing_profile,  $
      strikes_pr=strikes_pr, auto_strike=auto_strike,   $
      xys_fp_fault_profile_intersection=xys_fp, $
      length_profile=length_profile, ofile=pfile
    ;PROFILE_LINES_AUTO, xys_fvec,  oxys=oxys, spacing=2, auto_strike=auto_strike, length_profile=400
    ;PROFILE_LINES_AUTO, xys_fvec,  oxys=oxys, spacing=2, auto_strike=auto_strike, length_profile=40
      
    pxys=oxys
  ;STOP
  ENDELSE
  
  
  np=N_ELEMENTS(pxys[0,0,*])
  
  
  
  xmin_ov=MIN([REFORM(lls[0,*]), REFORM(xys_fvec[0,0,*]), REFORM(xys_fvec[0,1,*]),  REFORM(pxys[0,1,*]),  REFORM(pxys[0,1,*]) ],max=xmax_ov)
  ymin_ov=MIN([REFORM(lls[1,*]), REFORM(xys_fvec[1,0,*]), REFORM(xys_fvec[1,1,*]),  REFORM(pxys[1,1,*]),  REFORM(pxys[1,1,*]) ],max=ymax_ov)
  ;stop
  ;loop for each profile
  PRINT,'['+prog+']INFO:loop for each profile ...'
  
    FOR pi=0,np-1 DO BEGIN  ;loop for each profile
;  FOR pi=70,70 DO BEGIN  ;test
  
    WINDOW,1,xsize=1500,ysize=900,title='Profile '+STRING(pi+1,format='(i03)'),/pixmap
    DEVICE,decomposed=1
    
    !p.MULTI=[0,3,2,0,0]
    !p.CHARSIZE=1.5
    ;!p.MULTI=-1
    PLOT,lls[0,*],lls[1,*],psym=1,background='ffffff'x,color='0'x, $
      title='Overview Map', $
      xrange=[xmin_ov,xmax_ov], $
      yrange=[ymin_ov,ymax_ov], $
      /ynozero,/nodata;,/iso
    OPLOT,lls[0,*],lls[1,*],psym=1,color='aaaaaa'x
    IF N_ELEMENTS(ffile) GT 0 && ffile NE '' THEN BEGIN
      OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='0000ff'x
    ;stop
    ENDIF
    
    xys=REFORM(pxys[*,*,pi])
    xy1=xys[*,0]
    xy2=xys[*,1]
    
    a1=xy1  ;the starting and ending vertices of the profile
    b1=xy2
    OPLOT,[a1[0],b1[0]], [a1[1],b1[1]], color='ff0000'x
    
    c1=INTERSECT_BETWEEN_POLYLINE_AND_LINE( xys_vel_hull, a1,b1)
    IF FINITE(c1[0]) NE 1 THEN BEGIN
      PRINT,'['+prog+']WARNING: no data for profile '+STRTRIM(pi+1,2)+'!'
      CONTINUE
    ENDIF
    
    
    ;get the intersect point of fault lines (xys_fvec) and current profile (a1,b1)
    IF N_ELEMENTS(ffile) NE 0 && ffile NE '' THEN BEGIN
      ;stop
      xy3=SEGMENT_INTERSECT_POLYLINE(xys_fvec,a1,b1)
    ;if no intersection between profile and fault line, it returns null result.
    ;stop
    ENDIF ELSE BEGIN
      ;find the central point of current fault line
      xy3=(xy1+xy2)*.5d0
      IF N_ELEMENTS(flon) NE 0 THEN xy3[0]=flon
      IF N_ELEMENTS(flat) NE 0 THEN xy3[1]=flat
    ENDELSE
    
    ;calculate the angle between the fault strike and velocity vector
    ;         y
    ;         ^
    ;         |   /profile
    ;         |  /
    ;         | /  alpha
    ;--------------------->x
    ;         |  ~
    ;         |     ~
    ;         |        ~fault strike
    ;         |
    ;
    ;
    ;get the azimuth, positive: from east, ccw(counter-clockwise)
    alpha=ATAN(b1[1]-a1[1], b1[0]-a1[0])
    ;alpha=strikes_pr_2nd[0] ; for converting to 2nd fault parallel direction
    ;    IF alpha LT 0 THEN BEGIN
    ;      alpha=alpha+!dpi
    ;    ENDIF
    lbl_str='Creating profile '+STRTRIM(pi+1,2)+'/'+STRTRIM(np,2)+' angle: '+STRTRIM(alpha*180d0/!dpi,2)
    PRINT, lbl_str
    ;HELP,lbl_id
    IF N_ELEMENTS(lbl_id) NE 0 THEN WIDGET_CONTROL, lbl_id, set_value=lbl_str
    
    XYOUTS,(a1[0]+b1[0])*.5d0,(a1[1]+b1[1])*.5d0, $
      STRTRIM(alpha*180d0/!dpi,2),color='0000ff'x
    ;STOP
      
    ;Derive the rotation matrix for transforming velocities
    ;for rotating velocities into fault-parallel/normal coordinate system
    ;fault strike direction is profile direction minus 90 degrees
    strike_fa=alpha-!dpi/2
    IF strike_fa LT -!dpi/2 THEN strike_fa=strike_fa+!dpi ;to make fault strike within [-pi/2, pi/2]
    ;PRINT,'fault strike:'+STRTRIM(strike_fa*180d0/!dpi,2)
    rmat=[[COS(strike_fa), -1d0*SIN(strike_fa)], $
      [SIN(strike_fa), COS(strike_fa)] ]
    ;STOP
    ;
    ;CONTINUE
      
      
    ;find adjacent stations
    dists=DBLARR(nsit)
    p_lls=DBLARR(2,nsit)
    dists_fault=DBLARR(nsit)
    IF N_ELEMENTS(f2files) NE 0 && f2files[0] NE '' THEN BEGIN
      ;init variables for storing information for 2nd faults
      dists_fault_2nd=DBLARR(n_2nd_fault,nsit)
      dists_fault_2nd[*]=!values.D_NAN
      cross_angles_2nd=DBLARR(n_2nd_fault,nsit)
      cross_angles_2nd[*]=!values.D_NAN
      velocity_Scale_Factors=DBLARR(n_2nd_fault,nsit)
      velocity_Scale_Factors[*]=!values.D_NAN
      
    ENDIF
    FOR si=0ull, nsit-1 DO BEGIN
      c1=lls[*,si]
      ;      if sites[si] eq '2hom' then stop
      
      POINT_PERP_LINE,  a1,b1, c1, d1
      x0=d1[0]
      y0=d1[1]
      IF x0 EQ c1[0] AND y0 EQ c1[1] THEN BEGIN
        ;no intersect found?
        STOP
        CONTINUE
      ENDIF
      d1=[x0,y0]
      ;LINT, a1, b1, c1, d1, i1, i2
      LINE_INTERSECT_LINE, a1, b1, c1, (d1[1]-c1[1])/(d1[0]-c1[0]),i1
      
      ;      IF N_ELEMENTS(OUT_PLOT) NE 0 && OUT_PLOT EQ 1 THEN BEGIN
      ;        WINDOW,2,xsize=800,ysize=600,title='Map',/pixmap
      ;        ;stop
      ;        DEVICE,decomposed=1
      ;        PLOT,lls[0,*],lls[1,*],psym=1,background='ffffff'x,color='0'x, $
      ;          title=sites[si], $
      ;          /ynozero,/iso
      ;        OPLOT,[a1[0],b1[0]], [a1[1],b1[1]], color='ff0000'x
      ;        OPLOT,[c1[0],d1[0]],[c1[1],d1[1]],color='0000ff'x
      ;        PLOTS,c1[0],c1[1],psym=6,color='0000ff'x,symsize=1
      ;        PLOTS,d1[0],d1[1],psym=6,color='ff00ff'x,symsize=1
      ;        PLOTS,a1[0],a1[1],psym=2,color='0000ff'x,symsize=1
      ;        PLOTS,b1[0],b1[1],psym=2,color='0000ff'x,symsize=1
      ;        PLOTS,i1[0],i1[1],psym=5,color='0000ff'x,symsize=2
      ;        ;PLOTS,i2[0],i2[1],psym=5,color='ff00ff'x,symsize=2
      ;        XYOUTS,c1[0],c1[1],sites[si],color='0'x
      ;        OPLOT,[c1[0],i1[0]],[c1[1],i1[1]],color='00ff00'x,linestyle=2,thick=2
      ;        ofile=opath+PATH_SEP()+sites[si]+'_dist.jpg'
      ;        WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
      ;      ENDIF
      
      IF i1[0] LT MIN([a1[0],b1[0]]) || i1[0] GT MAX([a1[0],b1[0]]) THEN BEGIN  ;outside profile
        ;stop
        CONTINUE
      ENDIF
      
      tmp=MAP_2POINTS(c1[0],c1[1],i1[0],i1[1],/meters)
      dists[si]=tmp*1d-3  ;in km
      IF dists[si] GT search_radius THEN CONTINUE
      p_lls[*,si]=i1
      
      ;distance from gps site to fault line
      tmp=MAP_2POINTS(xy3[0],xy3[1],i1[0],i1[1],/meter)
      dists_fault[si]=tmp*1d-3*(i1[0]-xy3[0])/ABS(i1[0]-xy3[0])
      ;stop
      
      
      ;find distance to the 2nd fault
      IF N_ELEMENTS(f2files) NE 0 && f2files[0] NE '' THEN BEGIN
        FOR f2i=0, n_2nd_fault-1 DO BEGIN ;loop for each 2nd fault file
          ;HELP,xys_fvec_2nd,  oxys,oxys_2nd,  strikes_pr,strikes_pr_2nd, xys_fp_2nd
        
          ;for a given point c1
          ;calculate distances from c1 to profiles
          dists_c1_to_profiles=DBLARR(n_profiles_2nds[f2i])
          xys_c1_cross_profiles=DBLARR(2,n_profiles_2nds[f2i])
          FOR pj=0, n_profiles_2nds[f2i]-1 DO BEGIN  ;loop for each profile
            ;get the point passing c1 perpendicular to the profile
            a2=REFORM((*p_oxys_2nds[f2i])[*,0,pj])
            b2=REFORM((*p_oxys_2nds[f2i])[*,1,pj])
            POINT_PERP_LINE, a2, b2, c1, d2
            
            dists_c1_to_profiles[pj]=MAP_2POINTS(c1[0],c1[1],d2[0],d2[1],/meter)*1d-3
            xys_c1_cross_profiles[*,pj]=d2
          ENDFOR
          
          tmp=MIN(dists_c1_to_profiles,ind_min)
          IF tmp GT search_radius THEN BEGIN
            dists_fault_2nd[f2i,si]=!values.D_NAN
            CONTINUE
          ENDIF
          a2=REFORM((*p_oxys_2nds[f2i])[*,0,ind_min])
          b2=REFORM((*p_oxys_2nds[f2i])[*,1,ind_min])
          d2=REFORM(xys_c1_cross_profiles[*,ind_min])
          e2=REFORM((*p_xys_fp_2nds[f2i])[*,ind_min])
          tmp=MAP_2POINTS(d2[0],d2[1],e2[0],e2[1],/meter)*1d-3
          dists_fault_2nd[f2i,si]=tmp*(d2[0]-e2[0])/ABS(d2[0]-e2[0])
          cross_angles_2nd[f2i,si]=(*p_strikes_pr_2nds[f2i])[ind_min]-strikes_pr[pi] ;the angle between the two fault strikes
        ;cross_angles_2nd[f2i,si]=strikes_pr_2nd[ind_min] ;the strike direction
        ;stop
        ;print,'cross_angles_2nd[si]:',si,cross_angles_2nd[si],strikes_pr_2nd[ind_min],ind_min
        ;        PRINT,'si:',si
        ;        PRINT,'d2:',d2
        ;        PRINT,'e2:',e2
        ;        PRINT,'dist_2_fault_2nd:',dists_fault_2nd[si]
        ;        OPLOT,xys_fvec_2nd[0,*],xys_fvec_2nd[1,*],psym='-3',color='00ff00'x
        ;        OPLOT,[a2[0],b2[0]],[a2[1],b2[1]],psym='-5',color='00ff00'x
        ;        OPLOT,[d2[0],e2[0]],[d2[1],e2[1]],color='0'x,psym=-2,thick=2
        ;        OPLOT,[d2[0],c1[0]],[d2[1],c1[1]],color='00ffff'x,psym=-6,thick=3
        ;;        FOR j=0,n_profiles_2nd-1 DO BEGIN
        ;;          OPLOT,oxys_2nd[0,*,j],oxys_2nd[1,*,j],psym='-5',color='00ff00'x
        ;;        ENDFOR
        ;xyouts,e2[0],e2[1],'e2',color='0'x
        ;xyouts,d2[0],d2[1],'d2',color='0'x
        ;xyouts,a2[0],a2[1],'a2',color='0'x
        ;xyouts,b2[0],b2[1],'b2',color='0'x
        ;xyouts,c1[0],c1[1],'c1',color='0'x
        ;print,'a2 ',a2
        ;print,'b2',b2
        ;print,'c1',c1
        ;print,'d2',d2
        ;STOP
          
        ;        IF auto_strike EQ 2 OR auto_strike EQ 3 THEN BEGIN
        ;          slope_profile_2nd=strikes_fa_2nd+!dpi/2
        ;          strike_fa_2nd=strikes_fa_2nd
        ;
        ;          LINE_INTERSECT_POLYLINE, xys_fvec_2nd,  $
        ;            c1,  $
        ;            xy_intersect=xy_intersect, $
        ;            auto_strike=auto_strike,  $
        ;            strike=strike_fa_2nd,  $
        ;            in_or_out=in_or_out,  $
        ;            segment_index=segment_index, $
        ;            dist_point2polyline=dist_point2polyline
        ;          IF segment_index NE -1 THEN BEGIN
        ;            dists_fault_2nd[si]=dist_point2polyline
        ;          ;slope_profile_2nd=ATAN(xy_intersect[1]-c1[1],xy_intersect[0]-c1[0])
        ;          ;strike_fa_2nd=slope_profile_2nd+!dpi/2
        ;          ENDIF
        ;        ;stop
        ;        ENDIF
        ;        IF auto_strike EQ 1 THEN BEGIN
        ;          strike_fa_2nd=!values.D_NAN
        ;
        ;          LINE_INTERSECT_POLYLINE, xys_fvec_2nd,  $
        ;            c1,  $
        ;            xy_intersect=xy_intersect, $
        ;            auto_strike=auto_strike,  $
        ;            in_or_out=in_or_out,  $
        ;            segment_index=segment_index, $
        ;            dist_point2polyline=dist_point2polyline
        ;          IF segment_index NE -1 THEN BEGIN
        ;            dists_fault_2nd[si]=dist_point2polyline
        ;            slope_profile_2nd=ATAN(xy_intersect[1]-c1[1],xy_intersect[0]-c1[0])
        ;            strike_fa_2nd=slope_profile_2nd+!dpi/2
        ;          ENDIF
        ;        ;STOP
        ;        ENDIF
        ;
        ;        ;
        ;        IF FINITE(strike_fa_2nd) EQ 1 THEN BEGIN
        ;          tmp=slope_profile_2nd-strikes_pr[pi]
        ;          IF tmp GT !dpi/2 THEN tmp=!dpi-tmp
        ;          cross_angles_2nd[si]=tmp
        ;        ENDIF
        ;STOP
        ENDFOR ;end-of-loop for 2nd faults
      ENDIF  ;end-of-distance-to-2nd-fault
      
    ENDFOR
    
    pos=WHERE(dists GT 0 AND dists LE search_radius)
    ;help,pos
    ;stop
    IF pos[0] EQ -1 THEN BEGIN  ;no velocity
      PRINT,'no data'
      CONTINUE
    ENDIF
    
    WSET,1
    ;WINDOW,1,xsize=800,ysize=800,title='Map';,/pixmap
    ;DEVICE,decomposed=1
    ;    lonmin=MIN([ a1[0],b1[0],REFORM(xys_fvec[0,*]) ],max=lonmax)
    ;    latmin=MIN([ a1[1],b1[1],REFORM(xys_fvec[1,*]) ],max=latmax)
    lonmin=MIN([ a1[0],b1[0] ],max=lonmax)
    latmin=MIN([ a1[1],b1[1] ],max=latmax)
    PLOT,lls[0,*],lls[1,*],psym=1,background='ffffff'x,color='0'x, $
      title='Site Map for Profile '+STRING(pi+1,format='(i2)'), $
      xrange=[lonmin,lonmax], $
      yrange=[latmin,latmax], $
      /ynozero,/iso,/nodata
    OPLOT,lls[0,*],lls[1,*],psym=1,color='aaaaaa'x
    OPLOT,[a1[0],b1[0]], [a1[1],b1[1]], color='ff0000'x
    PLOTS,lls[0,pos],lls[1,pos],psym=1,color='ff0000'x
    PLOTS,[xy3[0]],[xy3[1]],psym=2,color='0000ff'x
    IF N_ELEMENTS(ffile) GT 0 && ffile NE '' THEN BEGIN
      OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='0000ff'x
    ENDIF
    IF N_ELEMENTS(f2file) GT 0 && f2file NE '' THEN BEGIN
      FOR f2i=0, n_2nd_fault-1 DO BEGIN
        ;OPLOT,xys_fvec_2nd[0,*],xys_fvec_2nd[1,*],psym='-3',color='00ff00'x
        OPLOT,xys_fvec_2nd[0,*],xys_fvec_2nd[1,*],psym='-3',color='00ff00'x
        FOR j=0,n_profiles_2nds[f2i]-1 DO BEGIN
          OPLOT,oxys_2nd[0,*,j],oxys_2nd[1,*,j],psym='-5',color='00ff00'x
        ENDFOR
      ENDFOR
    ENDIF
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i03)')+'_map.jpg'
    ;WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
    
    vel_along_all=DBLARR(N_ELEMENTS(pos))
    vel_tang_all=DBLARR(N_ELEMENTS(pos))
    vele_along_all=DBLARR(N_ELEMENTS(pos))
    vele_tang_all=DBLARR(N_ELEMENTS(pos))
    vel_up_all=DBLARR(N_ELEMENTS(pos))
    vele_up_all=DBLARR(N_ELEMENTS(pos))
    vel_los_all=DBLARR(N_ELEMENTS(pos))
    vele_los_all=DBLARR(N_ELEMENTS(pos))
    
    vel_along_all[*]=-999.99d0
    vel_tang_all[*]=-999.99d0
    vel_up_all[*]=-999.99d0
    vel_los_all[*]=-999.99d0
    
    FOR posi=0, N_ELEMENTS(pos)-1 DO BEGIN
      ;      IF sites[pos[posi]] EQ '2hom' THEN STOP ;test
      ;vel:
      ;   0     1  2   3   4   5   6
      ;  Long  Lat Vn  Sn  Ve  Se  Cne
      ;      vel=REFORM(vels[*,pos[posi]])
      ;      vel_amp=SQRT(vel[4]^2+vel[2]^2)
      ;      vel_azi=ATAN(vel[2],vel[4])
      ;
      ;      ;data array (data)
      ;   0  1   2   3  4   5  6   7  8    9  10  11   12
      ;[ lon lat Ve dVe Vn dVn Vu dVu Cen Ceu Cnu Los dLos ]
    
    
      vel=REFORM(data[*,pos[posi]])
      ;STOP
      ;if has los velocity
      IF FINITE(vel[11]) EQ 1 THEN BEGIN
        vel_los_all[posi]=vel[11]
        vele_los_all[posi]=vel[12]
      ENDIF
      
      
      ;if has vertical velocity
      IF FINITE(vel[6]) EQ 1 THEN BEGIN
        vel_up_all[posi]=vel[6]
        vele_up_all[posi]=vel[7]
      ENDIF
      
      ;if has horizontal velocity
      IF FINITE(vel[2]) EQ 1 && FINITE(vel[4]) EQ 1 THEN BEGIN
        vel_amp=SQRT(vel[2]^2+vel[4]^2)
        vel_azi=ATAN(vel[4],vel[2])
        ;PRINT,'angle of velocity vector is:',vel_azi*180/!dpi
        ;PRINT,vel_azi*180/!dpi
        
        
        ;calculate the angle (phi) between the profile and velocity vector
        ;            /profile
        ;           /
        ;          /  alpha
        ;---------------------
        ;         |\ ~
        ;         | \    ~
        ;         | _\|     ~fault strike
        ;         |  GPS
        ;
        ;      phi=alpha-vel_azi
        ;      ;PRINT,'angle between the profile and velocity vector is:',phi*180/!dpi
        ;      phi=vel_azi-strike_fa
        ;      PRINT,'angle between fault and velocity vector is:',phi*180/!dpi
        
        vel_azi_rot=vel_azi-strike_fa
        ;PRINT,'angle between fault and velocity vector is:',vel_azi_rot*180/!dpi
        
        IF N_ELEMENTS(f2files) NE 0 && f2files[0] NE '' THEN BEGIN ;for the 2nd fault
          ;stop
          FOR f2i=0, n_2nd_fault-1 DO BEGIN
            alpha_2nd=cross_angles_2nd[f2i,pos[posi]] ;profile direction
            strike_fa_2nd=alpha_2nd-!dpi/2
            ;phi_2nd=vel_azi-strike_fa_2nd
            vel_azi_rot_2nd=vel_azi-strike_fa_2nd
            ;PRINT,'angle between the 2nd fault and velocity vector is:',vel_azi_rot_2nd*180/!dpi
            velocity_Scale_Factors[f2i,pos[posi]]=ABS(COS(vel_azi_rot)/COS(vel_azi_rot_2nd))
          ENDFOR
        ;stop
        ENDIF
        
        
        ;      vel_normal=vel_amp*COS(phi)
        ;      vel_parallel=vel_amp*SIN(phi)
        ;      vel_along_all[posi]=vel_normal
        ;      vel_tang_all[posi]=vel_parallel
        ;      ;print,vel_ss,vel_stc ; signs need further operation
        
        ;another way, the matrix transformation
        ;        vel_en=[vel[4],vel[2]]  ;velocities in the east and north directions (old indices)
        ;        vele_en=[[vel[5]^2,vel[6]*vel[5]*vel[3]], $   ;(old)
        ;          [vel[6]*vel[5]*vel[3], vel[3]^2] ]
        ;
        ;      ;vel (old):
        ;      ;   0     1  2   3   4   5   6
        ;      ;  Long  Lat Vn  Sn  Ve  Se  Cne
        ;      ;new:
        ;   0  1   2   3  4   5  6   7  8    9  10  11   12
        ;[ lon lat Ve dVe Vn dVn Vu dVu Cen Ceu Cnu Los dLos ]
        vel_en=[vel[2],vel[4]]
        vele_en=[[vel[3]^2,vel[8]*vel[3]*vel[5]], $ ;new
          [vel[8]*vel[3]*vel[5], vel[5]^2] ]
          
        vel_en_rot=vel_en#rmat  ;velocities in the along-profile and tangent-profile (counter-clockwise 90 degrees from the along-profile) directions
        ;print,vel_at[*]
        ;PRINT,'e , n velocities:', vel_en
        ;PRINT,'e , n velocities in new coordiante system:',vel_en_rot
        vel_amp_rot=SQRT(vel_en_rot[0]^2+vel_en_rot[1]^2)
        
        vel_normal2=vel_amp_rot*SIN(vel_azi_rot)
        vel_parallel2=vel_amp_rot*COS(vel_azi_rot)
        
        ;PRINT,'velocity of fault-normal:',vel_normal,'  fault-parallel:',vel_parallel
        ;PRINT,'velocity of fault-normal:',vel_normal2,'  fault-parallel:',vel_parallel2, ' (2nd Method)
        
        vel_along_all[posi]=vel_normal2
        vel_tang_all[posi]=vel_parallel2
        ;STOP
        
        ;
        ;transform uncertainty
        ;print,vel[[5,3]]
        vele_at=rmat#vele_en#TRANSPOSE(rmat)
        vele_along_all[posi]=SQRT(vele_at[0,0])
        vele_tang_all[posi]=SQRT(vele_at[1,1])
      ENDIF
      
    ;stop
      
    ;      IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
    ;        WINDOW,3,/pixmap
    ;        DEVICE,decomposed=1
    ;        PLOT,vels[0,*],vels[1,*],psym=1,background='ffffff'x,color='0'x, $
    ;          /nodata, xrange=[-20,20],yrange=[-20,20], $
    ;          title=sites[pos[posi]],$
    ;          /ynozero,/iso
    ;        OPLOT,[0,vel[4]],[0,0],color='0'x
    ;        OPLOT,[0,0],[0,vel[3]],color='0'x
    ;        OPLOT,[0,vel[4]],[0,vel[3]],color='0'x,thick=2
    ;        x=INDGEN(100)*40d0-20
    ;        y=x*TAN(alpha)
    ;        OPLOT,x,y,color='0'x,linestyle=2
    ;        OPLOT,[0,vel_ss],[0,vel_st],color='ff0000'x,thick=2,linestyle=3
    ;        ofile=opath+PATH_SEP()+sites[pos[posi]]+'_vel_components.jpg'
    ;        WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
    ;      ENDIF
    ;STOP
    ENDFOR
    ;STOP
    lls_used=p_lls[*,pos]
    ind=SORT(lls_used[0,*])
    
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i03)')+'_vel.psxy'
    
    odata=DBLARR(21,N_ELEMENTS(ind))
    odata[[3,5,7,14,15],*]=-999.99d0
    FOR j=0, N_ELEMENTS(ind)-1 DO BEGIN
      odata[*,j]=[p_lls[*,pos[ind[j]]],dists[pos[ind[j]]],  $
        vel_along_all[ind[j]],vele_along_all[ind[j]],vel_tang_all[ind[j]],vele_tang_all[ind[j]], $
        vel_up_all[ind[j]],vele_up_all[ind[j]], lls[*,pos[ind[j]]], $
        dists_fault[pos[ind[j]]], $
        vel_los_all[ind[j]],vele_los_all[ind[j]],  $
        data[[2,4,3,5,8,9,10],pos[ind[j]]] $
        ]
    ;stop
    ENDFOR
    ;stop
    IF N_ELEMENTS(f2files) NE 0 && f2files[0] NE '' THEN BEGIN ;with 2nd fault(s)
      odata_2nd=DBLARR(2*n_2nd_fault,N_ELEMENTS(ind))
      odata_2nd[*]=!values.D_NAN
      
      FOR f2i=0, n_2nd_fault-1 DO BEGIN ;loop for each 2nd fault file
        odata_2nd[f2i*2,*]=dists_fault_2nd[f2i,pos[ind]]
        odata_2nd[f2i*2+1,*]=cross_angles_2nd[f2i,pos[ind]]*180/!dpi
      ;      odata_2nd[1,*]=velocity_Scale_Factors[pos[ind]]
      ENDFOR
      
      WRITE_VEL_PROFILE, ofile $
        , odata $
        , sites=sites[pos[ind]] $
        , fa_xys=xys_fvec  $
        , pf_xys=[[a1],[b1]]  $
        , fa_pf_xy=xy3 $
        , odata_2nd=odata_2nd $
        , src=[vfile,ffile,pfile,f2files] $
        , headers=STRING(n_2nd_fault,format='("* Number of second faults:",1x,i5)')
        
    ENDIF ELSE BEGIN ;no 2nd fault
    
      WRITE_VEL_PROFILE, ofile $
        , odata $
        , sites=sites[pos[ind]] $
        , fa_xys=xys_fvec  $
        , pf_xys=[[a1],[b1]]  $
        , fa_pf_xy=xy3 $
        , odata_2nd=odata_2nd $
        , src=[vfile,ffile,pfile]
    ENDELSE
    ;STOP
    WSET,1
    ;!p.MULTI=[1,2,2]
    yrange=[-20,20]
    yrange=[-12,12]
    ind_nan=FINITE(vel_along_all[ind])
    IF TOTAL(ind_nan) EQ 0 THEN BEGIN
      CONTINUE
    ENDIF
    ;PLOT,REFORM(lls_used[0,ind]),vel_along_all[ind],background='ffffff'x,color='0'x, $
    PLOT,REFORM(dists_fault[pos[ind]]),vel_along_all[ind],background='ffffff'x,color='0'x, $
      title='Velocities Along (fault-normal) Profile '+STRING(pi+1,format='(i2)'), $
      ;yrange=[-50,50] ,  $
      /ynozero,psym=2,/nodata;,yrange=yrange
    OPLOT,REFORM(dists_fault[pos[ind]]),vel_along_all[ind],psym=2,color='ff0000'x
    ;OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    FOR j=0,N_ELEMENTS(ind)-1 DO BEGIN
      OPLOT,[dists_fault[pos[ind[j]]], dists_fault[pos[ind[j]]] ], $
        [vel_along_all[ind[j]]+ABS(vele_along_all[ind[j]]),vel_along_all[ind[j]]-ABS(vele_along_all[ind[j]]) ], $
        color='0000ff'x,thick=2
    ENDFOR
    
    ;1 Plus sign (+)
    ;2 Asterisk (*)
    ;3 Period (.)
    ;4 Diamond
    ;5 Triangle
    ;6 Square
    ;7 X
    
    
    ;PLOT,lls_used[0,ind],vel_tang_all[ind],background='ffffff'x,color='0'x, $
    PLOT,dists_fault[pos[ind]],vel_tang_all[ind],background='ffffff'x,color='0'x, $
      title='Velocities Tangent (fault-parallel) to Profile '+STRING(pi+1,format='(i2)'), $
      ;yrange=[-50,50] ,  $
      /ynozero,psym=4,/nodata;,yrange=yrange
    OPLOT,dists_fault[pos[ind]],vel_tang_all[ind],psym=4,color='ff0000'x
    ;    OPLOT,lls_used[0,ind],vel_tang_all[ind],color='0000ff'x, $
    ;      psym=5
    ;OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    FOR j=0,N_ELEMENTS(ind)-1 DO BEGIN
      OPLOT,[ dists_fault[pos[ind[j]]], dists_fault[pos[ind[j]]] ], $
        [vel_tang_all[ind[j]]+ABS(vele_tang_all[ind[j]]),vel_tang_all[ind[j]]-ABS(vele_tang_all[ind[j]]) ], $
        color='0000ff'x,thick=2
    ENDFOR
    
    PLOT,dists_fault[pos[ind]],vel_up_all[ind],background='ffffff'x,color='0'x, $
      title='Vertical (up) Velocities along Profile '+STRING(pi+1,format='(i2)'), $
      ;yrange=[-50,50] ,  $
      /ynozero,psym=5,/nodata;,yrange=yrange
    OPLOT,dists_fault[pos[ind]],vel_up_all[ind],psym=5,color='ff0000'x
    OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    FOR j=0,N_ELEMENTS(ind)-1 DO BEGIN
      OPLOT,[ dists_fault[pos[ind[j]]], dists_fault[pos[ind[j]]] ], $
        [vel_up_all[ind[j]]+ABS(vele_up_all[ind[j]]),vel_up_all[ind[j]]-ABS(vele_up_all[ind[j]]) ], $
        color='0000ff'x,thick=2
    ENDFOR
    
    
    PLOT,dists_fault[pos[ind]],vel_los_all[ind],background='ffffff'x,color='0'x, $
      title='InSAR LOS Velocities along Profile '+STRING(pi+1,format='(i2)'), $
      ;yrange=[-50,50] ,  $
      /ynozero,psym=6,/nodata;,yrange=yrange
    OPLOT,dists_fault[pos[ind]],vel_los_all[ind],psym=6,color='ff0000'x
    OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    FOR j=0,N_ELEMENTS(ind)-1 DO BEGIN
      OPLOT,[ dists_fault[pos[ind[j]]], dists_fault[pos[ind[j]]] ], $
        [vel_los_all[ind[j]]+ABS(vele_los_all[ind[j]]),vel_los_all[ind[j]]-ABS(vele_los_all[ind[j]]) ], $
        color='0000ff'x,thick=2
    ENDFOR
    
    
    !p.MULTI=-1
    
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i03)')+'_vel.jpg'
    WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
    
  ;PRINT,'a1:',a1
  ;PRINT,'b1:',b1
  ;      BREAK
  ENDFOR
  
  lbl_str='Ready'
  IF N_ELEMENTS(lbl_id) NE 0 THEN WIDGET_CONTROL, lbl_id, set_value=lbl_str
  
  PRINT,'['+prog+']Normal end.'
;stop
END