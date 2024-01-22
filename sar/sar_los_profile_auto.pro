PRO SAR_LOS_PROFILE_AUTO, vfile, $  ;velocity file (in varied formats)
    ;SAR_PROFILE_AUTO, vfile, $
    ffile=ffile,  $ ;(if exist) fault trace (only one polyline in GMT format)
    opath, $   ;output pathout_plot=out_plot,  $ ;whether output temporary plots
    inputfmt=inputfmt,  $ ;input velocity format (0-  ; 1-psvelo)f
    ;0 (default):1x site lon lat vn sign ve sige cne
    ;1 (psvelo): 1x lon lat ve vn sige sign cne site
    ;2
    ;3 XYZ format with non-blank-first-column lines as comments
    ;4 XYZ format
    ;
    length_profile=length_profile,  $
    spacing_profile=spacing_profile,  $
    auto_strike=auto_strike,  $
    pfile=pfile,  $
    maxdist=maxdist  ; the maximum distance away from the line to create the velocity profile
    
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  
  
  ;//begin-of-test-parameters
  IF N_ELEMENTS(vfile) EQ 0 || N_ELEMENTS(ffile) EQ 0  THEN BEGIN
    ;
    ;inputfmt=3  ; xyz 
    ;inputfmt=4  ; xyze
    
    vfile=FILEPATH('meanvel_01.txt',subdirectory=['example','insar','xyz_profiles','xyz'],$
      root=!igps_root)
    ffile=FILEPATH('fault_ydgl2.psxy',subdirectory=['example','insar','xyz_profiles','fault_trace'],$
      root=!igps_root)
    opath=FILEPATH('p',subdirectory=['example','insar','xyz_profiles'],$
      root=!igps_root)
      
    ;    vfile='D:\tmp\file.xyz'
    ;    ffile='D:\data\vector\profile\fault_jiali.psxy'
    ;    opath='d:\tmp\p'
    ;
    ;    vfile='D:\tmp\gyaringco\fault_bc4.txt'
    ;    ffile='D:\tmp\gyaringco\fault_bc.psxy'
    ;    opath='D:\tmp\gyaringco\p'
    ;auto_strike=2
      
    ;GOTO, end_of_default_input_parameters
      
    ;    vfile='D:\gsar\interseismic\085-a-m3-0088_0093_0098-gaize_yzs5\f123.1\sbas.3.0.0400.9999.20150515.20190927.103.0320.01.___\vel_mask_ll3.xyz'
    ;    ffile='C:\GMT_pub\vector\profile\fa_dawaco_maiqiongco.psxy'
    ;    opath='D:\gsar\interseismic\085-a-m3-0088_0093_0098-gaize_yzs5\f123.1\sbas.3.0.0400.9999.20150515.20190927.103.0320.01.___\p.fa_dawaco_maiqiongco'
    ;    auto_strike=3
    ;
    ;
    ;    vfile='D:\gsar\interseismic\026-a-m4-0087_0092_0097_0102-kangding1M3\f123\sbas.4.0.0367.9999.20170124.20210527.130.0866.01.___\vel_mask_ll3.xyze'
    ;    ffile='C:\GMT_pub\vector\profile\fa_xsh_b.psxy'
    ;    opath='D:\gsar\interseismic\026-a-m4-0087_0092_0097_0102-kangding1M3\f123\sbas.4.0.0367.9999.20150123.20210527.157.1388.01.___\p.fa_xsh_b\1'
    ;    auto_strike=2
      
    ;    vfile='D:\gsar\interseismic\165-d-m6-0467_0472_0477_0482_0487_0492-woniuhu4M3\f123\sbas.4.0.0001.9999.20141029.20230208.062.0144.01.___\vel_mask_ll3.xyze'
    ;    ffile='C:\GMT_pub\vector\profile\fa_karakoram.psxy'
    ;    opath='D:\gsar\interseismic\165-d-m6-0467_0472_0477_0482_0487_0492-woniuhu4M3\f123\sbas.4.0.0001.9999.20141029.20230208.062.0144.01.___\p.fa_karakoram'
      
     vfile='D:\gsar\interseismic\033-d-m2-0463_0468-menyuan\f123\sbas.4.0.0001.9999.20220110.20231219.058.0795.01.___\vel_mask_ll3.xyz'
     ffile='C:\GMT_pub\vector\profile\fa_haiyuan.psxy'
     opath='D:\gsar\interseismic\033-d-m2-0463_0468-menyuan\f123\sbas.4.0.0001.9999.20220110.20231219.058.0795.01.___\p.fa_haiyuan1'
     
    out_plot=1
    is_debug=0
    length_profile=660
    ;;
    
    
    
    
    ;    ;mean LOS rates derived by TYF
    ;    orbtyp='a'  ;ascending
    ;    ;
    ;    orbtyp='d' ;descending
    ;    ;
    ;    ;stop
    ;
    
    IF N_ELEMENTS(ffile) EQ 0 THEN BEGIN
      PROFILE_NAME2VECTORFILE,   $
        fa,   $ ;input, fault name
        ffile=ffile   ;output, fault file
    ENDIF
    
    end_of_default_input_parameters:
    
  ENDIF ;\\end-of-test-parameters
  
  IF N_ELEMENTS(is_debug) EQ 0 THEN is_debug=0
  IF N_ELEMENTS(opath) EQ 0 THEN opath=getpathname(vfile)+PATH_SEP()+'p'
  IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN PRINT,'output to :',opath
  IF FILE_TEST(opath,/directory) NE 1 THEN FILE_MKDIR,opath ;create the output path if not exsit
  
  ;defaults for input parameters
  IF N_ELEMENTS(out_plot) EQ 0 THEN out_plot=1
  IF N_ELEMENTS(inputfmt) EQ 0 THEN inputfmt=3
  IF N_ELEMENTS(auto_strike) EQ 0 THEN auto_strike=2
  IF N_ELEMENTS(spacing_profile) EQ 0 THEN spacing_profile=10  ;in km (distance between two neighboring profiles)
  IF N_ELEMENTS(length_profile) EQ 0 THEN length_profile=200  ;in km (the total length of the profile generated automatically by iGPS)
  IF N_ELEMENTS(maxdist) EQ 0 THEN BEGIN
    MaxDist=55d0 ;maximum searching distance beside the profile line, in kilometers. for gps, this value may be quite large
    MaxDist=5d0  ;for InSAR LOS grids, set it to a small one.
    maxdist=spacing_profile/2d0  ;half the distance between two neighboring profiles
  ;maxdist=1d0
  ENDIF
  
  VEL_PROFILE_CREATE, vfile, $  ;velocity file (in varied formats)
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
    
    
  PRINT,'['+prog+']Normal end.'
  
  
  
  
END