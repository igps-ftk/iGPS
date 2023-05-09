PRO RUN_TEST_XYZ_PROFILE

  vfile=FILEPATH('meanvel_01.txt',root_dir=!igps_root,SUBDIRECTORY=['example','sar','xyz_profiles','xyz'])
  ffile=FILEPATH('fault_ydgl2.psxy',root_dir=!igps_root,SUBDIRECTORY=['example','sar','xyz_profiles','fault_trace'])
  opath=FILEPATH('p',root_dir=!igps_root,SUBDIRECTORY=['example','sar','xyz_profiles'])
  HELP,vfile,ffile,opath
  
  inputfmt=4
  
  ;stop
  SAR_LOS_PROFILES_AUTO_LLV, vfile, $  ;velocity file (in varied formats)
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
    maxdist=maxdist
END