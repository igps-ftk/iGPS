;;+
;; :DESCRIPTION:
;;    Sub-routine for PROFILE_LINES_AUTO.
;;
;; :PARAMS:
;;    xy1
;;    xy2
;;    xy3
;;
;; :KEYWORDS:
;;    oxy
;;    strike
;;    km_per_deg
;;    isplot
;;    length_profile
;;
;; :AUTHOR: tianyf
;;-
;PRO PROFILE_LINE_AUTO_GET_PROFILE, xy1, xy2, xy3, oxy=oxy, $
;    strike=strike,  $
;    km_per_deg=km_per_deg,  $
;    isplot=isplot,  $
;    length_profile=length_profile ;input; optional; in km
;
;
;  rate_pr=-1d0/TAN(strike)
;  ;rate_pr=TAN(strike)
;  intercept_pr=xy3[1]-rate_pr*xy3[0]
;  x4=-1d3
;  x4=xy3[0]-(length_profile/2d0/km_per_deg)*COS(ATAN(rate_pr))
;  y4=rate_pr*x4+intercept_pr
;  x5=1d3
;  x5=xy3[0]+(length_profile/2d0/km_per_deg)*COS(ATAN(rate_pr))
;  y5=rate_pr*x5+intercept_pr
;
;  IF isplot EQ 1 THEN BEGIN
;    OPLOT,[x5,x4],[y5,y4],color='00ff00'x
;    PLOTS,[xy1[0],xy2[0]],[xy1[1],xy2[1]],color='ffff00'x,thick=2,psym=-2
;  ENDIF
;
;  oxy=[[x4,y4],[x5,y5]]
;;stop
;END

;+
; :DESCRIPTION:
;    Given an fault trace, generate profiles (perpendicular to the fault strike) with even spacing.
;
; :PARAMS:
;    xys_fa - longitudes and latitudes of nodes of fault trace; [2,npt]
;
; :KEYWORDS:
;    strike - strike angle (in radians) of the fault. optional. if not given, calculated using the fault polyline.
;    spacing - the distance between two neighbouring profiles. in km.
;    length_profile - the total length of one profile  ;in km
;    isplot - 1: create plot; 0: no plot
;    auto_strike  - if strike is not given, then 1: calculate strikes for individual segments
;                                                2: use the average strike for all segments
;    oxys - output profiles; [2,2,npf]
;
; :AUTHOR: tianyf
;-
PRO PROFILE_LINES_AUTO, xys_fa,  $ ;fault trace; input; mondatory; double [2,npt]; in decimal degrees
    strikes_fault=strikes_fa,  $ ;fault strikesl; input; optional; double; in radians
    spacing=spacing,  $ ;profile spacing; input; optional; double; in km
    length_profile=length_profile,  $ ;length of profile; input; optional; in km
    isplot=isplot,  $
    auto_strike=auto_strike,  $ ;fault strike mode (1-vary for each segment; 2-mean strike; 3-direction of the 1st and last vertices
    oxys=oxys,  $ ;profile vertices; output; in decimal degrees
    xys_fp_fault_profile_intersection=xys_fp,  $ ;intersection points of profile and fault lines
    strikes_profile=strikes_pr,  $ ;input; optional; double; in radians
    ofile=ofile,  $   ;output filename (if given, write output profile files)
    xys_fa2=xys_fa2
    
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    ;test file
    ffile=FILEPATH(root_dir=!igps_root,subdirectory=['example','vector'],'fault_ydgl.psxy')
    ;
    ffile='D:\data\vector\profile\fault_fake_sangri.psxy'
    ffile='D:\tmp\gyaringco\fault_bc.psxy'
    ffile='D:\tmp\114.psxy.org'
    ffile='C:\GMT_pub\vector\profile\fa_dawaco_maiqiongco.psxy'
    ffile='C:\GMT_pub\vector\profile\fa_minjiang.psxy'
    
    ffile='C:\GMT_pub\vector\profile\fa_ygr.psxy'
    ;ffile='\\192.168.11.68\tianyf\iGPS\ftk\sar\profile\fa_ygr1.psxy'
    ;
    ffile='\\192.168.11.68\tianyf\iGPS\ftk\sar\profile\fa_test.psxy'
    ffile='C:\GMT_pub\vector\profile\fa_QusumDetachment.psxy'
    ffile='C:\GMT_pub\vector\profile\pf_aksaichin_west.psxy'
    
    
    ffile='C:\GMT_pub\vector\profile\fa_Kunlun_Fault.psxy'
    ffile='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy'
    
    ofile=desuffix(ffile)+'_autoprofiles.psxy'
    ;read fault vector (if specified)
    
    ;stop
    lines_fvec=read_txt(ffile)
    lines_fvec2=STRTRIM(lines_fvec,2)
    pos=WHERE(strmids(lines_fvec2,0,1) NE '>')
    IF N_ELEMENTS(pos) LT 2 THEN BEGIN
      PRINT,'['+prog+']ERROR: invalid fault line vector file <'+ffile+'>!!'
      RETURN
    ENDIF
    xys_fa=DOUBLE(str_lines2arr(lines_fvec2[pos]))
    
    
    ;STOP
    
    auto_strike=3 ;use starting-ending points to calcualte strike angle
    auto_strike=2 ;use mean of strikes of all segments
  ;      auto_strike=1 ;use individual strike for each segment
  ENDIF
  
  IF N_ELEMENTS(isplot) EQ 0 THEN isplot=0
  IF N_PARAMS() LT 1 THEN isplot=1
  
  IF N_ELEMENTS(strike) EQ 0 THEN BEGIN
    PRINT, '['+PROG+']No strike given. Use auto_strike mode '+STRTRIM(auto_strike,2)+'.'
    IF N_ELEMENTS(auto_strike) EQ 0 THEN BEGIN
      auto_strike=1
    ;auto_strike=2
    ENDIF
  ENDIF ELSE BEGIN
    PRINT, '['+PROG+']Fault strike: ',strike
    auto_strike=0
    strike=!dpi/2-strike  ;convert strike to rate angle; strike is calculated clockwise from the North.
  ENDELSE
  
  
  IF N_ELEMENTS(spacing) EQ 0 THEN spacing=5 ;km
  ;convert km to degree using the mean latitude
  latmid=MEAN(xys_fa[1,*])
  km_per_deg=MAP_2POINTS(0,latmid,1,latmid,/meters)*1d-3
  
  IF N_ELEMENTS(length_profile) EQ 0 THEN length_profile=600 ;km
  ;
  lonmin=MIN(xys_fa[0,*],max=lonmax)
  latmin=MIN(xys_fa[1,*],max=latmax)
  
  lonmin=lonmin-length_profile/2/km_per_deg
  lonmax=lonmax+length_profile/2/km_per_deg
  latmin=latmin-length_profile/2/km_per_deg
  latmax=latmax+length_profile/2/km_per_deg
  
  FA_STRIKE_CAL,  '', $   ;fault file (if given); otherwise fault coordinate (xys_fa) should be given
    xys_fa=xys_fa,  $   ;fault coordinates (lon, lat) array (2*N)
    strikes=strikes_fa,  $ ;output strikes
    auto_strike=auto_strike ;strike mode
    
  IF isplot EQ 1 THEN BEGIN
    ;plot fault line
    oldwin=!d.WINDOW
    WINDOW,/free,xsize=900,ysize=600*(latmax-latmin)/(lonmax-lonmin)
    !p.MULTI=-1
    PLOT,xys_fa[0,*],xys_fa[1,*],background='ffffff'x,color='0'x,/nodata,/ynozero,  $
      xrange=[lonmin,lonmax],yrange=[latmin,latmax],  $
      xtitle='Longitude',ytitle='Latitude', $
      title='Fault and Profiles',/iso  ;,xstyle=1,ystyle=1
    OPLOT,xys_fa[0,*],xys_fa[1,*],color='0'x,psym=-4
  ENDIF
  
  len_acc_i=0d0
  npf=0
  xy3=xys_fa[*,0]
  spacing_i=spacing
  xys_fp=[-9999d0,-9999d0]
  oxys=REPLICATE(-9999d0,2,2)
  strikes_pr=[-999.99d0]
  
  FOR i=0,N_ELEMENTS(xys_fa[0,*])-2 DO BEGIN
    xy1=xys_fa[*,i]
    xy2=xys_fa[*,i+1]
    
    IF auto_strike EQ 1 THEN BEGIN
      strike=strikes_fa[i]
    ENDIF ELSE BEGIN
      strike=strikes_fa
    ENDELSE
    IF isplot EQ 1 THEN PRINT, '['+PROG+']Fault strike for segment '+STRTRIM(i+1,2)+':',  $
      strike*180/!dpi, ' degrees'
      
      
      
    ;PLOTS,[xy1[0],xy2[0]],[xy1[1],xy2[1]],color='ffff00'x,thick=2,psym=-2
    tmp=(xy1[1]-xy2[1])/(xy1[0]-xy2[0])
    len_seg_i=MAP_2POINTS(xy1[0],xy1[1],xy2[0],xy2[1],/meters)*1d-3
    IF isplot EQ 1 THEN PLOTS,[xy1[0],xy2[0]],[xy1[1],xy2[1]],psym=2,color='0'x
    ;STOP
    IF len_seg_i+len_acc_i GT spacing THEN BEGIN
      IF len_acc_i EQ 0 THEN BEGIN
        len_acc_i=len_seg_i
        WHILE len_acc_i GT spacing DO BEGIN
          npf=npf+1
          GEO_LINE_XY_FAR_DIST, xy1, xy2, xy3, spacing,oxy=oxy
          ;          PROFILE_LINE_AUTO_GET_PROFILE, xy1, xy2, oxy, oxy=oxy_pr, $
          ;            strike=strike, km_per_deg=km_per_deg, isplot=isplot,length_profile=length_profile
          LINE_POINT_EXTEND,  oxy,  $
            -1d0/TAN(strike), $
            length_profile/2/km_per_deg,   $ ;
            oxy=oxy_pr, $
            dummy=dummy
          xys_fp=[[xys_fp],[oxy]]
          oxys=[[[oxys]],[[oxy_pr]]]
          strikes_pr=[strikes_pr,strike-!dpi/2]
          IF isplot EQ 1 THEN BEGIN
            PLOTS,oxy[0],oxy[1],psym=5,color='0000ff'x
            OPLOT,[oxy_pr[0,*]],[oxy_pr[1,*]],color='00ff00'x
          ENDIF
          len_acc_i=len_acc_i-spacing
          IF len_acc_i GT spacing THEN BEGIN
            xy3=oxy
            xy1=xy3
          ENDIF ELSE BEGIN
            xy3=xy2
          ENDELSE
        ENDWHILE
        
      ENDIF ELSE BEGIN
        ;STOP
        npf=npf+1
        GEO_LINE_XY_FAR_DIST, xy1, xy2, xy3, spacing-len_acc_i,oxy=oxy
        ;        PROFILE_LINE_AUTO_GET_PROFILE, xy1, xy2, oxy, oxy=oxy_pr, $
        ;          strike=strike, km_per_deg=km_per_deg, isplot=isplot,length_profile=length_profile
        LINE_POINT_EXTEND,  oxy,  $
          -1d0/TAN(strike), $
          length_profile/2/km_per_deg,   $ ;
          oxy=oxy_pr, $
          dummy=dummy
        xys_fp=[[xys_fp],[oxy]]
        oxys=[[[oxys]],[[oxy_pr]]]
        strikes_pr=[strikes_pr,strike-!dpi/2]
        IF isplot EQ 1 THEN BEGIN
          PLOTS,oxy[0],oxy[1],psym=5,color='0000ff'x
          OPLOT,[oxy_pr[0,*]],[oxy_pr[1,*]],color='00ff00'x
        ENDIF
        len_acc_i=len_acc_i+len_seg_i-spacing
        IF len_acc_i GT spacing THEN BEGIN
          xy3=oxy
          xy1=xy3
        ENDIF ELSE BEGIN
          xy3=xy2
        ENDELSE
        WHILE len_acc_i GT spacing DO BEGIN
          ;STOP
          npf=npf+1
          GEO_LINE_XY_FAR_DIST, xy1, xy2, xy3, spacing,oxy=oxy
          ;          PROFILE_LINE_AUTO_GET_PROFILE, xy1, xy2, oxy, oxy=oxy_pr, $
          ;            strike=strike, km_per_deg=km_per_deg, isplot=isplot,length_profile=length_profile
          LINE_POINT_EXTEND,  oxy,  $
            -1d0/TAN(strike), $
            length_profile/2/km_per_deg ,  $ ;
            oxy=oxy_pr, $
            dummy=dummy
          xys_fp=[[xys_fp],[oxy]]
          oxys=[[[oxys]],[[oxy_pr]]]
          strikes_pr=[strikes_pr,strike-!dpi/2]
          IF isplot EQ 1 THEN BEGIN
            PLOTS,oxy[0],oxy[1],psym=5,color='0000ff'x
            OPLOT,[oxy_pr[0,*]],[oxy_pr[1,*]],color='00ff00'x
          ENDIF
          len_acc_i=len_acc_i-spacing
          
          IF len_acc_i GT spacing THEN BEGIN
            xy3=oxy
            xy1=xy3
          ENDIF ELSE BEGIN
            xy3=xy2
          ENDELSE
        ENDWHILE
      ENDELSE
    ENDIF ELSE BEGIN
      xy3=xy2
      len_acc_i=len_acc_i+len_seg_i
    ENDELSE
  ;xys_fp=[[xys_fp],[oxy]]
  ;STOP
  ENDFOR
  IF N_ELEMENTS(xys_fp) LE 2 THEN BEGIN
    PRINT,'['+PROG+']WARNING: no profiles created!'
    RETURN
  ENDIF
  xys_fp=xys_fp[*,1:*]
  oxys=oxys[*,*,1:*]
  strikes_pr=strikes_pr[1:*]
  ;HELP,xys_fp, oxys, strikes_pr
  
  ;write shapefile file
  IF N_ELEMENTS(ofile) NE 0 THEN BEGIN
    PRINT, '['+PROG+']writing GMT(pxsy) file '+ofile
    np=N_ELEMENTS(oxys[0,0,*])
    OPENW,fid,ofile,/get_lun
    FOR i=0,np-1 DO BEGIN
      PRINTF,fid,'> profile_'+STRTRIM(i+1,2)
      PRINTF,fid,REFORM(oxys[*,*,i]),format='(2(1x,f))'
    ENDFOR
    FREE_LUN,fid
    ;
    shp_file=desuffix(ofile)+'.shp'
    PRINT, '['+PROG+']writing Shapefile file '+shp_file
    pxys=REPLICATE(PTR_NEW(),np)
    FOR i=0,np-1 DO BEGIN
      pxys[i]=PTR_NEW(REFORM(oxys[*,*,i]))
    ENDFOR
    ;stop
    SHP_POLYLINE,  $
      shp_file,   $ ;output shapefile name
      attdat='profile_'+STRTRIM(INDGEN(np)+1,2),  $
      field_name='p_name',$
      pxys=pxys  ;x/y-coordiantes of points
    FOR i=0,np-1 DO PTR_FREE,pxys[i]
    ;
    ;convert shapfile to kml format
    kml_file=desuffix(shp_FILE)+'.kml'
    PRINT, '['+PROG+']writing KML file '+kml_file
    POLYLINE_SHP2KML, shp_FILE, kml_file
  ENDIF
  
  
  IF isplot EQ 1 THEN BEGIN
    IF N_ELEMENTS(ffile) NE 0 THEN BEGIN
      ;jfile=ffile+'.jpg'
      jfile=desuffix(ffile)+'_autoprofiles.jpg'
      PRINT, '['+PROG+']writing preview image file '+jfile
      WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
    ENDIF
    WSET, oldwin
  ;HELP,oldwin
  ENDIF
  PRINT, '['+PROG+']Normal end.'
END