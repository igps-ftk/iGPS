

PRO VEL_PROFILES, vfile, $  ;velocity file (in varied formats)
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
    ;
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
    inputfmt=1
    
    
    vfile='D:\iGPS\example\profile\Supp_Table_S1.psvelo'
    opath='D:\iGPS\example\profile\p_auto'
    
    ;
    ;
    ffile='D:\iGPS\example\profile\fa_honghe1.psxy'
    ;pfile='D:\iGPS\example\profile\pf_honghe1.psxy'
    
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
    
    
    vfile='D:\iGPS\tables\wang_shen_2019JB018774_Table.S4S5.psvelo'
    inputfmt=1
    ffile='D:\iGPS\tables\fa_ganzi.psxy'
    opath='D:\gsar\interseismic\004-d-m5-0476_0481_0486_0491_0496-jiali8\f123\sbas.4.0.0001.9999.20170311.20230831.060.0410.01.___\pg.fa_ganzi'
    
    ffile='C:\GMT_pub\vector\profile\fa_Kunlun_Fault.psxy'
    f2file='C:\GMT_pub\vector\profile\fa_ganzi_xianshuihe.psxy'
    opath='C:\tmp\gic3dv\kunlun\pg.fa_ganzi_xianshuihe'
    ;pfile='C:\tmp\gic3dv\kunlun\pg.fa_ganzi_xianshuihe\profiles_auto.psxy'
    auto_strike=2
    auto_strike_2nd=2
    
    
  ENDIF
  
  IF FILE_TEST(opath,/directory) NE 1 THEN FILE_MKDIR,opath
  
  IF N_ELEMENTS(out_plot) EQ 0 THEN out_plot=0
  IF N_ELEMENTS(inputfmt) EQ 0 THEN inputfmt=0
  IF N_ELEMENTS(auto_strike) EQ 0 THEN auto_strike=2
  IF N_ELEMENTS(spacing_profile) EQ 0 THEN spacing_profile=30  ;in km (distance between two neighboring profiles)
  IF N_ELEMENTS(length_profile) EQ 0 THEN length_profile=900  ;in km (the total length of the profile generated automatically by iGPS)
  
  
  np=0
  pxys=-9999d0
  IF N_ELEMENTS(search_radius) EQ 0 THEN search_radius=50d0 ;maximum searching distance beside the profile line, in kilometers
  ;search_radius=100d0
  
  
  ;read velocity field
  CASE inputfmt OF
    0: BEGIN
      ;Site Long  Lat Vn  Sn  Ve  Se  Cne
      ;H095  102.232 27.8745 -10.8 1.3 10.5  1.3 0.0064
      READ_GNSS_VELH_SLLNE, vfile,   $
        sites=sites,  $
        lls=lls,  $
        vels=vels,  $
        nsit=nsit
    END
    1: BEGIN  ;
      ;read psvelo velocity field
      ;lon lat Ve Vn Se Sn Cen Site
      READ_PSVELO, vfile,   $
        sites=sites,  $
        lls=lls,  $
        vels=vels,  $
        nsit=nsit
    END
    2: BEGIN  ;
      ;read qoca map velocity field
      ;*Station   Longitude   Latitude Ve_init Ve_incr    Ve     dVe   Vn_init Vn_incr    Vn     dVn   Cen
      ; ARTU_GPS   58.5583   56.4278     0.0     0.0    24.9     0.0     0.0     0.0     6.1     0.0   0.0000
      READ_GNSS_VELH_QOCA_MAP, vfile,   $
        sites=sites,  $
        lls=lls,  $
        vels=vels,  $
        nsit=nsit
    END
    3: BEGIN  ;
      ;  Long.    Lat.      Ve       Vn     Se     Sn    Cne  Sta.      Source
      ; 74.336  39.842   0.170   13.790  0.560  0.503  0.000  I089  This_study
      ; 78.680  29.848  10.840   32.656  1.550  1.450 -0.002  LAN2  Kreemer et al. [2014] from Banerjee et al. [2008]
      READ_PSVELO_EXT, vfile,   $
        sites=sites,  $
        lls=lls,  $
        vels=vels,  $
        nsit=nsit
    END
    ELSE: BEGIN
      PRINT,'['+prog+']ERROR: invalid input velocity format!!'
      RETURN
    END
  ENDCASE
  
  xmin=MIN(lls[0,*],max=xmax)
  ymin=MIN(lls[1,*],max=ymax)
  rect=[Xmin, Ymin, Xmax, Ymax]
  ;  rect=[Xmin-1, Ymin-1, Xmax+1, Ymax+1] ;enlarge the rect range
  
  
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
  
  
  ;read 2nd fault vector (if present)
  IF N_ELEMENTS(f2file) GT 0 && f2file NE '' THEN BEGIN
    lines2_fvec=read_txt(f2file)
    lines2_fvec2=STRTRIM(lines2_fvec,2)
    pos2=WHERE(strmids(lines2_fvec2,0,1) NE '>')
    IF N_ELEMENTS(pos2) LT 2 THEN BEGIN
      PRINT,'['+prog+']ERROR: invalid fault line vector file <'+f2file+'>!!'
      RETURN
    ENDIF
    xys_fvec_2nd=DOUBLE(str_lines2arr(lines2_fvec2[pos2]))
    OPLOT,xys_fvec_2nd[0,*],xys_fvec_2nd[1,*],psym='-3',color='00ff00'x
    ;
    ;calculate strikes for 2nd fault
    FA_STRIKE_CAL,  '', $   ;fault file (if given); otherwise fault coordinate (xys) should be given
      xys=xys_fvec_2nd,  $   ;fault coordinates (lon, lat) array (2*N)
      strikes=strikes_fa_2nd,  $ ;output strikes
      auto_strike=auto_strike2 ;strike mode
      
    pfile_2nd=opath+PATH_SEP()+'profiles_auto_2nd.psxy'
    PROFILE_LINES_AUTO, xys_fvec_2nd,  oxys=oxys_2nd, spacing=spacing_profile,  $
      strikes_pr=strikes_pr_2nd, auto_strike=auto_strike_2nd,   $
      xys_fp_fault_profile_intersection=xys_fp_2nd,  $ ;intersection points of profile and fault lines
      length_profile=length_profile, ofile=pfile_2nd
      
    n_profiles_2nd=N_ELEMENTS(xys_fp_2nd[0,*])
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
    
  ;    lines=read_txt(pfile)
  ;    np=0
  ;    FOR li=0, N_ELEMENTS(lines)-1 DO BEGIN
  ;      line=STRTRIM(lines[li],2)
  ;      IF STRMID(line,0,1) EQ '>' THEN BEGIN
  ;        np=np+1
  ;        xyi=REPLICATE(-9999d0,2,2)
  ;        CONTINUE
  ;      ENDIF
  ;      IF line EQ '' THEN BEGIN  ;skip blank lines
  ;        CONTINUE
  ;      ENDIF
  ;
  ;
  ;      line_p=STRSPLIT(line,/extract)
  ;      IF xyi[0,0] EQ -9999d0 THEN BEGIN
  ;
  ;        ;starting point
  ;        xyi[*,0]=DOUBLE(line_p)
  ;      ENDIF ELSE BEGIN
  ;
  ;        ;ending point
  ;        xyi[*,1]=DOUBLE(line_p)
  ;
  ;        IF np EQ 1 THEN BEGIN
  ;          pxys=xyi
  ;        ENDIF ELSE BEGIN
  ;          pxys=[[[pxys]],[[xyi]]]
  ;        ENDELSE
  ;
  ;      ENDELSE
  ;
  ;    ENDFOR
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
  
  ;stop
  ;loop for each profile
  
  FOR pi=20,np-1 DO BEGIN  ;loop for each profile
  
    WINDOW,1,xsize=1500,ysize=900,title='Profile '+STRING(pi+1,format='(i03)');,/pixmap
    DEVICE,decomposed=1
    !p.MULTI=[0,2,2]
    ;!p.MULTI=-1
    PLOT,vels[0,*],vels[1,*],psym=1,background='ffffff'x,color='0'x, $
      title='Sites Overview Map', $
      xrange=[90,110], $
      yrange=[25,44], $
      /ynozero;,/iso
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
    
    
    
    ;get the intersect point of fault lines (xys_fvec) and current profile (a1,b1)
    IF N_ELEMENTS(ffile) NE 0 && ffile NE '' THEN BEGIN
      xy3=SEGMENT_INTERSECT_POLYLINE(xys_fvec,a1,b1)
    ;if no intersection between profile and fault line, it returns null result.
    ;stop
    ENDIF ELSE BEGIN
      ;find the central point of current fault line
      xy3=(xy1+xy2)*.5d0
      IF N_ELEMENTS(flon) NE 0 THEN xy3[0]=flon
      IF N_ELEMENTS(flat) NE 0 THEN xy3[1]=flat
    ENDELSE
    
    ;get the azimuth, positive: from east, ccw(counter-clockwise)
    alpha=ATAN(b1[1]-a1[1], b1[0]-a1[0])
    IF alpha LT 0 THEN BEGIN
      alpha=alpha+!dpi
    ENDIF
    lbl_str='Creating profile '+STRTRIM(pi+1,2)+' angle:'+STRTRIM(alpha*180d0/!dpi,2)
    PRINT, lbl_str
    ;HELP,lbl_id
    IF N_ELEMENTS(lbl_id) NE 0 THEN WIDGET_CONTROL, lbl_id, set_value=lbl_str
    
    XYOUTS,(a1[0]+b1[0])*.5d0,(a1[1]+b1[1])*.5d0, $
      STRTRIM(alpha*180d0/!dpi,2),color='0000ff'x
    ;stop
      
    ;Derive the rotation matrix for transforming velocities
    rmat=[[COS(-1d0*alpha), -1d0*SIN(-1d0*alpha)], $
      [SIN(-1d0*alpha), COS(-1d0*alpha)] ]
    ;STOP
      
    ;CONTINUE
      
      
    ;find adjacent stations
    dists=DBLARR(nsit)
    p_lls=DBLARR(2,nsit)
    dists_fault=DBLARR(nsit)
    IF N_ELEMENTS(f2file) NE 0 && f2file NE '' THEN BEGIN
      dists_fault_2nd=DBLARR(nsit)
      dists_fault_2nd[*]=!values.D_NAN
      cross_angles_2nd=DBLARR(nsit)
      cross_angles_2nd[*]=!values.D_NAN
    ENDIF
    FOR si=1647, nsit-1 DO BEGIN
      c1=lls[*,si]
      
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
      ;        PLOT,vels[0,*],vels[1,*],psym=1,background='ffffff'x,color='0'x, $
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
      IF N_ELEMENTS(f2file) NE 0 && f2file NE '' THEN BEGIN
        ;HELP,xys_fvec_2nd,  oxys,oxys_2nd,  strikes_pr,strikes_pr_2nd, xys_fp_2nd
      
        ;for a given point c1
        ;calculate distances from c1 to profiles
        dists_c1_to_profiles=DBLARR(n_profiles_2nd)
        xys_c1_cross_profiles=DBLARR(2,n_profiles_2nd)
        FOR pj=0, n_profiles_2nd-1 DO BEGIN  ;loop for each profile
          ;get the point passing c1 perpendicular to the profile
          a2=REFORM(oxys_2nd[*,0,pj])
          b2=REFORM(oxys_2nd[*,1,pj])
          POINT_PERP_LINE, a2, b2, c1, d2
          
          dists_c1_to_profiles[pj]=MAP_2POINTS(c1[0],c1[1],d2[0],d2[1],/meter)*1d-3
          xys_c1_cross_profiles[*,pj]=d2
        ENDFOR
        
        tmp=MIN(dists_c1_to_profiles,ind_min)
          a2=REFORM(oxys_2nd[*,0,ind_min])
          b2=REFORM(oxys_2nd[*,1,ind_min])
        d2=REFORM(xys_c1_cross_profiles[*,ind_min])
        e2=REFORM(xys_fp_2nd[*,ind_min])
        tmp=MAP_2POINTS(d2[0],d2[1],e2[0],e2[1],/meter)*1d-3
        dists_fault_2nd[si]=tmp*(d2[0]-e2[0])/ABS(d2[0]-e2[0])
        cross_angles_2nd[si]=strikes_pr_2nd[ind_min]-strikes_pr[pi]
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
      ENDIF  ;end-of-distance-to-2nd-fault
      
    ENDFOR
    
    pos=WHERE(dists GT 0 AND dists LE search_radius)
    ;help,pos
    ;stop
    IF pos[0] EQ -1 THEN BEGIN  ;no velocity
      CONTINUE
    ENDIF
    
    WSET,1
    ;WINDOW,1,xsize=800,ysize=800,title='Map';,/pixmap
    ;DEVICE,decomposed=1
    lonmin=MIN([ a1[0],b1[0],REFORM(xys_fvec[0,*]) ],max=lonmax)
    latmin=MIN([ a1[1],b1[1],REFORM(xys_fvec[1,*]) ],max=latmax)
    PLOT,vels[0,*],vels[1,*],psym=1,background='ffffff'x,color='0'x, $
      title='Site Map for Profile '+STRING(pi+1,format='(i2)'), $
      xrange=[lonmin,lonmax], $
      yrange=[latmin,latmax], $
      /ynozero,/iso
    OPLOT,[a1[0],b1[0]], [a1[1],b1[1]], color='ff0000'x
    PLOTS,lls[0,pos],lls[1,pos],psym=1,color='ff0000'x
    PLOTS,[xy3[0]],[xy3[1]],psym=2,color='0000ff'x
    IF N_ELEMENTS(ffile) GT 0 && ffile NE '' THEN BEGIN
      OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='0000ff'x
    ENDIF
    IF N_ELEMENTS(f2file) GT 0 && f2file NE '' THEN BEGIN
      OPLOT,xys_fvec_2nd[0,*],xys_fvec_2nd[1,*],psym='-3',color='00ff00'x
      FOR j=0,n_profiles_2nd-1 DO BEGIN
        OPLOT,oxys_2nd[0,*,j],oxys_2nd[1,*,j],psym='-5',color='00ff00'x
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
    
    vel_along_all[*]=-999.99d0
    vel_tang_all[*]=-999.99d0
    vel_up_all[*]=-999.99d0
    
    FOR vi=0, N_ELEMENTS(pos)-1 DO BEGIN
      vel=REFORM(vels[*,pos[vi]])
      ;      vel_ss_e=vel[4]*sin(alpha)
      ;      vel_st_e=vel[4]*cos(alpha)
      ;      vel_ss_n=vel[8]*cos(alpha)
      ;      vel_st_n=vel[8]*sin(alpha)
      ;      vel_ss=vel_ss_e+vel_ss_n
      ;      vel_st=vel_st_e+vel_st_n
      vel_amp=SQRT(vel[4]^2+vel[2]^2)
      vel_azi=ATAN(vel[2],vel[4])
      ;PRINT,vel_azi*180/!dpi
      
      phi=alpha-vel_azi
      vel_ss=vel_amp*COS(phi)
      vel_st=vel_amp*SIN(phi)
      vel_along_all[vi]=vel_ss
      vel_tang_all[vi]=vel_st
      ;print,vel_ss,vel_st
      
      ;another way, the matrix transformation
      vel_en=[vel[4],vel[2]]  ;velocities in the east and north directions
      vel_at=vel_en#rmat  ;velocities in the along-profile and tangent-profile (counter-clockwise 90 degrees from the along-profile) directions
      ;print,vel_at[*]
      ;stop
      
      vele_amp=SQRT(vel[5]^2+vel[3]^2)
      vele_ss=vele_amp*COS(phi)
      vele_st=vele_amp*SIN(phi)
      ;if vele_st lt 0 || vele_ss le 0 then stop
      vele_along_all[vi]=vele_ss
      vele_tang_all[vi]=vele_st
      
      ;another way, the matrix transformation
      vele_en=[[vel[5]^2,vel[6]*vel[5]*vel[3]], $
        [vel[6]*vel[5]*vel[3], vel[3]^2] ]
      ;print,vel[[5,3]]
      vele_at=rmat#vele_en#TRANSPOSE(rmat)
      vele_along_all[vi]=SQRT(vele_at[0,0])
      vele_tang_all[vi]=SQRT(vele_at[1,1])
      
    ;stop
      
    ;      IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
    ;        WINDOW,3,/pixmap
    ;        DEVICE,decomposed=1
    ;        PLOT,vels[0,*],vels[1,*],psym=1,background='ffffff'x,color='0'x, $
    ;          /nodata, xrange=[-20,20],yrange=[-20,20], $
    ;          title=sites[pos[vi]],$
    ;          /ynozero,/iso
    ;        OPLOT,[0,vel[4]],[0,0],color='0'x
    ;        OPLOT,[0,0],[0,vel[3]],color='0'x
    ;        OPLOT,[0,vel[4]],[0,vel[3]],color='0'x,thick=2
    ;        x=INDGEN(100)*40d0-20
    ;        y=x*TAN(alpha)
    ;        OPLOT,x,y,color='0'x,linestyle=2
    ;        OPLOT,[0,vel_ss],[0,vel_st],color='ff0000'x,thick=2,linestyle=3
    ;        ofile=opath+PATH_SEP()+sites[pos[vi]]+'_vel_components.jpg'
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
        -999.99d0,0,  $
        vels[[4,2,5,3],pos[ind[j]]] $
        , 0,0,0 ]
    ENDFOR
    
    IF N_ELEMENTS(f2file) NE 0 && f2file NE '' THEN BEGIN
      odata_2nd=DBLARR(2,N_ELEMENTS(ind))
      odata_2nd[*]=!values.D_NAN
      odata_2nd[0,*]=dists_fault_2nd[pos[ind]]
      odata_2nd[1,*]=cross_angles_2nd[pos[ind]]*180/!dpi
    ENDIF
    
    WRITE_VEL_PROFILE, ofile $
      , odata $
      , sites=sites[pos[ind]] $
      , fa_xys=xys_fvec  $
      , pf_xys=[[a1],[b1]]  $
      , fa_pf_xy=xy3 $
      , odata_2nd=odata_2nd $
      , headers=[vfile,ffile,pfile]
      
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
      title='Velocities Along Profile '+STRING(pi+1,format='(i2)'), $
      ;yrange=[-50,50] ,  $
      /ynozero,psym=2;,yrange=yrange
    ;OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    FOR j=0,N_ELEMENTS(ind)-1 DO BEGIN
      OPLOT,[dists_fault[pos[ind[j]]], dists_fault[pos[ind[j]]] ], $
        [vel_along_all[ind[j]]+ABS(vele_along_all[ind[j]]),vel_along_all[ind[j]]-ABS(vele_along_all[ind[j]]) ], $
        color='0000ff'x,thick=2
    ENDFOR
    
    ;PLOT,lls_used[0,ind],vel_tang_all[ind],background='ffffff'x,color='0'x, $
    PLOT,dists_fault[pos[ind]],vel_tang_all[ind],background='ffffff'x,color='0'x, $
      title='Velocities Tangent to Profile '+STRING(pi+1,format='(i2)'), $
      ;yrange=[-50,50] ,  $
      /ynozero,psym=5;,yrange=yrange
    ;    OPLOT,lls_used[0,ind],vel_tang_all[ind],color='0000ff'x, $
    ;      psym=5
    ;OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    FOR j=0,N_ELEMENTS(ind)-1 DO BEGIN
      OPLOT,[ dists_fault[pos[ind[j]]], dists_fault[pos[ind[j]]] ], $
        [vel_tang_all[ind[j]]+ABS(vele_tang_all[ind[j]]),vel_tang_all[ind[j]]-ABS(vele_tang_all[ind[j]]) ], $
        color='0000ff'x,thick=2
    ENDFOR
    
    !p.MULTI=-1
    
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i03)')+'_vel.jpg'
    WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
    
    ;PRINT,'a1:',a1
    ;PRINT,'b1:',b1
    BREAK
  ENDFOR
  
  lbl_str='Ready'
  IF N_ELEMENTS(lbl_id) NE 0 THEN WIDGET_CONTROL, lbl_id, set_value=lbl_str
;stop
END