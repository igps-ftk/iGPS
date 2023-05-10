
PRO SAR_LOS_PROFILES_AUTO_LLV, vfile, $  ;velocity file (in varied formats)
    ffile=ffile,  $ ;(if exist) fault trace (only one polyline in GMT format)
    pfile=pfile,  $ ;(if exist) profiles (in GMT psxy format)
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
    maxdist=maxdist  ; the maximum distance away from the line to create the velocity profile
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  ;readme first
  ;this program needs files parameters: 3 inputs and 1 output
  ;inputs:
  ;  vfile - the velocity file in XYZ format;
  ;  ffile - fault file (continuous polyline in GMT psxy format)
  ;
  ;output
  ;  opath - output directory
  ;
  ;If available, the DEM along the profiles will be generated based upon ETOPO1 grid.
  ;
  ;(c)Copyright by Yunfeng Tian. 2016-2017.
  ;This program is part of iGPS package (https://www.ngs.noaa.gov/gps-toolbox/Tian.htm).
  
  
  ;//begin-of-test-parameters
  IF N_ELEMENTS(vfile) EQ 0 || N_ELEMENTS(ffile) EQ 0  THEN BEGIN
    ;
    inputfmt=3  ; xyz with one leading blank column (i.e., non-blank-first-column lines are comments)
    inputfmt=4  ; xyz
    
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
  
  ;stop
  ;read velocity field (various formats: 0- ; 1-psvelo; 2-qoca map; 3- xyz; 4-xyz)
  ;                                                                   ^
  CASE inputfmt OF
    0: BEGIN
      ;Site Long  Lat Vn  Sn  Ve  Se  Cne
      ;H095  102.232 27.8745 -10.8 1.3 10.5  1.3 0.0064
      ;stop
      lines=read_txt(vfile)
      lines1=lines[1:*]
      ;pos=WHERE(strmids(lines,0,1) EQ ' ')
      ;lines1=lines[pos]
      lines1=str_lines2arr(lines1)
      sites=strmids(lines1[0,*],0,4)
      lls=DOUBLE(lines1[1:2,*])
      vels=DOUBLE(lines1[1:*,*])  ;stop
      nsit=N_ELEMENTS(sites)
    END
    1: BEGIN  ;
      ;read psvelo velocity field
      ;stop
      lines=read_txt(vfile)
      pos=WHERE(strmids(lines,0,1) EQ ' ')
      lines1=lines[pos]
      lines1=str_lines2arr(lines1)
      sites=strmids(lines1[7,*],0,4)
      lls=DOUBLE(lines1[0:1,*])
      vels=DOUBLE(lines1[[0,1,3,5,2,4,6],*])
      ;stop
      nsit=N_ELEMENTS(sites)
    END
    2: BEGIN  ;
      ;read qoca map velocity field
      ;*Station   Longitude   Latitude Ve_init Ve_incr    Ve     dVe   Vn_init Vn_incr    Vn     dVn   Cen
      ; ARTU_GPS   58.5583   56.4278     0.0     0.0    24.9     0.0     0.0     0.0     6.1     0.0   0.0000
      ;stop
      lines=read_txt(vfile)
      pos=WHERE(strmids(lines,0,1) EQ ' ')
      lines1=lines[pos]
      lines1=str_lines2arr(lines1)
      sites=strmids(lines1[0,*],0,4)
      lls=DOUBLE(lines1[1:2,*])
      vels=DOUBLE(lines1[[1,2,9,10,5,6,11],*])
      ;stop
      nsit=N_ELEMENTS(sites)
    END
    3: BEGIN ; X          Y         R     [R_sig]
      ; 89.892319  30.813284  2.033756
      ; 89.892669  30.813284  2.815592
      ; 89.893019  30.813284  2.838231
      ;      IF FILE_TEST(VFILE+'.sav') EQ 0 THEN BEGIN
      ;stop
      ;file_info,vfile
      ;READ_COLS,VFILE,DATA=DATA
      lines=read_txt(vfile,comment='~ ')
      data=DOUBLE(str_lines2arr(lines))
      ;        SAVE,DATA,FILENAME=VFILE+'.sav'
      ;      ENDIF
      ;RESTORE,VFILE+'.sav'  ;!!! If file changed later, delete the .sav file and run this program again.
      ; Otherwise, the old file will be used.
      LLS=DATA[0:1,*]
      VELS=REFORM(DATA[2,*])
      IF N_ELEMENTS(data[*,0]) GT 3 THEN BEGIN
        VELES=REFORM(DATA[3,*])
      ENDIF
      NSIT=N_ELEMENTS(VELS)
      SITES=STRING(LINDGEN(NSIT)+1,FORMAT='(I)')
    ;STOP
    END
    4: BEGIN ;X          Y         R     [R_sig]
      ;89.892319  30.813284  2.033756
      ;89.892669  30.813284  2.815592
      ;89.893019  30.813284  2.838231
      ;IF FILE_TEST(VFILE+'.sav') EQ 0 THEN BEGIN
      ;stop
      ;file_info,vfile
      READ_COLS,VFILE,DATA=DATA
      ;lines=read_txt(vfile,comment='~ ')
      ;data=DOUBLE(str_lines2arr(lines))
      ;SAVE,DATA,FILENAME=VFILE+'.sav'
      ;ENDIF
      ;RESTORE,VFILE+'.sav'  ;!!! If file changed later, delete the .sav file and run this program again.
      ; Otherwise, the old file will be used.
      LLS=DATA[0:1,*]
      VELS=REFORM(DATA[2,*])
      IF N_ELEMENTS(data[*,0]) GT 3 THEN BEGIN
        VELES=REFORM(DATA[3,*])
      ENDIF
      NSIT=N_ELEMENTS(VELS)
      SITES=STRING(LINDGEN(NSIT)+1,FORMAT='(I)')
    ;STOP
    END
    ELSE: BEGIN
      PRINT,'['+prog+']ERROR: invalid input velocity format!!'
      RETURN
    END
  ENDCASE
  ;stop
  IF N_ELEMENTS(vels) NE N_ELEMENTS(veles) THEN veles=DBLARR(N_ELEMENTS(vels))
  
  
  xmin=MIN(lls[0,*],max=xmax)
  ymin=MIN(lls[1,*],max=ymax)
  rect=[Xmin, Ymin, Xmax, Ymax]
  ;  rect=[Xmin-1, Ymin-1, Xmax+1, Ymax+1] ;enlarge the rect range
  
  ;stop
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
  ;stop
  ENDIF
  
  
  
  ;  ;Generate DEM profiles (optional; Needs ENVI; if not desired, comment out below lines)
  ;  demfile='C:\dem.gmt\ETOPO1_Bed_g_geotiff\ETOPO1_Bed_g_geotiff.tif'
  ;  ;stop
  ;  IF FILE_TEST(demfile,/regular) THEN BEGIN
  ;    ENVI,/restore_base_save_files
  ;    ENVI_BATCH_INIT
  ;    DEM_GRID_EXTRACT_PROFILE, demfile,  $  ;DEM file (ENVI raw image format; with map projection)
  ;      pfile,  $  ;profile line (two vertices; extended GMT psxy format)
  ;      opath,  $  ;output path
  ;      ffile=ffile  ;fault polyline (extended GMT psxy format)
  ;    ENVI_BATCH_EXIT
  ;  ;RETURN
  ;  ENDIF
  ;  ;STOP
  ;
  ;  ;read profiles
  ;  READ_PSXY,   $
  ;    pfile,   $ ;input file
  ;    region=pfs,   $ ;x,y coorinates of each polygons (pointer type)
  ;    nps=nps, $  ;number of point pairs for each polygon
  ;    count=np,  $ ;number of polygons
  ;    names=names   ;region names (if exist)
  ;
  
  
  ofile=opath+PATH_SEP()+'profiles__map.jpg'
  
  
  ;stop
  pos=WHERE(xys_fvec[0,*] LT 0)
  IF pos[0] NE -1 THEN BEGIN
    PRINT,'['+PROG+']INFO: scale the longitude to 0~360 range'
    xys_fvec[0,pos]=xys_fvec[0,pos]+360
  ENDIF
  ;STOP
  ;
  
  IF N_ELEMENTS(pfile) EQ 0 THEN BEGIN
    pfile=opath+PATH_SEP()+'profiles_auto.psxy'
    ;generate profiles lines by calling PROFILE_LINES_AUTO program
    PROFILE_LINES_AUTO, xys_fvec,  oxys=oxys, spacing=spacing_profile, auto_strike=auto_strike, length_profile=length_profile, ofile=pfile
  ;PROFILE_LINES_AUTO, xys_fvec,  oxys=oxys, spacing=2, auto_strike=auto_strike, length_profile=400
  ;PROFILE_LINES_AUTO, xys_fvec,  oxys=oxys, spacing=2, auto_strike=auto_strike, length_profile=40
  ENDIF ELSE BEGIN
    lines_fpro=read_txt(pfile)
    lines_fpro2=STRTRIM(lines_fpro,2)
    pos=WHERE(strmids(lines_fpro2,0,1) NE '>')
    IF N_ELEMENTS(pos) LT 2 THEN BEGIN
      PRINT,'['+prog+']ERROR: invalid profile line vector file <'+pfile+'>!!'
      RETURN
    ENDIF
    xys_fpro=DOUBLE(str_lines2arr(lines_fpro2[pos]))
    ;stop
    np=1
    oxys=DBLARR(2,2,np)
    oxys=xys_fpro
  ENDELSE
  
  pxys=oxys
  np=N_ELEMENTS(oxys[0,0,*])
  
  ;return
  ;STOP
  ;
  ;  pxys=DBLARR(2,2,np)
  ;  FOR i=0,np-1 DO BEGIN
  ;    IF nps[i] NE 2 THEN STOP
  ;    pxys[*,*,i]=*pfs[i]
  ;  ;STOP
  ;  ENDFOR
  ;  pxys2=pxys
  
  
  ;plot fault line and profiles
  tmplons=0d0
  tmplats=0d0
  FOR i=0,np-1 DO BEGIN
    tmplons=[tmplons,REFORM(oxys[0,*,i])]
    tmplats=[tmplats,REFORM(oxys[1,*,i])]
  ENDFOR
  tmplons=tmplons[1:*]
  tmplats=tmplats[1:*]
  
  IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
    WINDOW,1,/pixmap
    !p.MULTI=-1
    PLOT,tmplons,tmplats,background='ffffff'x,color='0'x,/nodata,/ynozero,  $
      title='Fault and Profiles',/iso  ;,xstyle=1,ystyle=1
    FOR i=0,np-1 DO BEGIN
      lons=REFORM(oxys[0,*,i])
      lats=REFORM(oxys[1,*,i])
      OPLOT,lons,lats,color='ff0000'x,psym=-5
      XYOUTS,lons[0],lats[0],STRTRIM(i+1,2),color='ff0000'x
    ENDFOR
    OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='0000ff'x
    WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
  ENDIF
  
  ;loop for each profile
  ;FOR pi=0,np-1 DO BEGIN
  FOR pi=0,np-1 DO BEGIN
  
    xys=REFORM(pxys[*,*,pi])
    
    IF TOTAL(FINITE(xys)) NE 4 THEN BEGIN
      PRINT,'['+prog+']WARNING:error for profile '+STRTRIM(pi+1)+'! Skipped.'
      CONTINUE
    ENDIF
    IF MIN(xys[0,*]) GT xmax || MAX(xys[0,*]) LT xmin || MIN(xys[1,*]) GT ymax || $
      MAX(xys[1,*]) LT ymin THEN BEGIN  ;#no intersection between profile and velocity field
      PRINT,'['+prog+']WARNING:no intersection between profile '+STRTRIM(pi+1,2)+' and velocity field!'
      ;      print,MIN(xys[0,*]) GT xmax , MAX(xys[0,*]) LT xmin , MIN(xys[1,*]) GT ymax , $
      ;      MAX(xys[1,*]) LT ymin
      ;      stop
      CONTINUE
    ENDIF
    ;STOP
    
    
    IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
      WINDOW,1,xsize=1000,ysize=300,title='Profile '+STRING(pi+1,format='(i2)'),/pixmap
      DEVICE,decomposed=1
      ;!p.MULTI=[0,2,2,0,0]
      ;!p.MULTI=[0,4,1,0,0]
      !p.MULTI=-1
      inds=LINDGEN(N_ELEMENTS(lls[0,*])/10)*10
      PLOT,lls[0,inds],lls[1,inds],psym=1,background='ffffff'x,color='0'x, $
        title='(a) Overview Map', $
        /nodata, $
        position=[0.08,0.15,.28, .92], $
        /ynozero,/iso
      OPLOT,lls[0,inds],lls[1,inds],psym=1,color='aaaaaa'x
      IF N_ELEMENTS(ffile) GT 0 && ffile NE '' THEN BEGIN
        OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='0000ff'x
      ;stop
      ENDIF
    ;stop
    ENDIF
    
    
    
    xy1=xys[*,0]
    xy2=xys[*,1]
    
    a1=xy1  ;the starting and ending vertices of the profile
    b1=xy2
    
    ;STOP
    IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN OPLOT,[xy1[0],xy2[0]], [xy1[1],xy2[1]], color='ff0000'x
    
    ;get the intersect point of fault lines (xys_fvec) and current profile (a1,b1)
    IF N_ELEMENTS(ffile) NE 0 && ffile NE '' THEN BEGIN
      ;
      ;stop
      xy3=INTERSECT_BETWEEN_POLYLINE_AND_LINE(xys_fvec,a1,b1,beta=beta,x1=x1,y1=y1)
      ;if no intersection between profile and fault line, it returns null result.
      IF FINITE(xy3[0]) NE 1 THEN BEGIN
        PRINT,'['+prog+']WARNING: no intersection between profile '+STRTRIM(pi+1,2)+' and fault line!'
        ;        WINDOW,1
        ;        !p.MULTI=-1
        ;        PLOT,xys_fvec[0,*],xys_fvec[1,*],psym=2,/ynozero,background='ffffff'x,color='0'x
        ;        ;PLOT,[a1[0],b1[0]], [a1[1],b1[1]],psym=2,/ynozero,background='ffffff'x,color='0'x
        ;        oPLOT,[a1[0],b1[0]], [a1[1],b1[1]],psym=2,color='0'x
        ;        OPLOT,[x1[0],y1[0]],[x1[1],y1[1]], psym=-1,color='0'x
        ;        OPLOT,[a1[0],b1[0]], [a1[1],b1[1]],color='ff0000'x
        ;        print,x1,y1
        ;        stop
        CONTINUE
      ENDIF
    ;stop
    ENDIF ELSE BEGIN
      ;find the central point of current fault line
      xy3=(xy1+xy2)*.5d0
      IF N_ELEMENTS(flon) NE 0 THEN xy3[0]=flon
      IF N_ELEMENTS(flat) NE 0 THEN xy3[1]=flat
      x1=xy1
      y1=xy2
    ENDELSE
    ;
    ;calculate strike of fault segment
    tmp=(x1[1]-y1[1])/(x1[0]-y1[0])
    ;sar_beta=!dpi/2-ATAN(tmp);+!dpi
    ;PRINT,'['+prog+']fault strike:',sar_beta*180/!dpi
    IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN PLOTS,[x1[0],y1[0]],[x1[1],y1[1]],color='ffff00'x,thick=2,psym=-2
    ;stop
    
    ;get the azimuth, positive: from east, ccw(counter-clockwise)
    alpha=ATAN(b1[1]-a1[1], b1[0]-a1[0])
    IF alpha LT 0 THEN BEGIN
      alpha=alpha+!dpi
    ENDIF
    PRINT,'['+prog+']profile ',pi+1,' angle:',alpha*180d0/!dpi
    
    ;IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN XYOUTS,(a1[0]+b1[0])*.5d0,(a1[1]+b1[1])*.5d0, $
    ;  STRTRIM(string(alpha*180d0/!dpi,format='(f7.2)'),2),color='0000ff'x
    ;    ;stop
    
    ;Derive the rotation matrix for transforming velocities
    ;    rmat=[[COS(-1d0*alpha), -1d0*SIN(-1d0*alpha)], $
    ;      [SIN(-1d0*alpha), COS(-1d0*alpha)] ]
    ;    ;STOP
    
    ;CONTINUE
    ;  ENDFOR
    ;  ;  STOP
    ;
    ;  FOR pi=0,np-1 DO BEGIN
    
    
    ;find adjacent stations
    dists=DBLARR(nsit)
    p_lls=DBLARR(2,nsit)
    dists_fault=DBLARR(nsit)
    FOR si=0ULL, nsit-1 DO BEGIN
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
      POINT_CROSS_LINE, a1, b1, c1, (d1[1]-c1[1])/(d1[0]-c1[0]),i1
      
      ;stop
      IF is_debug EQ 1 THEN BEGIN
        WINDOW,2,xsize=800,ysize=600,title='Map',/pixmap
        ;stop
        DEVICE,decomposed=1
        PLOT,vels[0,*],vels[1,*],psym=1,background='ffffff'x,color='0'x, $
          title=sites[si], $
          /ynozero,/iso
        OPLOT,[a1[0],b1[0]], [a1[1],b1[1]], color='ff0000'x
        OPLOT,[c1[0],d1[0]],[c1[1],d1[1]],color='0000ff'x
        PLOTS,c1[0],c1[1],psym=6,color='0000ff'x,symsize=1
        PLOTS,d1[0],d1[1],psym=6,color='ff00ff'x,symsize=1
        PLOTS,a1[0],a1[1],psym=2,color='0000ff'x,symsize=1
        PLOTS,b1[0],b1[1],psym=2,color='0000ff'x,symsize=1
        PLOTS,i1[0],i1[1],psym=5,color='0000ff'x,symsize=2
        ;PLOTS,i2[0],i2[1],psym=5,color='ff00ff'x,symsize=2
        XYOUTS,c1[0],c1[1],sites[si],color='0'x
        OPLOT,[c1[0],i1[0]],[c1[1],i1[1]],color='00ff00'x,linestyle=2,thick=2
        ofile=opath+PATH_SEP()+sites[si]+'_dist.jpg'
        WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
      ENDIF
      
      IF i1[0] LT MIN([a1[0],b1[0]]) || i1[0] GT MAX([a1[0],b1[0]]) THEN BEGIN  ;outside profile
        ;stop
        CONTINUE
      ENDIF
      
      tmp=MAP_2POINTS(c1[0],c1[1],i1[0],i1[1],/meters)
      ;dists[si]=tmp*1d-3  ;in km
      dists[si]=tmp*1d-3*(ABS(c1[0]-i1[0])/(c1[0]-i1[0]))  ;in km
      
      p_lls[*,si]=i1
      
      ;distance from gps site to fault line
      tmp=MAP_2POINTS(xy3[0],xy3[1],i1[0],i1[1],/meter)
      dists_fault[si]=tmp*1d-3*(i1[0]-xy3[0])/ABS(i1[0]-xy3[0])
    ;stop
      
    ENDFOR
    
    pos=WHERE(ABS(dists) GT 0 AND ABS(dists) LE MaxDist)
    IF pos[0] EQ -1 THEN BEGIN  ;no velocity
      PRINT,'['+prog+']WARNING:no velocity!'
      CONTINUE
    ENDIF
    ;stop
    
    PRINT,'['+PROG+']INFO:writing output files...'
    
    IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
      WSET,1
      ;WINDOW,1,xsize=800,ysize=800,title='Map';,/pixmap
      ;DEVICE,decomposed=1
      ;      lonmin=MIN([ a1[0],b1[0],REFORM(xys_fvec[0,*]) ],max=lonmax)
      ;      latmin=MIN([ a1[1],b1[1],REFORM(xys_fvec[1,*]) ],max=latmax)
      ;      PLOT,vels[0,*],vels[0,*],psym=1,background='ffffff'x,color='0'x, $
      ;        title='(b) Site Map for Profile '+STRING(pi+1,format='(i2)'), $
      ;        xrange=[lonmin,lonmax], $
      ;        yrange=[latmin,latmax], $
      ;        ;POSITION=[0.1, 0.1, 0.9, 0.9], $
      ;        /ynozero,/iso
      ;      OPLOT,[a1[0],b1[0]], [a1[1],b1[1]], color='ff0000'x
      PLOTS,lls[0,pos],lls[1,pos],psym=3,color='ff0000'x,symsize=1.1
    ;      PLOTS,[xy3[0]],[xy3[1]],psym=2,color='0000ff'x
    ;      IF N_ELEMENTS(ffile) GT 0 && ffile NE '' THEN BEGIN
    ;        OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='0000ff'x
    ;      ENDIF
    ;ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i02)')+'_map.jpg'
    ;WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
    ENDIF
    
    vel_along_all=DBLARR(N_ELEMENTS(pos))
    vel_tang_all=DBLARR(N_ELEMENTS(pos))
    vele_along_all=DBLARR(N_ELEMENTS(pos))
    vele_tang_all=DBLARR(N_ELEMENTS(pos))
    ;LOS velocity for InSAR
    vel_los_all=DBLARR(N_ELEMENTS(pos))
    vele_los_all=DBLARR(N_ELEMENTS(pos))
    FOR vi=0ULL, N_ELEMENTS(pos)-1 DO BEGIN
      vel_los_all[vi]=vels[POS[VI]]
      vele_los_all[vi]=veles[POS[VI]]
      ;vel_along_all[vi]=vel_los_all[vi]*COS(sar_theta)
      ;vel_tang_all[vi]=vel_los_all[vi]/(SIN(sar_theta)*SIN(sar_alpha-sar_beta))
      
      ;for bengco fault
      ;sar_beta=120d0*!dpi/180d0
      
      ;stop
      ;      INSAR_LOS2FAULT_PURE_STRIKE_SLIP, vel_los_all[vi], sar_beta,   $
      ;        orbtyp=orbtyp,  $
      ;        fx=fx, fy=fy, $
      ;        ds=ds, dd=dd
      ;      vel_along_all[vi]=fy
      ;      vel_tang_all[vi]=fx
      ;vel_along_all[vi]=dd
      ;vel_tang_all[vi]=ds
      ;stop
      IF is_debug EQ 1 THEN BEGIN
        WINDOW,3,/pixmap
        DEVICE,decomposed=1
        PLOT,vels[0,*],vels[1,*],psym=1,background='ffffff'x,color='0'x, $
          /nodata, xrange=[-20,20],yrange=[-20,20], $
          title=sites[pos[vi]],$
          /ynozero,/iso
        OPLOT,[0,vel[4]],[0,0],color='0'x
        OPLOT,[0,0],[0,vel[3]],color='0'x
        OPLOT,[0,vel[4]],[0,vel[3]],color='0'x,thick=2
        x=INDGEN(100)*40d0-20
        y=x*TAN(alpha)
        OPLOT,x,y,color='0'x,linestyle=2
        OPLOT,[0,vel_ss],[0,vel_st],color='ff0000'x,thick=2,linestyle=3
        ofile=opath+PATH_SEP()+sites[pos[vi]]+'_vel_components.jpg'
        WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
      ENDIF
    ;STOP
    ENDFOR
    ;STOP
    lls_used=p_lls[*,pos]
    ind=SORT(lls_used[0,*])
    
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i03)')+'_vel.psxy'
    OPENW,fid,ofile,/get_lun
    WRITE_SYS_INFO,fid,prog=prog,src=[vfile,pfile],user=user
    ;output profile vertex
    PRINTF,fid,a1,format='("# PSXY_PROFILE",2f10.3)'
    PRINTF,fid,b1,format='("# PSXY_PROFILE",2f10.3)'
    PRINTF,fid,XY3,format='("# PSXY_FAULT_PROFILE_INTERSECT",2f10.3)'
    FOR j=0,N_ELEMENTS(xys_fvec[0,*])-1 DO BEGIN
      PRINTF,fid,xys_fvec[*,j],format='("# PSXY_FAULT_TRACE",2f10.3)'
    ENDFOR
    ;output stations
    captions=['site','p_long','p_lati','p_dist','v_along','ve_along','v_tang','ve_tang',$
      'long','lati','dist_to_fault','v_los','ve_los']
    caption_comments=['location label','longitude (degrees) of the shadow point','latitude of the shadow point',  $
      'distance from current data point to the its shadow on the profile (i.e., the shadow point)',  $
      'velocity along the profile','uncertainity along the profile','velocity tangent to the profile',  $
      'uncertainity tangent to the profile',$
      'longitude of data point','latitude of data point','distance from data point to the fault trace',  $
      'velocity along the los direction','velocity uncertainity along the los orientation']
    IF N_ELEMENTS(captions) NE N_ELEMENTS(caption_comments) THEN BEGIN
      caption_comments=REPLICATE('UNKOWN',N_ELEMENTS(captions))
    ENDIF
    FOR ci=0, N_ELEMENTS(captions)-1 DO BEGIN
      PRINTF,fid,captions[ci],caption_comments[ci],format='("*",1x,a10,2x,"-",2x,a)'
    ENDFOR
    PRINTF,fid,captions, $
      format='("*",a16,1x,2a10,1x,a10,1x,2a10,1x,2a10,1x,2a10,1x,a13,1x,2(1x,a10))'
    FOR j=0ULL, N_ELEMENTS(ind)-1 DO BEGIN
      PRINTF,fid,sites[pos[ind[j]]],p_lls[*,pos[ind[j]]],dists[pos[ind[j]]],vel_along_all[ind[j]],$
        vele_along_all[ind[j]],vel_tang_all[ind[j]],vele_tang_all[ind[j]], $
        lls[*,pos[ind[j]]], $
        dists_fault[pos[ind[j]]], $
        vel_los_all[ind[j]], vele_los_all[ind[j]],   $
        format='(1x,a16,1x,2f10.3,1x,f10.2,1x,2f10.2,1x,2f10.2,1x,2f10.3,1x,f13.6,1x,2(1x,f10.2))'
    ENDFOR
    FREE_LUN,fid
    
    ;STOP
    IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
      ;WINDOW,4
      WSET,1
      ;!p.MULTI=[1,2,2]
      ;!p.MULTI=[1,1,2,0,1]
      ;!p.MULTI=[0,3,1,0,0]
      ;yrange=[-20,20]
      ;yrange=[-50,50]
      ;yrange=[-12,12]
      ;stop
      ;PLOT,lls_used[0,ind],vel_los_all[ind],background='ffffff'x,color='0'x, $
      PLOT,dists_fault[pos[ind]],vel_los_all[ind],background='ffffff'x,color='0'x, $
        title='(b) Velocity Profile'+STRING(pi+1,format='(i2)'), $
        position=[.38,.15,.92,.92], /noerase, $
        xtitle='Distance to Fault (km)',ytitle='LOS Velocitiy( mm/yr)',  $
        /ynozero,psym=4,/nodata,yrange=yrange
      FOR j=0ULL,N_ELEMENTS(ind)-1 DO BEGIN
        ;OPLOT,[lls_used[0,ind[j]], lls_used[0,ind[j]] ], $
        OPLOT,[dists_fault[pos[ind[j]]], dists_fault[pos[ind[j]]] ], $
          [vel_los_all[ind[j]]+ABS(vele_los_all[ind[j]]),vel_los_all[ind[j]]-ABS(vele_los_all[ind[j]]) ], $
          color='aaaaaa'x,thick=1
      ENDFOR
      OPLOT,dists_fault[pos[ind]],vel_los_all[ind],color='0000ff'x, psym=1
      ;OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
      OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
      
      ;    PLOT,lls_used[0,ind],vel_along_all[ind],background='ffffff'x,color='0'x, $
      ;      title='DEM (m) '+STRING(pi+1,format='(i2)'), $
      ;      /ynozero,psym=4,/nodata,yrange=yrange
      ;    OPLOT,lls_used[0,ind],vel_los_all[ind],color='0000ff'x, psym=5
      ;    OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
      ;    FOR j=0ULL,N_ELEMENTS(ind)-1 DO BEGIN
      ;      OPLOT,[lls_used[0,ind[j]], lls_used[0,ind[j]] ], $
      ;        [vel_los_all[ind[j]]+ABS(vele_los_all[ind[j]]),vel_los_all[ind[j]]-ABS(vele_los_all[ind[j]]) ], $
      ;        color='0000ff'x,thick=2
      ;    ENDFOR
      
      ;    PLOT,lls_used[0,ind],vel_along_all[ind],background='ffffff'x,color='0'x, $
      ;      title='Velocities Along Profile '+STRING(pi+1,format='(i2)'), $
      ;      /ynozero,psym=4,/nodata,yrange=yrange
      ;    OPLOT,lls_used[0,ind],vel_along_all[ind],color='0000ff'x, psym=5
      ;    OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
      ;    FOR j=0ULL,N_ELEMENTS(ind)-1 DO BEGIN
      ;      OPLOT,[lls_used[0,ind[j]], lls_used[0,ind[j]] ], $
      ;        [vel_along_all[ind[j]]+ABS(vele_along_all[ind[j]]),vel_along_all[ind[j]]-ABS(vele_along_all[ind[j]]) ], $
      ;        color='0000ff'x,thick=2
      ;    ENDFOR
      
      ;    PLOT,lls_used[0,ind],vel_tang_all[ind],background='ffffff'x,color='0'x, $
      ;      title='Velocities Tangent to Profile '+STRING(pi+1,format='(i2)'), $
      ;      /ynozero,psym=5,/nodata,yrange=yrange
      ;    OPLOT,lls_used[0,ind],vel_tang_all[ind],color='0000ff'x, psym=5
      ;    OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
      ;    FOR j=0ULL,N_ELEMENTS(ind)-1 DO BEGIN
      ;      OPLOT,[lls_used[0,ind[j]], lls_used[0,ind[j]] ], $
      ;        [vel_tang_all[ind[j]]+ABS(vele_tang_all[ind[j]]),vel_tang_all[ind[j]]-ABS(vele_tang_all[ind[j]]) ], $
      ;        color='0000ff'x,thick=2
      ;    ENDFOR
      
      !p.MULTI=-1
      
      ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i03)')+'_vel.jpg'
      WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
      
    ENDIF
  ;STOP
  ;PRINT,'a1:',a1
  ;PRINT,'b1:',b1
  ;RETURN
  ENDFOR
  
  PRINT,'['+prog+']Normal end.'
END