
PRO SAR_PROFILE_AUTO, vfile, $  ;velocity file (in varied formats)
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
    
    ;    vfile=FILEPATH('meanvel_01.txt',subdirectory=['example','sar','xyz_profiles','xyz'],$
    ;      root=!igps_root)
    ;    ffile=FILEPATH('fault_ydgl2.psxy',subdirectory=['example','sar','xyz_profiles','fault_trace'],$
    ;      root=!igps_root)
    ;    opath=FILEPATH('p',subdirectory=['example','sar','xyz_profiles'],$
    ;      root=!igps_root)
    
    ;    vfile='D:\tmp\file.xyz'
    ;    ffile='D:\data\vector\profile\fault_jiali.psxy'
    ;    opath='d:\tmp\p'
    ;
    ;    vfile='D:\tmp\gyaringco\fault_bc4.txt'
    ;    ffile='D:\tmp\gyaringco\fault_bc.psxy'
    ;    opath='D:\tmp\gyaringco\p'
    ;auto_strike=2
    
    ;GOTO, end_of_default_input_parameters
    
    vfile='D:\gsar\interseismic\085-a-m3-0088_0093_0098-gaize_yzs5\f123.1\sbas.3.0.0400.9999.20150515.20190927.103.0320.01.___\vel_mask_ll3.xyz'
    ffile='C:\GMT_pub\vector\profile\fa_dawaco_maiqiongco.psxy'
    opath='D:\gsar\interseismic\085-a-m3-0088_0093_0098-gaize_yzs5\f123.1\sbas.3.0.0400.9999.20150515.20190927.103.0320.01.___\p.fa_dawaco_maiqiongco'
    
    vfile='D:\gsar\interseismic\062-d-m3-0462_0467_0472-haiyuan4M3\f123\sbas.4.0.0367.9999.20170120.20210423.120.0926.01.___\vel_mask_ll3.xyz'
    ffile='C:\GMT_pub\vector\profile\fa_haiyuan.psxy'
    opath='D:\gsar\interseismic\062-d-m3-0462_0467_0472-haiyuan4M3\f123\sbas.4.0.0367.9999.20170120.20210423.120.0926.01.___\p.fa_haiyuan2'
    
    vfile='D:\gsar\interseismic\158-a-m3-0083_0088_0093_0098-mht\f123\sbas.4.0.0367.9999.20151128.20210430.131.1329.01.___\vel_mask_ll3.xyz'
    ffile='C:\GMT_pub\vector\profile\fa_iys.psxy'
    opath='D:\gsar\interseismic\158-a-m3-0083_0088_0093_0098-mht\f123\sbas.4.0.0367.9999.20151128.20210430.131.1329.01.___\p.fa_iys3'
    spacing_profile=50
    maxdist=5d0
    
    ;auto_strike=3
    
    out_plot=0
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
  IF N_ELEMENTS(out_plot) EQ 0 THEN out_plot=0
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
  PRINT,'[]INFO:reading velocity ...'
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
  PRINT,'[]INFO:velocity read.'
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
  
  xpmin=MIN(pxys[0,*,*],max=xpmax)
  ypmin=MIN(pxys[1,*,*],max=ypmax)
  
  ;intersection point of profile and fault
  pfxys=DBLARR(2,np)
  FOR pi=0,np-1 DO BEGIN
  
    xys=REFORM(pxys[*,*,pi])
    
    xy1=xys[*,0]
    xy2=xys[*,1]
    
    a1=xy1  ;the starting and ending vertices of the profile
    b1=xy2
    
    
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
    pfxys(*,pi)=xy3
    
  ENDFOR
  
  ;   if 1 then begin
  ;  ;IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
  ;    WINDOW,1,xsize=1200,ysize=800
  ;    !p.MULTI=-1
  ;    PLOT,[xpmin,xpmax],[ypmin,ypmax],background='ffffff'x,color='0'x,/nodata,/ynozero,  $
  ;      title='Fault and Profiles';,/iso  ;,xstyle=1,ystyle=1
  ;    FOR i=0,np-1 DO BEGIN
  ;      lons=REFORM(pxys[0,*,i])
  ;      lats=REFORM(pxys[1,*,i])
  ;      OPLOT,lons,lats,color='ff0000'x,psym=-5
  ;      XYOUTS,lons[0],lats[0],STRTRIM(i+1,2),color='ff0000'x
  ;    ENDFOR
  ;    OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='0000ff'x
  ;    OPLOT,pfxys[0,*],pfxys[1,*],psym='2',color='00ffff'x
  ;    ;WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
  ;  ENDIF
  
  ;STOP
  ;
  
  
  
  IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
  
    ;plot fault line and profiles
    tmplons=0d0
    tmplats=0d0
    FOR i=0,np-1 DO BEGIN
      tmplons=[tmplons,REFORM(oxys[0,*,i])]
      tmplats=[tmplats,REFORM(oxys[1,*,i])]
    ENDFOR
    tmplons=tmplons[1:*]
    tmplats=tmplats[1:*]
    WINDOW,1
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
  
  
  ;find adjacent stations
  dists=DBLARR(nsit)
  p_lls=DBLARR(2,nsit)
  dists_fault=DBLARR(nsit)
  vInPfs=INTARR(nsit)
  vInPfs(*)=-1
  step_si=CEIL(nsit/10.)
  FOR si=0ULL, nsit-1 DO BEGIN
    IF ( si MOD step_si EQ 0 ) THEN BEGIN
      PRINT,STRING((si+1d0)/nsit*100,format='(f6.2)'),'% done ...'
    ENDIF
    
    c1=lls[*,si]
    loni=c1[0]
    lati=c1[1]
    IF loni GT xpmax OR loni LT xpmin OR lati GT ypmax OR lati LT ypmin THEN CONTINUE
    
    
    FOR pi=0,np-1 DO BEGIN
    
      xys=REFORM(pxys[*,*,pi])
      
      IF TOTAL(FINITE(xys)) NE 4 THEN BEGIN
        ;PRINT,'['+prog+']WARNING:error for profile '+STRTRIM(pi+1)+'! Skipped.'
        CONTINUE
      ENDIF
      ;      IF MIN(xys[0,*]) GT xmax || MAX(xys[0,*]) LT xmin || MIN(xys[1,*]) GT ymax || $
      ;        MAX(xys[1,*]) LT ymin THEN BEGIN  ;#no intersection between profile and velocity field
      ;        ;PRINT,'['+prog+']WARNING:no intersection between profile '+STRTRIM(pi+1,2)+' and velocity field!'
      ;        ;      print,MIN(xys[0,*]) GT xmax , MAX(xys[0,*]) LT xmin , MIN(xys[1,*]) GT ymax , $
      ;        ;      MAX(xys[1,*]) LT ymin
      ;        ;      stop
      ;        CONTINUE
      ;      ENDIF
      ;STOP
      
      ;
      IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
        WINDOW,1,xsize=1000,ysize=600,title='Profile '+STRING(pi+1,format='(i2)'),/pixmap
        DEVICE,decomposed=1
        !p.MULTI=[0,2,2,0,0]
        inds=LINDGEN(N_ELEMENTS(lls[0,*])/10)*10
        PLOT,lls[0,inds],lls[1,inds],psym=1,background='ffffff'x,color='0'x, $
          title='Sites Overview Map', $
          /nodata, $
          /ynozero;,/iso
        OPLOT,lls[0,inds],lls[1,inds],psym=1,color='aaaaaa'x
        IF N_ELEMENTS(ffile) GT 0 && ffile NE '' THEN BEGIN
          OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='0000ff'x
        ;stop
        ENDIF
      ENDIF
      
      
      
      xy1=xys[*,0]
      xy2=xys[*,1]
      
      a1=xy1  ;the starting and ending vertices of the profile
      b1=xy2
      
      
      POINT_PERP_LINE,  a1,b1, c1, d1
      tmp=MAP_2POINTS(c1[0],c1[1],d1[0],d1[1],/meters)
      d_v2p=tmp*1d-3 ;convert meter to km
      IF d_v2p GT maxdist THEN CONTINUE
      
      vInPfs(si)=pi
      
      ;set the direction of point: negative for west of profile
      d_v2p=d_v2p*(ABS(c1[0]-d1[0])/(c1[0]-d1[0]))  ;in km
      dists[si]=d_v2p
      p_lls[*,si]=d1
      
      ;distance from gps site to fault line
      tmp=MAP_2POINTS(pfxys[0,pi],pfxys[1,pi],d1[0],d1[1],/meter)
      d_v2f=tmp*1d-3 ;convert meter to km
      dists_fault[si]=d_v2f*(d1[0]-pfxys[0,pi])/ABS(d1[0]-pfxys[0,pi])
      
      ;STOP
      IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN OPLOT,[xy1[0],xy2[0]], [xy1[1],xy2[1]], color='ff0000'x
      
      
      
      
      ;      ;stop
      ;      IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
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
      ;
      ;      IF i1[0] LT MIN([a1[0],b1[0]]) || i1[0] GT MAX([a1[0],b1[0]]) THEN BEGIN  ;outside profile
      ;        ;stop
      ;        CONTINUE
      ;      ENDIF
      ;
      
      GOTO, next_site
    ENDFOR ;end-of-loop-profiles
    ;STOP
    Next_Site:
  ENDFOR ;end-of-loop-velocity-points
  PRINT,STRING((si+1d0)/nsit*100,format='(f6.2)'),'% done ...'
  ;STOP
  
  
  FOR pi=0,np-1 DO BEGIN
    pos=WHERE(vInPfs EQ pi)
    IF pos[0] EQ -1 THEN BEGIN  ;no velocity
      PRINT,'['+prog+']WARNING:no velocity!'
      CONTINUE
    ENDIF
    
    nv_out=N_ELEMENTS(pos)
    
    d_v2fs=dists_fault(pos)
    ind=SORT(d_v2fs)
    
    
    xys=REFORM(pxys[*,*,pi])
    xy1=xys[*,0]
    xy2=xys[*,1]
    a1=xy1  ;the starting and ending vertices of the profile
    b1=xy2
    XY3=pfxys(*,pi)
    
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i03)')+'_vel.psxy'
    PRINT,'[]INFO:writing '+ofile+' ...'
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
      PRINTF,fid,sites[pos[ind[j]]],p_lls[*,pos[ind[j]]],dists[pos[ind[j]]],0,$
        0,0,0, $
        lls[*,pos[ind[j]]], $
        dists_fault[pos[ind[j]]], $
        vels[pos[ind[j]]], veles[pos[ind[j]]],   $
        format='(1x,a16,1x,2f10.3,1x,f10.2,1x,2f10.2,1x,2f10.2,1x,2f10.3,1x,f13.6,1x,2(1x,f10.2))'
    ENDFOR
    FREE_LUN,fid
    
    
    IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
      ;WINDOW,4
      WSET,1
      ;!p.MULTI=[1,2,2]
      !p.MULTI=[1,1,2,0,1]
      ;yrange=[-20,20]
      ;yrange=[-50,50]
      ;yrange=[-12,12]
      
      PLOT,lls_used[0,ind],vel_los_all[ind],background='ffffff'x,color='0'x, $
        title='LOS Velocities (mm/yr) '+STRING(pi+1,format='(i2)'), $
        /ynozero,psym=4,/nodata,yrange=yrange
      OPLOT,lls_used[0,ind],vel_los_all[ind],color='0000ff'x, psym=5
      OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
      FOR j=0ULL,N_ELEMENTS(ind)-1 DO BEGIN
        OPLOT,[lls_used[0,ind[j]], lls_used[0,ind[j]] ], $
          [vel_los_all[ind[j]]+ABS(vele_los_all[ind[j]]),vel_los_all[ind[j]]-ABS(vele_los_all[ind[j]]) ], $
          color='0000ff'x,thick=2
      ENDFOR
      !p.MULTI=-1
      ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i03)')+'_vel.jpg'
      WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
      
    ENDIF
    
    
  ENDFOR
  
  
  PRINT,'['+prog+']Normal end.'
END