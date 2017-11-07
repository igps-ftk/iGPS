
PRO SAR_LOS_PROFILES_XYZ, vfile=vfile, $  ;velocity file (in varied formats)
    pfile=pfile,  $   ;profiles file (two-end-points lines)
    opath=opath, $   ;output path
    ffile=ffile,  $ ;(if exist) fault trace (only one polyline in GMT format)
    flon=flon, $  ;fault longitude (if ffile is not present && it it not given, use the middle point of the profile)
    flat=flat, $     ;fault latitude (if ffile is not present && it is not given, use the middle point of the profile)
    out_plot=out_plot,  $ ;whether output temporary plots
    inputfmt=inputfmt,  $ ;input velocity format (0-  ; 1-psvelo)
    ;0 (default):1x site lon lat vn sign ve sige cne
    ;1 (psvelo): 1x lon lat ve vn sige sign cne site
    maxdist=maxdist  ; the maximum distance away from the line to create the velocity profile
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  ;readme first
  ;this program needs files parameters: 3 inputs and 1 output
  ;inputs:
  ;  vfile - the velocity file in XYZ format;
  ;  ffile - fault file (continuous polyline in GMT psxy format)
  ;  pfile - profiles file (two-points lines in GMT psxy format)
  ;
  ;output
  ;  opath - output directory
  ;
  ;If available, the DEM along the profiles will be generated based upon ETOPO1 grid.
  ;
  ;(c)Copyright by Yunfeng Tian. 2016-2017.
  ;This program is part of iGPS package (https://www.ngs.noaa.gov/gps-toolbox/Tian.htm).
  
  ;pre-defined inline parameters
  ;
  ;sar_theta - satellite looking angle
  sar_theta=33d0*!dpi/180d0
  ;
  ;sar_alpha - azimuth angle of satellite orbit
  sar_alpha=193d0*!dpi/180d0  ; -167+360 for descending orbit
  sar_alpha=(-13+360d0)*!dpi/180d0 ; for ascending orbit
  sar_alpha=(-13+0d0)*!dpi/180d0 ; for ascending orbit
  ;
  ;sar_beta - strike of fault (clockwise from north)
  ;sar_beta=?
  
  ;//begin-of-test-parameters
  IF N_ELEMENTS(vfile) EQ 0 || N_ELEMENTS(ffile) EQ 0 || N_ELEMENTS(pfile) EQ 0 || $
    N_ELEMENTS(opath) EQ 0 THEN BEGIN
    ;
    inputfmt=3  ;insar xyz
    
    ;    fa='ydgl2'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\profile.ydgl2\p'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\profile.ydgl2\p3'
    ;
    fa='bengco'
    opath='D:\gsar\des\gyaringco3\des_F1\SBAS\bc\p'
    opath='D:\gsar\des\envisat.d.bengco.t405f2979\SBAS\p'
    opath='D:\gsar\des\envisat.d.t176f2979.bengco\SBAS\p'
    ;    opath='D:\gsar\des\envisat.d.t176f2979.bengco\SBAS\p'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\profile.bengco\p'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\profile.bengco\pta2'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\Bengco_T150\p'
    ;    opath='D:\gsar\des\envisat.d.bengco.t405f2979\SBAS2\p'
    ;    opath='D:\gsar\des\envisat.d.bengco.t405f2979\SBAS3\p'
    
    ;    fa='dongqiao2'
    ;    opath='D:\gsar\des\envisat.d.t176f2961.bengco\SBAS\p'
    ;    opath='D:\gsar\des\envisat.d.t448f2961.bengco\SBAS\p'
    ;        opath='D:\gsar\des\gyaringco3\des_F1\SBAS\p'
    ;        opath='D:\gsar\asc\bengco20161228\asc_F1\SBAS3\p\'
    ;        opath='D:\gsar\asc\dongqiao\asc_F2\SBAS\p'
    ;        opath='D:\gsar\des\envisat.d.t176f2961.bengco\SBAS\p'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\profile.dongqiao2\pta2'
    ;    ;;    ;
    ;        fa='gyaringco'
    ;        opath='\\gpsac5\root\g5c\tianyf\envisat.d.gyaringco.f491f2979\SBAS\p'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\profile.gyaringco\p'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\T114\p2'
    ;    opath='D:\Papers\paper.bengco\figure\profile.gyaringco.des_F2.1\p'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\descending\S1_GyaringCoFault_T48\p'
    
    ;        fa='riganpei'
    ;        opath='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\profile.riganpei\p'
    ;    ;
    ;    fa='jiali'
    ;    opath='D:\gsar\des\envisat.d.t133f2979.bengco\SBAS\p'
    ;        opath='D:\gsar\asc\alos.f600p496.jiali\SBAS\p'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\profile.jiali\p'
    ;    opath='D:\Papers\paper.jiali\figure\profile.jiali.asc\p'
    ;;    opath='D:\Papers\paper.jiali\figure\profile.jiali.des\p'
    ;    ;
    ;    fa='gulu'
    ;    opath='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\profile.gulu\p'
    
    ;    fa='altyntagh'
    ;    ;    opath='D:\Papers\paper.bengco\figure\profile.altyntagh3.asc\p'
    ;    ;    opath='D:\Papers\paper.bengco\figure\profile.altyntagh3.des\p'
    ;    opath='D:\gsar\asc\altyntagh2\asc_F1\SBAS\p'
    ;    opath='D:\gsar\asc\altyntagh3\asc_F1\SBAS\p'
    ;    opath='\\gpsac4\root\g4c\gsar\altyntagh3\asc_F1\SBAS\p'
    ;    opath='D:\gsar\asc\alos.yutian\SBAS\p'
    
    ;    fa='xianshuihe'
    ;    opath='D:\gsar\asc\alos.xianshuihe\SBAS\p'
    ;    ;    ;    opath='D:\gsar\des\xianshuihe4\p'
    ;    ;    ;    opath='D:\gsar\asc\xianshuihe1\p'
    ;    ;    opath='\\homew\root\g5c\tianyf\xianshuihe4\des_F1\SBAS\p'
    ;    opath='\\gpsac5\root\g5c\tianyf\xianshuihe4\des_F2\SBAS\p'
    ;    opath='\\gpsac5\root\g5c\tianyf\xianshuihe4\des_F3\SBAS.1\p'
    
    ;        fa='kunlun'
    ;        opath='D:\gsar\asc\kunlun4\asc_F3\SBAS\p'
    ;    opath='D:\gsar\des\kunlun3\p'
    ;    opath='D:\gsar\des\kunlun3\des_F2.2017apr18.ac5.3\SBAS\p'
    ;    opath='D:\gsar\des\kunlun3\des_F2.2017apr18.ac5.2\SBAS.4\p'
    ;    opath='D:\gsar\des\kunlun3\des_F2.2017apr18.ac5\SBAS.2\p'
    
    ;    fa='lenglongling'
    ;    opath='D:\gsar\asc\lenglongling1\p'
    ;    opath='D:\gsar\des\lenglongling2\p'
    ;    opath='\\gpsac4\root\g4c\gsar\lenglongling1\asc_F3\SBAS\p'
    
    ;        fa='cona_east'
    ;        opath='D:\gsar\des\envisat.d.t405f2961.bengco\SBAS\p'
    ;    opath='D:\gsar\asc\alos.f630p496.anduo\SBAS\p'
    
    ;above settings
    orbtyp=''
    
    ;mean LOS rates derived by TYF
    orbtyp='a'  ;ascending
    ;
    orbtyp='d' ;descending
    ;
    ;stop
    
    CASE orbtyp OF
      'a': BEGIN; ascending orbit
        vfile='D:\Papers\paper.bengco\figure\rate.map\rates-asc.xyz'
        ;vfile='D:\gsar\asc\riganpei2.asc\asc_F1\SBAS\out\rates.xyz'
        ;vfile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\T114\out\rates.xyz'
        vfile='D:\Papers\paper.jiali\figure\3.velocity.map\rates-asc.xyz'
        vfile='D:\gsar\asc\xianshuihe1\ratemap\rates-asc.xyz'
        vfile='D:\gsar\asc\altyntagh2\asc_F1\SBAS\out\rates.xyz'
        vfile='D:\gsar\asc\altyntagh3\asc_F1\SBAS\out\rates.xyz'
        vfile='\\gpsac4\root\g4c\gsar\altyntagh3\asc_F1\SBAS\out\rates.xyz'
        vfile='D:\gsar\asc\lenglongling1\ratemap\rates-asc.xyz'
        vfile='\\gpsac4\root\g4c\gsar\lenglongling1\asc_F3\SBAS\out\rates.xyz'
        vfile='D:\gsar\asc\alos.yutian\SBAS\out\rates.xyz'
        vfile='D:\gsar\asc\alos.xianshuihe\SBAS\vel_ll.xyz'
        vfile='\\gpsac4\root\g4b\tianyf\alos.f600p496.jiali\SBAS\vel_ll.xyz'
        vfile='D:\gsar\asc\alos.f630p496.anduo\SBAS\vel_ll.xyz'
        vfile='D:\gsar\asc\bengco20161228\asc_F1\SBAS3\out\rates.xyz'
        vfile='D:\gsar\asc\dongqiao\asc_F2\SBAS\out\rates.xyz'
        vfile='D:\gsar\asc\kunlun4\asc_F3\SBAS\vel_ll.xyz'
      ;opath='D:\Papers\paper.bengco\figure\profile.altyntagh2.asc_Fs.1\p'
      ;
      ;vfile='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\rate.los.asc\rate_asc.xyz'
      ;fa='bengco'
      ;opath='D:\Papers\paper.bengco\figure\profile.asc\bengco\p'
      ;
      END
      'd': BEGIN
        vfile='D:\Papers\paper.bengco\figure\rate.map\rates-des.xyz'
        vfile='D:\Papers\paper.jiali\figure\3.velocity.map\rates-des.xyz'
        vfile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\descending\S1_GyaringCoFault_T48\pp\tmp-rate.xyz'
        vfile='D:\ICD\projects\DirectorFund\Application.2012\InSAR\2016\from.ZhangQingyun\Bengco_T150\pp\tmp-rate.xyz'
        vfile='D:\gsar\des\xianshuihe4\ratemap\rates-des.xyz'
        vfile='D:\gsar\des\kunlun3\ratemap\rates-des.xyz'
        vfile='D:\gsar\des\kunlun3\des_F2.2017apr18.ac5.2\SBAS\out\rates.xyz'
        vfile='D:\gsar\des\kunlun3\des_F2.2017apr18.ac5.3\SBAS\out\rates.xyz'
        vfile='D:\gsar\des\kunlun3\des_F2.2017apr18.ac5.2\SBAS.4\out\rates.xyz'
        vfile='D:\gsar\des\kunlun3\des_F2.2017apr18.ac5.4\SBAS.2\out\rates.xyz'
        vfile='D:\gsar\des\kunlun3\des_F2.2017apr18.ac5\SBAS.2\out\rates.xyz'
        vfile='\\homew\root\g5c\tianyf\xianshuihe4\des_F1\SBAS\out\rates.xyz'
        vfile='\\gpsac5\root\g5c\tianyf\xianshuihe4\des_F2\SBAS\out\rates.xyz'
        vfile='\\gpsac5\root\g5c\tianyf\xianshuihe4\des_F3\SBAS.1\out\rates.xyz'
        vfile='D:\gsar\des\lenglongling2\ratemap\rates-des.xyz'
        vfile='D:\gsar\des\envisat.d.bengco.t405f2979\SBAS2\out\rates.xyz'
        vfile='D:\gsar\des\envisat.d.bengco.t405f2979\SBAS3\vel_ll.xyz'
        vfile='\\gpsac5\root\g5c\tianyf\envisat.d.gyaringco.f491f2979\SBAS\out\rates.xyz'
        vfile='D:\gsar\des\envisat.d.t176f2961.bengco\SBAS\out\rates.xyz'
        vfile='D:\gsar\des\envisat.d.t176f2979.bengco\SBAS\vel_ll.xyz'
        vfile='D:\gsar\des\gyaringco3\des_F1\SBAS\out\rates.xyz'
        vfile='D:\gsar\des\envisat.d.bengco.t405f2979\SBAS\vel_ll.xyz'
        vfile='D:\gsar\des\envisat.d.t176f2979.bengco\SBAS\vel_ll.xyz'
      ;        vfile='D:\gsar\des\envisat.d.t176f2961.bengco\SBAS\vel_ll.xyz'
      ;        vfile='D:\gsar\des\envisat.d.t133f2979.bengco\SBAS\vel_ll.xyz'
      ;        vfile='D:\gsar\des\envisat.d.t405f2961.bengco\SBAS\vel_ll.xyz'
      ;        vfile='D:\gsar\des\envisat.d.t448f2961.bengco\SBAS\vel_ll.xyz'
      ;vfile='D:\gsar\des\dangxiong2\vel_ll.xyz'
      ;opath='D:\Papers\paper.bengco\figure\profile.altyntagh.des_Fs\p'
      ;vfile='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\rate.los.asc\rate_des.xyz'
      ;
      END
      ELSE: BEGIN
      ;do nothing
      END
    ENDCASE
    
    
    PROFILE_NAME2VECTORFILE,   $
      fa,   $ ;input, fault name
      ffile=ffile,  $ ;output, fault file
      pfile=pfile ;output, profile file
      
  ;pfile='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\vector\profiles.psxy'
      
  ENDIF ;\\end-of-test-parameters
  
  IF FILE_TEST(opath,/directory) NE 1 THEN FILE_MKDIR,opath
  IF N_ELEMENTS(out_plot) EQ 0 THEN out_plot=0
  IF N_ELEMENTS(inputfmt) EQ 0 THEN inputfmt=3
  IF N_ELEMENTS(maxdis) EQ 0 THEN BEGIN
    MaxDist=55d0 ;maximum searching distance beside the profile line, in kilometers. for gps, this value may be quite large
    MaxDist=5d0  ;for InSAR LOS grids, set it to a small one.
  ENDIF
  
  ;stop
  
  ;Generate DEM profiles (optional; Needs ENVI; if not desired, comment out below lines)
  demfile='C:\dem.gmt\ETOPO1_Bed_g_geotiff\ETOPO1_Bed_g_geotiff.tif'
  ;stop
  IF FILE_TEST(demfile,/regular) THEN BEGIN
    ENVI,/restore_base_save_files
    ENVI_BATCH_INIT
    DEM_GRID_EXTRACT_PROFILE, demfile,  $  ;DEM file (ENVI raw image format; with map projection)
      pfile,  $  ;profile line (two vertices; extended GMT psxy format)
      opath,  $  ;output path
      ffile=ffile  ;fault polyline (extended GMT psxy format)
    ENVI_BATCH_EXIT
  ;RETURN
  ENDIF
  ;STOP
  
  ;read profiles
  READ_PSXY,   $
    pfile,   $ ;input file
    region=pfs,   $ ;x,y coorinates of each polygons (pointer type)
    nps=nps, $  ;number of point pairs for each polygon
    count=np,  $ ;number of polygons
    names=names   ;region names (if exist)
  ;
  pxys=DBLARR(2,2,np)
  FOR i=0,np-1 DO BEGIN
    IF nps[i] NE 2 THEN STOP
    pxys[*,*,i]=*pfs[i]
  ;STOP
  ENDFOR
  pxys2=pxys
  
  ;read velocity field (various formats: 0- ; 1-psvelo; 2-qoca map; 3-xyz)
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
    3: BEGIN ;X          Y         Z
      ;89.892319  30.813284  2.033756
      ;89.892669  30.813284  2.815592
      ;89.893019  30.813284  2.838231
      IF FILE_TEST(VFILE+'.sav') EQ 0 THEN BEGIN
        ;stop
        ;file_info,vfile
        ;READ_COLS,VFILE,DATA=DATA
        lines=read_txt(vfile,comment='~ ')
        data=DOUBLE(str_lines2arr(lines))
        SAVE,DATA,FILENAME=VFILE+'.sav'
      ENDIF
      RESTORE,VFILE+'.sav'  ;!!! If file changed later, delete the .sav file and run this program again.
      ; Otherwise, the old file will be used.
      LLS=DATA[0:1,*]
      VELS=REFORM(DATA[2,*])
      NSIT=N_ELEMENTS(VELS)
      SITES=STRING(LINDGEN(NSIT)+1,FORMAT='(I)')
    ;STOP
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
  ;stop
  ENDIF
  
  ;plot fault line and profiles
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
  PLOT,tmplons,tmplats,background='ffffff'x,color='0'x,/nodata,/ynozero,  $
    title='Fault and Profiles',/iso  ;,xstyle=1,ystyle=1
  FOR i=0,np-1 DO BEGIN
    lons=REFORM((*pfs[i])[0,*])
    lats=REFORM((*pfs[i])[1,*])
    OPLOT,lons,lats,color='ff0000'x,psym=-5
    XYOUTS,lons[0],lats[0],STRTRIM(i+1,2),color='ff0000'x
  ENDFOR
  OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='0000ff'x
  ofile=opath+PATH_SEP()+'profiles__map.jpg'
  WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
  ;STOP
  
  ;loop for each profile
  FOR pi=0,np-1 DO BEGIN
  
    WINDOW,1,xsize=1500,ysize=900,title='Profile '+STRING(pi+1,format='(i2)');,/pixmap
    DEVICE,decomposed=1
    inds=LINDGEN(N_ELEMENTS(lls[0,*])/10)*10
    !p.MULTI=[0,3,2]
    PLOT,lls[0,inds],lls[1,inds],psym=1,background='ffffff'x,color='0'x, $
      title='Sites Overview Map', $
      /nodata, $
      /ynozero;,/iso
    OPLOT,lls[0,inds],lls[1,inds],psym=1,color='aaaaaa'x
    IF N_ELEMENTS(ffile) GT 0 && ffile NE '' THEN BEGIN
      OPLOT,xys_fvec[0,*],xys_fvec[1,*],psym='-4',color='0000ff'x
    ;stop
    ENDIF
    
    xys=REFORM(pxys[*,*,pi])
    xy1=xys[*,0]
    xy2=xys[*,1]
    
    a1=xy1  ;the starting and ending vertices of the profile
    b1=xy2
    
    ;stop
    OPLOT,[xy1[0],xy2[0]], [xy1[1],xy2[1]], color='ff0000'x
    
    ;get the intersect point of fault lines (xys_fvec) and current profile (a1,b1)
    IF N_ELEMENTS(ffile) NE 0 && ffile NE '' THEN BEGIN
      xy3=INTERSECT_BETWEEN_POLYLINE_AND_LINE(xys_fvec,a1,b1,beta=beta,x1=x1,y1=y1)
      ;if no intersection between profile and fault line, it returns null result.
      IF FINITE(xy3[0]) NE 1 THEN CONTINUE
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
    sar_beta=ATAN(tmp)+!dpi
    PRINT,'fault strike:',sar_beta*180/!dpi
    PLOTS,[x1[0],y1[0]],[x1[1],y1[1]],color='ffff00'x,thick=2,psym=-2
    ;stop
    
    ;get the azimuth, positive: from east, ccw(counter-clockwise)
    alpha=ATAN(b1[1]-a1[1], b1[0]-a1[0])
    IF alpha LT 0 THEN BEGIN
      alpha=alpha+!dpi
    ENDIF
    PRINT,'profile ',pi+1,' angle:',alpha*180d0/!dpi
    
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
      IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
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
      dists[si]=tmp*1d-3  ;in km
      p_lls[*,si]=i1
      
      ;distance from gps site to fault line
      tmp=MAP_2POINTS(xy3[0],xy3[1],i1[0],i1[1],/meter)
      dists_fault[si]=tmp*1d-3*(i1[0]-xy3[0])/ABS(i1[0]-xy3[0])
    ;stop
      
    ENDFOR
    
    pos=WHERE(dists GT 0 AND dists LE MaxDist)
    IF pos[0] EQ -1 THEN BEGIN  ;no velocity
      CONTINUE
    ENDIF
    ;stop
    WSET,1
    ;WINDOW,1,xsize=800,ysize=800,title='Map';,/pixmap
    ;DEVICE,decomposed=1
    lonmin=MIN([ a1[0],b1[0],REFORM(xys_fvec[0,*]) ],max=lonmax)
    latmin=MIN([ a1[1],b1[1],REFORM(xys_fvec[1,*]) ],max=latmax)
    PLOT,vels[0,*],vels[0,*],psym=1,background='ffffff'x,color='0'x, $
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
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i02)')+'_map.jpg'
    ;WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
    
    vel_along_all=DBLARR(N_ELEMENTS(pos))
    vel_tang_all=DBLARR(N_ELEMENTS(pos))
    vele_along_all=DBLARR(N_ELEMENTS(pos))
    vele_tang_all=DBLARR(N_ELEMENTS(pos))
    ;LOS velocity for InSAR
    vel_los_all=DBLARR(N_ELEMENTS(pos))
    vele_los_all=DBLARR(N_ELEMENTS(pos))
    FOR vi=0ULL, N_ELEMENTS(pos)-1 DO BEGIN
      vel_los_all[vi]=vels[POS[VI]]
      ;vel_along_all[vi]=vel_los_all[vi]*COS(sar_theta)
      ;vel_tang_all[vi]=vel_los_all[vi]/(SIN(sar_theta)*SIN(sar_alpha-sar_beta))
      
      ;for bengco fault
      ;sar_beta=120d0*!dpi/180d0
      
      ;stop
      INSAR_LOS2FAULT_PURE_STRIKE_SLIP, vel_los_all[vi], sar_beta,   $
        orbtyp=orbtyp,  $
        fx=fx, fy=fy, $
        ds=ds, dd=dd
      vel_along_all[vi]=fy
      vel_tang_all[vi]=fx
      ;vel_along_all[vi]=dd
      ;vel_tang_all[vi]=ds
      ;stop
      IF N_ELEMENTS(out_plot) NE 0 && Out_Plot EQ 1 THEN BEGIN
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
    
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i02)')+'_vel.psxy'
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
    PRINTF,fid,'site','p_long','p_lati','p_dist','v_along','ve_along','v_tang','ve_tang',$
      'long','lati','dist_to_fault','v_los','ve_los', $
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
    ;WINDOW,4
    WSET,1
    ;!p.MULTI=[1,2,2]
    yrange=[-20,20]
    yrange=[-12,12]
    
    PLOT,lls_used[0,ind],vel_along_all[ind],background='ffffff'x,color='0'x, $
      title='LOS Velocities (mm/yr) '+STRING(pi+1,format='(i2)'), $
      /ynozero,psym=4,/nodata,yrange=yrange
    OPLOT,lls_used[0,ind],vel_los_all[ind],color='0000ff'x, psym=5
    OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    FOR j=0ULL,N_ELEMENTS(ind)-1 DO BEGIN
      OPLOT,[lls_used[0,ind[j]], lls_used[0,ind[j]] ], $
        [vel_los_all[ind[j]]+ABS(vele_los_all[ind[j]]),vel_los_all[ind[j]]-ABS(vele_los_all[ind[j]]) ], $
        color='0000ff'x,thick=2
    ENDFOR
    
    PLOT,lls_used[0,ind],vel_along_all[ind],background='ffffff'x,color='0'x, $
      title='DEM (m) '+STRING(pi+1,format='(i2)'), $
      /ynozero,psym=4,/nodata,yrange=yrange
    OPLOT,lls_used[0,ind],vel_los_all[ind],color='0000ff'x, psym=5
    OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    FOR j=0ULL,N_ELEMENTS(ind)-1 DO BEGIN
      OPLOT,[lls_used[0,ind[j]], lls_used[0,ind[j]] ], $
        [vel_los_all[ind[j]]+ABS(vele_los_all[ind[j]]),vel_los_all[ind[j]]-ABS(vele_los_all[ind[j]]) ], $
        color='0000ff'x,thick=2
    ENDFOR
    
    PLOT,lls_used[0,ind],vel_along_all[ind],background='ffffff'x,color='0'x, $
      title='Velocities Along Profile '+STRING(pi+1,format='(i2)'), $
      /ynozero,psym=4,/nodata,yrange=yrange
    OPLOT,lls_used[0,ind],vel_along_all[ind],color='0000ff'x, psym=5
    OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    FOR j=0ULL,N_ELEMENTS(ind)-1 DO BEGIN
      OPLOT,[lls_used[0,ind[j]], lls_used[0,ind[j]] ], $
        [vel_along_all[ind[j]]+ABS(vele_along_all[ind[j]]),vel_along_all[ind[j]]-ABS(vele_along_all[ind[j]]) ], $
        color='0000ff'x,thick=2
    ENDFOR
    
    PLOT,lls_used[0,ind],vel_tang_all[ind],background='ffffff'x,color='0'x, $
      title='Velocities Tangent to Profile '+STRING(pi+1,format='(i2)'), $
      /ynozero,psym=5,/nodata,yrange=yrange
    OPLOT,lls_used[0,ind],vel_tang_all[ind],color='0000ff'x, psym=5
    OPLOT,[xy3[0],xy3[0]],[-1d3,1d3],linestyle=2,color='ff0000'x,thick=2
    FOR j=0ULL,N_ELEMENTS(ind)-1 DO BEGIN
      OPLOT,[lls_used[0,ind[j]], lls_used[0,ind[j]] ], $
        [vel_tang_all[ind[j]]+ABS(vele_tang_all[ind[j]]),vel_tang_all[ind[j]]-ABS(vele_tang_all[ind[j]]) ], $
        color='0000ff'x,thick=2
    ENDFOR
    
    !p.MULTI=-1
    
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i02)')+'_vel.jpg'
    WRITE_JPEG, ofile, TVRD(true=1),true=1,quality=100
  ;STOP
  ;PRINT,'a1:',a1
  ;PRINT,'b1:',b1
  ;RETURN
  ENDFOR
  
END