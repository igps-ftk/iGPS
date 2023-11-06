FUNCTION GET_INTERSECT_POINT_BETWEEN_FAULT_AND_PROFILE, xys_fvec, a1,b1
  npt=N_ELEMENTS(xys_fvec[0,*])
  FOR pi=0, npt-2 DO BEGIN
    x1=xys_fvec[*,pi]
    y1=xys_fvec[*,pi+1]
    rate=(y1[1]-x1[1])/(y1[0]-x1[0])
    POINT_CROSS_LINE, a1,b1, x1, rate, c1
    OPLOT,[x1[0],y1[0]],[x1[1],y1[1]],color='ff0000'x
    OPLOT,[x1[0],c1[0]],[x1[1],c1[1]],color='ff0000'x,linestyle=2
    IF c1[0] GT MAX([x1[0],y1[0]]) || c1[0] LT MIN([x1[0],y1[0]]) || $
      c1[1] GT MAX([x1[1],y1[1]]) || c1[1] LT MIN([x1[1],y1[1]]) THEN CONTINUE
    RETURN,c1
  ENDFOR
  RETURN,REPLICATE(!values.D_NAN,2)
END

PRO VEL_PROFILES, vfile, $  ;velocity file (in varied formats)
    opath, $   ;output path
    ffile=ffile,  $ ;(if exist) fault trace (only one polyline in GMT format)
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
    
  ENDIF
  
  IF FILE_TEST(opath,/directory) NE 1 THEN FILE_MKDIR,opath
  
  IF N_ELEMENTS(out_plot) EQ 0 THEN out_plot=0
  IF N_ELEMENTS(inputfmt) EQ 0 THEN inputfmt=0
  IF N_ELEMENTS(auto_strike) EQ 0 THEN auto_strike=2
  IF N_ELEMENTS(spacing_profile) EQ 0 THEN spacing_profile=30  ;in km (distance between two neighboring profiles)
  IF N_ELEMENTS(length_profile) EQ 0 THEN length_profile=900  ;in km (the total length of the profile generated automatically by iGPS)
  
  
  np=0
  pxys=-9999d0
  IF N_ELEMENTS(search_radius) EQ 0 THEN search_radius=95d0 ;maximum searching distance beside the profile line, in kilometers
  ;search_radius=100d0
  
  
  ;read velocity field
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
      ;sites=strmids(lines1[0,*],0,4)
      sites=REFORM(lines1[0,*])
      lls=DOUBLE(lines1[1:2,*])
      vels=DOUBLE(lines1[1:*,*])  ;stop
      nsit=N_ELEMENTS(sites)
    END
    1: BEGIN  ;
      ;read psvelo velocity field
      ;stop
      lines=read_txt(vfile)
      pos=WHERE(strmids(lines,0,1) EQ ' ')
      lines1=REFORM(lines[pos])
      ;stop
      lines1=str_lines2arr(lines1)
      ;sites=strmids(lines1[7,*],0,4)
      sites=REFORM(lines1[7,*])
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
      ;sites=strmids(lines1[0,*],0,4)
      sites=REFORM(lines1[0,*])
      lls=DOUBLE(lines1[1:2,*])
      vels=DOUBLE(lines1[[1,2,9,10,5,6,11],*])
      ;stop
      nsit=N_ELEMENTS(sites)
    END
    3: BEGIN  ;
      ;  Long.    Lat.      Ve       Vn     Se     Sn    Cne  Sta.      Source
      ; 74.336  39.842   0.170   13.790  0.560  0.503  0.000  I089  This_study
      ; 78.680  29.848  10.840   32.656  1.550  1.450 -0.002  LAN2  Kreemer et al. [2014] from Banerjee et al. [2008]
      lines=read_txt(vfile)
      lines1=STRARR(8,N_ELEMENTS(lines))
      FOR li=0ull, N_ELEMENTS(lines)-1 DO BEGIN
        line_p=STRSPLIT(lines[li],/extract)
        lines1[*,li]=line_p[0:7]
      ENDFOR
      ;sites=strmids(lines1[7,*],0,4)
      sites=REFORM(lines1[7,*])
      lls=DOUBLE(lines1[0:1,*])
      vels=DOUBLE(lines1[[0,1,3,5,2,4,6],*])
      ;stop
      nsit=N_ELEMENTS(sites)
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
  
  ;stop
  
  ;read profiles
  ;
  IF N_ELEMENTS(pfile) GT 0 && pfile NE '' THEN BEGIN
    lines=read_txt(pfile)
    np=0
    FOR li=0, N_ELEMENTS(lines)-1 DO BEGIN
      line=STRTRIM(lines[li],2)
      IF STRMID(line,0,1) EQ '>' THEN BEGIN
        np=np+1
        xyi=REPLICATE(-9999d0,2,2)
        CONTINUE
      ENDIF
      IF line EQ '' THEN BEGIN  ;skip blank lines
        CONTINUE
      ENDIF
      
      
      line_p=STRSPLIT(line,/extract)
      IF xyi[0,0] EQ -9999d0 THEN BEGIN
      
        ;starting point
        xyi[*,0]=DOUBLE(line_p)
      ENDIF ELSE BEGIN
      
        ;ending point
        xyi[*,1]=DOUBLE(line_p)
        
        IF np EQ 1 THEN BEGIN
          pxys=xyi
        ENDIF ELSE BEGIN
          pxys=[[[pxys]],[[xyi]]]
        ENDELSE
        
      ENDELSE
      
    ENDFOR
  ;STOP
  ENDIF ELSE BEGIN
  
    ofile=opath+PATH_SEP()+'profiles__map.jpg'
    pfile=opath+PATH_SEP()+'profiles_auto.psxy'
    ;STOP
    ;generate profiles lines by calling PROFILE_LINES_AUTO program
    PROFILE_LINES_AUTO, xys_fvec,  oxys=oxys, spacing=spacing_profile, auto_strike=auto_strike, length_profile=length_profile, ofile=pfile
    ;PROFILE_LINES_AUTO, xys_fvec,  oxys=oxys, spacing=2, auto_strike=auto_strike, length_profile=400
    ;PROFILE_LINES_AUTO, xys_fvec,  oxys=oxys, spacing=2, auto_strike=auto_strike, length_profile=40
    
    pxys=oxys
  ;STOP
  ENDELSE
  
  
  np=N_ELEMENTS(pxys[0,0,*])
  
  ;stop
  ;loop for each profile
  
  FOR pi=0,np-1 DO BEGIN  ;loop for each profile
  
    WINDOW,1,xsize=1500,ysize=900,title='Profile '+STRING(pi+1,format='(i03)'),/pixmap
    DEVICE,decomposed=1
    !p.MULTI=[0,2,2]
    PLOT,vels[0,*],vels[1,*],psym=1,background='ffffff'x,color='0'x, $
      title='Sites Overview Map', $
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
    
    OPLOT,[xy1[0],xy2[0]], [xy1[1],xy2[1]], color='ff0000'x
    
    
    
    ;get the intersect point of fault lines (xys_fvec) and current profile (a1,b1)
    IF N_ELEMENTS(ffile) NE 0 && ffile NE '' THEN BEGIN
      xy3=GET_INTERSECT_POINT_BETWEEN_FAULT_AND_PROFILE(xys_fvec,a1,b1)
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
    FOR si=0, nsit-1 DO BEGIN
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
    
    ofile=opath+PATH_SEP()+'profile_'+STRING(pi+1,format='(i03)')+'_vel.psxy'
    
    odata=DBLARR(21,N_ELEMENTS(ind))
    odata[[3,5,7,14,15],*]=-999.99d0
    FOR j=0, N_ELEMENTS(ind)-1 DO BEGIN
      ;      ;convert gps velocity to insar los direction
      ;      ;descending
      ;      enu_j=[vels[[4,2],pos[ind[j]]], 0d0]
      ;      enu_sig_j=[vels[[5,3],pos[ind[j]]], 0d0]
      ;      vlos_des=sar_enu2los(enu_j)
      ;      vlos_asc=sar_enu2los(enu_j, alpha=(-13+360d0)*!dpi/180d0 )
      ;      vlos_sig_des=sar_enu2los(enu_sig_j)
      ;      vlos_sig_asc=sar_enu2los(enu_sig_j, alpha=(-13+360d0)*!dpi/180d0 )
    
    
      odata[*,j]=[p_lls[*,pos[ind[j]]],dists[pos[ind[j]]],  $
        vel_along_all[ind[j]],vele_along_all[ind[j]],vel_tang_all[ind[j]],vele_tang_all[ind[j]], $
        vel_up_all[ind[j]],vele_up_all[ind[j]], lls[*,pos[ind[j]]], $
        dists_fault[pos[ind[j]]], $
        -999.99d0,0,  $
        vels[[4,2,5,3],pos[ind[j]]] $
        , 0,0,0 ]
    ENDFOR
    
    WRITE_VEL_PROFILE, ofile $
      , odata $
      , sites=sites[pos[ind]] $
      , fa_xys=xys_fvec  $
      , pf_xys=[[a1],[b1]]  $
      , fa_pf_xy=xy3 $
      , headers=[vfile,ffile,pfile]
      
    return
      
    ;    OPENW,fid,ofile,/get_lun
    ;    WRITE_SYS_INFO,fid,prog=prog,src=[vfile,pfile],user=user
    ;    ;output profile vertex
    ;    PRINTF,fid,a1,format='("# PSXY_PROFILE",2f10.3)'
    ;    PRINTF,fid,b1,format='("# PSXY_PROFILE",2f10.3)'
    ;    PRINTF,fid,XY3,format='("# PSXY_FAULT_PROFILE_INTERSECT",2f10.3)'
    ;    FOR j=0,N_ELEMENTS(xys_fvec[0,*])-1 DO BEGIN
    ;      PRINTF,fid,xys_fvec[*,j],format='("# PSXY_FAULT_TRACE",2f10.3)'
    ;    ENDFOR
    ;    ;output stations
    ;    PRINTF,fid,'site','p_long','p_lati','p_dist','v_along','ve_along','v_tang','ve_tang',$
    ;      'v_up','ve_up','long','lati','dist_to_fault', $
    ;      've','vn','ve_sig','vn_sig',  $
    ;      'vlos_d','vlos_d_sig','vlos_a','vlos_a_sig',  $
    ;      format='("*",a4,1x,2a10,1x,a10,1x,2a10,1x,2a10,1x,2a10,1x,2a10,1x,a13, 1x,4(1x,a10),1x,4(1x,a10))'
    ;    FOR j=0, N_ELEMENTS(ind)-1 DO BEGIN
    ;      ;convert gps velocity to insar los direction
    ;      ;descending
    ;      enu_j=[vels[[4,2],pos[ind[j]]], 0d0]
    ;      enu_sig_j=[vels[[5,3],pos[ind[j]]], 0d0]
    ;      vlos_des=sar_enu2los(enu_j)
    ;      vlos_asc=sar_enu2los(enu_j, alpha=(-13+360d0)*!dpi/180d0 )
    ;      vlos_sig_des=sar_enu2los(enu_sig_j)
    ;      vlos_sig_asc=sar_enu2los(enu_sig_j, alpha=(-13+360d0)*!dpi/180d0 )
    ;
    ;
    ;      PRINTF,fid,sites[pos[ind[j]]],p_lls[*,pos[ind[j]]],dists[pos[ind[j]]],vel_along_all[ind[j]],$
    ;        vele_along_all[ind[j]],vel_tang_all[ind[j]],vele_tang_all[ind[j]], $
    ;        vel_up_all[ind[j]],vele_up_all[ind[j]], lls[*,pos[ind[j]]], $
    ;        dists_fault[pos[ind[j]]], $
    ;        vels[[4,2,5,3],pos[ind[j]]], $
    ;        vlos_des,vlos_sig_des,vlos_asc,vlos_sig_asc,$
    ;        format='(1x,a4,1x,2f10.3,1x,f10.2,1x,2f10.2,1x,2f10.2,1x,2f10.2,1x,2f10.3,1x,f13.6,1x,4(1x,f10.3),1x,4(1x,f10.3))'
    ;    ENDFOR
    ;    FREE_LUN,fid
      
    ;STOP
    ;WINDOW,4
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
  ;BREAK
  ENDFOR
  
END