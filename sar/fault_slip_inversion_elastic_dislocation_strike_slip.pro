FUNCTION FAULT_SLIP_INVERSION_ELASTIC_DISLOCATION_STRIKE_SLIP,  $
    ;inputs
    d,  $ ;profile X-axis (distance to fault in km)
    x0,  $ ;velocities along the profile (in mm/yr)
    pxys=pxys,  $ ;profile x/y
    pxy3=pxy3,  $ ;intersection of profile and fault
    ;
    ;optional keywords
    theta=thetas, $ ;rotation of axes
    fts=ftss,  $ ;fault trace shifts
    fs=fss,  $  ;far-field fault slip rates
    ld=lds, $ ;fault locking depths
    xms_all=xms_all,  $
    ;
    ;outputs
    slip=slip,  $
    locking_depath=locking_depth, $
    d2s=d2, x2s=x2, $
    d3s=d3, x3s=x3, $
    d3_x_axis_x=d3_x_axis_x, d3_x_axis_y=d3_x_axis_y, $
    d3_y_axis_x=d3_y_axis_x, d3_y_axis_y=d3_y_axis_y, $
    ind2=ind2,  $
    out_fss=out_fss,  $
    out_ld=out_ld,  $
    out_fts=out_fts,  $
    out_xm=out_xm,  $ ;  'de-mean of velocity:'
    out_theta=out_theta,  $ ;'angle of axis rotation (deg):'
    ;
    dummy=dummy
    
    
  ;pxy1=REFORM(pxys[*,0])
  ;pxy2=REFORM(pxys[*,1])
  
  slip=-9999d0
  locking_depth=-9999d0
  
  IF N_ELEMENTS(thetas) EQ 0 THEN thetas=0
  IF N_ELEMENTS(ftss) EQ 0 THEN begin
    ftss=0
    ftss=(indgen(9)-4)*10
  endif
  IF N_ELEMENTS(fss) EQ 0 THEN BEGIN
    nfs=61
    fss=INDGEN(400)/10d0-20 ;fault-slip-s, [-20,20] with step of 0.1 mm/a
    fss=1d0*(INDGEN(nfs)/1d0-nfs/2)
    ;fss=1*(INDGEN(21)/1d0-10)
    ;  fss=fss*3
    fss=fss*.5d0
  ENDIF
  print,'fss:',fss
  IF N_ELEMENTS(lds) EQ 0 THEN BEGIN
    lds=INDGEN(50)+1d0  ;locking-depth-s, [1, 50] with step of 1 km
  ENDIF
  ;distance thresholds
  IF N_ELEMENTS(distmax) EQ 0 THEN distmax=11500 ;maximum distance used, in km; specify a large value to use all points
  IF N_ELEMENTS(distmin) EQ 0 THEN distmin=-11220 ;minimum distance used, in km
  ;distmin=-90
  ;stop
  
  IF N_ELEMENTS(sig_max) EQ 0 THEN sig_max=1.6 ;
  ;C) MEANS (demean the velocity profile)
  ;the xm (mean of x) ranges between the means of profile segment on the two sides of fault trace (xm1, xm2)
  IF N_ELEMENTS(nxm) EQ 0 THEN nxm=17
  
  ntheta=N_ELEMENTS(thetas)
  nfts=N_ELEMENTS(ftss)
  nfs=N_ELEMENTS(fss)
  nld=N_ELEMENTS(lds)
  help, ntheta, nfts, nfs, nld
  
  rchi2s=DBLARR(nfts,nxm,ntheta,nfs,nld)
  xms_all=DBLARR(nxm,ntheta)
  ;stop
  FOR thetai=0,ntheta-1 DO BEGIN ;loop for rotation of axis
    theta=thetas[thetai]
    ;theta=0
    ;theta=-2*!dpi/180
    ;x=replicate(0d0,n_elements(d))
    d1=d*COS(theta)-x0*SIN(theta)
    x1=d*SIN(theta)+x0*COS(theta)
    
    ;    ;                    PLOT,d,x0,background='ffffff'x,color='0'x,psym=1,/nodata,/iso
    ;    ;                    OPLOT,d,x0,color='ffaaee'x,psym=1
    ;    OPLOT,d1,x1,color='0000ff'x,psym=4
    ;    ;                jfile=desuffix(ofile)+STRING(theta*180/!dpi,format='("-",f06.2)')+'.jpg'
    ;    ;                WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
    ;    ;                x=d
    ;    ;                y=replicate(0d0,n_elements(d))
    ;    ;                x1=x*COS(theta)-y*SIN(theta)
    ;    ;                y1=x*SIN(theta)+y*COS(theta)
    ;    ;                OPLOT,x,y,color='ffaaee'x,psym=1
    ;    ;                OPLOT,x1,y1,color='0000ff'x,psym=4
    ;
    ;STOP
    xm1=MAX(x1,min=xm2)
    xms=INDGEN(nxm)*ABS(xm2-xm1)/nxm+MIN([xm1,xm2])
    ;xms=MEAN(x1)
    ;nxm=1
    xms_all[*,thetai]=xms
    PRINT,'velocities means for rotation angle '+STRTRIM(theta*180/!dpi,2)+':',xms
    ;stop
    
    FOR ftsi=0, nfts-1 DO BEGIN ;loop for fault trace shift
      fts=ftss[ftsi]
      
      FOR xmi=0, nxm-1 DO BEGIN ;loop for mean of velocity profile
      
        xm=xms[xmi]
        x=x1-xm
        
        FOR fsi=0, nfs-1 DO BEGIN ;loop for far-field slip rate
          fs=fss[fsi]
          FOR ldi=0, nld-1 DO BEGIN ;loop for locking depth
            ld=lds[ldi]
            xp=(fs/!dpi)*ATAN((d1-fts)/ld) ;use formular: v(y)=Vmax/pi*atan(y/D)
            ;            OPLOT,d,xp+xm, color='00ffff'x
            ;            ;chi2=TOTAL( ((x-xp)^2/xp) )
            ;            ;chi2=TOTAL( (x-xp)^2/(stddev(xp))^2 )
            ;            ;chi2=TOTAL( (x-xp)^2/xe^2 )
            ;            ;rchi2=chi2/(N_ELEMENTS(x)-2-1)
            rchi2=SQRT(TOTAL((x-xp)^2)/(N_ELEMENTS(x)))
            rchi2s[ftsi,xmi,thetai,fsi,ldi]=rchi2
          ;PRINT,rchi2,fs,ld
          ;stop
          ENDFOR
        ENDFOR
      ENDFOR  ;end-of-loop-axis-rotation
    ;PRINT,pfile
    ;STOP
    ENDFOR
  ENDFOR
  
  tmp=MIN(ABS(rchi2s-0),ind)
  PRINT,'minimum rchi2:', tmp
  ind2=ARRAY_INDICES(rchi2s,ind)
  ;STOP
  ;coli=(ind MOD nfs)
  ;rowi=ind/nfs
  ;PRINT,rchi2s[ind2[0],ind2[1],ind2[2],ind2[3]],tmp
  ;;d2=[-1d0*INDGEN(ABS(distmin)),INDGEN(distmax)+1]
  d2=d1
  ;d2=FINDGEN(1000)-500d0
  d2=d2[SORT(d2)]
  d2=d2[UNIQ(d2)]
  x2=xms_all[ind2[1],ind2[2]]+(fss[ind2[3]]/!dpi)*ATAN((d2-ftss[ind2[0]])/lds[ind2[4]])
  
  theta=thetas[ind2[2]]
  d1=d*COS(theta)-x0*SIN(theta)
  x1=d*SIN(theta)+x0*COS(theta)
  
  ;vels_los2=vels_los-xm
  ;
  ;  d_all=dists*COS(theta)-vels_los*SIN(theta)
  ;  x_all=dists*SIN(theta)+vels_los*COS(theta)
  
  ;fts_final=ftss[ind2[0]]
  ;GEO_LINE_XY_FAR_DIST, pxy1, pxy2, pxy3,fts_final,oxy=poxy
  ;  OPLOT,[poxy[0]],[poxy[1]],color='0000ff'x,psym=5
  ;stop
  
  ;rotate the fitted curve to the raw data coordinate system
  ;OPLOT,d2,x2,color='0000ff'x,psym=1
  out_fts=ftss[ind2[0]]
  ;  'de-mean of velocity:'
  out_xm=xms_all[ind2[1],ind2[2]]
  ;  'angle of axis rotation (deg):'
  out_theta=thetas[ind2[2]]
  
  out_fss=fss[ind2[3]]
  out_ld=lds[ind2[4]]
  
  d3=d2;-out_fts
  x3=x2;-out_xm
  ;first, rotate back
  d3_rot=d3*COS(-1*out_theta)-x3*SIN(-1*out_theta)
  x3_rot=d3*SIN(-1*out_theta)+x3*COS(-1*out_theta)
  ;second, add back the distance shift (fault-trace shift)
  d3_rot_fts=d3_rot;+out_fts
  ;third, add back meman of velocities
  x3_rot_xm=x3_rot;+out_xm
  OPLOT,d3_rot_fts,x3_rot_xm,color='0000ff'x,psym=0
  ;
  ;rotated y-axis
  a1=[0,0]+out_fts
  b1=[-3,3]+out_xm
  a2=a1*COS(-1*out_theta)-b1*SIN(-1*out_theta)
  b2=a1*SIN(-1*out_theta)+b1*COS(-1*out_theta)
  d3_y_axis_x=a2;+out_fts
  d3_y_axis_y=b2;+out_xm
  OPLOT,d3_y_axis_x,d3_y_axis_y,color='0'x,linestyle=1,thick=1
  ;rotated x-axis
  a1=[-100,100]+out_fts
  b1=[0,0]+out_xm
  a2=a1*COS(-1*out_theta)-b1*SIN(-1*out_theta)
  b2=a1*SIN(-1*out_theta)+b1*COS(-1*out_theta)
  d3_x_axis_x=a2;+out_fts
  d3_x_axis_y=b2;+out_xm
  OPLOT,d3_x_axis_x,d3_x_axis_y,color='0'x,linestyle=1
  
  RETURN, [slip, locking_depth]
  
END

PRO FAULT_SLIP_INVERSION_ELASTIC_DISLOCATION_STRIKE_SLIP, pfile,   $
    theta_in=theta_in, $
    fts_in=fts_in, $
    distmax=distmax, $
    distmin=distmin,  $
    overwrite=is_overwrite, $
    dummy=dummy
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
  
    path='D:\gsar\interseismic\099-a-m3-1245_1250_1255-honghe\f123\sbas.4.0.0001.9999.20170318.20201227.112.0448.01.___\p.fa_xiaojiang_ext'
    theta_in=1.0
    fts_in=-50
    fid_in=93
    distmin=-1300
    distmax=1003
    
    pfiles=FILE_SEARCH(path+PATH_SEP()+'profile_*_vel.psxy', count=nf)
    ;pfiles=FILE_SEARCH(path+PATH_SEP()+'*rot.txt', count=nf)
    
    fnames=GETFILENAME(pfiles)
    tmp=strsplits(fnames,'_',/extract)
    ids=FIX(REFORM(tmp[1,*]))
    
    ;stop
    fids=ids
    IF N_ELEMENTS(fid_in) NE 0 THEN BEGIN
      fids=fid_in
    ENDIF
    
    FOR i=0,N_ELEMENTS(fids)-1 DO BEGIN
      ;FOR fi=pid,pid DO BEGIN
      fi=WHERE(ids EQ fids[i])
      pfile=pfiles[fi]
      PRINT,pfile
      FAULT_SLIP_INVERSION_ELASTIC_DISLOCATION_STRIKE_SLIP, pfile, theta_in=theta_in, fts_in=fts_in, distmax=distmax, distmin=distmin,overwrite=1
    ;RETURN
    ENDFOR
    RETURN
  ;
  ENDIF
  
  IF N_ELEMENTS(is_overwrite) EQ 0 THEN is_overwrite=0
  
  ;output rotated profile
  ofile_rot=desuffix(pfile)+'_rot.txt'
  ofile_rot2=desuffix(pfile)+'_rot2.txt'
  ;output modeled profile
  ofile=desuffix(pfile)+'_mdl.txt'
  ofile=desuffix(pfile)+'_mdl_los.txt'
  ofile_mdl_raw=desuffix(pfile)+'_mdl_raw.txt'
  ;output residual (rot2 - mdl)
  ofile_resid=desuffix(pfile)+'_resid_los.txt'
  
  ;stop
  IF FILE_TEST(ofile,/regular) EQ 1 && is_overwrite NE 1 THEN BEGIN
    PRINT,'['+prog+']WARNING: output file already exist ('+pfile+')! Skipped.'
    RETURN
  ENDIF
  ;
  ;distance thresholds
  IF N_ELEMENTS(distmax) EQ 0 THEN distmax=500 ;maximum distance used, in km; specify a large value to use all points
  IF N_ELEMENTS(distmin) EQ 0 THEN distmin=-220 ;minimum distance used, in km
  ;distmin=-90
  ;stop
  
  IF N_ELEMENTS(sig_max) EQ 0 THEN sig_max=1.6 ;
  ;
  ;GRID SEARCH PARAMETERS
  ;A) ROTATION OF AXIS
  theta_max=2  ;degree
  theta_step=.2  ;degree
  ;stop
  thetas=(FINDGEN((theta_max*2/theta_step)+1)*theta_step-theta_max)
  ;thetas=thetas*2
  ;thetas=-1
  ;thetas=1.5
  ;thetas=0
  IF N_ELEMENTS(theta_in) NE 0 THEN thetas=theta_in
  ;
  
  PRINT,'rotation of axis (degree):', thetas
  thetas=thetas*!dpi/(-180d0)
  ;STOP
  ;
  ;A) FAR-FIELD INTERSEISMIC FAULT SLIP RATES
  nfs=61
  fss=INDGEN(400)/10d0-20 ;fault-slip-s, [-20,20] with step of 0.1 mm/a
  fss=1d0*(INDGEN(nfs)/1d0-nfs/2)
  ;fss=1*(INDGEN(21)/1d0-10)
  ;  fss=fss*3
  fss=fss*.2d0
  ;fss=(indgen(40)-90)*.1
  ;fss=4d0
  PRINT,'far-field slips:',fss
  ;stop
  
  ;B) ANOTHER TYPE OF GRID SEARCH TO CORRECT THE POSSIBLE SHIFT OF FAULT TRACE
  nfts=21
  nfts=9
  ;nfts=3
  sf_fts=1
  ftss=INDGEN(nfts)*sf_fts-(nfts/ 2)*sf_fts
  ;ftss=ftss*5
  ;ftss=ftss+5
  ;ftss=-60
  ;ftss=-4d0
  ;ftss=0
  IF N_ELEMENTS(fts_in) NE 0 THEN ftss=fts_in
  PRINT,'fault trace shifts:',ftss
  ;stop
  ;
  ;C) MEANS (demean the velocity profile)
  ;the xm (mean of x) ranges between the means of profile segment on the two sides of fault trace (xm1, xm2)
  nxm=17
  ;(see below)
  ;
  ;D) LOCKING DEPTH
  lds=INDGEN(50)+1d0  ;locking-depth-s, [1, 50] with step of 1 km
  ;lds=INDGEN(20)*2+1d0
  ;  lds=INDGEN(10)*5+1d0
  ;lds=INDGEN(10)*2+.001d0
  ;lds=[0.001,0.002]
  ;lds=lds*.2
  ;lds=[6d0,6.001d0]
  ;not working !!;  lds=6
  ;lds=[10,20]
  PRINT,'locking depths:',lds
  ;STOP
  ;
  
  ;Read velocity profile
  lines=read_txt(pfile)
  pos=WHERE(strmids(lines,0,1) NE ' ')
  IF pos[0] NE -1 THEN BEGIN
    header_lines=lines[pos]
  ENDIF ELSE BEGIN
    header_lines=''
  ENDELSE
  ;stop
  pxy3=DOUBLE((STRSPLIT(grepi(lines,'PSXY_FAULT_PROFILE_INTERSECT'), /extract))[2:3])
  tmp=grepi(lines,'PSXY_PROFILE')
  pxys=DOUBLE((str_lines2arr(tmp))[2:3,*])
  pxy1=REFORM(pxys[*,0])
  pxy2=REFORM(pxys[*,1])
  ;stop
  ;
  lines2=grepi(lines,'^ ')
  lines3=str_lines2arr(lines2)
  
  dists=DOUBLE(REFORM(lines3[10,*]))  ;distances to fault trace
  p_dists=DOUBLE(REFORM(lines3[3,*]))  ;distances to profile
  
  ;  ;use insar los
  vels_los=DOUBLE(REFORM(lines3[11,*]))
  veles_los=DOUBLE(REFORM(lines3[12,*]))
  ;stop
  ;gps strike-slip rate
  ;  vels_los=DOUBLE(REFORM(lines3[6,*]))
  ;  veles_los=DOUBLE(REFORM(lines3[7,*]))
  
  IF TOTAL(veles_los) GT 0 THEN BEGIN ;screen out worse rate estimates
    pos=WHERE(dists GE distmin AND dists LE distmax AND veles_los LT sig_max)
  ENDIF ELSE BEGIN  ;no check for rate uncertainity
    pos1=WHERE(dists LT -40)
    pos2=WHERE(dists GT 10)
    IF pos1[0] NE -1 AND pos2[0] NE -1 THEN BEGIN
      PRINT,MEAN(vels_los[pos1])-MEAN(vels_los[pos2]),'mm/a'
    ENDIF
    ;stop
    pos=WHERE(dists GE distmin AND dists LE distmax AND vels_los GT -131d0)
    
  ;pos=WHERE(dists lt distmin or (dists gE 10 and dists le 400) and  vels_los gt -1000.51d0)
    
  ENDELSE
  ;pos=WHERE((dists GE 0 AND dists LE distmax) or (dists le -30))
  IF N_ELEMENTS(pos) LE 3 THEN BEGIN
    PRINT,'['+prog+']WARNING: not enough number of data!'
    RETURN
  ENDIF
  
  ;"station" locations (longitude, latitude)
  lls=DOUBLE(lines3[1:2,pos])
  lls_site=DOUBLE(lines3[8:9,pos])
  ;distances
  d=dists[pos]
  
  ;PRINT,';for strike-slip component'
  x0_raw=vels_los[pos]
  
  ;x0_raw=smooth(x0_raw,25)
  
  
  WINDOW,0,xsize=1600,ysize=699;,/pixmap
  !p.MULTI=[0,2,3]
  ;PLOT,d,x0_raw,background='ffffff'x,color='0'x,psym=1,/nodata, $
  PLOT,dists,vels_los,background='ffffff'x,color='0'x,psym=1,/nodata, $
    title='Raw Profile',  $
    xtitle='Distance from Given Fault Trace (km)',  $
    ytitle='Far-field LOS Velocity (mm/yr)';,/iso
  OPLOT,d,x0_raw,color='ffaaee'x,psym=1
  OPLOT,[-1d3,1d3],[0,0],linestyle=2,color='0'x,thick=2
  OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='0'x,thick=2
  ;ERRPLOT,d,x0_raw-xe,x0_raw+xe,color='aaaa00'x
  ;HELP, lines2
  ;stop
  ;
  ;tmp=LINFIT(d,x0_raw,/double)
  ;x0=x0_raw-tmp[0]-tmp[1]*d
  x0=x0_raw
  ;oPLOT,d,x0+mean(x0_raw),color='aa9922'x,psym=1
  ;STOP
  ind1=WHERE(d-MEAN(ftss) LT 0)
  ind2=WHERE(d-MEAN(ftss) GT 0)
  IF ind1[0] EQ -1 OR ind2[0] EQ -1 THEN RETURN
  
  xe=veles_los[pos]
  pd=p_dists[pos]
  
  ;stop
  tmp=FAULT_SLIP_INVERSION_ELASTIC_DISLOCATION_STRIKE_SLIP( $
    d,  $ ;profile X-axis (distance to fault in km)
    x0,  $ ;velocities along the profile (in mm/yr)
    pxys=pxys,  $ ;profile x/y
    pxy3=pxy3,  $ ;intersection of profile and fault
    ;
    ;optional keywords
    theta=thetas, $ ;rotation of axes
    fts=ftss,  $ ;fault trace shifts
    fs=fss,  $  ;far-field fault slip rates
    ld=lds, $ ;fault locking depths
    xms_all=xms_all,  $
    ;
    ;outputs
    slip=slip,  $
    locking_depath=locking_depth, $
    d3_x_axis_x=d3_x_axis_x, d3_x_axis_y=d3_x_axis_y, $
    d3_y_axis_x=d3_y_axis_x, d3_y_axis_y=d3_y_axis_x, $
    ind2=ind2,  $
    out_fss=out_fss,  $
    out_ld=out_ld,  $
    out_fts=out_fts,  $
    out_xm=out_xm,  $ ;  'de-mean of velocity:'
    out_theta=out_theta,  $ ;'angle of axis rotation (deg):'
    ;
    dummy=dummy)
  ;
  ;stop
  ;  OPENW,fid,ofile_mdl_raw,/get_lun
  ;  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
  ;  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
  ;  PRINTF,fid,'> Y-axis',format='("# PSXY_AXIS_MDL_RAW",2(1x,a))'
  ;  PRINTF,fid,d3_x_axis_x[0],d3_x_axis_y[0],format='("# PSXY_AXIS_MDL_RAW",2(1x,f))'
  ;  PRINTF,fid,d3_x_axis_x[1],d3_x_axis_y[1],format='("# PSXY_AXIS_MDL_RAW",2(1x,f))'
  ;  PRINTF,fid,'> X-axis',format='("# PSXY_AXIS_MDL_RAW",2(1x,a))'
  ;  PRINTF,fid,d3_y_axis_x[0],d3_y_axis_y[0],format='("# PSXY_AXIS_MDL_RAW",2(1x,f))'
  ;  PRINTF,fid,d3_y_axis_x[1],d3_y_axis_y[1],format='("# PSXY_AXIS_MDL_RAW",2(1x,f))'
  ;  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
  ;    format='("*",1x,a,1x,f8.2,1x,a,1x,f8.2)'
  ;  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
  ;  ;PRINTF,fid,'shifted fault trace (deg):',poxy,format='("*",1x,a,1x,2(1x,f12.6))'
  ;  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
  ;  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
  ;  FOR i=0, N_ELEMENTS(d2)-1 DO BEGIN
  ;    PRINTF,fid, d3_rot_fts[i], x3_rot_xm[i],  format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6))'
  ;  ENDFOR
  ;  FREE_LUN,fid
  ;  ;
  ;
  ;
  ;  ;STOP
  ;
  ;  PLOT,d1,x1,color='0'x,psym=1,background='ffffff'x, $
  ;    title='Best-fit Profile', $
  ;    xrange=[-300,200],  $
  ;    xtitle='Distance from Given Fault Trace (km)',  $
  ;    ytitle='Far-field LOS Velocity (mm/yr)',/nodata
  ;  OPLOT,d1,x1,color='aaaaaa'x,psym=1
  ;
  ;  OPLOT,d2,x2, color='0000ff'x,psym=-4,thick=1
  ;  OPLOT,[-1d3,1d3],[0,0],linestyle=2,color='0'x,thick=2
  ;  OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='0'x,thick=2
  ;
  ;  OPLOT,[-1d3,1d3],[xms_all[ind2[1],ind2[2]],xms_all[ind2[1],ind2[2]]],linestyle=2,color='00ff00'x,thick=2
  ;  OPLOT,[ftss[ind2[0]],ftss[ind2[0]]],[-1d3,1d3],linestyle=2,color='00ff00'x,thick=2
  ;  ;
  ;  XYOUTS,0,4,STRING('far-field slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],format='(a,f7.2,a,f5.1)'),color='0'x,alignment=.5
  ;  XYOUTS,0,2,STRING('angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='(a,f,a,f)'),color='0'x,alignment=.5
  ;  XYOUTS,0,-2,STRING('de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='(a,f7.2,a,f)'),color='0'x,alignment=.5
  ;  XYOUTS,0,-4,STRING('fault trace shift:',ftss[ind2[0]],format='(a,f5.1,a,f)'),color='0'x,alignment=.5
  ;  ;stop
  ;  ;
  PRINT,'far-field slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]]
  PRINT,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi
  ;PRINT,'de-mean of velocity:',xms[ind2[1]]
  PRINT,'de-mean of velocity:',xms_all[ind2[1],ind2[2]]
  PRINT,'fault trace shift:',ftss[ind2[0]]
;
;  ;get the lon&lat of these distances
;  lons2=DBLARR(N_ELEMENTS(d2))
;  lats2=DBLARR(N_ELEMENTS(d2))
;  lines4=grepi(lines,'PSXY_PROFILE')
;  ;stop
;  a1=DOUBLE((STRSPLIT(lines4[0],/extract))[2:3])
;  b1=DOUBLE((STRSPLIT(lines4[1],/extract))[2:3])
;  rate_p=(b1[1]-a1[1])/(b1[0]-a1[0])
;  FOR i=0, N_ELEMENTS(d2)-1 DO BEGIN
;  ;stop
;  ENDFOR
;
;
;  ;  WINDOW,3
;  ;  !p.MULTI=-1
;  ;  PLOT,[pxy1[0],pxy2[0]],[pxy1[1],pxy2[1]],psym=-2,background='ffffff'x,color='0'x,/ynozero
;  ;  XYOUTS,pxy1[0],pxy1[1],'xy1',color='0'x
;  ;  XYOUTS,pxy2[0],pxy2[1],'xy2',color='0'x
;  ;  OPLOT,[pxy3[0]],[pxy3[1]],color='ff0000'x,psym=4
;
;
;
;  OPENW,fid,ofile,/get_lun
;  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
;  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
;  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
;    format='("*",1x,a,1x,f8.2,1x,a,1x,f8.2)'
;  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
;  PRINTF,fid,'shifted fault trace (deg):',poxy,format='("*",1x,a,1x,2(1x,f12.6))'
;  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
;  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
;  FOR i=0, N_ELEMENTS(d2)-1 DO BEGIN
;    PRINTF,fid, d2[i], x2[i], lons2[i], lats2[i], format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6))'
;  ENDFOR
;  FREE_LUN,fid
;  ;
;  PLOT,d1,x1,color='0'x,psym=1,background='ffffff'x, $
;    title='Rotated Fitted Profile', $
;    xtitle='Distance from Given Fault Trace (km)',  $
;    ytitle='Far-field LOS Velocity (mm/yr)',/nodata
;  OPLOT,d2,x2,color='aaaaaa'x,psym=1
;  ;write rotated profile
;  OPENW,fid,ofile_rot,/get_lun
;  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
;  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
;  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
;    format='("*",1x,a,1x,f,1x,a,1x,f)'
;  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
;  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
;  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
;  FOR i=0ull, N_ELEMENTS(d1)-1 DO BEGIN
;    PRINTF,fid, d1[i], x1[i],   $
;      pd[i], xe[i], $
;      format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6))'
;  ENDFOR
;  FREE_LUN,fid
;
;
;  PLOT,d1,x1,color='0'x,psym=1,background='ffffff'x, $
;    title='Rotate Profile (all)', $
;    xtitle='Distance from Given Fault Trace (km)',  $
;    ytitle='Far-field LOS Velocity (mm/yr)',/nodata
;  OPLOT,d_all,x_all,color='aaaaaa'x,psym=1
;  ;write rotated profile (all data points)
;  OPENW,fid,ofile_rot2,/get_lun
;  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
;  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
;  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
;    format='("*",1x,a,1x,f,1x,a,1x,f)'
;  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
;  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
;  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
;  FOR i=0ull, N_ELEMENTS(d_all)-1 DO BEGIN
;    PRINTF,fid, d_all[i], x_all[i],   $
;      p_dists[i], veles_los[i], $
;      format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6))'
;  ENDFOR
;  FREE_LUN,fid
;
;
;  PLOT,d1,x1,color='0'x,psym=1,background='ffffff'x, $
;    title='Residual Profile (used)', $
;    xtitle='Distance from Given Fault Trace (km)',  $
;    ytitle='Far-field LOS Velocity (mm/yr)',/nodata
;  OPLOT,d2,x1-x2,color='aaaaaa'x,psym=1
;  ;write residual
;  OPENW,fid,ofile_resid,/get_lun
;  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
;  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
;  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
;    format='("*",1x,a,1x,f,1x,a,1x,f)'
;  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
;  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
;  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
;  FOR i=0ull, N_ELEMENTS(d1)-1 DO BEGIN
;    PRINTF,fid, d1[i], x1[i]-x2[i],   $
;      pd[i], xe[i], $
;      lls_site[*,i], $
;      format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6),1x,2(1x,f12.6))'
;  ENDFOR
;  FREE_LUN,fid
;
;  jfile=desuffix(ofile)+'.jpg'
;  WRITE_JPEG, jfile, TVRD(true=1), true=1, quality=100
;return
;STOP
END