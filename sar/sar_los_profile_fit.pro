PRO SAR_LOS_PROFILE_FIT, pfile

  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    path='D:\gsar\asc\m_jiali1\asc_F1\SBAS\p'
    path='D:\gsar\asc\m_jiali1\yzs\p'
    ;
    pfiles=FILE_SEARCH(path+PATH_SEP()+'*.psxy', count=nf)
    ;pfiles=FILE_SEARCH(path+PATH_SEP()+'*rot.txt', count=nf)
    ;FOR fi=0, nf-1 DO BEGIN
    FOR fi=15,15 DO BEGIN
      pfile=pfiles[fi]
      PRINT,pfile
      SAR_LOS_PROFILE_FIT, pfile
    ;RETURN
    ENDFOR
    RETURN
  ;
  ENDIF
  
  ;output rotated profile
  ofile_rot=desuffix(pfile)+'_rot.txt'
  ofile_rot2=desuffix(pfile)+'_rot2.txt'
  ;output modeled profile
  ofile=desuffix(pfile)+'_mdl.txt'
  ofile=desuffix(pfile)+'_mdl_los.txt'
  ;
  ;distance thresholds
  distmax=50 ;maximum distance used, in km; specify a large value to use all points
  distmin=-50 ;minimum distance used, in km
  ;distmin=-90
  
  IF N_ELEMENTS(sig_max) EQ 0 THEN sig_max=1.6 ;
  ;
  ;GRID SEARCH PARAMETERS
  ;A) ROTATION OF AXIS
  theta_max=5  ;degree
  theta_step=1  ;degree
  thetas=(FINDGEN((theta_max*2/theta_step)+1)*theta_step-theta_max)
  thetas=thetas*1
  thetas=0.5
  ntheta=N_ELEMENTS(thetas)
  PRINT,'rotation of axis (degree):', thetas
  thetas=thetas*!dpi/(-180d0)
  ;STOP
  ;
  ;A) FAR-FIELD INTERSEISMIC FAULT SLIP RATES
  nfs=21
  fss=INDGEN(400)/10d0-20 ;fault-slip-s, [-20,20] with step of 0.1 mm/a
  fss=1d0*(INDGEN(nfs)/1d0-nfs/2)
  ;fss=2*(INDGEN(21)/1d0-10)
  ;fss=fss*2
  ;fss=fss*.25d0
  nfs=N_ELEMENTS(fss)
  PRINT,'far-field slips:',fss
  ;stop
  
  ;B) ANOTHER TYPE OF GRID SEARCH TO CORRECT THE POSSIBLE SHIFT OF FAULT TRACE
  nfts=21
  nfts=9
  ;nfts=3
  sf_fts=1
  ftss=INDGEN(nfts)*sf_fts-(nfts/ 2)*sf_fts
  ftss=ftss*5
  ;ftss=ftss+5
  ftss=-9
  nfts=N_ELEMENTS(ftss)
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
  lds=INDGEN(20)*2+1d0
  lds=INDGEN(10)*5+1d0
  ;lds=[10,20]
  nld=N_ELEMENTS(lds)
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
  tmp=DOUBLE((str_lines2arr(tmp))[2:3,*])
  pxy1=REFORM(tmp[*,0])
  pxy2=REFORM(tmp[*,1])
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
  
  IF TOTAL(veles_los) GT 0 THEN BEGIN ;screen out worse rate estimates
    pos=WHERE(dists GE distmin AND dists LE distmax AND veles_los LT sig_max)
  ENDIF ELSE BEGIN  ;no check for rate uncertainity
    pos=WHERE(dists GE distmin AND dists LE distmax)
  ENDELSE
  ;pos=WHERE((dists GE 0 AND dists LE distmax) or (dists le -30))
  IF N_ELEMENTS(pos) LE 3 THEN BEGIN
    PRINT,'['+prog+']WARNING: not enough number of data!'
    RETURN
  ENDIF
  
  ;"station" locations (longitude, latitude)
  lls=DOUBLE(lines3[1:2,pos])
  ;distances
  d=dists[pos]
  
  ;PRINT,';for strike-slip component'
  x0_raw=vels_los[pos]
  
  
  WINDOW,0,xsize=1600,ysize=699;,/pixmap
  !p.MULTI=[0,1,2]
  PLOT,d,x0_raw,background='ffffff'x,color='0'x,psym=1,/nodata, $
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
  tmp=LINFIT(d,x0_raw,/double)
  x0=x0_raw-tmp[0]-tmp[1]*d
  ;x0=x0_raw
  oPLOT,d,x0+mean(x0_raw),color='aa9922'x,psym=1
  ;STOP
  ind1=WHERE(d-MEAN(ftss) LT 0)
  ind2=WHERE(d-MEAN(ftss) GT 0)
  IF ind1[0] EQ -1 OR ind2[0] EQ -1 THEN RETURN
  
  xe=veles_los[pos]
  pd=p_dists[pos]
  
  ;stop
  
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
            xp=(fs/!dpi)*ATAN((d1-fts)/ld) ;v(y)=Vmax/pi*atan(y/D)
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
  d2=[-1d0*INDGEN(ABS(distmin)),INDGEN(distmax)+1]
  d2=FINDGEN(1000)-500d0
  d2=d2[SORT(d2)]
  d2=d2[UNIQ(d2)]
  x2=xms_all[ind2[1],ind2[2]]+(fss[ind2[3]]/!dpi)*ATAN((d2-ftss[ind2[0]])/lds[ind2[4]])
  
  theta=thetas[ind2[2]]
  d1=d*COS(theta)-x0*SIN(theta)
  x1=d*SIN(theta)+x0*COS(theta)
  
  ;vels_los2=vels_los-xm
  ;
  d_all=dists*COS(theta)-vels_los*SIN(theta)
  x_all=dists*SIN(theta)+vels_los*COS(theta)
  
  PLOT,d1,x1,color='0'x,psym=1,background='ffffff'x, $
    title='Best-fit Profile', $
    xtitle='Distance from Given Fault Trace (km)',  $
    ytitle='Far-field LOS Velocity (mm/yr)',/nodata
  OPLOT,d1,x1,color='aaaaaa'x,psym=1
  
  OPLOT,d2,x2, color='0000ff'x,psym=-4,thick=1
  OPLOT,[-1d3,1d3],[0,0],linestyle=2,color='0'x,thick=2
  OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='0'x,thick=2
  
  OPLOT,[-1d3,1d3],[xms_all[ind2[1],ind2[2]],xms_all[ind2[1],ind2[2]]],linestyle=2,color='00ff00'x,thick=2
  OPLOT,[ftss[ind2[0]],ftss[ind2[0]]],[-1d3,1d3],linestyle=2,color='00ff00'x,thick=2
  ;
  XYOUTS,0,4,STRING('far-field slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],format='(a,f7.2,a,f5.1)'),color='0'x,alignment=.5
  XYOUTS,0,2,STRING('angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='(a,f,a,f)'),color='0'x,alignment=.5
  XYOUTS,0,-2,STRING('de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='(a,f7.2,a,f)'),color='0'x,alignment=.5
  XYOUTS,0,-4,STRING('fault trace shift:',ftss[ind2[0]],format='(a,f5.1,a,f)'),color='0'x,alignment=.5
  ;stop
  ;
  PRINT,'far-field slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]]
  PRINT,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi
  ;PRINT,'de-mean of velocity:',xms[ind2[1]]
  PRINT,'de-mean of velocity:',xms_all[ind2[1],ind2[2]]
  PRINT,'fault trace shift:',ftss[ind2[0]]
  
  ;get the lon&lat of these distances
  lons2=DBLARR(N_ELEMENTS(d2))
  lats2=DBLARR(N_ELEMENTS(d2))
  lines4=grepi(lines,'PSXY_PROFILE')
  ;stop
  a1=DOUBLE((STRSPLIT(lines4[0],/extract))[2:3])
  b1=DOUBLE((STRSPLIT(lines4[1],/extract))[2:3])
  rate_p=(b1[1]-a1[1])/(b1[0]-a1[0])
  FOR i=0, N_ELEMENTS(d2)-1 DO BEGIN
  ;stop
  ENDFOR
  
  
  ;  WINDOW,3
  ;  !p.MULTI=-1
  ;  PLOT,[pxy1[0],pxy2[0]],[pxy1[1],pxy2[1]],psym=-2,background='ffffff'x,color='0'x,/ynozero
  ;  XYOUTS,pxy1[0],pxy1[1],'xy1',color='0'x
  ;  XYOUTS,pxy2[0],pxy2[1],'xy2',color='0'x
  ;  OPLOT,[pxy3[0]],[pxy3[1]],color='ff0000'x,psym=4
  fts_final=ftss[ind2[0]]
  GEO_LINE_XY_FAR_DIST, pxy1, pxy2, pxy3,fts_final,oxy=poxy
  ;  OPLOT,[poxy[0]],[poxy[1]],color='0000ff'x,psym=5
  ;stop
  
  
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
    format='("*",1x,a,1x,f8.2,1x,a,1x,f8.2)'
  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'shifted fault trace (deg):',poxy,format='("*",1x,a,1x,2(1x,f12.6))'
  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
  FOR i=0, N_ELEMENTS(d2)-1 DO BEGIN
    PRINTF,fid, d2[i], x2[i], lons2[i], lats2[i], format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6))'
  ENDFOR
  FREE_LUN,fid
  ;
  ;write rotated profile
  OPENW,fid,ofile_rot,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
    format='("*",1x,a,1x,f,1x,a,1x,f)'
  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
  FOR i=0ull, N_ELEMENTS(d1)-1 DO BEGIN
    PRINTF,fid, d1[i], x1[i],   $
      pd[i], xe[i], $
      format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6))'
  ENDFOR
  FREE_LUN,fid
  
  ;write rotated profile (all data points)
  OPENW,fid,ofile_rot2,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
    format='("*",1x,a,1x,f,1x,a,1x,f)'
  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
  FOR i=0ull, N_ELEMENTS(d_all)-1 DO BEGIN
    PRINTF,fid, d_all[i], x_all[i],   $
      p_dists[i], veles_los[i], $
      format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6))'
  ENDFOR
  FREE_LUN,fid
  
  jfile=desuffix(ofile)+'.jpg'
  WRITE_JPEG, jfile, TVRD(true=1), true=1, quality=100
;return
;STOP
END