PRO VEL_PROFILE_FIT, pfile, ofile
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 2 THEN BEGIN
    pfile='D:\Papers\yarlung.tsangpo\figure\gps.profiles\profiles\profile_04_vel.psxy'
    ofile='D:\Papers\yarlung.tsangpo\figure\gps.profiles\profiles.fitting\profile_04_vel.model'
    
    
    pfile='D:\ICD\projects\DirectorFund\Application.2012\Final\ppt\figure\gps.profile.jiali\p\profile_01_vel.psxy'
    
    pfiles='D:\ICD\Eighth\2019\20190613.Hezong.Zone2\report.2020\figure\gps.xiaojiang_ext\profiles\profile_077_vel.psxy'
    pfiles='D:\ICD\Eighth\2019\20190613.Hezong.Zone2\report.2020\figure\gps.xiaojiang_ext\profiles\profile_057_vel.psxy'
    pfiles='D:\ICD\Eighth\2019\20190613.Hezong.Zone2\report.2020\figure\gps.xiaojiang_ext\profiles\profile_055_vel.psxy'
    ;pfiles='D:\ICD\Eighth\2019\20190613.Hezong.Zone2\report.2020\figure\gps.xiaojiang_ext\profiles\profile_068_vel.psxy'
    ;pfiles='D:\ICD\Eighth\2019\20190613.Hezong.Zone2\report.2020\figure\jgrb52327\gps.xiaojiang_ext\profiles\profile_083_vel.psxy'
    pfiles='D:\ICD\Eighth\2021\20210329.tanlu\figure\gps\p.fa_tanlu_gps\profile_106_vel.psxy'
    pfiles='D:\ICD\Eighth\2019\20190613.Hezong.Zone2\report.2020\figure\gps.xiaojiang_ext\profiles\profile_095_vel.psxy'
    pfiles='D:\ICD\projects\nsfc\2019\report\2021\figure\profile.gps.igps\pg.fa_jiali\profile_051_vel.psxy'
    pfiles='D:\gsar\doc\eq.20220108.m6.9.menyuan\figure\profile.gps.wangmin\pg.fa_haiyuan\profile_026_vel.psxy'
    pfiles='D:\gsar\doc\eq.20220108.m6.9.menyuan\figure\profile.gps.wangmin\pg.fa_haiyuan\profile_072_vel.psxy'
    
    ofile=desuffix(pfile)+'_mdl.txt'
    
    ;    ;icd solution
    ;    path='D:\Papers\yarlung.tsangpo\figure\gps.profiles\profiles'
    ;    opath='D:\Papers\yarlung.tsangpo\figure\gps.profiles\profiles.fitting'
    ;
    ;;    ;gan2013 solution
    ;;    path='D:\Papers\yarlung.tsangpo\figure\gps.profiles.gan\profiles'
    ;;    opath='D:\Papers\yarlung.tsangpo\figure\gps.profiles.gan\profiles.fitting'
    ;
;    path='D:\ICD\projects\DirectorFund\Application.2012\Final\ppt\figure\gps.profile.jiali\p'
;    path='D:\gsar\des\m_dayi2\f12\SBAS1\pg.xj'
;    path='D:\ICD\Eighth\2021\20210329.tanlu\figure\gps\p.fa_tanlu_gps\'
    ;
    
;    path='D:\gsar\doc\eq.20220108.m6.9.menyuan\figure\profile.gps.wangmin\pg.fa_haiyuan\'
;    
;    pfiles=FILE_SEARCH(path+PATH_SEP()+'*.psxy', count=nf)
    
    nf=n_elements(pfiles)
    FOR fi=0, nf-1 DO BEGIN
;    FOR fi=20, 21 DO BEGIN
      pfile=pfiles[fi]
      ofile=desuffix(pfile)+'_mdl.txt'
      VEL_PROFILE_FIT, pfile, ofile
    ;return
    ENDFOR
    RETURN
  ENDIF
  
;  fss=INDGEN(200)/10d0+1d-1 ;interseismic fault slip rates
;  print,'fss: ', fss
;  lds=INDGEN(50)+1d0  ;locking depths
;  print,'locking depth:', lds
;  nfs=N_ELEMENTS(fss)
;  nld=N_ELEMENTS(lds)
  ;stop
  
  lines=read_txt(pfile)
  lines2=grepi(lines,'^ ', LINES_NOT=LINES_HEADER)
  ;help, lines_header
  ;stop
  lines3=str_lines2arr(lines2)
  
  
  dists=DOUBLE(REFORM(lines3[10,*]))
  distmax=800 ;in km
  distmin=-250
  ;distmin=-90
  
  
  vels_along=DOUBLE(REFORM(lines3[4,*]))
  veles_along=DOUBLE(REFORM(lines3[5,*]))
  vels_tang=DOUBLE(REFORM(lines3[6,*]))
  veles_tang=DOUBLE(REFORM(lines3[7,*]))
  
  pos=WHERE(dists GE distmin AND dists LE distmax)
  IF N_ELEMENTS(pos) LE 3 THEN BEGIN
    PRINT,'[]WARNING: not enough number of data!'
    RETURN
  ENDIF
  
  
  lls=DOUBLE(lines3[1:2,pos])
  
  XSHIFT=20
  d=dists[pos]+XSHIFT
  
  PRINT,';for strike-slip component'
  x0=vels_tang[pos]
  y0=vels_along[pos]
  ind1=WHERE(d LT 0)
  ind2=WHERE(d GT 0)
  xm1=MEAN(x0[ind1])
  xm2=MEAN(x0[ind2])
  ym1=MEAN(y0[ind1])
  ym2=MEAN(y0[ind2])
  PRINT,'xm1,xm2: ',xm1,xm2
  PRINT,'ym1,ym2: ',ym1,ym2
  xm0=MEAN(xm1+xm2)
  ym0=MEAN(ym1+ym2)
  PRINT,'xm0:' ,xm0
  PRINT,'ym0:' ,ym0
  x=x0-xm0
  y=y0-ym0
  PRINT,MEAN(x[ind1]),MEAN(x[ind2]),MEAN(MEAN(x[ind1])+MEAN(x[ind2]))
  xe=veles_tang[pos]
  ye=veles_along[pos]
  
  xms=INDGEN(FIX(ABS(xm2-xm1)*10)>1)/10d0+xm1
  yms=INDGEN(FIX(ABS(ym2-ym1)*10)>1)/10d0+ym1
  ;xms=0
  nxm=N_ELEMENTS(xms)
  nym=N_ELEMENTS(yms)
  ;stop
  
  WINDOW,0,xsize=1800
  device,decomposed=1
  !p.multi=[0,1,2]
  PLOT,d,x0,background='ffffff'x,color='0'x,psym=2
  ERRPLOT,d,x0-xe,x0+xe,color='aaaaaa'x
  
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
    d2s=d2, x2s=x2, $
    d3s=d3, x3s=x3, $
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
;  rchi2s=DBLARR(nxm,nfs,nld)
;  FOR xmi=0, nxm-1 DO BEGIN
;    xm=xms[xmi]
;    x=x0-xm
;    FOR fsi=0, nfs-1 DO BEGIN
;      fs=fss[fsi]
;      FOR ldi=0, nld-1 DO BEGIN
;        ld=lds[ldi]
;        xp=(fs/!dpi)*ATAN(d/ld) ;v(y)=Vmax/pi*atan(y/D)
;        ;oplot,d,xp, color='0000ff'x
;        ;chi2=TOTAL( ((x-xp)^2/xp) )
;        ;chi2=TOTAL( (x-xp)^2/(stddev(xp))^2 )
;        chi2=TOTAL( (x-xp)^2/xe^2 )
;        rchi2=chi2/(N_ELEMENTS(x)-2-1)
;        rchi2=SQRT(TOTAL((x-xp)^2)/(N_ELEMENTS(x)))
;        rchi2s[xmi,fsi,ldi]=rchi2
;      ;stop
;      ENDFOR
;    ENDFOR
;  ENDFOR
;  tmp=MIN(ABS(rchi2s-0),ind)
;  ind2=ARRAY_INDICES(rchi2s,ind)
;  ;stop
;  ;coli=(ind MOD nfs)
;  ;rowi=ind/nfs
;  PRINT,rchi2s[ind2[0],ind2[1],ind2[2]],tmp
;  d2=[-1d0*INDGEN(ABS(distmin)),INDGEN(distmax)+1]
;  d2=d2[SORT(d2)]
;  d2=d2[UNIQ(d2)]
;  x2=xms[ind2[0]]+(fss[ind2[1]]/!dpi)*ATAN(d2/lds[ind2[2]])
  OPLOT,d3,x3, color='0000ff'x,psym=-4
  OPLOT,[-1d3,1d3],[0,0],linestyle=2,color='0'x
  OPLOT,[-1d3,1d3],[out_xm,out_xm],linestyle=2,color='00ff00'x
  PRINT,'far-field slip rates:',out_fss,'    locking depth:',out_ld
  PRINT,'xm:',out_xm
  ;stop
  
  ;get the lon&lat of these distances
  lons2=DBLARR(N_ELEMENTS(d2))
  lats2=DBLARR(N_ELEMENTS(d2))
  lines4=grepi(lines,'PSXY_PROFILE')
  ;stop
  a1=DOUBLE((STRSPLIT(lines4[0],/extract))[2:3])
  b1=DOUBLE((STRSPLIT(lines4[1],/extract))[2:3])
  rate_p=(b1[1]-a1[1])/(b1[0]-a1[0])
  
  
  
  PLOT,d,y0,background='ffffff'x,color='0'x,psym=2
  ERRPLOT,d,y0-ye,y0+ye,color='aaaaaa'x
  HELP, lines2
  
  
  tmp=FAULT_SLIP_INVERSION_ELASTIC_DISLOCATION_NORMAL( $
    d,  $ ;profile X-axis (distance to fault in km)
    y0,  $ ;velocities along the profile (in mm/yr)
    pxys=pxys,  $ ;profile x/y
    pxy3=pxy3,  $ ;intersection of profile and fault
    ;
    ;optional keywords
    theta=thetas, $ ;rotation of axes
    fts=ftss,  $ ;fault trace shifts
    fs=fss,  $  ;far-field fault slip rates
    ld=[0.001,.0001], $ ;fault locking depths
    xms_all=yms_all,  $
    ;
    ;outputs
    slip=slip,  $
    locking_depath=locking_depth, $
    d2s=d2, x2s=y2, $
    d3s=d3, x3s=y3, $
    d3_x_axis_x=d3_x_axis_x, d3_x_axis_y=d3_x_axis_y, $
    d3_y_axis_x=d3_y_axis_x, d3_y_axis_y=d3_y_axis_x, $
    ind2=ind2,  $
    out_fss=out_fss_y,  $
    out_ld=out_ld_y,  $
    out_fts=out_fts_y,  $
    out_xm=out_ym,  $ ;  'de-mean of velocity:'
    out_theta=out_theta_y,  $ ;'angle of axis rotation (deg):'
    ;
    dummy=dummy)
    
  OPLOT,d3,y3, color='0000ff'x,psym=-4
  OPLOT,[-1d3,1d3],[0,0],linestyle=2,color='0'x
  OPLOT,[-1d3,1d3],[out_ym,out_ym],linestyle=2,color='00ff00'x
  PRINT,'far-field slip rates:',out_fss_y,'    locking depth:',out_ld_y
  PRINT,'ym:',out_ym
    ;return
  
  ;FOR i=0, N_ELEMENTS(d2)-1 DO BEGIN
  ;stop
  ;ENDFOR
  
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=pfile,user=user
  printf,fid,['* original header:',lines_header],format='(a)'
  PRINTF,fid,'far-field strike-slip rates:',out_fss,'    locking depth:',out_ld,  $
    format='("*",1x,a,1x,f,1x,a,1x,f)'
  PRINTF,fid,'far-field extentional-compressional rates:',out_fss_y,'    locking depth:',out_ld_y,  $
    format='("*",1x,a,1x,f,1x,a,1x,f)'
  printf,fid,'distance','strik-slip','ext/comp',format='("*",a10,2(1x,a15),1x,2(1x,a9))'
  FOR i=0, N_ELEMENTS(d2)-1 DO BEGIN
    PRINTF,fid, d2[i]-XSHIFT, x2[i], y2[i], lons2[i], lats2[i], format='(1x,f10.2,2(1x,f15.7),1x,2(1x,f9.3))'
  ENDFOR
  FREE_LUN,fid
  
  jfile=desuffix(ofile)+'.jpg'
  WRITE_JPEG, jfile, TVRD(true=2), true=2, quality=100
;return
;STOP
END