PRO VEL_PROFILE_FIT, pfile, ofile
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 2 THEN BEGIN
    pfile='D:\Papers\yarlung.tsangpo\figure\gps.profiles\profiles\profile_04_vel.psxy'
    ofile='D:\Papers\yarlung.tsangpo\figure\gps.profiles\profiles.fitting\profile_04_vel.model'
    
    pfile='D:\Papers\yarlung.tsangpo\figure\gps.profiles.gan\profiles\profile_03_vel.psxy'
    ofile='D:\Papers\yarlung.tsangpo\figure\gps.profiles.gan\profiles.fitting\profile_03_vel.model'
    
    pfile='D:\ICD\meeting\dragon3.2016\figure\gps.profiles\profiles\profile_01_vel.psxy'
    ofile='D:\ICD\meeting\dragon3.2016\figure\gps.profiles\profiles.fitting\profile_01_vel.model'
    
    pfile='D:\ICD\meeting\dragon3.2016\figure\gps.profiles.2016jun30\profiles.bcjlf\profile_02_vel.psxy'
    ofile='D:\ICD\meeting\dragon3.2016\figure\gps.profiles.2016jun30\profiles.bcjlf.fitting\profile_02_vel.model'
    
    pfile='D:\ICD\meeting\AOGS2016\figure\vel.profile\profiles.jiali\profile_01_vel.psxy'
    ofile='D:\ICD\meeting\AOGS2016\figure\vel.profile\profiles.jiali.fitting\profile_01_vel.psxy.model'
    
    pfile='D:\ICD\meeting\AOGS2016\figure\vel.profile\profiles.ydgl\profile_01_vel.psxy'
    ofile='D:\ICD\meeting\AOGS2016\figure\vel.profile\profiles.ydgl.fitting\profile_01_vel.model'
    
    pfile='D:\ICD\meeting\AOGS2016\figure\vel.profile\profiles.bc\profile_02_vel.psxy'
    ofile='D:\ICD\meeting\AOGS2016\figure\vel.profile\profiles.bc.fitting\profile_02_vel.model'
    
    pfile='D:\ICD\projects\2015nov.lys.yunnan\gps.profile\ynxj\profiles\profile_02_vel.psxy'
    ofile='D:\ICD\projects\2015nov.lys.yunnan\gps.profile\ynxj\profiles.fitting\profile_02_vel.model'
    
    pfile='D:\ICD\projects\DirectorFund\Application.2012\Final\figure\profile.jiali\p\profile_08_vel.psxy'
    pfile='D:\ICD\相关课题\IAA\figure\1.nepal.interseismic\p\profile_01_vel.psxy'
    
    pfile='D:\ICD\projects\DirectorFund\Application.2012\Final\ppt\figure\gps.profile.jiali\p\profile_01_vel.psxy'
    
    ofile=desuffix(pfile)+'_mdl.txt'
    
    ;    ;icd solution
    ;    path='D:\Papers\yarlung.tsangpo\figure\gps.profiles\profiles'
    ;    opath='D:\Papers\yarlung.tsangpo\figure\gps.profiles\profiles.fitting'
    ;
    ;;    ;gan2013 solution
    ;;    path='D:\Papers\yarlung.tsangpo\figure\gps.profiles.gan\profiles'
    ;;    opath='D:\Papers\yarlung.tsangpo\figure\gps.profiles.gan\profiles.fitting'
    ;
    path='D:\ICD\projects\DirectorFund\Application.2012\Final\ppt\figure\gps.profile.jiali\p'
    ;
    pfiles=FILE_SEARCH(path+PATH_SEP()+'*.psxy', count=nf)
    FOR fi=0, nf-1 DO BEGIN
      pfile=pfiles[fi]
      ofile=desuffix(pfile)+'_mdl.txt'
      VEL_PROFILE_FIT, pfile, ofile
    ;return
    ENDFOR
    RETURN
  ENDIF
  
  fss=INDGEN(100)/10d0+1d-1 ;interseismic fault slip rates
  lds=INDGEN(50)+1d0  ;locking depths
  nfs=N_ELEMENTS(fss)
  nld=N_ELEMENTS(lds)
  ;stop
  
  lines=read_txt(pfile)
  lines2=grepi(lines,'^ ')
  lines3=str_lines2arr(lines2)
  
  
  dists=DOUBLE(REFORM(lines3[10,*]))
  distmax=2000 ;in km
  distmin=-1000
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
  
  d=dists[pos]
  
  PRINT,';for strike-slip component'
  x0=vels_tang[pos]
  ind1=WHERE(d LT 0)
  ind2=WHERE(d GT 0)
  xm1=MEAN(x0[ind1])
  xm2=MEAN(x0[ind2])
  PRINT,xm1,xm2
  xm0=MEAN(xm1+xm2)
  PRINT,xm0
  x=x0-xm0
  PRINT,MEAN(x[ind1]),MEAN(x[ind2]),MEAN(MEAN(x[ind1])+MEAN(x[ind2]))
  xe=veles_tang[pos]
  
  xms=INDGEN(FIX(ABS(xm2-xm1)*10))/10d0+xm1
  ;xms=0
  nxm=N_ELEMENTS(xms)
  ;stop
  
  WINDOW,0,xsize=1800
  PLOT,d,x0,background='ffffff'x,color='0'x,psym=2
  ERRPLOT,d,x0-xe,x0+xe,color='aaaaaa'x
  HELP, lines2
  
  rchi2s=DBLARR(nxm,nfs,nld)
  FOR xmi=0, nxm-1 DO BEGIN
    xm=xms[xmi]
    x=x0-xm
    FOR fsi=0, nfs-1 DO BEGIN
      fs=fss[fsi]
      FOR ldi=0, nld-1 DO BEGIN
        ld=lds[ldi]
        xp=(fs/!dpi)*ATAN(d/ld) ;v(y)=Vmax/pi*atan(y/D)
        ;oplot,d,xp, color='0000ff'x
        ;chi2=TOTAL( ((x-xp)^2/xp) )
        ;chi2=TOTAL( (x-xp)^2/(stddev(xp))^2 )
        chi2=TOTAL( (x-xp)^2/xe^2 )
        rchi2=chi2/(N_ELEMENTS(x)-2-1)
        rchi2=SQRT(TOTAL((x-xp)^2)/(N_ELEMENTS(x)))
        rchi2s[xmi,fsi,ldi]=rchi2
      ;stop
      ENDFOR
    ENDFOR
  ENDFOR
  tmp=MIN(ABS(rchi2s-0),ind)
  ind2=ARRAY_INDICES(rchi2s,ind)
  ;stop
  ;coli=(ind MOD nfs)
  ;rowi=ind/nfs
  PRINT,rchi2s[ind2[0],ind2[1],ind2[2]],tmp
  d2=[-1d0*INDGEN(ABS(distmin)),INDGEN(distmax)+1]
  d2=d2[SORT(d2)]
  d2=d2[UNIQ(d2)]
  x2=xms[ind2[0]]+(fss[ind2[1]]/!dpi)*ATAN(d2/lds[ind2[2]])
  OPLOT,d2,x2, color='0000ff'x,psym=-4
  OPLOT,[-1d3,1d3],[0,0],linestyle=2,color='0'x
  OPLOT,[-1d3,1d3],[xms[ind2[0]],xms[ind2[0]]],linestyle=2,color='00ff00'x
  PRINT,'far-field slip rates:',fss[ind2[1]],'    locking depth:',lds[ind2[2]]
  PRINT,'xm:',xms[ind2[0]]
  
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
  
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=pfile,user=user
  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[1]],'    locking depth:',lds[ind2[2]],  $
    format='("*",1x,a,1x,f,1x,a,1x,f)'
  FOR i=0, N_ELEMENTS(d2)-1 DO BEGIN
    PRINTF,fid, d2[i], x2[i], lons2[i], lats2[i], format='(1x,f10.2,1x,f15.7,1x,2(1x,f9.3))'
  ENDFOR
  FREE_LUN,fid
  
  jfile=desuffix(ofile)+'.jpg'
  WRITE_JPEG, jfile, TVRD(true=2), true=2, quality=100
;return
;STOP
END