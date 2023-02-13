PRO SAR_ALOS_PREP_SBAS, path

  IF N_PARAMS() LT 1 THEN BEGIN
    path='\\gpsac4\root\g4c\gsar\envisat.d.bengco.t405f2979'
    path='\\gpsac4\root\g4c\gsar\envisat.d.t405f2979.test'
    path='\\vmshare\root\data\FTP\user\tianyf\envisat.d.t405f2979.test'
    ;    path='\\gpsac5\root\b1\gsar\envisat.a.t398f621.bengco'
    ;    path='\\gpsac5\root\b1\gsar\envisat.d.t448f2907.shuanghu'
    path='\\gpsac5\root\b1\gsar\envisat.d.t219f2979.silingco'
    path='\\gpsac5\root\b1\gsar\envisat.d.t333f2853.haiyuan'
    path='\\gpsac4\root\g4c\gsar\alos.f670p505.burog'
    path='\\gpsac5\root\g5b\gsar\f640p476.longriba'
    jdmin=2000
  ENDIF
  
  IF N_ELEMENTS(blenmax) EQ 0 THEN blenmax=1000d0
  
  file_baseline_tab=path+PATH_SEP()+'baseline_table.dat'
  file_intf_in=path+PATH_SEP()+'intf.in'
  ;
  ofile_scene=path+PATH_SEP()+'scene.tab'
  ofile_intf=path+PATH_SEP()+'intf.tab'
  ;
  ofile_scene=path+PATH_SEP()+'scene.tab1'
  ofile_intf=path+PATH_SEP()+'intf.tab1'
  
  path_intf_all="../intf_all/"
  path_intf_all='../ia/'
  
  IF N_ELEMENTS(jdmin) EQ 0 THEN jdmin=-99999d0
  
  
  lines=read_txt(file_intf_in)
  tmp=strsplits(lines,':',/extract)
  tmp=REFORM(REFORM(tmp,1,N_ELEMENTS(tmp)))
  tmp=tmp[SORT(tmp)]
  scenes_in=tmp[UNIQ(tmp)]
  ;STOP
  
  lines=read_txt(file_baseline_tab)
  
  
  masterId=0
  ;stop
  lines_p=str_lines2arr(lines)
  
  scenes_all=REFORM(lines_p[0,*])
  inds=INTARR(N_ELEMENTS(scenes_in))
  FOR i=0, N_ELEMENTS(scenes_in)-1 DO BEGIN
    ;pos=WHERE(scenes_all EQ scenes_in[i])
    ;inds[i]=pos
    inds[i]=-1
    ;stop
    FOR j=0, N_ELEMENTS(scenes_all)-1 DO BEGIN
      pos=STRPOS(scenes_in[i],scenes_all[j])
      IF pos[0] NE -1 THEN BEGIN
        inds[i]=j
        CONTINUE
      ENDIF
    ENDFOR
  ENDFOR
  ;STOP
  pos=WHERE(inds NE -1)
  IF pos[0] EQ -1 THEN RETURN
  
  lines_p=lines_p[*,inds[pos]]
  
  ;STOP
  
  ids=REFORM(STRTRIM(LONG(lines_p[1,*]),2))
  ndays=REFORM(STRTRIM(FIX(lines_p[2,*]),2))
  blens=REFORM(DOUBLE(lines_p[4,*]))
  
  np=N_ELEMENTS(ids)
  HELP,ids
  
  ;stop
  links=BYTARR(np,np)
  dyrs=DBLARR(np)
  PRINT,'master image : ',lines_p[0,masterId]
  ;Create intf.dat
  count=0
  OPENW,fid,ofile_intf,/get_lun
  FOR i=0,np-1 DO BEGIN
    yearI=FIX(STRMID(ids[i],0,4))
    dayI=FIX(STRMID(ids[i],4,3))
    ;stop
    DOY,yearI,1,dayI,12,0,jd=jdI,dyear=dyri
    dyrs[i]=dyri
    FOR j=i+1,np-1 DO BEGIN
      yearJ=FIX(STRMID(ids[j],0,4))
      dayJ=FIX(STRMID(ids[j],4,3))
      DOY,yearJ,1,dayJ,12,0,jd=jdJ
      ;
      ;IF jdi gt jdmin and jdj gt jdmin and ABS(jdJ-jdI) LE 190 THEN CONTINUE
      ;IF ABS(jdJ-jdI) LE 190 THEN CONTINUE
      ;      IF (FIX(ABS(jdJ-jdI)) MOD 360) GT 90 THEN CONTINUE
      ;check the perpendicular distance (baseline length)
      ;if abs(blens[j]-blens[i]) ge blenmax then continue
      
      links[i,j]=1
      count=count+1
      PRINTF,fid,path_intf_all,ids[i],ids[j], $
        path_intf_all,ids[i],ids[j],  $
        ids[i],ids[j], blens[j]-blens[i], $
;        format='(a,a,"_",a,"/unwrap.grd ",a,a,"_",a,"/corr.grd",1x,a,1x,a,1x,f)'
    format='(a,a,"_",a,"/unwrap_mask.grd ",a,a,"_",a,"/corr_cut.grd",1x,a,1x,a,1x,f)'
    ;format='(a,a,"_",a,"/unwrap_mask.grd ",a,a,"_",a,"/corr.grd",1x,a,1x,a,1x,f)'
    ENDFOR
  ENDFOR
  FREE_LUN,fid
  
  pos=WHERE(links EQ 1)
  IF pos[0] EQ -1 THEN RETURN
  WINDOW,1,xsize=800,ysize=800
  !p.MULTI=-1
  PLOT,dyrs,blens,background='ffffff'x,color='0'x,psym=2,$
    xtitle='Time',$
    ytitle='Baseline (m)',$
    title=STRTRIM(N_ELEMENTS(pos),2)+' pairs of interferograms', $
    /nodata
  FOR i=0,N_ELEMENTS(pos)-1 DO BEGIN
    indx=(pos[i] MOD np)
    indy=(pos[i] / np)
    OPLOT,[dyrs[indx],dyrs[indy]],[blens[indx],blens[indy]],psym=-6,color='ff0000'x
    XYOUTS,dyrs[indx],blens[indx],ids[indx],color='0'x
    XYOUTS,dyrs[indy],blens[indy],ids[indy],color='0'x
  ;stop
  ENDFOR
  jfile=ofile_intf+'.jpg'
  WRITE_JPEG, jfile, TVRD(true=1),true=1,quality=100
  ;STOP
  ;Create scene.dat
  OPENW,fid,ofile_scene,/get_lun
  FOR i=0,np-1 DO BEGIN
    PRINTF,fid,ids[i],ndays[i],format='(a,1x,a)'
  ENDFOR
  FREE_LUN,fid
  
  
  
  
END