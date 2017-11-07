PRO SAR_S1_PREP_SBAS, path

  IF N_PARAMS() LT 1 THEN BEGIN
    path='\\gpsac5\root\d1\gsar\mila2\des_F3'  
    path='\\gpsac4\root\g4b\tianyf\m_jiali1\asc_F3'  
    path='\\gpsac5\root\b1\gsar\mila4\des_F1'
    path='\\gpsac4\root\g4b\tianyf\mila3\asc_F3'
    ;path='\\gpsac4\root\dcd0\gsar\mila4\des_F1'
    path='D:\gsar\des\mila2_oct\des_F3'
    path='D:\gsar\des\mila2s\des_F3'
    path='\\gpsac4\root\g4b\tianyf\mila3s\asc_F3'
  ENDIF
  
  file_baseline_tab=path+PATH_SEP()+'baseline_table.dat'
  file_intf_in=path+PATH_SEP()+'intf.in'
  
  ofile_scene=path+PATH_SEP()+'scene.tab'
  ofile_intf=path+PATH_SEP()+'intf.tab'
  ofile_scene=path+PATH_SEP()+'scene.tab1'
  ofile_intf=path+PATH_SEP()+'intf.tab1'
  
  path_intf_all="../intf_all/"
  path_intf_all='../ia/'
  
  
  lines=read_txt(file_intf_in)
  tmp=strsplits(lines,':',/extract)
  tmp=REFORM(REFORM(tmp,1,N_ELEMENTS(tmp)))
  tmp=tmp[SORT(tmp)]
  scenes_in=tmp[UNIQ(tmp)]
  ;stop
  
  lines=read_txt(file_baseline_tab)
  
  
  masterId=0
  ;stop
  lines_p=str_lines2arr(lines)
  
  scenes_all=REFORM(lines_p[0,*])
  inds=INTARR(N_ELEMENTS(scenes_in))
  FOR i=0, N_ELEMENTS(scenes_in)-1 DO BEGIN
    pos=WHERE(scenes_all EQ scenes_in[i])
    inds[i]=pos
  ENDFOR
  pos=WHERE(inds NE -1)
  IF pos[0] EQ -1 THEN RETURN
  
  lines_p=lines_p[*,inds[pos]]
  
  ;stop
  
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
      ;IF ABS(jdJ-jdI) LE 330 THEN CONTINUE
      ;      IF (FIX(ABS(jdJ-jdI)) MOD 360) GT 90 THEN CONTINUE
      
      links[i,j]=1
      count=count+1
      PRINTF,fid,path_intf_all,ids[i],ids[j], $
        path_intf_all,ids[i],ids[j],  $
        ids[i],ids[j], blens[j]-blens[i], $
;        format='(a,a,"_",a,"/unwrap.grd ",a,a,"_",a,"/corr.grd",1x,a,1x,a,1x,f)'
        format='(a,a,"_",a,"/unwrap.grd ",a,a,"_",a,"/corr_cut.grd",1x,a,1x,a,1x,f)'
        ;format='(a,a,"_",a,"/unwrap_mask.grd ",a,a,"_",a,"/corr_cut.grd",1x,a,1x,a,1x,f)'
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