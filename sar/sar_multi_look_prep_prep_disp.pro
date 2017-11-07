PRO SAR_MULTI_LOOK_PREP_PREP_DISP
  ;Paths storing LOS time series for all site.
  ;each path contains time series files for one individual satellite looking direction
  paths=['D:\gsar\asc\dangxiong.b\asc_F3\SBAS\x5\raw.flt',$
    'D:\gsar\asc\mila1\asc_F1\SBAS4\nc.detrend\x5\raw.flt',$
    'D:\gsar\des\dangxiong2.b\des_F1\SBAS\x5\raw',$
    'D:\gsar\des\mila2\des_F3\SBAS8\x5\raw.flt']
    
  lfile='D:\gsar\asc\mila1\asc_F1\SBAS4\nc.detrend\x5\raw\sites.net'
  ;STOP
  
  opath='C:\Papers.data\sse.mila\multi-look\ts'
  
  nlook=N_ELEMENTS(paths) ; number of looking direction
  
  dt=30d0/365.25d0
  
  ;first, get the common sites
  sites=''
  WINDOW,1,xsize=1800
  PLOT,[0],[0],xrange=[2014,2018],yrange=[-40,40],background='ffffff'x,color='0'x,/nodata
  dyrs1=REPLICATE(PTR_NEW(),nlook)
  nday=-1
  FOR pi=0, nlook-1 DO BEGIN
    files=FILE_SEARCH(paths[pi]+PATH_SEP()+'*.neu', count=nf)
    IF nf LE 0 THEN BEGIN
      PRINT,'[]ERROR: no data files found for '+paths[pi]+'!!'
      RETURN
    ENDIF
    
    READ_SIO, files[0], data=data
    
    sites_i=strmids(GETFILENAME(files),0,4)
    HELP,sites_i
    IF sites[0] EQ '' THEN BEGIN
      sites=sites_i
      dyrs2=REFORM(data[0,*])
    ENDIF ELSE BEGIN
      tmp=set_intersect(sites,sites_i,ind0=ind0,ind1=ind1)
      sites=tmp
      dyrs2=[dyrs2,REFORM(data[0,*])]
    ENDELSE
    dyrs1[pi]=PTR_NEW(REFORM(data[0,*]))
    ;if n_elements(data[0,*]) gt nday then nday=n_elements(data[0,*])
    ;
    psym='-'+STRTRIM(pi+1,2)
    OPLOT,data[0,*],(data[3,*]-data[3,0])*1d3,color='0'x,psym=psym
    
  ENDFOR
  tmp=dyrs2[SORT(dyrs2)]
  dyrs2b=tmp[UNIQ(tmp)]
  OPLOT,dyrs2b,REPLICATE(20,N_ELEMENTS(dyrs2)),psym=5,color='0000ff'x
  nday=N_ELEMENTS(dyrs2b)
  dyrs3=DBLARR(nday,nlook)
  FOR i=0, nlook-1 DO BEGIN
    tmp=*dyrs1[i]
    FOR j=0,N_ELEMENTS(tmp)-1 DO BEGIN
      pos=WHERE(dyrs2b EQ tmp[j])
      IF pos[0] EQ -1 THEN STOP
      dyrs3[pos,i]=tmp[j]
    ENDFOR
  ENDFOR
  ;
  dyrs_1=*dyrs1[0]
  inds_time=INTARR(N_ELEMENTS(dyrs_1),4)
  inds_time[*]=-1
  FOR i=0,N_ELEMENTS(dyrs_1)-1 DO BEGIN
    dyr=dyrs_1[i]
    inds_time[i,0]=i
    FOR j=1,nlook-1 DO BEGIN
      dyrs_j=*dyrs1[j]
      tmp=MIN(ABS(dyrs_j-dyr),ind)
      IF tmp GT dt THEN GOTO, next_epoch
      inds_time[i,j]=ind
    ENDFOR
    
    ;STOP
    IF (WHERE(inds_time[i,*] EQ -1))[0] EQ -1 THEN BEGIN
      OPLOT,[dyr],[10],psym=1,color='ff0000'x
    ENDIF
    
    next_epoch:
  ENDFOR
  ;STOP
  
  FOR i=0,N_ELEMENTS(dyrs1)-1 DO BEGIN
    IF PTR_VALID(dyrs1[i]) THEN PTR_FREE,dyrs1[i]
  ENDFOR
  
  PRINT,'epochs of output:',dyrs_1[inds_time[*,0]]
  
  ;read all time series
  files1=FILE_SEARCH(paths[0]+PATH_SEP()+'*.neu', count=nf1)
  sites1=strmids(GETFILENAME(files1),0,4)
  
  READ_NET, lfile, site=sites1,llh=llhs
  
  ;get the satellite looking incident angles
  
  
  ;RETURN
  ;stop
  data_all=DBLARR(N_ELEMENTS(inds_time[*,0]),nf1,nlook)
  data_all[*]=!values.D_NAN
  FOR si=0,N_ELEMENTS(sites1)-1 DO BEGIN  ;look for each site
    ;find site files for all looks
    site=sites1[si]
    files_site=STRARR(nlook)
    files_site[0]=files1[si]
    FOR j=1,nlook-1 DO BEGIN
      files2=FILE_SEARCH(paths[j]+PATH_SEP()+site+'*.neu', count=nf2)
      IF nf2 LE 0 THEN GOTO, next_site
      files_site[j]=files2
    ENDFOR
    ;
    tmp=WHERE(files_site EQ '')
    ;stop
    IF tmp[0] EQ -1 THEN BEGIN
      FOR j=0,nlook-1 DO BEGIN
        READ_SIO, files_site[j], data=data
        data_all[*,si,j]=data[3,inds_time[*,j]]
      ;STOP
      ENDFOR
    ENDIF
    
    next_site:
  ENDFOR
  
  FOR i=0,N_ELEMENTS(inds_time[*,0])-1 DO BEGIN
    ofile=opath+PATH_SEP()+'disp_'+STRTRIM(dyrs_1[inds_time[i,0]],2)+'.txt'
    OPENW,fid,ofile,/get_lun
    FOR j=0,nf1-1 DO BEGIN
      PRINTF,fid,sites1[j],llhs[0:1,j],data_all[i,j,*],format='(1x,a4,2(1x,f12.6),1000(1x,f12.6))'
    ENDFOR
    FREE_LUN,fid
  ENDFOR
;STOP
  
END