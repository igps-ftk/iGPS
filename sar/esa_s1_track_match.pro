PRO ESA_S1_TRACK_MATCH

  path='C:\Downloads\esa.data\safe\manifest.safe\'  
  opath='D:\gsar\landslide\'
  
;  track='D4'
;  target_date='20141029'
;  ;target_date='20200102'
;  ;exclude_date=['20160101','20160804','20160711']
;  perc_min=.56
  
;  track='D33'
;  target_date='20141031'
;  exclude_date=['20150204']
;  perc_min=.56
  
  
;  track='D62'
;  target_date='20141009'
;  perc_min=.53  
;  exclude_date=['20151227']
  
  
  
;  track='D77'
;  target_date='20141103'
;  ;target_date='20200102'
;  ;exclude_date=['20160101','20160804','20160711']
;  perc_min=.56
  
  
  
  track='D106'
  target_date='20141024'
  target_date='20200102'
  ;exclude_date=['20160101','20160804','20160711']
  perc_min=.55
  
;  track='D135'
;  target_date='20141026'
;  exclude_date=['20160101','20160804','20160711']
;  perc_min=.56
;  
;  
;  track='D150'
;  target_date='20141027'
;  ;target_date='20200102'
;  exclude_date=['20150131','20160102','20160712']
;  perc_min=.56
  
;  track='A26'
;  target_date='20141206'
;  ;exclude_date=['20150131','20160102','20160712']
;  perc_min=.56
  
;  track='A41'
;  target_date='20141020'
;  ;exclude_date=['20150131','20160102','20160712']
;  perc_min=.7
;  
;  track='A55'
;  target_date='20141021'
;  ;exclude_date=['20160811','20160102','20160712']
;  perc_min=.56
  
  
;  track='A70'
;  target_date='20141022'
;  ;exclude_date=['20150131','20160102','20160712']
;  perc_min=.56
  
;  track='A99'
;  target_date='20190107'
;  exclude_date=['20170306']
;  perc_min=.56
  
;  track='A128'
;  target_date='20151126'
;  ;exclude_date=['20150131','20160102','20160712']
;  perc_min=.7
  
;  track='A172'
;  target_date='20141017'
;  ;exclude_date=['20170306']
;  perc_min=.56


  files=FILE_SEARCH(path+PATH_SEP()+track+PATH_SEP(), 'S*.safe', count=nf)
  PRINT,'[]INFO: total scenes '+ STRTRIM( nf,2)
  fnames=GETFILENAME(files)
  
  
  ;remove excluded scenes
  IF N_ELEMENTS(exclude_date) NE -1 THEN BEGIN
    inds_exclude=INTARR(N_ELEMENTS(fnames))
    inds_exclude[*]=0
    FOR ei=0,N_ELEMENTS(exclude_date)-1 DO BEGIN
      tmp=grepi(fnames, exclude_date[ei], line_number=ind_ei)
      IF tmp[0] NE '' THEN BEGIN
        inds_exclude[ind_ei]=1
      ENDIF
    ENDFOR
    pos=WHERE(inds_exclude EQ 0, complement=ind_out)
    IF pos[0] EQ -1 THEN BEGIN
      PRINT,'[]WARNING: no files left after excluding scens!!'
      RETURN
    ENDIF
    IF N_ELEMENTS(pos) NE N_ELEMENTS(fnames) THEN BEGIN
      FOR i=0, N_ELEMENTS(ind_out)-1 DO PRINT,'[]INFO: exclude '+fnames[ind_out[i]]
      PRINT,'[]INFO:'+STRTRIM(N_ELEMENTS(fnames)-N_ELEMENTS(pos),2)+' scenes excluded.'
      fnames=fnames[pos]
      files=files[pos]
      nf=N_ELEMENTS(files)
      PRINT,'[]INFO: remain scenes '+ STRTRIM( nf,2)
    ;STOP
    ENDIF
  ENDIF
  ;STOP
  
  sess_dates=grepi(fnames, target_date,LINE_NUMBER=inds)
  HELP,sess_dates
  IF sess_dates[0] EQ '' THEN BEGIN
    PRINT,'[]WARNING: no files found for '+target_date+'!!'
    RETURN
  ENDIF
  ns=N_ELEMENTS(sess_dates)
  ;stop
  
  ;
  
  FOR si=0, ns-1 DO BEGIN
    sess_date=sess_dates[si]
    tmp1=STRSPLIT(sess_date,'_',/extract)
    tmp2=STRSPLIT(tmp1[8],'.',/extract)
    ;HELP,tmp1,tmp2
    sess_id=track+'-'+tmp1[4]+'.'+tmp2[0]
    PRINT,sess_id
    opath_t=opath+PATH_SEP()+sess_id
    IF FILE_TEST(opath_t,/directory) NE 1 THEN BEGIN
      FILE_MKDIR, opath_t
    ENDIF
    
    ofile=opath_t+PATH_SEP()+'overlapping.'+sess_date+'.txt'
    IF FILE_TEST(ofile,/regular) EQ 1 THEN BEGIN
      PRINT,'skip ',sess_id
      CONTINUE
      
    ENDIF
    ;
    PRINT, '[]INFO: searching files for '+sess_date+' ...'
    PRINT,files[inds[si]]
    
    exclude_date_si=''
    
    file_exclude=opath_t+PATH_SEP()+'exclude_date.txt'
    IF FILE_TEST(file_exclude,/regular) EQ 1 THEN BEGIN
      exclude_date_si=read_txt(file_exclude,comment='~ ')
      exclude_date_si=STRTRIM(exclude_date_si,2)
    ENDIF
    HELP,exclude_date_si
    
    ;STOP
    SAR_S1_MANIFEST_OVERLAPPING, files=files, opath=opath_t,target=sess_date, perc_min=perc_min, exclude_date=exclude_date_si
    
    ;STOP
    ;BREAK
  ENDFOR
  
END