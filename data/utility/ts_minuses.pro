PRO TS_MINUSES, path1, path2, opath, $
  prog=prog_in, $
  tlb=tlb   ; used for update igps status text, if specified.

  PROG='TS_MINUSES'
  IF N_ELEMENTS(PROG_IN) NE 0 THEN PROG=PROG_IN+':'+PROG

  IF N_PARAMS() LT 3 THEN BEGIN
    path1='J:\phd\expt\gpsf\external\iGPS\example\eq.nepal20150425\pos.neu.transient\cmc0'
    path2='J:\phd\expt\gpsf\external\iGPS\example\eq.nepal20150425\pos.neu.transient\cmc3'
    opath='J:\phd\expt\gpsf\external\iGPS\example\eq.nepal20150425\pos.neu.transient\cmc.diff.0-3'
  ENDIF
  files1=FILE_SEARCH(path1+PATH_SEP()+'*.neu', count=nf1)
  ;nf1=5

  IF NF1 LE 0 THEN BEGIN
    PRINT,'['+prog+'] ERROR: no input files!'
    RETURN
  ENDIF

  LBL_ID=-1
  IF N_ELEMENTS(TLB) NE 0 THEN BEGIN
    LBL_ID=WIDGET_INFO(TLB, FIND_BY_UNAME='LBL_STATUS')
  ENDIF


  ;loop for each file
  FOR FI=0,NF1-1 DO BEGIN
    FILE1=FILES1[FI]
    SITE=STRMID(GETFILENAME(FILE1),0,4)

    files2=FILE_SEARCH(path2+PATH_SEP()+site+'*.neu', count=nf2)
    IF nf2 LE 0 THEN BEGIN
      str_status=STRING('['+prog+'] WARNING: no second file found for site {'+site+'}!')
      PRINT, str_status
      IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE=STR_STATUS
      CONTINUE
    ENDIF
    file2=files2[0]

    str_status=STRING('['+prog+'] Processing '+STRTRIM(FI+1,2)+'/'+STRTRIM(NF1,2)+' '+SITE+' <'+GETFILENAME(FILE1)+';'+GETFILENAME(FILE2)+'> ...')
    PRINT, str_status
    IF LBL_ID NE -1 THEN WIDGET_CONTROL,LBL_ID,SET_VALUE=STR_STATUS

    READ_SIO, FILE1, DATA=DATA1
    SRC=FILE1
    READ_SIO, FILE2, DATA=DATA2
    SRC=[SRC,FILE2]

    YDOYSTR1=STRING(DATA1[1,*],FORMAT='(I04)')+STRING(DATA1[2,*],FORMAT='(I03)')
    YDOYSTR2=STRING(DATA2[1,*],FORMAT='(I04)')+STRING(DATA2[2,*],FORMAT='(I03)')
    YDOYSTR12=SET_INTERSECT(YDOYSTR1,YDOYSTR2,IND0=IND1,IND1=IND2)
    ODATA1=DATA1[*,IND1]
    ODATA2=DATA2[*,IND2]

    ODATA=ODATA1[0:5,*]
    ODATA[3:5,*]=0
    FOR NEUI=3,5 DO BEGIN
      ;POS=WHERE(ODATA1[NEUI,*] NE 0 AND ODATA2[NEUI,*] NE 0)
      ;IF POS[0] EQ -1 THEN CONTINUE
      ;ODATA[NEUI,POS]=ODATA1[NEUI,POS]-ODATA2[NEUI,POS]
      ODATA[NEUI,*]=ODATA1[NEUI,*]-ODATA2[NEUI,*]
    ENDFOR

    OFILE=OPATH+PATH_SEP()+SITE+'-diff.neu'
    WRITE_SIO, OFILE, DATA=ODATA, USER=USER, $
      PROG=prog, $
      SRC=SRC
    ;BREAK
  ENDFOR
  PRINT,'['+prog+'] Normal end.'
END