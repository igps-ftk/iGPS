;APR09 2007 TIAN
;  +A LITTLE PROGRESS FOR LOOP, MUCH FASTER NOW;
;SIO:
;	COMMENTS LINES START WITH A '#'
;
PRO READ_SIO_ATS, FILE, SITE = SITE, $
    FIRSTEPOCH = FIRSTEPOCH, $
    LASTEPOCH = LASTEPOCH, $
    XYZREF = XYZREF, $
    NEUREF = NEUREF, $
    DATA = DATA, $
    NH=NH, $
    NS=NS, $
    NL=NL, $
    HEADERS = HEADERS, $
    IOERR=IOERR ;ADDED BY TIANYF ON APRIL 14, 2012
  ;
  IOERR=0
  ;
  IF N_PARAMS() LT 1 THEN BEGIN  ;test example
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','sio', $
      'cleanedNeuUnf'],'bjfsCleanUnf.neu')
    FILE='J:\tmp\sopac\measures\ats\WesternNorthAmerica\WNAM_Filter_ResidNeuTimeSeries_comb_20140326\p066FilterResid.neu'
  ENDIF
  ;
  ;ON APRIL 14, 2012 BY TIANYF
  ;check whether the file is zero
  IF TXT_LINES(FILE) LE 0 THEN BEGIN
    IOERR=1
    DATA=''
    RETURN
  ENDIF
  
  QUERY_SIO_ATS, FILE, $
    NS=NS, $
    NL=NL, $
    NH=NH, $
    HEADERS=HEADERS,	$
    SLOPE_N=SLOPE_N, PSDECAY_N=PSDECAY_N, OFFSET_N=OFFSET_N, ANNUAL_N=ANNUAL_N, SEMIANNUAL_N=SEMIANNUAL_N,	$
    SLOPE_E=SLOPE_E, PSDECAY_E=PSDECAY_E, OFFSET_E=OFFSET_E, ANNUAL_E=ANNUAL_E, SEMIANNUAL_E=SEMIANNUAL_E,	$
    SLOPE_U=SLOPE_U, PSDECAY_U=PSDECAY_U, OFFSET_U=OFFSET_U, ANNUAL_U=ANNUAL_U, SEMIANNUAL_U=SEMIANNUAL_U,	$
    FAILED = FAILED
  ;stop
  IF NL GT 0 THEN BEGIN
    READ_COLS_ASCII,FILE,SKIP=NH,DATA=DATAC,HEADERS = HEADERS
    IF KEYWORD_SET(STR) THEN BEGIN
      DATA=DATAC
    ENDIF ELSE BEGIN
      DATA=DOUBLE(DATAC)
    ENDELSE
    IF N_PARAMS() LT 1 THEN BEGIN
      HELP,HEADERS,DATAC,DATA
      PRINT, DATA[*,0:1]
    ENDIF
  ENDIF
  
  ;Remove NaN lines
  ; by tyf on Nov-29-2013
  ind_N=FINITE(reform(data[3,*]))
  ind_E=FINITE(reform(data[4,*]))
  ind_U=FINITE(reform(data[5,*]))
  ind2=WHERE(ind_N EQ 0 or IND_E EQ 0 OR IND_U EQ 0)
  IF ind2[0] NE -1 THEN BEGIN
    IF N_ELEMENTS(ind2) EQ N_ELEMENTS(ind_N) THEN BEGIN
      ;all NaN.
      PRINT,'[READ_SIO]WARNING: no valid data in file ',file+' !'
    ENDIF ELSE BEGIN
      ;partial NaN
      rowids=ind2/N_ELEMENTS(data[*,0])
      rowids2=rowids[UNIQ(rowids)]
      remids=inv_ind(rowids2,top=N_ELEMENTS(data[0,*]))
      data=data[*,remids]
      ;STOP
    ENDELSE
  ENDIF
END
