;+
; :Author: tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
PRO ON_CW_TSTYPE_INIT, ID
  STASH = WIDGET_INFO(ID, /CHILD)
  WIDGET_CONTROL, STASH, GET_UVALUE=ST
  WIDGET_CONTROL, STASH, SET_DROPLIST_SELECT=WHERE(ST.VAL EQ ST.INITIAL)
  
END

;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
PRO ON_CW_TSTYPE_DP_DATATYPE,EVENT
END

;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
PRO TSTYPE_SET_VALUE, ID, VALUE
END

;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION TSTYPE_GET_VALUE, ID
  STASH = WIDGET_INFO(ID, /CHILD)
  WIDGET_CONTROL, STASH, GET_UVALUE=ST
  SEL=WIDGET_INFO(ST.ID,/DROPLIST_SELECT)
  VALS=ST.VAL
  RETURN,VALS[SEL]
END

;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION TSTYPE_EVENT, EV
  PARENT=EV.HANDLER 
  RETURN, { ID:PARENT, TOP:EV.TOP, HANDLER:0L }
END

;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION CW_TSTYPE, PARENT, $
    UVALUE = UVAL, $
    UNAME = UNAME, $
    TAB_MODE = TAB_MODE, $
    TITLE=TITLE, $
    INITIAL=INITIAL, $
    EVENT_PRO=EPRO, $
    VALUE=VALUE
    
    
  IF (N_PARAMS() EQ 0) THEN MESSAGE, 'Must specify a parent for CW_TSTYPE'
  
  ; DEFAULTS FOR KEYWORDS
  IF NOT (KEYWORD_SET(UVAL))  THEN UVAL = 0
  IF NOT (KEYWORD_SET(UNAME))  THEN UNAME = 'CW_TSTYPE_UNAME'
  IF NOT (KEYWORD_SET(INITIAL))  THEN INITIAL = 'SIO NEU'
  
  IF NOT (KEYWORD_SET(CAP)) THEN CAP=['A','B','C']
  ;IF NOT (KEYWORD_SET(TITLE)) THEN TITLE='Data Type:'
  IF N_ELEMENTS (TITLE) EQ 0 THEN TITLE='Data Type:'
  IF N_ELEMENTS(EPRO) EQ 0 THEN EPRO=''
  
  IF N_ELEMENTS(VALUE) EQ 0 THEN BEGIN
    ;FOR IGPS TIME SERIES
    ;Mon May 10 10:53:58 CST 2010 tianyf
    ;  Some items are experimental/personal. Thus, in the final release of iGPS, 
    ;  these types are commented out.
    ;  However, the codes related to them are still kept in iGPS collection. But
    ;  these "redundant" codes do not interfere with the official iGPS functions.
    VALUE=[ 'SIO NEU [ATS]', $
      'SIO NEU',  $
      'QOCA MAP', $
      'SIO XYZ',	$
      'SIO RAW XYZ',  $
      'PBO XYZ',  $
      'PBO NEU',  $
      'NGL TENV3',  $
      'NGL TENV',  $
      'JPL SERIES',  $
      'JPL LAT/LON/RAD', $
      'APLO LOAD COMB', $
      'Caltech Nepal', $
      'CATS', $
      ;'CATS PSD', $
      'CMONOC TS', $				;TIME SERIES FOR CMONOC
      ;'CORRCOEF XY', $
      ;'CORRCOEF MAT', $
      'EOST Loading Service', $
      ;'EST_NOISE OTD', $
      ;'EST_NOISE OTR', $
      ;'EST_NOISE OTX', $
      'EST_NOISE RESID', $
      ;'EST_NOISE RESID MODEL', $
      ;'EST_NOISE RESID DATA', $
      'GEONET', $
      'GLOBK', $  ;globk mb_SITE_GPS.dat? files
      'GPS LAB', $
      'GSI', $
      ;'Highrate NEU' $
      'ISCEA',  $
      'ISCEA [Detrend]',  $
      'ISCEA BERN',  $
      ;'EABM NEU', $
      'ITRF05 RESIDUAL', $
      ;'PANGA LAT/LON/RAD', $
      ;'POWER SPECTRUM', $
      'QOCA ATML LOAD', $
      'QOCA OTL LOAD', $
      'QOCA SNOW LOAD', $
      'QOCA SOIL LOAD', $
      'SBL LOAD', $
      'SBL LOAD [Operational]', $
      'SCEC Transient CSV', $
      'Tah mb_ files', $
      'TRACK', $
      ;'USGS PASADENA', $
      'USGS RNEU', $
      'YICE', $
      'YICE2', $
      'ERCE', $
      'Taiwan' $
      ]
  ENDIF
  ST = { ID:0, $
    INITIAL:INITIAL, $
    EPRO:EPRO, $
    VAL:VALUE }
    
  ;
  MAINBASE = WIDGET_BASE(PARENT, UVALUE = UVAL, UNAME = UNAME, $
    EVENT_FUNC = "TSTYPE_EVENT", $
    FUNC_GET_VALUE = "TSTYPE_GET_VALUE", $
    PRO_SET_VALUE = "TSTYPE_SET_VALUE", $
    NOTIFY_REALIZE='ON_CW_TSTYPE_INIT')
    
  IF ( N_ELEMENTS(TAB_MODE) NE 0 ) THEN $
    WIDGET_CONTROL, MAINBASE, TAB_MODE = TAB_MODE
    
  UNAMESTR=STRJOIN(STRSPLIT(SYSTIME(),/EXTRACT))
  UNAMESTR=STRJOIN(STRSPLIT(UNAMESTR,':',/EXTRACT))
  IF EPRO EQ '' THEN BEGIN
    ST.ID = WIDGET_DROPLIST( MAINBASE ,  $
      VALUE=VALUE,  $
      UNAME=UNAMESTR, TITLE=TITLE)
  ENDIF ELSE BEGIN
    ST.ID = WIDGET_DROPLIST( MAINBASE ,  $
      VALUE=VALUE,  $
      UNAME=UNAMESTR, TITLE=TITLE, $
      EVENT_PRO=EPRO)
  ENDELSE
  WIDGET_CONTROL, WIDGET_INFO(MAINBASE, /CHILD), SET_UVALUE=ST, /NO_COPY
  RETURN, MAINBASE
  
END

;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;TEST
PRO CW_TSTYPE
  BASE=WIDGET_BASE()
  DPL_TEST=CW_TSTYPE(BASE,UNAME='DPL_TEST')
  WIDGET_CONTROL,BASE,/REALIZE
  
END





