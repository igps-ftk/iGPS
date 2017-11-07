;+
;*Modifications:
;  +fix a bug on site name matching, by tyf on March 18, 2013
;    for details see the below comment.
;  +return NaN values when site not found. by tyf on May 13, 2015
;
;READ QOCA NET A PRIORI COORDINATE FILE
;*****************************************************************************************************************************
;* Datum:  WGS84
;* Format:  V30
;* Transfered from:   itrf05.apr
;* Time:  2010  5 12  8: 55: 55
;* Operator:  tianyf
;*
;*  site    full-name      latitude        longitude         height       u       v       w    epoch       t1        t2
; ALBH_2PS  itrf_apr  N48 23 23.213242 W123 29 14.892299     31.75972 -0.0076 -0.0076  0.0010 2001.0470 1900.0000 2500.0000
; ALBH_3PS  itrf_apr  N48 23 23.212690 W123 29 14.892888     31.74527 -0.0076 -0.0076  0.0010 2003.3030 1900.0000 2500.0000
; ALBH_4PS  itrf_apr  N48 23 23.212467 W123 29 14.893289     31.74477 -0.0076 -0.0076  0.0010 2003.9980 1900.0000 2500.0000
;
PRO READ_NET, FILE, $ ; QOCA Network format coordinate file {input, string}
  SITE=SITES, $ ; 4-char site names {input, [NSIT]}. If missing or blank, return data of all sites.
  LLH=LLHS      ; geographic coordinates (longitude, latitude, height) {output, [3,NSIT]}

  ;If no FILE given, use the default one.
  IF N_PARAMS() LT 1 THEN BEGIN
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['tables'],'sio.net')  ;the default a priori coordinates file
    ;SITES=['BJFS','SHAO', 'sha2']
  ENDIF
  ;STOP

  ;READ IN ALL LINES BEGIN WITH A BLANK
  LINES=READ_TXT(FILE,COMMENT='~ ')

  IF N_ELEMENTS(LINES) LE 0 || LINES[0] EQ '' THEN BEGIN
    PRINT,'[READ_NET]ERROR: no data lines found in input file '+FILE+'!!'
    RETURN
  ENDIF

  ;INITIATE VARIABLES
  SITE_=''
  NS=''
  LAT_DD=0
  LAT_MM=0
  LAT_SS=0D0
  EW=''
  LON_DD=0
  LON_MM=0
  LON_SS=0D0
  HT=0D0
  VELN=0D0
  VELE=0D0
  VELU=0D0
  DYR=0D0
  T1=0D0
  T2=0D0



  IF N_ELEMENTS(SITES) GT 0 && STRTRIM(STRJOIN(SITES),2) NE '' THEN BEGIN ;IS REQUEST FOR SPECIFIC SITES
    LLHS=DBLARR(3,N_ELEMENTS(SITES))
    LLHS[*]=!VALUES.D_NAN
    LINESU=STRUPCASE(LINES)
    FOR SI=0,N_ELEMENTS(SITES)-1 DO BEGIN
      ;POS=STRPOS(LINESU,' '+STRUPCASE(SITES[SI]))
      POS=STRPOS(LINESU,' '+STRUPCASE(SITES[SI])+'_GPS')  ;if the site name is
      ;                 a 4-digit integer, it may appear in the corrdinate values,
      ;                 which causes the wrong searching results.
      ;                 by tyf on March 18, 2013
      IND=WHERE(POS NE -1)
      IF IND[0] EQ -1 THEN CONTINUE
      LINE=LINES[LAST(IND)]
      ;STOP
      ;print,si
      READS,LINE,SITE_,NS,LAT_DD,LAT_MM,LAT_SS,EW,LON_DD,LON_MM,LON_SS,HT,VELN,VELE,VELU, $
        DYR,T1,T2, $
        FORMAT='(1X,A4,4X,2X,8X,2X,A1,I2,1X,I2,1X,F9.6,1X,A1,I3,1X,I2,1X,F9.6,1X,F12.5, 3(1X,F7.4),3(1X,F9.4))'
      LON=LON_DD+LON_MM/60D0+LON_SS/3600D0
      LAT=LAT_DD+LAT_MM/60D0+LAT_SS/3600D0
      IF EW EQ 'W' THEN LON=-1*LON
      IF NS EQ 'S' THEN LAT=-1*LAT
      ;STOP
      LLHS[*,SI]=[LON,LAT,HT]
    ENDFOR
    LLHS=REFORM(LLHS)
  ENDIF ELSE BEGIN  ;IF NO SITES SPECIFIED, THEN RETURN ALL SITES.
    ;STOP
    NL=N_ELEMENTS(LINES)
    SITES=STRARR(NL)
    LLHS=DBLARR(3,NL)
    FOR LI=0, NL-1 DO BEGIN
      ;help,li
      LINE=LINES[LI]
      ;STOP
      READS,LINE,SITE_,NS,LAT_DD,LAT_MM,LAT_SS,EW,LON_DD,LON_MM,LON_SS,HT,VELN,VELE,VELU, $
        DYR,T1,T2, $
        FORMAT='(1X,A4,4X,2X,8X,2X,A1,I2,1X,I2,1X,F9.6,1X,A1,I3,1X,I2,1X,F9.6,1X,F12.5, 3(1X,F7.4),3(1X,F9.4))'
      LON=LON_DD+LON_MM/60D0+LON_SS/3600D0
      LAT=LAT_DD+LAT_MM/60D0+LAT_SS/3600D0
      IF EW EQ 'W' THEN LON=-1*LON
      IF NS EQ 'S' THEN LAT=-1*LAT
      SITES[LI]=SITE_
      ;STOP
      LLHS[*,LI]=[LON,LAT,HT]
    ENDFOR
  ENDELSE
  ;STOP
  
  IF (NOT ARG_PRESENT(SITE)) AND (NOT ARG_PRESENT(FILE)) THEN BEGIN
    HELP,FILE,SITES,LLHS
    ;PRINT,SITES,LLHS
  ENDIF


END