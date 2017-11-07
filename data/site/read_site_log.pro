;+
; :Description:
;    Read part information from IGS site log files.
;
; :Params:
;    FILE
;
; :Keywords:
;    SITE
;    OFFSET
;    XYZ
;    LLH
;
; :Author: tianyf
;-
PRO READ_SITE_LOG,FILE,SITE=SITE,OFFSET=OFFSET,XYZ=XYZ,LLH=LLH,DATE_INST=DATE_INST
  IF N_PARAMS() LT 1 THEN BEGIN
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','site_log'],'bjfs.log')
  ENDIF
  
  SITE=STRMID(GETFILENAME(FILE),0,4)
  LINES=READ_TXT(FILE)
  NL=N_ELEMENTS(LINES)
  IF NL LT 0 THEN RETURN
  
  
  IS_NEW_ANTENNA=0
  OFFSET=-9999D0
  XYZ=DBLARR(3)
  XYZ=[-9999,-9999,-9999]*1D0
  LLH=DBLARR(3)
  LLH=XYZ
  FOR LI=0,NL-1 DO BEGIN
    LINE=LINES[LI]
    
    IF ARG_PRESENT(XYZ) THEN BEGIN
    
      ;GET XYZ COORDINATES
      POS=STRPOS(LINE,'X coordinate (m)')
      IF POS[0] NE -1 THEN BEGIN
        ;STOP
        TMP=STRSPLIT(LINE,':',/EXTRACT)
        IF N_ELEMENTS(TMP) LT 2 THEN BEGIN
          XYZ=[-9999,-9999,-9999]
          LLH=[-9999,-9999,-9999]
          RETURN
        ENDIF
        XYZ[0]=DOUBLE(TMP[1])
        CONTINUE
      ENDIF
      POS=STRPOS(LINE,'Y coordinate (m)')
      IF POS[0] NE -1 THEN BEGIN
        TMP=STRSPLIT(LINE,':',/EXTRACT)
        XYZ[1]=DOUBLE(TMP[1])
        CONTINUE
      ENDIF
      POS=STRPOS(LINE,'Z coordinate (m)')
      IF POS[0] NE -1 THEN BEGIN
        TMP=STRSPLIT(LINE,':',/EXTRACT)
        XYZ[2]=DOUBLE(TMP[1])
        CONTINUE
      ENDIF
    ENDIF
    
    IF ARG_PRESENT(LLH) THEN BEGIN
      ;GET LATITUDE,LONGITUDE,HEIGHT
      POS=STRPOS(LINE,'Latitude (N is +)')
      ;POS=STRPOS(LINE,'Latitude (deg)')
      IF POS[0] NE -1 THEN BEGIN
        TMP=STRSPLIT(LINE,':',/EXTRACT)
        IF N_ELEMENTS(TMP) LT 2 THEN BEGIN
          XYZ=[-9999,-9999,-9999]
          LLH=[-9999,-9999,-9999]
          RETURN
        ENDIF
        LLH[1]=DOUBLE(TMP[1]);*1D-4
        CONTINUE
      ENDIF
      POS=STRPOS(LINE,'Longitude (E is +)')
      ;POS=STRPOS(LINE,'Longitude (deg)')
      IF POS[0] NE -1 THEN BEGIN
        TMP=STRSPLIT(LINE,':',/EXTRACT)
        LLH[0]=DOUBLE(TMP[1]);*1D-4
        CONTINUE
      ENDIF
      POS=STRPOS(LINE,'Elevation (m,ellips.)')
      IF POS[0] NE -1 THEN BEGIN
        TMP=STRSPLIT(LINE,':',/EXTRACT)
        LLH[2]=DOUBLE(TMP[1])
        CONTINUE
      ENDIF
    ENDIF
    
    
    IF ARG_PRESENT(OFFSET) THEN BEGIN
      ;PRINT,IS_NEW_ANTENNA,LINE
      POS=STRPOS(LINE,'Receiver Type')
      IF POS[0] NE -1 THEN BEGIN
        LINE=STRSPLIT(LINE,/EXTRACT)
        IF LINE[0] NE '3.x'  AND STRMID(LINE[0],0,2) EQ '3.' THEN BEGIN
          IS_NEW_ANTENNA=1
          ANT_TYPE=LINE[4:*]
        ENDIF
      ENDIF
      IF IS_NEW_ANTENNA THEN BEGIN
        POS=STRPOS(LINE,'Date Installed')
        IF POS[0] NE -1 THEN BEGIN
          LINE=STRSPLIT(LINE,/EXTRACT)
          if n_elements(line) lt 4 then continue
          DATE_INST=LINE[3]
          YEAR=FIX(STRMID(DATE_INST,0,4))
          MM=FIX(STRMID(DATE_INST,5,2))
          DD=FIX(STRMID(DATE_INST,8,2))
          ;DD=DD+1
          HH=FIX(STRMID(DATE_INST,11,2))
          MI=FIX(STRMID(DATE_INST,14,2))
          ;HH=12
          ;MI=0
          ;PRINT,YEAR,MM,DD,HH,MI
          ;PRINT,DATE_INST,ANT_TYPE
          ;STOP
          if year le 1900 then begin
            dyr=-9999
          endif else begin
            DOY,YEAR,MM,DD,HH,MI,DYEAR=DYR
          endelse
          ;PRINT,DATE_INST,DYR
          IF OFFSET[0] EQ -9999 THEN BEGIN
            OFFSET=DYR*1D0
          ENDIF ELSE BEGIN
            OFFSET=[OFFSET,DYR]
          ENDELSE
          IS_NEW_ANTENNA=0
        ENDIF
      ENDIF
    ENDIF
  ENDFOR
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,SITE,OFFSET
    PRINT,SITE,OFFSET
    HELP,XYZ,LLH
    PRINT,XYZ,LLH
  ENDIF
  
END