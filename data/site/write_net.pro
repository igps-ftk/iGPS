;+
; :Description:
;    Write QOCA NET a priori coordiante file (*.net).
;
;
;
;
; :Author: tianyf
;-
PRO WRITE_NET, $
    OFILE, $
    SITES=SITES, $
    LLHS=LLHS, $
    USER=USER, $
    PROG=PROG, $
    SRC=SRC, $
    EPOCHS=EPOCHS
    
  IF N_ELEMENTS(OFILE) EQ 0 THEN BEGIN
    PRINT,'[WRITE_NET]ERROR: no output file specified!'
    RETURN
  ENDIF
  
  IF N_ELEMENTS(SITES) LE 0 THEN BEGIN
    PRINT,'[WRITE_NET]ERROR: no input sites!'
    RETURN
  ENDIF
  
  IF N_ELEMENTS(PROG) EQ 0 THEN PROG='WRITE_NET'
  IF N_ELEMENTS(SRC) EQ 0 THEN SRC=''
  IF N_ELEMENTS(EPOCHS) EQ 0 THEN BEGIN
    EPOCHS=DBLARR(N_ELEMENTS(SITES))
    doy,dyear=dyr
    epochs[*]=dyr
  ENDIF
  
  NSIT=N_ELEMENTS(SITES)
  
  OPENW,FID,OFILE,/GET_LUN
  WRITE_SYS_INFO,FID,SRC=SRC,$
    USER=USER, $
    PROG=PROG
  PRINTF,FID,'*****************************************************************************************************************************'
  PRINTF,FID,'* Datum:  WGS84'
  PRINTF,FID,'* Format:  V30'
  PRINTF,FID,'* Transfered from:   '+SRC
  PRINTF,FID,'* Time:  '
  PRINTF,FID,'* Operator:  '
  PRINTF,FID,'*'
  PRINTF,FID,'*  site    full-name      latitude        longitude         height       u       v       w    epoch       t1        t2'
  
  ;FOR I=0ull,NSIT-1 DO printf,fid,STRUPCASE(SITES[I]),llhs[*,i],format='(1x,a,3(1x,f))'
  FOR I=0ull,NSIT-1 DO BEGIN
    IF LLHS[0,I] EQ -9999 || TOTAL(LLHS[*,I]) EQ 0 THEN CONTINUE
    LAT=LLHS[1,I]
    LON=LLHS[0,I]
    HT=LLHS[2,I]
    IF LAT LT 0 THEN BEGIN
      NS='S'
    ENDIF ELSE BEGIN
      NS='N'
    ENDELSE
        
    IF LON LT -180 THEN BEGIN
      LON=LON+360
    ENDIF
    IF LON GT 180 THEN BEGIN
      LON=LON-360
    ENDIF
    IF LON LT 0 THEN BEGIN
      EW='W'
    ENDIF ELSE BEGIN
      EW='E'
    ENDELSE
    LON=ABS(LON)
    LON_DD=FIX(LON)
    LON_MM=FIX((LON-LON_DD)*60D0)
    LON_SS=(LON-LON_DD-LON_MM/60D0)*3600D0
    LAT=ABS(LAT)
    LAT_DD=FIX(LAT)
    LAT_MM=FIX((LAT-LAT_DD)*60D0)
    LAT_SS=(LAT-LAT_DD-LAT_MM/60D0)*3600D0
    
    PRINTF,FID,STRUPCASE(SITES[I]),NS,LAT_DD,LAT_MM,LAT_SS,EW,LON_DD,LON_MM,LON_SS,HT,-0,-0,-0, $
      EPOCHS[I],1900,2500, $
      FORMAT='(1X,A4,"_GPS",2X,"site_log",2X,A1,I2,1X,I2,1X,F9.6,1X,A1,I3,1X,I2,1X,F9.6,1X,F12.5, 3(1X,F7.4),3(1X,F9.4))'
  ENDFOR
  FREE_LUN,FID
  
  ;PRINT,'[WRITE_NET]Normal end.'
  
END