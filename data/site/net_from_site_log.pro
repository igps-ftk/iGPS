;+
; :Description:
;    Create QOCA NET a priori coordiante file (*.net) from IGS site log files.
;
; :Params:
;    PATH
;    OFILE
;
;
;
; :Author: tianyf
;-
PRO NET_FROM_SITE_LOG,PATH,OFILE
  IF N_PARAMS() LT 2 THEN BEGIN
    ;PATH=DIALOG_PICKFILE(TITLE='Site Log Files Directory?',/DIRECTORY)
    PATH='F:\mirror_ftp\garner.ucsd.edu\pub\docs\site_logs
    path='C:\mirror_ftp\igs.bkg.bund.de\EUREF\station'
    path='C:\mirror_ftp\ftp.geonet.org.nz\gps\sitelogs\logs'
    IF PATH EQ '' THEN RETURN
    CD,PATH
    ;OFILE=DP(/WRITE,FILTER=[['*.net'],['QOCA NET A Priori Coordinates File (*.net)']],/AF)
    OFILE='D:\phd\expt\gpsf\external\iGPS\tables\sio.net'
    ofile='C:\mirror_ftp\igs.bkg.bund.de\EUREF\stations.net'
    ofile='C:\mirror_ftp\ftp.geonet.org.nz\gps\sitelogs\ngz.net'
    IF OFILE EQ '' THEN RETURN
    CD,GETPATHNAME(OFILE)
  ENDIF
  
  FILES=FILE_SEARCH(PATH+PATH_SEP()+'*.log', COUNT=NF)
  IF NF LE 0 THEN BEGIN
    PRINT,'[NET_FROM_SITE_LOG]ERROR: no site log files found!'
    RETURN
  ENDIF
  
  SITES=STRARR(NF)
  XYZS=DBLARR(3,NF)
  LLHS=DBLARR(3,NF)
  DATES=DBLARR(NF)
  FOR FI=0, NF-1 DO BEGIN
    FILE=FILES[FI]
    SITE=STRMID(GETFILENAME(FILE),0,4)
    SITES[FI]=SITE
    ;STOP
    READ_SITE_LOG, FILE, SITE=SITE,XYZ=XYZ,LLH=LLH,DATE_INST=DATE_INST,OFFSET=OFFSET
    XYZS[*,FI]=XYZ
    ;FOR EUREF
    LLH[0:1]=LLH[0:1]*1D-4
    LLHS[*,FI]=LLH
    YEAR=FIX(STRMID(DATE_INST[0],0,4))
    MON=FIX(STRMID(DATE_INST[0],5,2))
    DAY=FIX(STRMID(DATE_INST[0],8,2))
    PRINT,YEAR,MON,DAY
    IF YEAR LT 1900 THEN BEGIN
      YEAR=2000
      MON=1
      DAY=1
    ENDIF
    DOY,YEAR,MON,DAY,12,0,DYEAR=DYR
    DATES[FI]=DYR
  ;PRINT,XYZ,LLH
  ;RETURN
  ENDFOR
  
  OPENW,FID,OFILE,/GET_LUN
  WRITE_SYS_INFO,FID,SRC=PATH,$
    USER=USER, $
    PROG='NET_FROM_SITE_LOG'
  PRINTF,FID,'*****************************************************************************************************************************'
  PRINTF,FID,'* Datum:  WGS84'
  PRINTF,FID,'* Format:  V30'
  PRINTF,FID,'* Transfered from:   '+PATH
  PRINTF,FID,'* Time:  '
  PRINTF,FID,'* Operator:  '
  PRINTF,FID,'*'
  PRINTF,FID,'*  site    full-name      latitude        longitude         height       u       v       w    epoch       t1        t2'
  
  
  FOR I=0,NF-1 DO BEGIN
    IF LLHS[0,I] EQ -9999 || (LLHS[0,I] EQ 0 && LLHS[1,I] EQ 0) THEN CONTINUE
    ;IF SITES[I] EQ 'abgs' THEN STOP
    LAT=LLHS[1,I]
    LON=LLHS[0,I]
    HT=LLHS[2,I]
    IF LAT LT 0 THEN BEGIN
      NS='S'
    ENDIF ELSE BEGIN
      NS='N'
    ENDELSE
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
    
    ;STOP
    PRINTF,FID,STRUPCASE(SITES[I]),NS,LAT_DD,LAT_MM,LAT_SS,EW,LON_DD,LON_MM,LON_SS,HT,-0,-0,-0, $
      DATES[I],1900,2500, $
      FORMAT='(1X,A4,"_GPS",2X,"site_log",2X,A1,I2,1X,I2,1X,F9.6,1X,A1,I3,1X,I2,1X,F9.6,1X,F12.5, 3(1X,F7.4),3(1X,F9.4))'
  ENDFOR
  FREE_LUN,FID
  
  PRINT,'[NET_FROM_SITE_LOG]Normal end.'
  
END