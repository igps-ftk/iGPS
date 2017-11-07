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
PRO SITELOG2NET,PATH,OFILE
  IF N_PARAMS() LT 2 THEN BEGIN
    ;PATH=DIALOG_PICKFILE(TITLE='Site Log Files Directory?',/DIRECTORY)
    PATH='F:\mirror_ftp\garner.ucsd.edu\pub\docs\site_logs
    IF PATH EQ '' THEN RETURN
    CD,PATH
    ;OFILE=DP(/WRITE,FILTER=[['*.net'],['QOCA NET A Priori Coordinates File (*.net)']],/AF)
    OFILE='D:\phd\expt\gpsf\external\iGPS\tables\sio.net'
    IF OFILE EQ '' THEN RETURN
    CD,GETPATHNAME(OFILE)
  ENDIF
  
  FILES=FILE_SEARCH(PATH+PATH_SEP()+'*.log', COUNT=NF)
  IF NF LE 0 THEN BEGIN
    PRINT,'[NSITELOG2NET]ERROR: no site log files found!'
    RETURN
  ENDIF
  
  SITES=STRARR(NF)
  XYZS=DBLARR(3,NF)
  LLHS=DBLARR(3,NF)
  EPOCHS=DBLARR(NF)
  FOR FI=0, NF-1 DO BEGIN
    FILE=FILES[FI]
    SITE=STRMID(GETFILENAME(FILE),0,4)
    SITES[FI]=SITE
    ;STOP
    READ_SITE_LOG, FILE, SITE=SITE,XYZ=XYZ,LLH=LLH,DATE_INST=DATE_INST,OFFSET=OFFSET
    XYZS[*,FI]=XYZ
    LLHS[*,FI]=LLH
    YEAR=FIX(STRMID(DATE_INST[0],0,4))
    MON=FIX(STRMID(DATE_INST[0],5,2))
    DAY=FIX(STRMID(DATE_INST[0],8,2))
    ;PRINT,YEAR,MON,DAY
    IF YEAR LT 1900 THEN BEGIN
      YEAR=2000
      MON=1
      DAY=1
    ENDIF
    DOY,YEAR,MON,DAY,12,0,DYEAR=DYR
    EPOCHS[FI]=DYR
  ENDFOR
  
  WRITE_NET, $
    OFILE, $
    SITES=SITES, $
    LLHS=LLHS, $
    USER=USER, $
    PROG='NSITELOG2NET', $
    SRC=PATH+PATH_SEP()+'*.log', $
    EPOCHS=EPOCHS
    
  PRINT,'[NSITELOG2NET]Normal end.'
  
END