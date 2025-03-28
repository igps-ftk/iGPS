;EXTRACT ANNUAL PHASE AND AMPLITUDE FROM MODEL TERMS
PRO STAT_MODEL_FOR_PLOT, $
    FILE, $ ; INPUT FILE, CREATED BY MODEL (WITH "Output Statistics" CHECKED).
    OFILE, $  ;OUTPUT FILE, INPUT OF PSXY (GMT): LONGITUDE, LATITUDE, PHASE, AMPLITUDE
    CFILE=CFILE,  $
    scale_factor=sf,  $
    neu=neustr
    
  IF N_ELEMENTS(FILE) EQ 0 THEN BEGIN
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','sio', $
      'cleanedNeuUnf.resid'],'STAT.MODEL')
   ; FILE=DIALOG_PICKFILE(TITLE='Where is the STAT.MODEL file?',FILTER='STAT.MODEL')

;    file='D:\gsar\des\buruo2\des_F2.c\SBAS5\x15\raw.flt.resid\STAT.MODEL'
;    cfile='D:\gsar\des\buruo2\des_F2.c\SBAS5\x15\raw\sites.net'
;    
;    file='D:\gsar\des\dangxiong2\des_F3\SBAS2\x3\raw.resid\STAT.MODEL'
;    cfile='D:\gsar\des\dangxiong2\des_F3\SBAS2\x3\raw\sites.net'
;    
;    file='D:\gsar\des\dangxiong2\des_F3\SBAS3\x1\raw\STAT.MODEL'
;    cfile='D:\gsar\des\dangxiong2\des_F3\SBAS3\x1\raw\sites.net'
;    
    file='\\10.4.134.30\root\g6b\proc_gmtsar\interseismic\143-a-m3-0089_0095_0100-jiali_nujiang_yzs4\f123\sbas.4.0.0367.9999.20170108.20200808.107.1800.01.___\r4\x30\raw\STAT.MODEL'
    cfile='\\10.4.134.30\root\g6b\proc_gmtsar\interseismic\143-a-m3-0089_0095_0100-jiali_nujiang_yzs4\f123\sbas.4.0.0367.9999.20170108.20200808.107.1800.01.___\r4\x30\raw\sites.net'
    ;
    ;
    neustr='N'
    
    IF FILE EQ '' THEN RETURN
    CD, GETPATHNAME(FILE)
    OFILE=FILE+'_GMT'
  ;CFILE=GET_CFILE()
  ENDIF
  
  IF N_ELEMENTS(OFILE) EQ 0 THEN BEGIN
    OFILE=DIALOG_PICKFILE(TITLE='Where do you want to save it (input of psxy [GMT])?')
    IF OFILE EQ '' THEN RETURN
    CD, GETPATHNAME(OFILE)
  ENDIF
  
  IF N_ELEMENTS(CFILE) EQ 0 || FILE_TEST(CFILE,/REGULAR) NE 1 THEN BEGIN
    CFILE=DIALOG_PICKFILE(TITLE='Where is the priori coordinates file? Cancel to use the default file:'+GET_CFILE(),$
      FILTER=[['*.net'],['QOCA Network File (*.net)']])
    IF CFILE EQ '' THEN BEGIN
      CFILE=GET_CFILE()
      PRINT,'[STAT_MODEL_FOR_PLOT]WARNING: no input a prioir coordiante file. Using the default '+CFILE+'!',$
        FORMAT='(A)'
    ENDIF ELSE BEGIN
      CD, GETPATHNAME(CFILE)
    ENDELSE
  ENDIF
  
  if n_elements(sf) eq 0 then sf=1d0
  
  
  LINES=READ_TXT(FILE)
  NL=N_ELEMENTS(LINES)
  if n_elements(neustr) eq 0 then NEUSTR=['N','E','U']  ;OUTPUT ALL THREE COMPONENTS; APPENEDED TO OUTPUT FILES
 
  OFILES=STRARR(3)
  
  ;GET SITE NAMES
  SITES=STRARR(N_ELEMENTS(LINES))
  FOR I=0,N_ELEMENTS(LINES)-1 DO BEGIN
    LINE=LINES[I]
    ;print,i
    IF STRMID(LINE,0,1) NE ' ' THEN CONTINUE
    LINE_S=STRSPLIT(LINE,/EXTRACT)
    SITES[I]=LINE_S[0]
  ENDFOR
  SITES=SITES[WHERE(SITES NE '')]
  ;stop
  READ_NET,CFILE,SITE=SITES,LLH=LLHS
  
  
  FOR NEUI=0,N_ELEMENTS(NEUSTR)-1 DO BEGIN
    OPENW,FID,OFILE+'.'+NEUSTR[NEUI],/GET_LUN
    WRITE_SYS_INFO, FID, PROG='STAT_MODEL_FOR_PLOT', SRC=[FILE,CFILE],USER=USER
    PRINTF,FID,'longitude','latitude','phase(deg)','amp(mm)','','amp_sigma','site',$
      FORMAT='("*",2(1x,A20),1X,2(1x,A12),1X,A6,1X,A12,1X,A4)'
    OFILES[NEUI]=OFILE+'.'+NEUSTR[NEUI]
    FOR I=0,N_ELEMENTS(LINES)-1 DO BEGIN
      LINE=LINES[I]
      ;print,i
      IF STRMID(LINE,0,1) NE ' ' THEN CONTINUE
      LINE_S=STRSPLIT(LINE,/EXTRACT)
      IF LINE_S[1] NE NEUSTR[NEUI] THEN CONTINUE
      SITE=LINE_S[0]
      ANN=DOUBLE(LINE_S[4])*sf
      EANN=DOUBLE(LINE_S[9])*sf
      PHA=DOUBLE(LINE_S[5])
      IF PHA LT 0 THEN BEGIN
        PHA=PHA+2*!DPI
      ENDIF
      IF PHA GT 2*!DPI THEN BEGIN
        PHA=PHA-2*!DPI
      ENDIF
      POS=WHERE(SITES EQ SITE)
      LLH=LLHS[*,POS]
      ;PRINTF,FID,LLH[0:1],PHA*180/!DPI,ANN,SITE,EANN, $
      ;  FORMAT='(1x,2F20.8,1X,2F12.7,1X,A4,1X,F12.7)'
      
      PRINTF,FID,LLH[0],LLH[1],PHA*180/!DPI,ANN*1D3,' +/- ',EANN*1D3,SITE, $
        FORMAT='(1x,2(1X,F20.8),1X,2(1X,F12.5),A6,F12.5,1X,A4)'
        
    ;SAMPLE OUTPUT:
    ;        115.892         39.608       277.6        5.257 bjfs
    ;        116.223         40.250       266.9        4.350 bjsh
    ENDFOR
    FREE_LUN,FID
  ENDFOR
  
  ;RETURN
  
  ;STOP
  pos=where(ofiles ne '')
  if pos[0] ne -1 then begin
  ;CONVERT TO PSVELO INPUT FORMAT
  ;  WILL APPEND .PSVELO SUFFIX TO OUTPUT FILES.
  print,OFILES[pos]
  ANNSEMI2PSVELO, FILES=OFILES[pos]
  ;STAT_MODEL_VEL_2_PSVELO, FILE, OFILE+'.VEL', CFILE=CFILE
  
  endif
  PRINT,'[STAT_MODEL_FOR_PLOT]Normal end.',$
    FORMAT='(A)'
    
END