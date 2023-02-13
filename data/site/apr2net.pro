;+
; :Description:
;    Create QOCA NET a priori coordiante file (*.net) from ITRF/L-FILE APR files.
;
; :Params:
;    FILE
;    OFILE
;
;
;
; :Author: tianyf
;-
PRO APR2NET, $
    FILE, $
    OFILE
    
  IF N_PARAMS() LT 2 THEN BEGIN
    ;FILE=DIALOG_PICKFILE(TITLE='Input ITRF/L-file *.apr file?', $
    ;  FILTER=[['*.apr','*'],['GAMIT L-file (*.apr)','GAMIT L-file (*)']])
    file='D:\ICD\projects\CEA.2017.Tibet.GNSS_InSAR\2017sep\result\ls31_ls32.txt'
    file='C:\Downloads\izmi\org\izmi.xyz'
    file='D:\gpse\trns\comb\ls22\gsoln\20221225\icd.apr'
    
    
    IF FILE EQ '' THEN RETURN
    CD,GETPATHNAME(FILE)
    ;OFILE=DP(/WRITE,FILTER=[['*.net'],['QOCA Network File (*.net)']],/AF)
    ;OFILE='D:\phd\expt\gpsf\external\iGPS\tables\sio.net'
    OFILE=FILE+'.net'
    IF OFILE EQ '' THEN RETURN
    CD,GETPATHNAME(OFILE)
  ENDIF
  
  READ_APR, FILE, SITES=SITES, VALS=VALS, $
    XYZS=XYZS, $
    VXYZS=VXYZS, $
    EPOCHS=EPOCHS, $
    DATA=DATA
    
  NS=N_ELEMENTS(SITES)
  LLHS=DBLARR(3,NS)
  ;stop
  
  FOR I=0,NS-1 DO BEGIN
    XYZ=XYZS[*,I]
    ;stop
    LLHXYZ,X=XYZ[0],Y=XYZ[1],Z=XYZ[2],ALONG=LON,ALAT=LAT,HGHT=HEIGHT,Iflag=2;, verbose=1
    LLHS[*,I]=[LON,LAT,HEIGHT]
  ENDFOR
  
  WRITE_NET, $
    OFILE, $
    SITES=SITES, $
    LLHS=LLHS, $
    USER=USER, $
    PROG='APR2NET', $
    SRC=FILE, $
    EPOCHS=EPOCHS
    
  PRINT,'[APR2NET]Normal end.'
  
END