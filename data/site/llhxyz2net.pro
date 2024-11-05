;+
; :Description:
;    Create QOCA NET a priori coordiante file (*.net) from ITRF/L-FILE APR files.
;
; :Params:
;    FILE
;    OFILE
;
; :Modifications:
; + On Sat, Nov 28, 2015 10:39:56 PM by tianyf
;   Add the RECT keyword to extract sites for a specific geographical range
;   RECT=[lon_min, lon_max, lat_min, lat_max]
;
;
; :Author: tianyf
;-
PRO LLHXYZ2NET, $
    FILE, $
    OFILE, $
    RECT=RECT
    
  PROG='LLHXYZ2NET'
  
  IF N_PARAMS() LT 2 THEN BEGIN
    ;FILE=DIALOG_PICKFILE(TITLE='Input iGPS LLHXYZ *.llhxyz file?', $
    ;  FILTER=[['*.llhxyz','*'],['iGPS LLHXYZ File (*.llhxyz)','iGPS LLHXYZ File (*)']])
    file='D:\gpse\trns\comb\trns23\gsoln\ninh.llhxyz'
    
    IF FILE EQ '' THEN RETURN
    CD,GETPATHNAME(FILE)
    ;OFILE=DP(/WRITE,FILTER=[['*.net'],['QOCA Network File (*.net)']],/AF)
    OFILE=DESUFFIX(FILE)+'.net'
    IF OFILE EQ '' THEN RETURN
    CD,GETPATHNAME(OFILE)
    
    ;RECT=[100,105,23,28]*1D0
  ENDIF
  
  READ_LLHXYZ, FILE, SITE=SITES,LLH=LLHS
  
  IF N_ELEMENTS(RECT) EQ 4 THEN BEGIN
    POS=WHERE(LLHS[0,*] GE RECT[0] AND LLHS[1] LE RECT[1] AND $
      LLHS[1,*] GE RECT[2] AND LLHS[1,*] LE RECT[3])
    IF POS [0] EQ -1 THEN BEGIN
      PRINT,'['+PROG+']WARNING: no sites in the geographical range!'
      RETURN
    ENDIF
    SITES=STIES[POS]
    LLHS=LLHS[*,POS]
  ENDIF
  
  NS=N_ELEMENTS(SITES)
  IF NS EQ 0 THEN RETURN
  
  EPOCHS=DBLARR(NS)
  EPOCHS[*]=2005
  
  
  WRITE_NET, $
    OFILE, $
    SITES=SITES, $
    LLHS=LLHS, $
    USER=USER, $
    PROG=PROG, $
    SRC=FILE, $
    EPOCHS=EPOCHS
    
  PRINT,'['+PROG+']Normal end.'
  
END