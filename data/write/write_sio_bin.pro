PRO WRITE_SIO_BIN, FILE, DATA=DATA, HEADERs=HEADERs
  ;HEADER FILE
  ;help,headers
  HFILE=DESUFFIX(FILE)+'.hdr'
  OPENW, FIDO, HFILE, /GET_LUN
  IF N_ELEMENTS(HEADERS) NE 0 THEN BEGIN
    FOR HI=0, N_ELEMENTS(HEADERS)-1 DO BEGIN
      POS = STRPOS(HEADERS[HI],'COL')
      IF POS[0] NE -1 THEN CONTINUE
      POS = STRPOS(HEADERS[HI],'ROW')
      IF POS[0] NE -1 THEN CONTINUE
      PRINTF,FIDO,HEADERS[HI], FORMAT='(A)'
    ENDFOR
  ENDIF
  SZ = SIZE(DATA, /DIMENSION)
  ;HELP,SZ
  PRINTF, FIDO, 'COL', SZ[0], FORMAT='("#",A3,":",I20)'
  PRINTF, FIDO, 'ROW', SZ[1], FORMAT='("#",A3,":",I20)'
  FREE_LUN, FIDO
  
  ;DATA FILE
  OPENW, FIDO, FILE, /GET_LUN
  WRITEU, FIDO, DATA
  FREE_LUN, FIDO  
END