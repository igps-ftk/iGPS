
;Time Series and Residual Format
;Column 1: Decimal_YR 
;Columns 2-4: East(m) North(m) Vert(m) 
;Columns 5-7: E_sig(m) N_sig(m) V_sig(m) 
;Columns 8-10: E_N_cor E_V_cor N_V_cor
;Column 11: Time in Seconds past J2000
;Columns 12-17: Time in YEAR MM DD HR MN SS

PRO READ_Liu_zeng_2020, FILE, DATA=DATA
  IF N_PARAMS() LT 1 THEN BEGIN
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','formats', $
      'Liu-Zeng.2020'],'XZZF')
  ENDIF
  
  READ_COLS_ASCII, FILE, DATA=DATA1
  
  ;HELP, DATA1
  ;STOP
  DATA=DOUBLE(DATA1)
  
;  DATA=DATA1[[0,11,11,2,1,3,5,4,6],*]
;  YMDS=DATA1[[11,12,13],*]
;  
;  YMD_TO_DOYS, ymds, doys
;  DATA[2,*]=DOYS
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,FILE,DATA
  ENDIF
  ;STOP
END