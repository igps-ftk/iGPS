PRO READ_USGS_RNEU, FILE, DATA=DATA
  IF N_PARAMS() LT 1 THEN BEGIN
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','jpl', $
      '7param'],'ALGO.lat')
    FILE='E:\data\earthquake.usgs.gov\LongValley\itrf2005\agmt.rneu'
  ENDIF
  
  READ_COLS_ASCII, FILE, DATA=DATA_S
  ;HELP, DATA_S
  ;STOP
  
  DATA=DBLARR(7, N_ELEMENTS(DATA_S[0,*]))
  DATA[0,*]=DOUBLE(DATA_S[1,*])
  DATA[[1,4],*] = DOUBLE(DATA_S[[2,6],*])
  DATA[[2,5],*] = DOUBLE(DATA_S[[3,7],*])
  DATA[[3,6],*] = DOUBLE(DATA_S[[4,8],*])
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,FILE,DATA
  ENDIF
  ;STOP
END