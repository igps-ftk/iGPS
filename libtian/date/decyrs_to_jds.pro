PRO DECYRS_TO_JDS, DECYRS, JDS

  IF N_PARAMS() LT 1  THEN BEGIN
    DECYRS = 2007.172603D0
    DECYRS = 2005.172603D0
    decyrs = indgen(10)/365.25d0+2005.172603D0
    decyrs = 1999.2562d0
  ENDIF
  NDAYS=N_ELEMENTS(DECYRS)
  DATES = INTARR(5,NDAYS)
  
  DATES(0,*) = FIX(DECYRS)
  DATES(1,*) = 1
  DATES(2,*) = 1
  DATES(3,*) = 0
  DATES(4,*) = 0
  
  SECONDS    = DBLARR(NDAYS)
  
  YMDHMS_TO_JDS, DATES+0d0, SECONDS+0d0, JDSS
  ;IF N_PARAMS() LT 2 THEN PRINT, DATE,SECS, JDS, FORMAT='(7F20.4)'
  DATES(0,*) = DATES(0,*) + 1
  YMDHMS_TO_JDs, DATES+0d0, SECONDS+0d0, JDES
  ;IF N_PARAMS() LT 2 THEN PRINT, DATE,SECS, JDE, FORMAT='(7F20.4)'
  
  JDS = JDSS + (DECYRS-FIX(DECYRS))*(JDES-JDSS)
  
  IF N_PARAMS() LT 1 THEN BEGIN
    ;PRINT,JD,FORMAT='(F20.4)'
    HELP, DECYRS, JDS
    print,decyrs
    PRINT,JDS
  ENDIF
  ;STOP
END
