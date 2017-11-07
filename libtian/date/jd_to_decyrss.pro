PRO JD_TO_DECYRSS, JDs, DECYRS

  IF N_PARAMS() LT 1 THEN BEGIN
    JDs=2454243.9226D0
    JDs=2448826.5D0
    JDs=53450.5D0
    JDs= [53045.5D0,53450.5D0,2448826.5D0,2454243.9226D0]
  ENDIF
  
  ndays=N_ELEMENTS(jds)
  ;  IF JDI LT 2000000D0 THEN BEGIN
  ;    JDI = JDI + 2400000.5D0
  ;  ENDIF
  
  pos=WHERE(jds LT 2000000D0)
  IF pos[0] NE -1 THEN jds[pos]=jds[pos] + 2400000.5D0
  
  ;*     ROUTINE TO CONVERT JD TO DECIMAL YEARS (I.E. TO 1986.45)
  
  ;*   DATE(5) - DATE ARRAY NEEDED BY YMDHMS_TO_JD
  
  ;      INTEGER*4 DATE(5)
  DATEs = INTARR(5,ndays)
  
  ;*   DECYRS  - DECIMINAL YEARS VALUES
  ;*   JD      - JULIAN DATA
  ;*   JDI     - INPUT JULIAN DATE
  ;*   SECS    - SECONDS TAG NEEDED FOR YMDHMS_TO_JD
  
  
  ;      REAL*8 DECYRS, JD, JDI, SECS, JDE, NUM_DAYS
  
  ;***** GET YEARS PART OF DECYRS AND JD AT START OF YEAR
  
  JD_TO_YMDHMSS , JDs, DATEs, SECS1
  
  ;*                             ! JAN
  DATEs(1,*) = 1
  ;*                             ! FIRST
  DATEs(2,*) = 1
  ;*                             ! 0 HRS
  DATEs(3,*) = 0
  ;*                             ! 0 MINUTES
  DATEs(4,*) = 0
  
  SECS    = REPLICATE(0d0, ndays)
  ;*                                             ! JD AT START OF YEAR
  YMDHMS_TO_JDS, DATEs, SECS, JDs1
  ;IF N_PARAMS() LT 1 THEN PRINT, DATE, SECS,JD
  
  ;*     GET NUMBER OF DAYS IN YEAR
  DATEs(0,*) = DATEs(0,*) + 1
  YMDHMS_TO_JDS, DATEs, SECS, JDs2
  NUM_DAYS = JDs2 - JDs1
  pos=WHERE(NUM_DAYS NE 365.D0  AND  NUM_DAYS NE 366.D0)
  IF pos[0] NE -1 THEN BEGIN
    NUM_DAYS[pos] = 365.D0
  ENDIF
  ;  IF( NUM_DAYS NE 365.D0  AND  NUM_DAYS NE 366.D0 ) THEN BEGIN
  ;    NUM_DAYS = 365.D0
  ;  ENDIF
  DATEs(0,*) = DATEs(0,*) - 1
  
  DECYRS = reform(DATEs(0,*) + (JDs-JDs1)/NUM_DAYS)
  
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT, DECYRS, FORMAT='(F20.6)'
    HELP, jds
    PRINT,jds
  ENDIF
;***** THATS ALL
;RETURN
END

