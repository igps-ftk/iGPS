PRO DOY,A,B,C,D,E, $
    DATE=DATE, DAY_OF_YEAR=DAY_OF_YEAR, $
    JD=JD, MJD=MJD, $
    GPS_WEEK=GPS_WEEK, $
    GPS_DOW=GPS_DOW, GPS_SOW=GPS_SOW, $
    DYEAR=DYEAR, $
    sectag=sectag
    
  IF N_PARAMS() LT 1 THEN BEGIN
    A=2007
    B=3
    C=5
    D=12
    E=0
    
    A=1800
    B=1
    C=1
  ENDIF
  ;STOP
  
  RUNSTRING = STRARR(5)
  
  IF N_ELEMENTS(A) NE 0 THEN RUNSTRING[0]=A
  IF N_ELEMENTS(B) NE 0 THEN RUNSTRING[1]=B
  IF N_ELEMENTS(C) NE 0 THEN RUNSTRING[2]=C
  IF N_ELEMENTS(D) NE 0 THEN RUNSTRING[3]=D
  IF N_ELEMENTS(E) NE 0 THEN RUNSTRING[4]=E
  

  DATE = INTARR(5)
  
  GPS_START_MJD = 44243
  DAY_OF_WEEK= ['Sun ', 'Mon ', 'Tue ', 'Wed ',  $
    'Thu ', 'Fri ', 'Sat '  ]
  NUM_RUN = N_PARAMS()
  
  IF( NUM_RUN EQ 0 ) THEN BEGIN
    PRINT, 'For syntax information, please refer to doy document in gamit.'
    INDX = 0
  ENDIF
  DATE(3) = 0
  DATE(4) = 0
  
  if n_elements(sectag) eq 0 then begin
    SECTAG  = 0.01d0
  endif   
  
  RUNSTRING(0) = STRUPCASE( RUNSTRING(0) )
  INDX = STRPOS(RUNSTRING(0),'W')
  IF( INDX[0] GE 0 ) THEN BEGIN
    RUNSTRING(0)=RSUB_CHAR(RUNSTRING(0),'W',' ')
    GPS_WEEK = DOUBLE(RUNSTRING(0))
    IF( NUM_RUN EQ 2 ) THEN BEGIN
      GPS_DOW = ulong(RUNSTRING(1))
      IF( GPS_DOW GT 6  OR  GPS_DOW EQ 0 ) THEN BEGIN
        GPS_SOW = GPS_DOW
        GPS_DOW = GPS_SOW/86400
      ENDIF ELSE BEGIN
        GPS_SOW = (GPS_DOW)*86400
      ENDELSE
    ENDIF ELSE BEGIN
      GPS_DOW = 0
      GPS_SOW = 0
    ENDELSE
    MJD = DOUBLE(GPS_START_MJD + GPS_WEEK*7d0 + GPS_SOW/86400d0 + 1d0)
    JD  = MJD + 2400000.5D0
    JD_TO_YMDHMS,JD, DATE, SECTAG
    YMD_TO_DOY,DATE, DAY_OF_YEAR
    
  ENDIF ELSE BEGIN
    INDX = STRPOS(RUNSTRING(0),'Y')
    IF( INDX[0] NE -1 ) THEN  BEGIN ;  
      RUNSTRING(0)=RSUB_CHAR(RUNSTRING(0),'Y',' ')
      DYEAR=DOUBLE(RUNSTRING(0))
      DECYRS_TO_JD, DYEAR, JD
      JD_TO_YMDHMS,JD, DATE, SECTAG
      YMD_TO_DOY,DATE, DAY_OF_YEAR
    ENDIF ELSE BEGIN
      IF( NUM_RUN EQ 1 ) THEN  BEGIN
        JD = DOUBLE(RUNSTRING(0))
        IF( JD LT 100000 ) THEN JD = JD + 2400000.5D0
        JD_TO_YMDHMS,JD, DATE, SECTAG
        YMD_TO_DOY,DATE, DAY_OF_YEAR
        
      ENDIF ELSE BEGIN
        IF( NUM_RUN EQ 2 ) THEN BEGIN
          DATE(0) = DOUBLE(RUNSTRING(0))
          DATE(1) = 1
          DATE(2) = DOUBLE(RUNSTRING(1))
          YMDHMS_TO_JD,DATE, SECTAG, JD
          DAY_OF_YEAR = DATE(2)
          JD_TO_YMDHMS,JD, DATE, SECTAG
        ENDIF ELSE BEGIN
          IF( NUM_RUN GE 3 ) THEN BEGIN
            DATE(0) = DOUBLE(RUNSTRING(0))
            DATE(1) = DOUBLE(RUNSTRING(1))
            DATE(2) = DOUBLE(RUNSTRING(2))
            IF( NUM_RUN GE 4 ) THEN DATE(3) = DOUBLE(RUNSTRING(3))
            IF( NUM_RUN GE 5 )THEN DATE(4) = DOUBLE(RUNSTRING(4))
            
            YMDHMS_TO_JD,DATE, SECTAG, JD
            YMD_TO_DOY,DATE, DAY_OF_YEAR
          ENDIF ELSE BEGIN
            PRINT, 'Today is:'
            ;STOP
            TIMESTR=SYSTIME()
            TMP=STRSPLIT(TIMESTR,/EXTRACT)
            DATE(0)=FIX(TMP[4])
            MON_OF_YEAR = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
            POS=WHERE(TMP[1] EQ MON_OF_YEAR)
            DATE(1)=POS+1
            DATE(2)=FIX(TMP[2])
            TMP=STRSPLIT(TMP[3],':',/EXTRACT)
            DATE(3)=FIX(TMP[0])
            DATE(4)=FIX(TMP[1])
            SECTAG=FIX(TMP[2])
            YMDHMS_TO_JD,DATE, SECTAG, JD
            ;PRINT, DATE,JD,SECTAG,JD,FORMAT='(7F20.5)'
            YMD_TO_DOY,DATE, DAY_OF_YEAR
          ENDELSE
          
        ENDELSE
      ENDELSE
    ENDELSE
    
    MJD = JD - 2400000.5
    GPS_WEEK = (FLOOR(MJD) - GPS_START_MJD - 1)/7
    IF( MJD-GPS_START_MJD-1 LT 0 ) THEN GPS_WEEK = GPS_WEEK - 1
    GPS_SOW  = (MJD - (GPS_START_MJD+GPS_WEEK*7+1))*86400
    IF( MJD-GPS_START_MJD-1  LT 0  AND   $
      GPS_SOW GE  604800 ) THEN GPS_SOW = GPS_SOW - 604800
    GPS_DOW = GPS_SOW/86400
  ENDELSE
  
  JD_TO_YMDHMS,JD,DATE,SECTAG
  JD_TO_DECYRS, JD, DYEAR
  
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,DATE, DAY_OF_YEAR, JD, MJD, $
      FORMAT='("Date ",i4,"/",i2.2,"/",i2.2,1x,i2,":",i2.2,'+ $
      ' " hrs, DOY ",i3," JD ",F13.4," MJD ",F11.4)'
    PRINT,gps_week, $
      gps_dow, gps_sow, $
      day_of_week(gps_dow), $
      format='("GPS Week ",i5," Day of week ",i2,", GPS Seconds ",i6," Day of Week ",a4)'
    PRINT,$
      dyear, $
      format='("Decimal Year ",f12.6)'
  ENDIF
  
END