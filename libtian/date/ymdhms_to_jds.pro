PRO  YMDHMS_TO_JDS, dates, sectags, jds

  IF N_PARAMS() LT 2 THEN BEGIN
    dates=[1992,1,1,0,0]
    ;dates=[[dates],[2007,5,23,0,0]]
    sectags=REPLICATE(0d0,N_ELEMENTS(dates[0,*]))
  ENDIF
  
  dates=FIX(dates)
  YMDHMS_TO_MJDS,dates, sectags,mjds
  jds=mjds+2400000.5d0
  IF N_PARAMS() LT 2 THEN BEGIN
    PRINT,jds, format='(f20.4)'
    HELP, dates, sectags, jds, mjds
  ENDIF
END
