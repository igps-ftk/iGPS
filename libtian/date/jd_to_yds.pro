
PRO JD_TO_YDS , jd, yr, doy, sec


  IF N_PARAMS() LT 4 THEN BEGIN
    jd=53005.5d0
    jd=51180.5
    JD=53450.5D0
  ENDIF
  
  ;*     Routine to compute  year, day-of-year and seconds of day
  ;*     from julian date.
  
  ;* PASSED VARIABLES
  
  ;*   yr  - Year
  ;*   doy - Day-of-year
  ;*   sec - Seconds of day
  
  ;      integer*4 yr, doy, sec
  
  ;*   jd  - Julian date
  
  ;      real*8 jd
  
  ;* LOCAL VARIABLES
  
  ;*   date(5) - Date as an array
  
  ;      integer*4 date(5)
  date=INTARR(5)
  
  ;*   sectag  - Seconds as floating point value
  
  ;      real*8 sectag
  sectag=0d0
  
  ;*     Convert the JD to date
  JD_TO_YMDHMS, jd, date, sectag
  
  ;*     Convert calender date to doy form
  YMD_TO_DOY, date, doy
  
  ;*     Save the seconds part as integer
  sec = FIX(sectag) + date(3)*3600.d0 + date(4)*60.d0
  yr  = date(0) - 1900
  IF( yr GE 100 ) THEN yr = yr - 100
  IF( date(0) LT 1900 ) THEN BEGIN
    yr  = 0
    doy = 0
    sec = 0
  ENDIF
  
  yr=FIX(yr)
  IF (yr LE 50) THEN yr=yr+2000
  IF (yr GT 50 AND yr LT 100) THEN yr=yr+1900
  
  doy=FIX(doy)
  
  
  IF N_PARAMS() LT 4 THEN BEGIN
    PRINT,'jd:',jd
    PRINT,'yr:',yr,'doy:',doy,'sec:',sectag
  ENDIF
  
  ;*     Thats all
  RETURN
END


