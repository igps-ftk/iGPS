
PRO JD_TO_YMDHMS , epoch1, date, seconds, mjd=mjd
  IF N_PARAMS() LT 1 THEN BEGIN
    epoch1 = 2454243.3395d0
    epoch1 = 2454105.5000d0	;2007/01/05
    ;epoch = 2454136.5000d0	;2007/02/05
    epoch1 = 2454164.5000d0	;2007/03/05
    epoch1=[2454832.0000d0]
  ENDIF
  
  ;Revised by Tian Mar 31 2008
  IF epoch1 LT 2000000D0 THEN BEGIN
    epoch = epoch1 + 2400000.5D0
  ENDIF ELSE BEGIN
    epoch = epoch1
  ENDELSE
  
  date = INTARR(5)
  ;*     ------------------------
  ;*----------------------------------------------------------------------
  ;*     Routine to convert a Julian date (with fractions of a day)
  ;*     or a full Julian date, to a calender data with hours and minutes,
  ;*     and floating-point seconds (Real*8).
  ;*
  ;*     NOTE: If a full Julian date is used the resolution of the seconds
  ;*     will only be about 10 microseconds,  a MJD should yield a resolution
  ;*     of about 0.1 microseconds in the seconds.
  ;*
  ;*     This routine is only valid for dates after 1600 Jan 0 (MJD -94554)
  ;*
  ;*     CALLING SEQUENCE:
  ;*     =================
  ;*      JD_to_YMDHMS , epoch, date, seconds
  ;*
  ;*     WHERE:
  ;*     epoch   is a modified or full julian date with possibly a fractional
  ;*             part of a day.  The two type of input are destinguished by
  ;*             the full Julian date being greater than 2,000,000.
  ;*             (REAL*8 INPUT)
  ;*     date    is an array containing the calender date with (full) year,
  ;*             month of year, day of month, hours, minutes. Note the year
  ;*             will be returned with the centuries added.
  ;*             (I*4 5 element array OUTPUT)
  ;*     seconds is the floating point seconds part of the MJD
  ;*             (REAL*8 OUTPUT)
  ;*
  ;*----------------------------------------------------------------------
  ;*
  ;*$E
  ;
  ;
  ;
  ;*         century     - Number of century from 1600 Jan 0.
  ;*         date(5)     - the calender date corresponding to
  ;*                     - 'epoch' resolution to the minute
  ;*   day         - day of month
  ;*   day_of_year - Number of days from start of year
  ;*   days_to_month(13)   - number of days to start of each month
  ;*               - in a non-leap-year
  ;
  ;*   month       - month of year
  ;
  ;*   year        - years since start of century
  ;*   years_from_1600 - Number of years since 1600 Jan 0.
  ;
  ;*   days_from_1600  - Number of days elapsed since 1600 Jan 0.
  ;*               - (MJD -94554.0 Julian date 2305447.0)
  ;      integer*4 century, date(5), day, day_of_year, days_to_month(13),
  ;     .    month, year, years_from_1600, days_from_1600
  ;
  ;*      epoch    - the julian date or modified julian date
  ;*               - to be converted to calender date
  ;*   fraction    - the fraction of a day part of MJD
  ;
  ;*   mjd         - epoch converted to a MJD
  ;*   mjd_day     - the whole number days in the mjd
  ;
  ;*   seconds     - the seconds part of the MJD (<60)
  ;
  ;      real*8 epoch, fraction, mjd, mjd_day, seconds
  ;
  ;*       leap_year   - Indicates that this days is a leap year
  ;
  ;      logical leap_year
  
  days_to_month =   [0,  31,  59,  90, 120, 151, 181, $
    212, 243, 273, 304, 334, 365 ]
    
    
  ;***** START, convert full jd to mjd if we have too.
  ;*                                      ! epoch must be Full julian date
  IF( epoch GT 2000000.d0 ) THEN BEGIN
    ;*                                      ! convert to MJD.
  
    mjd = epoch - 2400000.5d0
  ENDIF ELSE BEGIN
    mjd = epoch
  ENDELSE
  
  ;***** Remove the fractional part of the mjd
  
  mjd_day  = long ( mjd )
  fraction =   ( mjd MOD 1 )
  fraction = mjd-LONG(mjd)
  
  IF( mjd LT 0  AND  fraction NE 0.d0 ) THEN BEGIN
    mjd_day  = mjd_day - 1
    fraction = fraction + 1.d0
  ENDIF
  
  ;***** Now convert MJD (even day) to date (year, month, day )
  ;*     Get number of days since 1600.
  
  days_from_1600 = LONG(mjd_day - (-94554.0d0))
  years_from_1600 = days_from_1600/365
  
  ;***** Now compute day_of_year and years_from_1600 accounting for
  ;*     leap years
  
  ;*                        ! Just to get us into loop
  day_of_year = 0
  WHILE ( day_of_year LE 0 ) DO BEGIN
  
    century = years_from_1600/100
    
    day_of_year =  days_from_1600 - years_from_1600*365 	$
      - (years_from_1600 -   1)/  4			$
      + (years_from_1600 +  99)/100			$
      - (years_from_1600 + 399)/400 - 1
      
    ;*         If we are 1600 then add one day
    IF( years_from_1600 EQ 0 ) THEN BEGIN
      day_of_year = day_of_year + 1
    ENDIF
    
    ;*         See if the leap days have taken us to a earlier year
    IF( day_of_year LE 0 ) THEN BEGIN
      years_from_1600 = years_from_1600 - 1
    ENDIF
  ENDWHILE
  
  ;***** We now have number of days from start of year and the year
  ;*     Convert years back to start of century
  
  year    = ( years_from_1600 MOD 100)
  
  ;***** See if this is a leap year
  
  leap_year = 0
  ;*                             ! we are at beginning of century
  IF( year EQ 0 ) THEN BEGIN
    IF( (century MOD 4) EQ 0 ) THEN leap_year = 1
  ENDIF ELSE BEGIN
    IF( (year MOD 4)    EQ 0 ) THEN leap_year = 1
  ENDELSE
  
  ;***** If the day of year is less than 60 then the leap years do no not
  ;*     matter,  if we are greater than or equal to 60, need to account
  ;*     for the leap years
  
  ;*                                     ! Dont worry about leap years
  IF( day_of_year LT 60 ) THEN BEGIN
    IF( day_of_year LE 31 ) THEN BEGIN
      month = 0
      day   = day_of_year
    ;*                 ! we are in February
    ENDIF ELSE BEGIN
      month = 1
      day   = day_of_year - 31
    ENDELSE
  ;*                                     ! Need to account for leap years
  ENDIF ELSE BEGIN
    IF( leap_year  AND  day_of_year EQ 60 ) THEN BEGIN
      month  = 1
      day    = 29
    ENDIF ELSE BEGIN
      IF( leap_year ) THEN day_of_year = day_of_year - 1
      
      ;*****         Now find month
      month = 1
      WHILE ( day_of_year GT  days_to_month(month) ) DO BEGIN
        month = month + 1
        ;print,day_of_year,month
      ENDWHILE
      month = month - 1
      day   = day_of_year - days_to_month(month)
    ENDELSE
  ENDELSE
  
  ;***** Now save the date
  
  date(0) = years_from_1600 + 1600
  date(1) = month+1
  date(2) = day
  
  ;***** Now convert the fraction of a day to hours, minutes and seconds
  
  date(3) = fraction*24.d0
  date(4) = fraction*1440.d0 - date(3)*60.d0
  
  seconds = 86400.d0*fraction - date(3)*3600.d0		$
    - date(4)*60.d0
    
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT, date
    PRINT, epoch
    PRINT, seconds
    PRINT, MJD
  ENDIF
  ;***** Thats all
  RETURN
END


