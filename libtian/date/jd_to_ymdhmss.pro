
PRO JD_TO_YMDHMSS , epochs1, dates, seconds, mjd=mjds
  IF N_PARAMS() LT 1 THEN BEGIN
    epochs1 = 2454243.3395d0
    epochs1 = 2454105.5000d0	;2007/01/05
    ;epochs = 2454136.5000d0	;2007/02/05
    epochs1 = 2454164.5000d0	;2007/03/05
    epochs1=[2454164.5000d0,2454136.5000d0,2454105.5000d0]
    epochs1=[2454832.0000d0]
    epochs1=[2454164.5000d0,replicate(2451910.0000d0,10000),2454105.5000d0]
    epochs1=[51238.500d0]
  ENDIF
  
  ;Revised by Tian Mar 31 2008
  ;  IF epoch1 LT 2000000D0 THEN BEGIN
  ;    epoch = epoch1 + 2400000.5D0
  ;  ENDIF ELSE BEGIN
  ;    epoch = epoch1
  ;  ENDELSE
  epochs=epochs1
  pos=WHERE(epochs1 LT 2000000D0)
  IF pos[0] NE -1 THEN epochs[pos]=epochs1[pos]+2400000.5D0
  
  dates = INTARR(5,N_ELEMENTS(epochs))
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
  ;  IF( epoch GT 2000000.d0 ) THEN BEGIN
  ;    ;*                                      ! convert to MJD.
  ;
  ;    mjd = epoch - 2400000.5d0
  ;  ENDIF ELSE BEGIN
  ;    mjd = epoch
  ;  ENDELSE
  ;
  mjds=epochs
  pos=WHERE(epochs GT 2000000.d0 )
  IF pos[0] NE -1 THEN mjds[pos]=mjds[pos] - 2400000.5d0
  
  
  ;***** Remove the fractional part of the mjd
  
  mjd_days  = long ( mjds )
  ;fractions =   ( mjds MOD 1 )
  fractions = mjds-LONG(mjds)
  
  ;  IF( mjd LT 0  AND  fraction NE 0.d0 ) THEN BEGIN
  ;    mjd_day  = mjd_day - 1
  ;    fraction = fraction + 1.d0
  ;  ENDIF
  pos=WHERE( mjds LT 0  AND  fractions NE 0.d0 )
  IF pos[0] NE -1 THEN BEGIN
    mjd_days[pos]  = mjd_days[pos] - 1
    fractions[pos] = fractions[pos] + 1.d0
  ENDIF
  
  ;***** Now convert MJD (even day) to date (year, month, day )
  ;*     Get number of days since 1600.
  
  days_from_1600s = LONG(mjd_days - (-94554.0d0))
  years_from_1600s = days_from_1600s/365
  
  ;***** Now compute day_of_year and years_from_1600 accounting for
  ;*     leap years
  
  ;*                        ! Just to get us into loop
  ;  day_of_year = 0
  ;  WHILE ( day_of_year LE 0 ) DO BEGIN
  ;
  ;    century = years_from_1600/100
  ;
  ;    day_of_year =  days_from_1600 - years_from_1600*365 	$
  ;      - (years_from_1600 -   1)/  4			$
  ;      + (years_from_1600 +  99)/100			$
  ;      - (years_from_1600 + 399)/400 - 1
  ;
  ;    ;*         If we are 1600 then add one day
  ;    IF( years_from_1600 EQ 0 ) THEN BEGIN
  ;      day_of_year = day_of_year + 1
  ;    ENDIF
  ;
  ;    ;*         See if the leap days have taken us to a earlier year
  ;    IF( day_of_year LE 0 ) THEN BEGIN
  ;      years_from_1600 = years_from_1600 - 1
  ;    ENDIF
  ;  ENDWHILE
  day_of_years=INTARR(N_ELEMENTS(years_from_1600s))
  centurys=INTARR(N_ELEMENTS(years_from_1600s))
  FOR di=0ull, N_ELEMENTS(years_from_1600s)-1 DO BEGIN
    day_of_year = 0
    WHILE ( day_of_year LE 0 ) DO BEGIN
    
      centurys[di] = years_from_1600s[di]/100
      
      day_of_year =  days_from_1600s[di] - years_from_1600s[di]*365   $
        - (years_from_1600s[di] -   1)/  4     $
        + (years_from_1600s[di] +  99)/100     $
        - (years_from_1600s[di] + 399)/400 - 1
        
      ;*         If we are 1600 then add one day
      IF( years_from_1600s[di] EQ 0 ) THEN BEGIN
        day_of_year = day_of_year + 1
      ENDIF
      
      ;*         See if the leap days have taken us to a earlier year
      IF( day_of_year LE 0 ) THEN BEGIN
        years_from_1600s[di] = years_from_1600s[di] - 1
      ENDIF
    ENDWHILE
    day_of_years[di]=day_of_year
  ENDFOR
  
  ;***** We now have number of days from start of year and the year
  ;*     Convert years back to start of century
  
  years    = ( years_from_1600s MOD 100)
  
  ;***** See if this is a leap year
  
  leap_years = REPLICATE(0,N_ELEMENTS(epochs))
  ;*                             ! we are at beginning of century
  ;  IF( year EQ 0 ) THEN BEGIN
  ;    IF( (century MOD 4) EQ 0 ) THEN leap_year = 1
  ;  ENDIF ELSE BEGIN
  ;    IF( (year MOD 4)    EQ 0 ) THEN leap_year = 1
  ;  ENDELSE
  pos=WHERE(years EQ 0 AND (centurys mod 4) eq 0)
  IF pos[0] NE -1 THEN leap_years[pos]=1
  pos=WHERE(years ne 0 AND  (years MOD 4) EQ 0 )
  IF pos[0] NE -1 THEN leap_years[pos]=1
  
  ;***** If the day of year is less than 60 then the leap years do no not
  ;*     matter,  if we are greater than or equal to 60, need to account
  ;*     for the leap years
  months=INTARR(N_ELEMENTS(day_of_years))
  days=INTARR(N_ELEMENTS(day_of_years))
  ;
  pos=WHERE(day_of_years LE 31 )
  IF pos[0] NE -1 THEN BEGIN
    months[pos] = 0
    days[pos]   = day_of_years[pos]
  ENDIF
  
  pos=WHERE(day_of_years GT 31 AND day_of_years LT 60  )
  IF pos[0] NE -1 THEN BEGIN
    months[pos] = 1
    days[pos]   = day_of_years[pos] - 31
  ENDIF
  
  pos=WHERE(leap_years EQ 1  AND  day_of_years EQ 60)
  IF pos[0] NE -1 THEN BEGIN
    months[pos] = 1
    days[pos]   = 29
  ENDIF
  
  pos=WHERE(day_of_years GE 60 AND (leap_years EQ 1  AND day_of_years EQ 60) eq 0)
  IF pos[0] NE -1 THEN BEGIN
    pos2=WHERE( leap_years[pos] EQ 1 )
    IF pos2[0] NE -1 THEN day_of_years[pos[pos2]] = day_of_years[pos[pos2]] - 1    
    ;*****         Now find month
    months[pos] = 1
    FOR di=0ull,N_ELEMENTS(pos)-1 DO BEGIN
    ;FOR di=1525ull,N_ELEMENTS(pos)-1 DO BEGIN
      WHILE ( day_of_years[pos[di]] GT  days_to_month(months[pos[di]]) ) DO BEGIN
        months[pos[di]] = months[pos[di]] + 1
      ENDWHILE
    ENDFOR
    months[pos] = months[pos] - 1
    days[pos]   = day_of_years[pos] - days_to_month(months[pos])
  ENDIF
  
;  ;*                                     ! Dont worry about leap years
;  IF( day_of_year LT 60 ) THEN BEGIN
;    IF( day_of_year LE 31 ) THEN BEGIN
;      month = 0
;      day   = day_of_year
;    ;*                 ! we are in February
;    ENDIF ELSE BEGIN
;      month = 1
;      day   = day_of_year - 31
;    ENDELSE
;  ;*                                     ! Need to account for leap years
;  ENDIF ELSE BEGIN
;    IF( leap_year  AND  day_of_year EQ 60 ) THEN BEGIN
;      month  = 1
;      day    = 29
;    ENDIF ELSE BEGIN
;      IF( leap_year ) THEN day_of_year = day_of_year - 1
;      
;      ;*****         Now find month
;      month = 1
;      WHILE ( day_of_year GT  days_to_month(month) ) DO BEGIN
;        month = month + 1
;      ENDWHILE
;      month = month - 1
;      day   = day_of_year - days_to_month(month)
;    ENDELSE
;  ENDELSE
  
  ;***** Now save the date
  
  dates(0,*) = years_from_1600s + 1600
  dates(1,*) = months+1
  dates(2,*) = days
  
  ;***** Now convert the fraction of a day to hours, minutes and seconds
  
  dates(3,*) = fractions*24.d0
  dates(4,*) = fractions*1440.d0 - dates(3,*)*60.d0
  
  seconds = 86400.d0*fractions - dates(3,*)*3600.d0		$
    - dates(4,*)*60.d0
    
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT, dates
    PRINT, epochs
    PRINT, seconds
    PRINT, MJDs
  help, dates
  ENDIF
  ;***** Thats all
  RETURN
END


