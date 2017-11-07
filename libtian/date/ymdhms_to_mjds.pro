PRO YMDHMS_TO_MJDs , dates, seconds, mjds
  IF N_PARAMS() LT 2 THEN BEGIN
    dates=[[2002, 1, 23, 12, 0],[2004, 1, 24, 12, 0],[2002, 1, 25, 12, 0]]
    ;dates=[2002, 1, 23, 12, 0]
    seconds=replicate(0d0,n_elements(dates[0,*]))
    PRINT,dates
    help, seconds
  ENDIF
  
  
  days_to_month = [   0,  31,  59,  90, 120, 151, $
    181, 212, 243, 273, 304, 334 ]
    
  ;***** START, Make sure year is from 1900
    
  years      = reform(dates(0,*))
  months     = reform(dates(1,*))
  days       = reform(dates(2,*))
  ;stop
  
;  ;* Y2K mod to map small number years to resonabale values
;  IF ( year LT 50 ) THEN BEGIN
;    year = year + 2000
;  ENDIF ELSE BEGIN
;    IF ( year LT  200 ) THEN BEGIN
;      year = year + 1900
;    ENDIF
;  ENDELSE
  
  ;***** Compute number of years from 1600
  
  years_from_1600 = years - 1600
  
  ;***** Now compute number of leap days upto the start of the year
  ;*     we are in (i.e., if the year we are in is a leap year do not
  ;*     add the leap day yet)
  
  leap_days =   (years_from_1600 -   1)/  4 $
    - (years_from_1600 +  99)/100  $
    + (years_from_1600 + 399)/400  + 1
    
  ;IF( years_from_1600 EQ 0 ) THEN leap_days = leap_days - 1
  pos=where(years_from_1600 eq 0)
  if pos[0] ne -1 then leap_days[pos]=leap_days[pos]-1
  
  ;***** Now see if we are in leap year
  
  leap_years = replicate(0,n_elements(dates[0,*]))
  pos=where((years_from_1600 MOD   4) EQ 0     AND $
    (years_from_1600 MOD 100) NE 0     OR  $
    (years_from_1600 MOD 400) EQ 0 )
  if pos[0] ne -1 then leap_years[pos]=1
;  IF(   (years_from_1600 MOD   4) EQ 0     AND $
;    (years_from_1600 MOD 100) NE 0     OR  $
;    (years_from_1600 MOD 400) EQ 0       )  THEN leap_year = 1
    
  ;***** Now compute number of days sinec 1600
    
  days_from_1600 = years_from_1600*365d0  + leap_days + $
    days_to_month(months-1) + days
    
  ;***** Add extra day if we are after Februrary and this is a leap year
  pos=where(months GT 2  AND  leap_years EQ 1)
  if pos[0] ne -1 then days_from_1600[pos] = days_from_1600[pos] + 1
;  IF( month GT 2  AND  leap_year EQ 1 ) THEN BEGIN
;    days_from_1600 = days_from_1600 + 1
;  ENDIF
  
  ;***** Compute the mjd and add in the fraction of a day part
  ;*     The MJD of 1600 Jan 0 is -94554
  ;print,'second:',seconds,date(4),date(3)
  fractions  = seconds/86400.d  + dates(4,*)/1440.d + dates(3,*)/24.d
  
  mjds   = -94554.d + days_from_1600 + fractions
  epoch = mjds
  ;print,mjd,days_from_1600,fraction,years_from_1600,leap_days,days_to_month(month),day,fraction
  ;***** THATS ALL
  if n_params() lt 2 then begin
    print,mjds
    help,mjds
  endif
  ;print,epoch
  ;RETURN
  
END