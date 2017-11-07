CTITLE 'YMDHMS_TO_JD'
 
      SUBROUTINE YMDHMS_to_JD ( date, seconds, epoch )
*     -------------------------
*    .,      26JUL85                <871210.1317>
 
*     Author: T.Herring            3:53 PM  THU., 10  JULY, 1986
*
*$S "YMDHMS_to_JD"
*-----------------------------------------------------------------------
*     Routine to convert a calender date with hours, minutes and
*     seconds to a Modified Julian date. The calender date is
*     ordered as year, month, day of month, hours, and
*     minutes.  These values are stored in a single I*2 array.
*
*     If the year is greater than 1000 then the it is assumed to
*     contain the centuries.
*
*     This routine is only valid for dates after 1600 Jan 0.
*
*     CALLING SEQUENCE:
*     =================
*     CALL ymdhms_to_JD ( date, seconds, epoch)
*
*     WHERE:
*     date    is a 5 element array containing year, month, day of month,
*             hour, and minutes.
*             (I*4 5 element array INPUT)
*     seconds is the seconds part of the epoch
*             (REAL*8 INPUT)
*     epoch   is the JD with fractional days corresponding to date and
*             seconds.
*             (REAL*8 OUTPUT)
*
*----------------------------------------------------------------------
*$E
 
 
*         day           - day of month
*         date(5)     - 5 element date array with year, month, day,
*               - hours and minutes.
 
*   days_to_month(12)   - number of days from start of year to
*               - each month
 
*   leap_days   - the number of leap days we need to include
 
*   month       - month of year
*   year        - the years since 1900.0
 
*   years_from_1600 - number of years since 1600 Jan 0.
 
*   days_from_1600  - number of days since 1600 Jan 0.
 
      integer*4 day, date(5), days_to_month(12), leap_days, month,
     .    year, years_from_1600, days_from_1600
 
*       epoch   - The JD corresponding to date and seconds
*   fraction    - Fraction of a day.  We compute this separately
*               - to avoiod some rounding error when added to MJD
*   mjd         - the computes MJD with fractional days
*   seconds     - the seconds part of the epoch.
 
      real*8 epoch, fraction, mjd, seconds
 
*       leap_year   - Indicates that this is a leap year
 
      logical leap_year
 
      data  days_to_month /   0,  31,  59,  90, 120, 151,
     .                      181, 212, 243, 273, 304, 334 /
 
***** START, Make sure year is from 1900
 
      year      = date(1)
      month     = date(2)
      day       = date(3)

* Y2K mod to map small number years to resonabale values 
      if( year.lt.50 ) then
          year = year + 2000
      else if ( year.lt. 200 ) then
          year = year + 1900
      end if
 
***** Compute number of years from 1600
 
      years_from_1600 = year - 1600
 
***** Now compute number of leap days upto the start of the year
*     we are in (i.e., if the year we are in is a leap year do not
*     add the leap day yet)
 
      leap_days =   (years_from_1600 -   1)/  4
     .            - (years_from_1600 +  99)/100
     .            + (years_from_1600 + 399)/400  + 1
 
      if( years_from_1600.eq.0 ) leap_days = leap_days - 1
 
***** Now see if we are in leap year
 
      leap_year = .false.
      if(   mod(years_from_1600,  4).eq.0     .and.
     .     (mod(years_from_1600,100).ne.0.or.
     .      mod(years_from_1600,400).eq.0)       ) leap_year = .true.
 
***** Now compute number of days sinec 1600
 
      days_from_1600 = years_from_1600*365  + leap_days +
     .                 days_to_month(month) + day
 
***** Add extra day if we are after Februrary and this is a leap year
      if( month.gt.2 .and. leap_year ) then
          days_from_1600 = days_from_1600 + 1
      end if
 
***** Compute the mjd and add in the fraction of a day part
*     The MJD of 1600 Jan 0 is -94554
 
      fraction  = seconds/86400.d0  + date(5)/1440.d0 + date(4)/24.d0
 
      mjd   = -94554.d0 + days_from_1600 + fraction
      epoch = mjd + 2 400 000.5d0
 
***** THATS ALL
      RETURN
      end
 
