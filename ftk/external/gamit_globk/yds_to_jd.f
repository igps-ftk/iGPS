CTITLE JDS_TO_JD
 
      subroutine yds_to_jd ( yr, doy, sec, jd )

      implicit none
 
*     Routine to compute Julian date from year, day-of-year
*     and seconds of day.
 
* PASSED VARIABLES
 
*   yr  - Year
*   doy - Day-of-year
*   sec - Seconds of day
 
      integer*4 yr, doy, sec
 
*   jd  - Julian date
 
      real*8 jd
 
* LOCAL VARIABLES
 
*   date(5) - Date as an array
 
      integer*4 date(5)
 
*   sectag  - Seconds as floating point value
 
      real*8 sectag
 
*     Set up date array
      date(1) = yr
      date(2) = 1
      date(3) = doy
      date(4) = 0
      date(5) = 0
      sectag  = sec

* MOD TAH 000308: Add check for all zero's.  In this case
*     we return 1900 0 0
      if( yr.eq.0 .and. doy.eq.0 ) then
         date(1) = 1900
      end if

      call ymdhms_to_jd( date, sectag, jd )
 
*     Thats all
      return
      end
 
CTITLE JDS_TO_MJD
 
      subroutine yds_to_mjd ( yr, doy, sec, mjd )
 
*     Routine to compute Modfied Julian date from year, day-of-year
*     and seconds of day.
 
* PASSED VARIABLES
 
*   yr  - Year
*   doy - Day-of-year
*   sec - Seconds of day
 
      integer*4 yr, doy, sec
 
*   mjd  - Modified Julian date
 
      real*8 mjd
 
* LOCAL VARIABLES
 
*   date(5) - Date as an array
 
      integer*4 date(5)
 
*   sectag  - Seconds as floating point value
 
      real*8 sectag
 
*     Set up date array
      date(1) = yr
      date(2) = 1
      date(3) = doy
      date(4) = 0
      date(5) = 0
      sectag  = sec

* MOD TAH 000308: Add check for all zero's.  In this case
*     we return 1900 0 0
      if( yr.eq.0 .and. doy.eq.0 ) then
         date(1) = 1900
      end if

      call ymdhms_to_mjd( date, sectag, mjd )
 
*     Thats all
      return
      end
