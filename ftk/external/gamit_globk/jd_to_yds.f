CTITLE JD_TO_YDS
 
      subroutine jd_to_yds ( jd, yr, doy, sec )

      implicit none
 
*     Routine to compute  year, day-of-year and seconds of day
*     from julian date.
 
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
 
*     Convert the JD to date
      call jd_to_ymdhms( jd, date, sectag)
      
*     Convert calender date to doy form
      call ymd_to_doy( date, doy )

*     Save the seconds part as integer
      sec = nint(sectag) + date(4)*3600.d0 + date(5)*60.d0
      yr  = date(1) - 1900
      if( yr.ge.100 ) yr = yr - 100
      if( date(1).lt.1900 ) then
          yr  = 0
          doy = 0
          sec = 0
      end if
       
*     Thats all
      return
      end
 
