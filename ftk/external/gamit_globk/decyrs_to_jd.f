CTITLE DECYRS_TO_JD
 
      subroutine decyrs_to_jd( decyrs, jd )
 
 
      implicit none

*     Routine to convert decimal years (i.e. 1986.45) to a JD.
* MOD TAH 041221: Changed to routine to use leap-years for number of daya
*     Nakes this routine consistent with jd_to_decyrs
 
*   date(5) - date array needed by YMDHMS_to_JD
 
      integer*4 date(5)
 
*   decyrs  - Deciminal years values
*   jd      - Julian data
*   jds, jde - Julian date are start and end of year 
*   secs    - Seconds tag needed for YMDHMS_to_JD
 
 
      real*8 decyrs, jd, secs, jds, jde
 
***** Get years part of decyrs and JD at start of year
 
      date(1) = int(decyrs)
*                             ! Jan
      date(2) = 1
*                             ! first
      date(3) = 1
*                             ! 0 hrs
      date(4) = 0
*                             ! 0 minutes
      date(5) = 0
 
      secs    = 0
*                                             ! JD at start of year
      call ymdhms_to_jd( date, secs, jds )
*     Now get jd at start of next year 
      date(1) = date(1) + 1
      call ymdhms_to_jd( date, secs, jde )

 
      jd = jds + (decyrs-int(decyrs))*(jde-jds)
 
***** Thats all
      return
      end
 
CTITLE DECYRS_TO_MJD
 
      subroutine decyrs_to_mjd( decyrs, mjd )
 
 
*     Routine to convert decimal years (i.e. 1986.45) to a MJD.
* MOD TAH 041221: Changed to routine to use leap-years for number of daya
*     Nakes this routine consistent with mjd_to_decyrs
 
*   date(5) - date array needed by YMDHMS_to_MJD
 
      integer*4 date(5)
 
*   decyrs  - Deciminal years values
*   mjd      - Julian data
*   mjds, mjde - Julian date are start and end of year 
*   secs    - Seconds tag needed for YMDHMS_to_MJD
 
 
      real*8 decyrs, mjd, secs, mjds, mjde
 
***** Get years part of decyrs and JD at start of year
 
      date(1) = int(decyrs)
*                             ! Jan
      date(2) = 1
*                             ! first
      date(3) = 1
*                             ! 0 hrs
      date(4) = 0
*                             ! 0 minutes
      date(5) = 0
 
      secs    = 0
*                                             ! JD at start of year
      call ymdhms_to_mjd( date, secs, mjds )
*     Now get mjd at start of next year 
      date(1) = date(1) + 1
      call ymdhms_to_mjd( date, secs, mjde )

 
      mjd = mjds + (decyrs-int(decyrs))*(mjde-mjds)
 
***** Thats all
      return
      end
 
