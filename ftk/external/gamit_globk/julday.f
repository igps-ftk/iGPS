      Function JULDAY( imonth,iday,iyear)

c     Compute an integer Julian Day given month, day, and year
c     according to the convention of the MIT Planetary Ephemeris
c     Program (PEP).

c     PEP Julian days (PJD) are defined as the (true) Julian 
c     day beginning at noon on the UTC day specified; hence
c     PJD = JD + 0.5. This is the opposite convention as that
c     used for modified Julian date, MJD = JD - 2430000.5, 
c     hence MJD = PJD - 2430000 -1 .
c
c     The algorithm used here is valid from the year 1 of the 
c     Common Era (CE) but we've arbitrarily limited the range 
c     in order to provide a check for reasonableness to trap input 
c     or code bugs elsewhere.  R. King July 1999

      implicit none

      character*80 prog_name
      character*256 message

      integer*4 len,rcpar,julday,imonth,iday,iyear,iyr,iyr4,iyr100
     .        , iyr400,montot(12)
                                                                          
      data montot/0,31,59,90,120,151,181,212,243,273,304,334/

c       imonth = month           (between 1 and 12)
c       iday   = day of month    (between 1 and 31)
c       iyear  = 4-digit year    (between 1960 and 2020)
c        
c     get the program name to report errors
      len = rcpar(0,prog_name)

cd      print *,'JULDAY iday ',iday  
c     check for valid year (may be changed with no impact on program)
      if( iyear.lt.1960 .or. iyear.gt.2100 ) then
         write(message,'(a,i8)') 'Unreasonable year: ',iyear
         call report_stat('FATAL',prog_name,'lib/julday','  ',message,0)
      endif

c     check for valid month
      if( imonth.le.0 .or. imonth.gt.12 ) then
         write(message,'(a,i8)') 'Unreasonable month: ',imonth
         call report_stat('FATAL',prog_name,'lib/julday','  ',message,0)
      endif
           
c     check for valid day 
c  rwk 090922: Don't check for iday=0 since this can happen in innocuous ways
      if( iday.lt.0 .or. iday.gt.31 ) then
         write(message,'(a,i8)') 'Unreasonable day: ',iday
         call report_stat('FATAL',prog_name,'lib/julday','  ',message,0)
      endif

      iyr = iyear
      if(imonth.le.2) iyr = iyr - 1  
      iyr4  = iyr/4
      iyr100=iyr/100
      iyr400=iyr/400   
c     1900 is Julian day 2415020
      JULDAY = 2415020 + 365*(iyear-1900) - 1899/4 + 1899/100 - 1899/400
     .         + (montot(imonth) + iday + iyr4 - iyr100 + iyr400) 
      return
      end

