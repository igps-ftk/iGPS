      LOGICAL FUNCTION LEAPYR ( IYEAR )

      implicit none

c     Determine if the input year is a leap year
c     R. King   2 January 1992
c     based on S.Shimada's integer function LEAP

      integer*4 iyear,iyear1

      leapyr = .false.
      iyear1 = iyear
      if( iyear1.lt.1900 ) iyear1 = iyear1 + 1900
      IF( MOD(IYEAR1,  4) .NE. 0 )  GO TO 9
      IF( MOD(IYEAR1,100) .NE. 0 )  GO TO 1
      IF( MOD(IYEAR1,400) .NE. 0 )  GO TO 9
    1 leapyr = .true.
    9 RETURN
      END
