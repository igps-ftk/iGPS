      SUBROUTINE xyz2sph(x,y,z,r,theta,lamda)
c     --PURPOSE--

c     --INPUT--
      real*8 x,y,z

c     --OUTPUT--
      real*8 r,theta,lamda

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --Global Variables--
      IMPLICIT NONE

c     --Command-line Parameters--

c     --Local Parameters--

c     <<VAR_DEC

      r=dsqrt(x**2+y**2+z**2)
      theta=datan(dsqrt(x**2+y**2)/z)
      lamda=datan(y/x)

      END
