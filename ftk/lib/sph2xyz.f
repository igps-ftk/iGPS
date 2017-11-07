      SUBROUTINE sph2xyz(r,theta,lamda,x,y,z)
c     --PURPOSE--

c     --INPUT--
      real*8 r,theta,lamda

c     --OUTPUT--
      real*8 x,y,z

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --Global Variables--
      IMPLICIT NONE

c     --Command-line Parameters--

c     --Local Parameters--

c     <<VAR_DEC

      x=r*dsin(theta)*dcos(lamda)
      y=r*dsin(theta)*dsin(lamda)
      z=r*dcos(theta)

      END
