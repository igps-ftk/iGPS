CTITLE
      subroutine rot_xy(xs,ys,NMAX,np,theta,oxs,oys)

c     --PURPOSE--


c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --Global Variables--
      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     --INPUT--

      integer*4 NMAX,np
      real*8 xs(NMAX),ys(NMAX),theta
      
c     --OUTPUT--
      real*8 oxs(NMAX),oys(NMAX)

c     --Local Parameters--
      integer*4 i

c     <<VAR_DEC

      do i=1,np
         oxs(i)=xs(i)*cos(theta)-ys(i)*sin(theta)
         oys(i)=xs(i)*sin(theta)+ys(i)*cos(theta)
      enddo
      
      RETURN
      END
