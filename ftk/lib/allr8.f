CTITLE
      logical FUNCTION allr8(line,nmax,n)
c     --PURPOSE--


c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --Global Variables--
      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     --INPUT--
      integer*4 nmax,n
      real*8 line(nmax)
      
c     --OUTPUT--

c     --Local Parameters--
      integer*4 i

c     <<VAR_DEC
      allr8=.false.
      do i=1,n
         if (line(i).eq.0) return
      enddo
      allr8=.true.

      RETURN
      END
