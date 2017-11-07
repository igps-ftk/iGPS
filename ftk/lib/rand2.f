CTITLE
      SUBROUTINE rand2(a,m,n)
c     --PURPOSE--
c     return a random array A(m,n)

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 m,n
      real*8 a(m,n)

c     --OUTPUT--

c     --Local Parameters--
      integer*4 i,j

c     <<VAR_DEC
      do i=m
         do j=1,n
            a(i,j)=drand(0)
         enddo
      enddo

      RETURN
      END
