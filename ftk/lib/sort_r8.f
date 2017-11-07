CTITLE
       SUBROUTINE sort_r8(arrin,nmax,n,arrout,indout)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 nmax,n
      real*8 arrin(nmax)

c     --OUTPUT--
      real*8 arrout(nmax)
      integer*4 indout(nmax)

c     --EXTERNAL--

c     --Local Parameters--
      integer*4 i,j,tmpind
      real*8 tmp

c     <<VAR_DEC
      
      do i=1,n
         arrout(i)=arrin(i)
         indout(i)=i
      enddo

      do i=1,n
         do j=i+1,n
            if (arrout(i).gt.arrout(j)) then
               tmp=arrout(i)
               arrout(i)=arrout(j)
               arrout(j)=tmp

               tmpind=indout(i)
               indout(i)=indout(j)
               indout(j)=tmpind
            endif
         enddo
      enddo

      RETURN
      END
