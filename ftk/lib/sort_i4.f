CTITLE
       SUBROUTINE sort_i4(arrin,nmax,n,arrout)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 nmax,n
      integer*4 arrin(nmax)

c     --OUTPUT--
      integer*4 arrout(nmax)

c     --EXTERNAL--

c     --Local Parameters--
      integer*4 i,j,tmp

c     <<VAR_DEC
      
      do i=1,n
         arrout(i)=arrin(i)
      enddo

      do i=1,n
         do j=i+1,n
            if (arrout(i).gt.arrout(j)) then
               tmp=arrout(i)
               arrout(i)=arrout(j)
               arrout(j)=tmp
            endif
         enddo
      enddo

      RETURN
      END
