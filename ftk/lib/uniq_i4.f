CTITLE
      subroutine uniq_i4(arr,n,np,indx,nindx)

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
      integer*4 n,indx(n),nindx,np
      integer*4 arr(n)
      
c     --Command-line Parameters--

c     --OUTPUT--

c     --EXTERNAL--

c     --Local Parameters--
      integer*4 i,j
      integer*4 tmp

c     <<VAR_DEC

      nindx=0

      tmp=-9999
      do i=1,np
         if (tmp.eq.arr(i)) then
            goto 801
         endif
         tmp=arr(i)
         nindx=nindx+1
         indx(nindx)=i   
c         write(*,*) tmp,arr(i)
 801  continue
      enddo
c      write(*,*) 'uniq ok',tmp
c      STOP
      END
