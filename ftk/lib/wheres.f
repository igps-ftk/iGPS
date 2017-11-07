CTITLE
       FUNCTION where(arr,n,np,x)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 n,np,where
      real*8 arr(n),x

c     --OUTPUT--

c     --EXTERNAL--

c     --Local Parameters--
      integer*4 i,j,np2,left,right,middle
      real*8 abs

c     <<VAR_DEC
      left=1
      right=np
 802  middle=(left+right)/2
c      write(*,*) left,middle,right
      if (left.gt.right.or.(right-left).eq.1) then
         where=-1
         goto 801
      endif
      
      if (abs(x-arr(middle)).lt.1e-12) then
         where=middle
         goto 801
      elseif (x.lt.arr(middle)) then
         right=middle
      else
         left=middle
      endif

      goto 802

c      do i=1,np
cc         if (arr(i).eq.x) then
c         if (abs(arr(i)-x).le.1e-12) then
c            where=i
c            goto 801
c         endif
c      enddo
c      where=-1

 801  continue
c      write(*,*) where
      RETURN
      END
