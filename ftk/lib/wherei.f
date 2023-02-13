CTITLE
       FUNCTION wherei(arr,n,np,x)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 n,np,where
      integer*4 arr(n),x

c     --OUTPUT--

c     --EXTERNAL--

c     --Local Parameters--
      integer*4 i,j,np2,left,right,middle
      real*8 abs

c     <<VAR_DEC
c      left=1
c      right=np
c 802  middle=(left+right)/2
cc      write(*,*) left,middle,right
c      if (left.gt.right.or.(right-left).eq.1) then
c         where=-1
c         goto 801
c      endif
c      
c      if (abs(x-arr(middle)).lt.1e-12) then
c         where=middle
c         goto 801
c      elseif (x.lt.arr(middle)) then
c         right=middle
c      else
c         left=middle
c      endif
c
c      goto 802

      do i=1,np
         if (arr(i).eq.x) then
c         if (abs(arr(i)-x).le.1e-12) then
            where=i
            goto 801
         endif
      enddo
      where=-1

 801  continue
c      write(*,*) where
      RETURN
      END
