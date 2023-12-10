CTITLE
       FUNCTION wheres(arr,n,np,x)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 n,np,wheres
      character*(*) arr(n),x

c     --OUTPUT--

c     --EXTERNAL--

c     --Local Parameters--
      integer*4 i,j,np2,left,right,middle
      real*8 abs

c     <<VAR_DEC
ccc      left=1
ccc      right=np
ccc 802  middle=(left+right)/2
cccc      write(*,*) left,middle,right
ccc      if (left.gt.right.or.(right-left).eq.1) then
ccc         wheres=-1
ccc         goto 801
ccc      endif
ccc      
ccc      if (abs(x-arr(middle)).lt.1e-12) then
ccc         wheres=middle
ccc         goto 801
ccc      elseif (x.lt.arr(middle)) then
ccc         right=middle
ccc      else
ccc         left=middle
ccc      endif
ccc
ccc      goto 802

      do i=1,np
c        write(*,*) i,arr(i),' ',x
         if (arr(i).eq.x) then
c         if (abs(arr(i)-x).le.1e-12) then
            wheres=i
            goto 801
         endif
      enddo
      wheres=-1

 801  continue
c      write(*,*) wheres
      RETURN
      END
