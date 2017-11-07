      subroutine moment(x0,nx0,mom)
C     ---
      implicit none
      include '../inc/cgps.h'
      real*8 x0(nmax_row),mom(4)
      integer*4 nx0
c     ---
      integer*4 i,j,k
      real*8 tmp,ave,stdv,var,min,max
C     ---
C     >> calculate mean
      tmp=0
      min=0
      max=0
      do i=1,nx0
         tmp=tmp+x0(i)
         if (x0(i).gt.max) then
            max=x0(i)
         endif
         if (x0(i).lt.min) min=x0(i)         
      enddo
      ave=tmp/nx0
c     <<
c     >> calculate variance
      tmp=0
      do i=1,nx0
         tmp=tmp+(x0(i)-ave)**2
      enddo
      var=tmp/(nx0-1)
      stdv=dsqrt(var)
C     <<
      mom(1)=ave
      mom(2)=var
      mom(3)=min
      mom(4)=max

      return
      end
