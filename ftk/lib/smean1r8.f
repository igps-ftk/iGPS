c+
c Name:
c     mean1
c Purpose:
c     calculate 1-d vector mean value for real*8 data type
c-
      subroutine smean1r8(mat,nmax,d1,tmean)
      
      integer*4 d1, nmax
      real*8 mat(nmax)
      real*8 tmp,tmean
      integer*4 i
      
      tmp=0
      do i=1,d1
         tmp=tmp+mat(i)
c         write(*,*) tmp
      enddo
c      write(*,*) tmp,d1,tmp/d1
      tmean=tmp/d1

      return
      end
