c+
c Name:
c     mean1
c Purpose:
c     calculate 1-d vector mean value
c-
      real*8 function mean1(mat,nmax,d1)
      
      integer*4 d1, nmax
      real*8 mat(nmax)
      real*8 tmp
      integer*4 i
      
      tmp=0
      do i=1,d1
         tmp=tmp+mat(i)
         write(*,*) tmp
      enddo
      write(*,*) tmp,d1,tmp/d1
      mean1=tmp/d1

      return
      end
