c+
c Name:
c     max2
c Purpose:
c     return the maximum element of a 2-d array
c-
      real*8 function max2(mat,d1,d2)

      
      real*8 mat(d1,d2)
      real*8 tmp
      integer*4 i,j
      
      tmp=mat(1,1)
      do i=1,d1
         do j=1,d2
            if (tmp.lt.mat(i,j)) then
               tmp=mat(i,j)
            endif
         enddo
      enddo
      max2=tmp


      return
      end
