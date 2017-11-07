c+
c Name:
c     max2i
c Purpose:
c     return the maximum element of a 2-d array with its index (i,j) 
c-
      real*8 function max2i(mat,d1,d2,ind)
      

      real*8 mat(d1,d2)
      real*8 tmp
      integer*4 i,j,ind(2)
      
      tmp=mat(1,1)
      ind(1)=1
      ind(2)=1
      do i=1,d1
         do j=1,d2
            if (tmp.gt.mat(i,j)) then
               tmp=mat(i,j)
               ind(1)=i
               ind(2)=j
            endif
         enddo
      enddo
      max2i=tmp


      return
      end
