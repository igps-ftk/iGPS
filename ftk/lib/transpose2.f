
c+
c Name:
c     transpose2
c Purpose:
c     tranpose a 2-d array
c-
      subroutine transpose2(a, at, m,n)


      integer*4  m,n
      real*8 a(m,n),at(n,m)
c     loop variables
      integer i,j

c      write(*,*),m,n

      do i=1,n
         do j=1,m
            at(i,j)=a(j,i)
c            print*,i,j,' hah ',at(i,j)
         enddo
      enddo
      
      return
      end
