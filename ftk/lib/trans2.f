 
c+
c     transpose
c-
      subroutine transpose(a,at,m,n,mmax,nmax)

      integer  m,n
      real*8 a(mmax,nmax),at(nmax,mmax)
c     loop variables
      integer i,j
c      write(*,*),m,n
      do i=1,n
         do j=1,m
            at(i,j)=a(j,i)
c            print*,i,j,' hah ',at(i,j)
         enddo
      enddo
      

      end
