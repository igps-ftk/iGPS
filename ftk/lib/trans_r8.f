 
c+
c     transpose
c-
      subroutine trans_r8(a,at,m,n,mmax,nmax)

      integer*4 m,n,mmax,nmax
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
