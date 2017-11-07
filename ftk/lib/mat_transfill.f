      subroutine mat_transfill(mat,nmax,n)
C     Input:
c     mat: (N*N)
c     n:
c     ---
      IMPLICIT NONE
      integer*4 n,nmax
      real*8 mat(nmax,nmax)
c     ---
      integer*4 i,j
c     ---
      do i=1,n
         do j=i,n
            mat(j,i)=mat(i,j)
         enddo
      enddo
      return
      end
      
