CTITLE
      SUBROUTINE covj_1(j_1,n)
c     --PURPOSE--

c     --ALGORITHM--
c     Zhang et al., 1997
c     pp.20
c     Formula (B3)

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 n
      real*8 j_1(n,n)

c     --OUTPUT--

c     --Local Parameters--
      real*8 j0(n,n)
      integer*4 i,j

c     <<VAR_DEC
      call j0_init(j0,n)
      do i=1,n
         do j=1,n
            if (i.eq.j) then
               j_1(i,j)=(3d0/4)**2*(24*1-j0(i,j))/12
            else
               j_1(i,j)=(3d0/4)**2*(24*1-j0(i,j))/12
c     I think the above formula is wrong. I should be,
               j_1(i,j)=(3d0/4)**2*(24*0-j0(i,j))/12
c     However, the wrong format is consistent with Zhang's example.
c     
            endif
         enddo
      enddo

      RETURN
      END


      subroutine j0_init(j0,n)
c     Zhang et al., 1997 JGR-SE 97JB01380 (B4) pp.20
      integer*4 n
      real*8 j0(n,n)

      integer*4 i,k
      
      do i=1,n
         do k=i,n
            if (i.eq.k) then
               j0(i,k)=0d0
            else
               j0(i,k)=dlog(1d0*k-i)/dlog(2d0)+2
               j0(k,i)=j0(i,k)
            endif
         enddo
      enddo

      return
      end
