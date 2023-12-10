CTITLE
       real*8 FUNCTION determ(a,n,np)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 n,np
      real*8 a(np,np)

c     --OUTPUT--

c     --Local Parameters--
      integer*4 indx(n),j,i
      

c     <<VAR_DEC

      call dludcmp(a,n,np,indx,determ)
c      write(*,*) 'determ:',determ
      do j=1,n
         determ=determ*a(j,j)
c         write(*,*) (a(j,i),i=1,np)
c         write(*,*) 'determ:',determ,n,np,j
      enddo

      RETURN
      END
