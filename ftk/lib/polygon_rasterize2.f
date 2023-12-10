CTITLE
       subroutine  polygon_rasterize2(xys,nmax_row,npt,
     +     rect,xstep,ystep,
     .     nx,ny,odata)

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 nmax_row,npt
      real*8 xys(2,nmax_row),rect(4),xstep,ystep
      integer*4 nx,ny

c     --OUTPUT--
      integer*4 nmax
      parameter(nmax=10000)
      integer*4 odata(nmax,nmax)

c     --Local Parameters--
      real*8 oxs,oys
      integer*4 i,j

      integer*4 is_in

c     external functoions
      integer*4 is_point_inside_polygon

c     <<VAR_DEC

c      write(*,*) 'input xys:'
c      do i=1,npt
c         write(*,*) (xys(j,i),j=1,2)
c      enddo
c      stop

      do i=1,nx
         oxs=rect(1)+xstep*i
         do j=1,ny
            oys=rect(3)+ystep*j
c            is_in=is_point_inside_polygon(xys,4,oxs,oys)
            is_in=is_point_inside_polygon(xys,nmax_row,npt,oxs,oys)
c            write(*,*) 'oxs,oys,is_in:',oxs,oys,is_in,nmax_row,npt
            if (is_in.eq.1) then
               odata(i,j)=1
c               write(*,*) i,j
c               stop
            else
               odata(i,j)=0
c               write(*,*) i,j,'zero'
c               stop
            endif
         enddo
      enddo   
     

      RETURN
      END
