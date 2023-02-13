CTITLE
       subroutine  polygon_rasterize(xys,rect,xstep,ystep,
     .     nx,ny,odata)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      real*8 xys(2,4),rect(4),xstep,ystep
      integer*4 nx,ny

c     --OUTPUT--
      integer*4 nmax
      parameter(nmax=1000)
      integer*4 odata(nmax,nmax)

c     --Local Parameters--
      real*8 oxs,oys
      integer*4 i,j

      integer*4 is_in

c     external functoions
      integer*4 is_point_inside_polygon

c     <<VAR_DEC


      do i=1,nx
         oxs=rect(1)+xstep*i
         do j=1,ny
            oys=rect(3)+ystep*j
c            is_in=is_point_inside_polygon(xys,4,oxs,oys)
            is_in=is_point_inside_polygon(xys,4,4,oxs,oys)
c            write(*,*) 'oxs,oys,is_in:',oxs,oys,is_in
            if (is_in.eq.1) then
               odata(i,j)=1
            else
               odata(i,j)=0
            endif
         enddo
      enddo   
     

      RETURN
      END
