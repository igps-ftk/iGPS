CTITLE
       integer*4 FUNCTION is_point_inside_polygon(xys,nmax,np,x,y)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 nmax,np
      real*8 xys(2,nmax),x,y

c     --OUTPUT--

c     --Local Parameters--
      integer*4 counts(2),rem1,rem2
      integer*4 i,j,li,li_next
      real*8 x1,y1,x2,y2,xmin1,xmax1,ymin1,ymax1
      real*8 rate,y0,px

c     <<VAR_DEC
      counts(1)=0
      counts(2)=0

c      do i=1,np
c         write(*,*) 'xys:',i,(xys(j,i),j=1,2)
c      enddo
c      write(*,*) 'x,y:',x,y
c      stop
      if (xys(1,1).eq.xys(1,np).and.xys(2,1).eq.xys(2,np)) then
         np=np-1
      endif

      do li=1,np
         
c     the two vertexes of current edge
c     starting point
         x1=xys(1,li)
         y1=xys(2,li)
c     endding point
         if (li.eq.np) then
            li_next=1
         else
            li_next=li+1
         endif
         x2=xys(1,li_next)
         y2=xys(2,li_next)
c$$$         write(*,*) 'x1,y1,x2,y2:',x1,y1,x2,y2

         xmin1=x1
         if (xmin1.gt.x2) xmin1=x2
         xmax1=x1
         if (xmax1.lt.x2) xmax1=x2
         ymin1=y1
         if (ymin1.gt.y2) ymin1=y2
         ymax1=y1
         if (ymax1.lt.y2) ymax1=y2

c$$$         write(*,*) 'xmin1,xmax1,ymin1,ymax1:',xmin1,xmax1,ymin1,ymax1

         rate=(y2-y1)/(x2-x1)
         y0=y1-rate*x1
         px=(y-y0)/rate
c$$$         write(*,*) 'rate,y0,px:',rate,y0,px
c         stop

c     if no intersection
         if (px.lt.xmin1.or.px.gt.xmax1) then
            goto 901
         endif

         if (px.lt.x) then
            counts(1)=counts(1)+1
         else
            counts(2)=counts(2)+1
         endif

 901     continue

      enddo

      rem1=mod(counts(1),2)
      rem2=mod(counts(2),2)
c$$$      write(*,*) 'counts:', counts
c$$$      write(*,*) 'rem1,rem2:', rem1,rem2

      if (rem1.eq.1.and.rem2.eq.1) then
         is_point_inside_polygon=1
      else
         is_point_inside_polygon=0
      endif

c$$$      write(*,*) 'is_in:',is_point_inside_polygon
c$$$c      if (is_point_inside_polygon.eq.1) then      
c$$$c         stop
c$$$c      endif

      RETURN
      END
