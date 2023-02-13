CTITLE
      subroutine  along_line_shift(x1,y1,x2,y2,x3,y3,d,ox1,oy1)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--
c     

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      real*8 x1,y1,x2,y2,x3,y3,d

c     --OUTPUT--
      real*8 ox1,oy1

c     --Local Parameters--

      real*8 d12,theta,rate,xsgn,ysgn,b
      integer*4 nmax
      parameter(nmax=1000)
      real*8 xs(nmax),ys(nmax),ds(nmax)
      
c     for map_2points
      integer*4 is_radian,is_meter,is_mile,indmin,i
      real*8 r_earth,dlen,dmin

c     <<VAR_DEC

      is_radian=0
      is_meter=1
      is_mile=0
      r_earth=0d0
      
      write(*,*) 'x1,y1,x2,y2:',x1,y1,x2,y2
c     if it is a vertical line
      if (abs(x1-x2).le.1d-6) then        
        do i=1,nmax
          xs(i)=x1
          ys(i)=y1+(i-1)*(y2-y1)/nmax
c          write(*,*),xs(i),ys(i)
        enddo
c        stop
      else
        rate=(y1-y2)/(x1-x2)
c      theta=atan(rate)
        b=y2-rate*x2
        write(*,*) 'rate,b:',rate,b
c      stop        
        do i=1,nmax
          xs(i)=i*((x2-x1)*1d0/nmax)+x1
          ys(i)=xs(i)*rate+b
c        write(*,*),xs(i),ys(i)
        enddo
c      stop
      endif
      
      indmin=-9999
      dmin=9999
      do i=1,nmax-1
c        write(*,*) 'x3,y3,xsi,ysi:',x3,y3,xs(i),ys(i)
        call map_2points(x3,y3,xs(i),ys(i),is_radian,is_meter,
     &     is_mile,r_earth, dlen)
c        write(*,*) 'dlen1:',dlen,x3,y3,xs(i),ys(i)
c       convert meter to km
        dlen=dlen*1d-3
        ds(i)=dlen
c        write(*,*) 'dlen:',dlen,i,d,dmin,abs(dlen-d)
        if (abs(dlen-d).lt.dmin) then
          dmin=abs(dlen-d)
          indmin=i
        endif
      enddo
      
      write(*,*) 'indmin:',indmin,dmin
      ox1=xs(indmin)
      oy1=ys(indmin)
      

c      xsgn=(x2-x1)/abs(x2-x1)
c      ysgn=(y2-y1)/abs(y2-y1)
c      write(*,*) 'sgn:',xsgn,x1,x2,y1,ysgn,y1,y2
c      
c      ox1=x3+d*cos(theta)*xsgn
c      oy1=y3+d*sin(theta)*ysgn
      
      end 
