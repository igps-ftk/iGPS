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

      real*8 d12,theta,rate,xsgn,ysgn

c     <<VAR_DEC

c      d12=sqrt((y1-y2)**2+(x1-x2)**2)
      rate=(y1-y2)/(x1-x2)
      theta=atan(rate)
      
c      b=y2-rate*y1

      xsgn=(x2-x1)/abs(x2-x1)
      ysgn=(y2-y1)/abs(y2-y1)
      write(*,*) 'sgn:',xsgn,x1,x2,y1,ysgn,y1,y2
      
      ox1=x3+d*cos(theta)*xsgn
      oy1=y3+d*sin(theta)*ysgn
      
      end 
