CTITLE
       subroutine point_perp_line(a1,b1,c1,d1)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      real*8 a1(2),b1(2),c1(2)

c     --OUTPUT--
      real*8 d1(2)

c     --Local Parameters--
      real*8 rate1,intercept1,rate2,intercept2,alpha1,alpha2,DPI
      real*8 x1,y1,x2,y2,r8zero

c     <<VAR_DEC

      DPI=4d0*datan(1d0)
      
      r8zero=0d0

      IF (a1(1).eq.0) THEN
        a1(1)=100.320d0
        a1(2)=25.841d0
        b1(1)=104.769d0
        b1(2)=27.037d0
        c1(1)=105.687d0
        c1(2)=29.356d0
c        
c        ;for vertical line    
c        b1=(100.320,    25.841)*1d0
c        c1=(104.769,    27.037)*1d0
c        a1=(100.320,    29.356)*1d0
c        
c        ;for horizontal line
c        b1=(100.320,    25.841)*1d0
c        c1=(104.769,    27.037)*1d0
c        a1=(105.687,    25.841)*1d0
      ENDIF
      
c      ;if vertical line
      if (abs(b1(1)-a1(1)).le.1d-6) then
        rate1=0/r8zero
        intercept1=0
        rate2=0
        intercept2=c1(2)
        d1(1)=a1(1)
        d1(2)=c1(2)
        goto 601
      endif
      
c      ;if horizontal line
      if (abs(b1(2)-a1(2)).le.1e-6) then
        rate2=0/r8zero
        intercept1=a1(2)
        rate1=0
        intercept2=0
        d1(1)=c1(1)
        d1(2)=a1(2)
        goto 601
      endif
      
      rate1=(b1(2)-a1(2))/(b1(1)-a1(1))
      intercept1=a1(2)-rate1*a1(1)
      alpha1=ATAN(rate1)
      alpha2=alpha1+dpi/2
      rate2=TAN(alpha2)
      intercept2=c1(2)-rate2*c1(1)
      
      x1=a1(1)
      y1=rate2*(x1-c1(1))+c1(2)
      
      x2=(intercept1-intercept2)/(rate2-rate1)
      y2=rate1*x2+intercept1
      
      d1(1)=x2
      d1(2)=y2
      
c      write(*,*) 'a1,b1,c1:',a1,b1,c1
c      write(*,*) 'line1:',rate1,intercept1,alpha1
c      write(*,*) 'line2:',rate2,intercept2,alpha2
c      write(*,*) 'x1,y1,x2,y2:',x1,y1,x2,y2
      
601   continue      
      

      RETURN
      END
