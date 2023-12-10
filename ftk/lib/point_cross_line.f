CTITLE
       subroutine point_cross_line(a1,b1,c2,rate2,d2)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      real*8 a1(2),b1(2),c2(2),rate2

c     --OUTPUT--
      real*8 d2(2)

c     --Local Parameters--
      real*8 rate1,intercept1,intercept2,alpha1,alpha2,DPI
      real*8 x1,y1,x2,y2,r8zero,xmin,xmax
      
      real*8 r8Nan,r8Inf

c     <<VAR_DEC

      DPI=4d0*datan(1d0)
      
      
      r8zero=0d0
      r8Inf=1/r8zero
      write(*,*) 'r8Inf:',r8Inf
c      stop

      if ((a1(1).eq.0)) then
        a1(1)=86.7303772579d0
        a1(2)=29.4418913671d0
        b1(1)=85.5126849284d0
        b1(2)=29.4418913671d0
        c2(1)=87.1
        c2(2)=30
        rate2=.5d0
      ENDIF
      
      xmin=a1(1)
      xmax=b1(1)
      if ((a1(1).gt.b1(1))) then
        xmin=b1(1)
        xmax=a1(1)
      endif
      
c      ;if (horizontal line
      if (ABS(b1(2)-a1(2)).le.1e-6) then 
c        ;rate2=!values.D_INFINITY
        intercept1=a1(2)
        rate1=0
        if (rate2.eq.r8Inf) then 
          d2(1)=c2(1)
          d2(2)=a1(2)
          intercept2=r8Inf
          GOTO 601
        ENDIF
      ENDIF
      
      intercept2=c2(2)-rate2*c2(1)    

c      ;if (vertical line
      if (ABS(b1(1)-a1(1)).le.1e-6) then 
        rate1=r8Inf
        intercept1=0        
        
c        ;rate2=0
c        ;intercept2=c2(2)
        if (rate2.eq.r8Inf) then 
          if (ABS(b1(1)-c2(1)).le.1e-6) then  
            write(*,*) '[]ERROR:two lines are identical!!'
            STOP
          ENDIF
c        ;      d2=(c2(1),a1(2)]
c        ;      intercept2=!values.D_INFINITY
c        ;      GOTO, end_prog
        ENDIF
        d2(1)=a1(1)
        d2(2)=rate2*a1(1)+intercept2
c        ;stop
        GOTO 601
      ENDIF
      
      rate1=(b1(2)-a1(2))/(b1(1)-a1(1))
      intercept1=a1(2)-rate1*a1(1)
      alpha1=ATAN(rate1)
      alpha2=alpha1+dpi/2
c      ;rate2=TAN(alpha2)
      
      x1=a1(1)
      
      
      if (rate2.eq.r8Inf) then 
        x2=c2(1)
        y2=rate1*x2+intercept1
        intercept2=r8Inf
      ELSE 
        y1=rate2*(x1-c2(1))+c2(2)
        x2=(intercept1-intercept2)/(rate2-rate1)
        y2=rate1*x2+intercept1
      ENDIF
      
      
      d2(1)=x2
      d2(2)=y2
      
601   continue      
      

      RETURN
      END
