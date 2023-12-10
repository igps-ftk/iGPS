CTITLE
      subroutine perp_line_shift(x1,y1,x2,y2,x3,y3,p_len,slope,kpd,oxy)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--
c     

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      real*8 x1,y1,x2,y2,x3,y3,p_len,slope,kpd

c     --OUTPUT--
      real*8 oxy(4)

c     --Local Parameters--

      real*8 rate_pf,intercept_pf,x4,y4,x5,y5

c     <<VAR_DEC

      
      write(*,*) 'slope in:',slope
      
      if (abs(slope).le.1d-9) then
        x4=x3
        y4=y3-(p_len/2d0/kpd)
        
        x5=x3
        y5=y3+(p_len/2d0/kpd)
      else
        rate_pf=-1d0/tan(slope)
        intercept_pf=y3-rate_pf*x3
        
        x4=x3-(p_len/2d0/kpd)*cos(atan(rate_pf))
        y4=rate_pf*x4+intercept_pf
        
        x5=x3+(p_len/2d0/kpd)*cos(atan(rate_pf))
        y5=rate_pf*x5+intercept_pf
      endif
      
      
      oxy=[x4,y4,x5,y5]
      
      end 
