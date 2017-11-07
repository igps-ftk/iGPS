CTITLE
       real*8 FUNCTION heaviside(x)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      real*8 x

c     --OUTPUT--

c     --Local Parameters--

c     <<VAR_DEC
c      write(*,*) 'dabs:',dabs(x)
      if (dabs(x).lt.0.000001d0) then
         heaviside=1d0/2
      else
         if (x.lt.0) then
            heaviside=0
         else
            heaviside=1
         endif
      endif

      RETURN
      END
