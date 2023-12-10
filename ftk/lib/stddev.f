CTITLE
      FUNCTION stddev(data,n)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      real*8 data(*)
      integer*4 n

c     --OUTPUT--
      real*8 stddev

c     --Local Parameters--
      real*8 mean,rms
      integer i
      
c     <<VAR_DEC

      mean=0d0
      do i=1,n
         mean=mean+data(i)
      enddo
      mean=mean/n

      stddev=0d0
      do i=1,n
         stddev=stddev+(data(i)-mean)**2
      enddo
      stddev=dsqrt(stddev)

      RETURN
      END
