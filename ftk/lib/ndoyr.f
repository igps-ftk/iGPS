CTITLE
       integer*4 FUNCTION ndoyr(yr)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 yr

c     --OUTPUT--

c     --Local Parameters--
      integer*4 date0(5),date1(5)
      real*8 jd0,jd1

c     <<VAR_DEC

      date0(1)=yr
      date1(1)=yr+1
      date0(2)=1
      date1(2)=1
      date0(3)=1
      date1(3)=1
      date0(4)=0
      date1(4)=0
      date0(5)=0
      date1(5)=0
      
      call ymdhms_to_jd(date0,0.d0,jd0)
      call ymdhms_to_jd(date1,0.d0,jd1)

c      write(*,*) date0
c      write(*,*) date1
c      write(*,*) ndoyr,jd1,jd0,jd1-jd0

      ndoyr=jd1-jd0


      RETURN
      END
