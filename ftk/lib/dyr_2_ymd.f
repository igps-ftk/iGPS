CTITLE
      PROGRAM dyr_2_ymd

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 nblen

c     --Local Parameters--
      character*1024 buf
      real*8 dyr,jd,sec
      integer*4 yr,doyr
      integer*4 date1(5)
  

c     <<VAR_DEC

 800  read(5,'(a)',end=899) buf
c      write(*,*) 'buf:',buf(1:nblen(buf))
      if (nblen(buf).eq.0) then
         goto 899
      endif
      read(buf,*) dyr
c      write(*,*) dyr
      call decyrs_to_jd(dyr,jd)
c      call jd_to_yds(jd,yr,doyr,sec)
      call jd_to_ymdhms(jd,date1,sec)
      write(*,700) buf(1:nblen(buf)),date1,sec
 700  format(a,1x,i04,4(1x,i02),1x,f9.6)
      goto 800

 899  continue
      STOP
      END
