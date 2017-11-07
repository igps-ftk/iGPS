CTITLE
      PROGRAM dyr_2_yrdoyr

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

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
  

c     <<VAR_DEC

 800  read(5,'(a)',end=899) buf
c      write(*,*) 'buf:',buf(1:nblen(buf))
      if (nblen(buf).eq.0) then
         goto 899
      endif
      read(buf,*) dyr
c      write(*,*) dyr
      call decyrs_to_jd(dyr,jd)
      call jd_to_yds(jd,yr,doyr,sec)
      write(*,700) buf(1:nblen(buf)),int(dyr),doyr
 700  format(a,1x,i04,1x,i03)
      goto 800

 899  continue
      STOP
      END
