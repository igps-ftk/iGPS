CTITLE
      PROGRAM dyr_2_yrdoyr

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
      integer*4 status,system

c     --Local Parameters--
      character*1024 buf,cmdstr
      real*8 dyr,jd,sec
      integer*4 yr,doyr,year,mon,day,date(5)
  

c     <<VAR_DEC

 800  read(5,'(a1024)',end=899) buf
c      write(*,*) 'buf:',buf(1:nblen(buf))
      if (nblen(buf).eq.0) then
         goto 899
      endif
      read(buf,*) year,mon,day
      date(1)=year
      date(2)=mon
      date(3)=day
      date(4)=12
      date(5)=0
      sec=0d0
      call ymdhms_to_jd(date, sec, jd)
      call jd_to_decyrs(jd, dyr)
      write(*,'(f10.5)') dyr
c      write(*,*) year,mon,day
c      write(cmdstr,'(a,1x,a)') 'doy',buf(1:nblen(buf))
c      write(*,*) cmdstr(1:nblen(cmdstr))
c      status=system(cmdstr)
c      call decyrs_to_jd(dyr,jd)
c      call jd_to_yds(jd,yr,doyr,sec)
c      write(*,700) buf(1:nblen(buf)),int(dyr),doyr
 700  format(a,1x,i04,1x,i03)


      goto 800

 899  continue
      STOP
      END
