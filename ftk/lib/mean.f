CTITLE
       program  mean
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--

c     --OUTPUT--

c     --Local Parameters--
      integer*4 NMAX,iargc
      Parameter (nmax=1000000)
      integer*4 npt,i
      integer*4 ioerr
      real*8 tmp,data(NMAX),avg


c     <<VAR_DEC

c      write(*,*) 'iargc():',iargc()
      npt=0
 601  read(5,*,iostat=ioerr) data
c      write(*,*) 'input is',tmp
      if (ioerr.eq.-1) then
c         print*,'error'
         go to 901
      endif
      go to 601
 901  continue
c      write(*,*) data(1:10)
      tmp=0
      do i=1,NMAX
         if (data(i).eq.0) then
             go to 602
          endif
          npt=npt+1
          tmp=tmp+data(i)
       enddo
 602   continue
       avg=tmp/npt

       print*,avg
      stop
      END
