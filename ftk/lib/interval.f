CTITLE
       program  interval
c     --PURPOSE--
c       Get the (minimum/mode) interval of an array.

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--

c     --OUTPUT--

c     --Local Parameters--
      integer*4 NMAX,iargc
      Parameter (nmax=1000000)
      integer*4 npt,i
      integer*4 ioerr
      real*8 tmp,data(NMAX),intervals(NMAX),interval_min
      character*100 strs(NMAX)
      integer*4 nblen


c     <<VAR_DEC

c      write(*,*) 'iargc():',iargc()
      npt=0
 601  read(5,*,iostat=ioerr) strs
c      write(*,*) 'input is',tmp
      if (ioerr.eq.-1) then
c         print*,'error'
         go to 901
      endif
      go to 601
 901  continue

c     count the number of inputs
      npt=0
      do i=1, NMAX
         if (nblen(strs(i)).eq.0) go to 902
         read(strs(i),*) data(i)
         npt=npt+1
      enddo
  902 continue
   
c      write(*,*) strs(1:npt)
c      write(*,*) data(1:npt)
c      stop
      tmp=0
      interval_min=9d24
      do i=1,npt-1
c         if (data(i+1).eq.0) then
c             go to 602
c          endif
          tmp=data(i+1)-data(i)
c          write(*,*) i,tmp,data(i+1),data(i)
          if (tmp.lt.interval_min) interval_min=tmp
          intervals(i)=tmp
       enddo
 602   continue
c       interval=tmp/npt

       print*,interval_min
      stop
      END
