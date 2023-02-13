CTITLE
      program  sar_cal_sat_incidence
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--
c     

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--

c     --OUTPUT--

c     --Local Parameters--
      integer*4 NMAX,iargc
      Parameter (nmax=10000000)
c      Parameter (nmax=10)
      integer*4 npt,i
      integer*4 ioerr
      real*8 tmp,data(NMAX),avg,r_zero,lookE,lookN,lookU,incidence
      real*8 tmp2,azimuth
      character*1023 tmpstr1,tmpstr2,tmpstr3,tmpstr4
      integer*4 nblen


c     <<VAR_DEC

c      write(*,*) 'iargc():',iargc()
      r_zero=0d0

      npt=0
 601  read(5,*,iostat=ioerr) lookE,lookN,lookU
c      write(*,*) 'input is',tmp
      if (ioerr.gt.0) then
         print *, 0/r_zero
         stop
c         print*,'sth error'
c         go to 901
      endif
      if (ioerr.lt.0) then
c         print *, 'end-of-input'
         goto 901
      endif
      npt=npt+1
c      data(1,npt)=lookE
      tmp=atan(lookU/sqrt(lookE**2+lookN**2))
      write(tmpstr1,*) tmp*180d0/3.1415926d0
      call strtrim(tmpstr1,tmpstr2)

c      tmp2=atan(lookN/lookE)
      tmp2=atan(lookE/lookN)
      write(tmpstr3,*) tmp2*180d0/3.1415926d0
      call strtrim(tmpstr3,tmpstr4)

      write(*,'(a,1x,a)') tmpstr2(1:nblen(tmpstr2)),
     +  tmpstr4(1:nblen(tmpstr4))
c     loop next
      go to 601

c     done with read inputs
 901  continue
c      write(*,*) '#total inputs:',npt
c      write(*,*) data(1:10)
c$$$      tmp=0
c$$$      npt=0
c$$$      do i=1,NMAX
c$$$         if (data(i).eq.0) then
c$$$             go to 602
c$$$          endif
c$$$          npt=npt+1
c$$$          tmp=tmp+data(i)
c$$$       enddo
c$$$ 602   continue
c$$$       avg=tmp/npt
c$$$
c$$$       write(tmpstr1,*),avg
c$$$       call strtrim(tmpstr1,tmpstr2)
c$$$       write(*,'(a)') tmpstr2(1:nblen(tmpstr2))
       stop
       END
c$$$c+
c$$$c     delete two wides blanks
c$$$c-
c$$$      subroutine strtrim(strin,strout)
c$$$      character*(*) strin,strout
c$$$      character*512 strtmp
c$$$      integer i,j, nblen
c$$$
c$$$c     delete right blanks 
c$$$c      strtmp=strin(1:nblen(strin))
c$$$c     delete left blanks
c$$$c      j=1
c$$$      do i=1,len(strin)
c$$$         if (strin(i:i).ne.' ') then
c$$$            goto 61
c$$$         endif
c$$$      enddo      
c$$$ 61   strout=strin(i:nblen(strin))
c$$$c      write(*,*) i, strin(1:nblen(strin))
c$$$      return
c$$$      end
