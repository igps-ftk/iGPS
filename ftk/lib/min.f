CTITLE
      program  min
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--
c     for file a.txt with three lines
c     1
c     2
c     3
c
c     cat a.txt | min
c     or,
c     min < a
c
c     However, "echo 1 2 3 | max" does not work!
c     

c     --MODIFICATIONS--

      IMPLICIT NONE
      INCLUDE './../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--

c     --OUTPUT--

c     --Local Parameters--
c      integer*4 NMAX
c      Parameter (nmax=10000000)
c      Parameter (nmax=10)
      integer*4 npt,i
      integer*4 ioerr
      real*8 tmp,data(NMAX),r_max,r_zero
      character*1023 tmpstr1,tmpstr2
      integer*4 nblen,iargc


c     <<VAR_DEC

c      write(*,*) 'iargc():',iargc()
      r_zero=0d0
      r_max=0/r_zero

      npt=0
 601  read(5,*,iostat=ioerr) tmp
c      write(*,*) 'input is',tmp
      if (ioerr.gt.0) then
         print *, 0/r_zero
         stop
c         print*,'sth error'
c         go to 901
      endif
      if (r_max.eq.(1/r_zero)) then
c         print*, "h1"
         r_max=tmp
      else
c         print*,'not nan',2
         if (tmp.lt.r_max) then
            r_max=tmp
         endif
      endif
      if (ioerr.lt.0) then
c         print *, 'end-of-input'
         goto 901
      endif
c      print*,r_max
c     loop next
      go to 601

c     done with read inputs
 901  continue
c      write(*,*) '#total inputs:',npt
c$$$c      write(*,*) data(1:10)

       write(tmpstr1,*),r_max
       call strtrim(tmpstr1,tmpstr2)
       write(*,'(a)') tmpstr2(1:nblen(tmpstr2))
       stop
       END
c+
c     delete two wides blanks
c-
      subroutine strtrim(strin,strout)
      character*(*) strin,strout
      character*512 strtmp
      integer i,j, nblen

c     delete right blanks 
c      strtmp=strin(1:nblen(strin))
c     delete left blanks
c      j=1
      do i=1,len(strin)
         if (strin(i:i).ne.' ') then
            goto 61
         endif
      enddo      
 61   strout=strin(i:nblen(strin))
c      write(*,*) i, strin(1:nblen(strin))
      return
      end
