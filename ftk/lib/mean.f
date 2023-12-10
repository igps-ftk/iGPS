CTITLE
      program  mean
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--
c     for file a.txt with three lines
c     1
c     2
c     3
c
c     cat a.txt | mean
c     or,
c     mean < a
c
c     However, "echo 1 2 3 | mean" does not work!
c     

c     --MODIFICATIONS--

      IMPLICIT NONE
      INCLUDE '../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--

c     --OUTPUT--

c     --Local Parameters--
c      integer*4 NMAX
c      Parameter (nmax=1000000000)
c      Parameter (nmax=10)
      integer*4 npt,i
      integer*4 ioerr
      real*8 tmp,data(NMAX),avg,r_zero
      character*1023 tmpstr1,tmpstr2
      integer*4 nblen,iargc


c     <<VAR_DEC

c      write(*,*) 'iargc():',iargc()
      r_zero=0d0

      npt=0
 601  read(5,*,iostat=ioerr) tmp
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
      data(npt)=tmp
c     loop next
      go to 601

c     done with read inputs
 901  continue
c      write(*,*) '#total inputs:',npt
c      write(*,*) data(1:10)
      tmp=0
      npt=0
      do i=1,NMAX
         if (data(i).eq.0) then
             go to 602
          endif
          npt=npt+1
          tmp=tmp+data(i)
       enddo
 602   continue
       avg=tmp/npt

       write(tmpstr1,*),avg
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
