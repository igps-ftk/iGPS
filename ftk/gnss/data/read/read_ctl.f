CTITLE
      SUBROUTINE read_ctl(file,xmin,xstep,ymin,ystep)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      character*(*) file

c     --OUTPUT--
      real*8 xmin,xstep,ymin,ystep

c     --EXTERNAL--
      integer*4 nblen

c     --Local Parameters--
      integer*4 i,j,fid,ioerr,pos
      character*1023 tmps,tmps1,tmps2

c     <<VAR_DEC

      call getlun(fid)
      open(unit=fid,file=file,status='old',iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,'(a)') 'Error open file '//file(1:nblen(file))
         stop
      endif

 800  read(fid,'(a1023)',end=899) tmps
      pos=index(tmps,'ydef')
      if (pos.gt.0) then
c$$$         write(*,*) tmps(1:nblen(tmps))
         read(tmps,*) tmps1,tmps1,tmps1,ymin,ystep
      endif
      pos=index(tmps,'xdef')
      if (pos.gt.0) then
c$$$         write(*,*) tmps(1:nblen(tmps))
         read(tmps,*) tmps1,tmps1,tmps1,xmin,xstep
      endif
      goto 800

 899  close(fid)
      RETURN
      END
