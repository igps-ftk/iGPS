CTITLE READ_MET
      SUBROUTINE read_met(file,data,nrow,ncol)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
      INCLUDE '../../../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      character*(*) file

c     --OUTPUT--
      real*8 data(nmax_row,nmax_col)
      integer*4 nrow,ncol

c     --Local Parameters--
      integer*4 fid,ioerr,i,j
      character*1024 strbuf,strtmps(100)
      integer*4 nblen

c     <<VAR_DEC

      call getlun(fid)
      open(unit=fid,file=file,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,*) 'Error: cannot open file ',file(1:nblen(file))
         stop
      endif

      nrow=0

 800  read(fid,'(a1024)', END=899) strbuf
c      write(*,*) strbuf(1:nblen(strbuf))
      if (strbuf(1:1).eq.'*') goto 800
      call strsplit(strbuf,' ',ncol,strtmps)
      nrow=nrow+1
      read(strbuf,'(f20.8)') (data(nrow,j),j=1,ncol)
      goto 800

 899  continue
      close(fid)

      RETURN
      END
