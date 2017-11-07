CTITLE
      SUBROUTINE read_txt(file,lines,nline)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      character*(*) file
      character*(*) lines(*)
      integer*4 nline

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 nblen

c     --Local Parameters--
      integer*4 fid
      character*10230 line

c     <<VAR_DEC

      call getlun(fid)
      open(unit=fid,file=file,status='old')
      nline=0
 801  read(fid,'(a10230)',end=999) line
c      write(*,'(a)') line(1:nblen(line))
      nline=nline+1
      lines(nline)=line
      goto 801
 999  continue
      close(fid)

      RETURN
      END
