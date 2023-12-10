CTITLE
       subroutine query_gsac_dhf(file,site,url)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      character*(*) file,site

c     --OUTPUT--
      character*(*) url

c     --EXTERNAL--

c     --Local Parameters--
      integer*4 fid,ioerr,pos
      character*1023 tmpstr,line

      integer*4 nblen

c     <<VAR_DEC

      url=''

      call getlun(fid)
      open(unit=fid,file=file,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,'(a)') ' [QUERY_GSAC_DHF]FATAL: error when open file',
     &        file(1:nblen(file)),'.'
         goto 899
      endif
      
 800  read(fid,'(a1023)',end=899) line
      pos=index(line,site)
      if (pos.gt.0) then
         url=line
         goto 899
      endif
      goto 800
      
 899  close(fid)

      RETURN
      END
