c      program file_info
      subroutine file_info_usgs(file,nrow,ncol,nhead,headers)
      
      implicit none
      include '../../../inc/ftk.h'

      integer*4 nrow, ncol, nhead
      character*(*) file, headers(nhead)
     
      integer*4 ioerr, fid
      character*1000 buf,tmpstr
      character*1000 tmpstrs(1000)
      integer*4 i

      integer nblen

c      logical debug

c      debug=.false.

c      file='/home/tianyf/data/sio/bjfsCleanUnf.neu'
      nrow=0
      ncol=0
      nhead=0

      fid=90
      call getlun(fid)
      open(unit=fid, file=file,iostat=ioerr,status='old')
      if (ioerr.ne.0) then
         write(*,*) 'error open file:',file(1:nblen(file))
         goto 899
      endif
 30   read(fid, '(a)', iostat=ioerr, end=90) buf
      if (ioerr.ne.0) then
         write(*,*) 'error read data:',file(1:nblen(file))
         goto 90
      endif
      if (buf(1:1).eq.'#') then
         nhead=nhead+1
         headers(nhead)=buf
      endif
      nrow = nrow+1
      goto 30
 90   continue
      close(fid)

      nrow=nrow-nhead
c      write(*,*) buf
      call strsplit(buf, ',', ncol, tmpstrs)

c     outpu debug information
      if (debug) then
         write(*,*) 'number of headers:', nhead
         do i=1,nhead
            tmpstr=headers(i)
            write(*,*) i,tmpstr(1:nblen(tmpstr))
         enddo
         write(*,*) 'first line of data:',buf(1:nblen(buf))
         write(*,*) '#row:', nrow
         write(*,*) '#col:', ncol
        
         write(*,*) 'end-of-file_info_usgs'
      endif
c      write(*,*) 'debug:',debug
 899  return
      end
