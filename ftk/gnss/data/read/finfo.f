c      program file_info
      subroutine finfo(file,nrow,ncol,nhead,headers,cmt)
c                finfo(file,nrow,ncol,nhead,headers,cmt)
      integer*4 nrow,ncol,nhead
      character*(*) file,headers(nhead)
      character*(*) cmt
     
      integer*4 ioerr,fid,i
      character*1000 buf
      character*1000 tmpstrs(1000)
      integer*4 nblen

c      file='/home/tianyf/data/sio/bjfsCleanUnf.neu'
c      write(*,*) file
      nrow=0
      ncol=0
      nhead=0

      call getlun(fid)
      open(unit=fid,file=file,status='old',iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,*) ' [finfo]FATALE: cannot open file [',
     &        file(1:nblen(file)),'].'
         stop
      endif
      
 30   read(fid,'(a)',iostat=ioerr,end=90) buf
      if ((cmt.eq.'~'.and.buf(1:1).ne.' ').or.buf(1:1).eq.cmt) then
         nhead=nhead+1
         headers(nhead)=buf
      endif
      nrow = nrow+1
      goto 30
 90   continue
      close(fid)
      call strsplit(buf, ' ', ncol, tmpstrs)
c      write(*,*) 'number of headers:', nhead
c      write(*,*) (headers(i),i=1,nhead)
c      write(*,*) '#row:', nrow
c      write(*,*) '#col:', ncol
c      write(*,*) buf
      return
      end
