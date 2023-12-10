c      program file_info
      subroutine query_pos(file,nrow,ncol,nhead,
     &     firstepoch,lastepoch,
     &     xyzref,neuref)

c      call query_pos(file,nrow,ncol,nhead,firstepoch,lastepoch,
c     &     xyzref,neuref)

c     ---
      implicit none
      integer*4 nrow,ncol,nhead
c     nhead: first-non-blank lines are headers.

      character*(*) file
      character*(*) firstepoch(2),lastepoch(2)
      real*8 xyzref(3),neuref(3)
c     ---
      integer*4 ioerr, fid
      character*1000 buf,tmpstr
      character*1000 tmpstrs(1000)
      character*4 site
      logical debug
c     ---
c      file='/home/tianyf/data/sio/bjfsCleanUnf.neu'
      nrow=0
      ncol=0
      nhead=0

      fid=90
      open(unit=fid,file=file)
 800  read(fid, '(a)', iostat=ioerr, end=899) buf
      nhead=nhead+1

      if (index(buf,'4-character ID').ge.1) then
         tmpstr=buf(index(buf,':')+1+1:)
c         call trim_lead(tmpstr,ioerr)
         read(tmpstr,'(a)') site
c         write(*,'(3a)') '|',site,'|'
      endif

      if (index(buf,'First Epoch').ge.1) then
         read(buf(index(buf,':')+1:),'(2a)') firstepoch
      endif

      if (index(buf,'Last Epoch').ge.1) then
         read(buf(index(buf,':')+1:),'(2a)') lastepoch
      endif

      if (index(buf,'XYZ Reference position').ge.1) then
         read(buf(index(buf,':')+1:),*) xyzref
      endif

      if (index(buf,'NEU Reference position').ge.1) then
         read(buf(index(buf,':')+1:),*) neuref
      endif

      if (buf(1:1).eq.' ') then 
         nhead=nhead-1
         goto 801
      endif
c      write(*,*) nrow, index(buf,'4-character ID')

      goto 800

c     reading data section to determine the number of rows & cols
 801  continue
      nrow=1
 802  read(fid,'(a)', iostat=ioerr, end=899) buf
      nrow=nrow+1
      goto 802

 899  continue
      close(fid)
      call strsplit(buf,' ',ncol,tmpstrs)
      debug=.false.
      if (debug) then
         write(*,*) 'site:',site
         write(*,*) 'first:',firstepoch
         write(*,*) 'last:',lastepoch
         write(*,*) 'xyz:',xyzref
         write(*,*) 'neu:',neuref
         write(*,*) 'nrow:',nrow
         write(*,*) 'ncol:',ncol
      endif
      return
      end
