
      subroutine read_sio_hdr(file, nhead, headers, nmax, nrow, ncol)

C     Inputs:
c     file: character*256
c     firstepoch: int(2)
c     lastepoch: int(2)
c     xyzref: double(3)
c     neuref: double(3)
c     headers: character*256 n
c     data: double (M*N)


      character*(*) file
      character*100 buf,tmp

      integer*4 fid, ioerr
      integer*4 nhead, nrow, ncol, nmax
      integer nblen
   
      character*(*) headers(nmax)
C     ---
      nhead=0
      nrow=0
      ncol=0

      fid=90
c      write(*,*) 'header file:', file(1:nblen(file))
      open(unit=fid,file=file)

 30   read(fid,'(a80)', end=90) buf
      if (buf(1:1).ne.'#') then
         goto 90
      endif
      if (index(buf,'ROW').gt.0) then
c         write(*,'("|",a,"-")') buf
         buf=buf(index(buf,':')+1:nblen(buf))
c         call strtrim(buf, tmp)
c         write(*,'(a)') buf
         read(buf,'(i10)')  nrow
c         write(*,*) buf(1:nblen(buf)),'-', nrow
c         goto 30
      endif
      if (index(buf,'COL').gt.0) then
         buf=buf(index(buf,':')+1:nblen(buf))
c         write(*,*) buf, ncol
         read(buf,'(i10)')  ncol
c         write(*,*) buf, ncol
c         goto 30
      endif
      nhead=nhead+1
      headers(nhead)=buf
c      write(*,'(a)') buf(1:nblen(buf))
      goto 30   
      
 90   continue

      if (nrow.eq.0.or.ncol.eq.0) then
         write(*,*) 'error reading header for', file(1:nblen(file))
         close(fid)
         stop
      endif
      close(fid)
      return
      end


