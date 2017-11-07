      subroutine site_coords_query(file,site,xyz,ll)
C     ---
      character*(*) file, site
      real*8 xyz(3),ll(2)
      character*512 buf,tmpstr(7)
      character*4 cursite
      integer*4 fid,ioerr,ntmp
C     ---
      fid=11
      open(unit=fid,file=file)
 800  read(fid,'(a512)',end=899) buf
c      write(*,*) buf(1:nblen(buf))
      read(buf,'(a)') cursite
c      write(*,*) cursite
      if (cursite.eq.site(1:4)) then
         call strsplit(buf,' ',ntmp,tmpstr)
c         write(*,*) (tmpstr(i),i=1,6)
         read(tmpstr(2),*) ll(1)
c         write(*,*) ll(1)
         read(tmpstr(3),*) ll(2)
         read(tmpstr(4),*) xyz(1)
         read(tmpstr(5),*) xyz(2)
         read(tmpstr(6),*) xyz(3)
c         write(*,'(a20,i6)') tmpstr(6),ntmp
         goto 899
      endif
      goto 800
 899  continue
      close(fid)
      return
      end
