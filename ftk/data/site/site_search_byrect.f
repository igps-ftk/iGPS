      subroutine site_search_byrect(file,lat,lon,sites,ns)
C     Input:
c     file: site coordinates filename
c     lat: latitudes
c     lon: longitudes
C     Output:
c     sites: found sites
c     ns: #sites
C     ---
      IMPLICIT NONE
      character*(*) file, sites(*)
      real*8 lat(2),lon(2)
      integer*4 ns
c     ---
      integer*4 fid,ioerr,ntmp
      character*512 buf,tmpstr(7)
      character*4 cursite
      real*8 xyz(3),ll(2)
C     ---
      ns=0
      fid=11
      open(unit=fid,file=file)
 800  read(fid,'(a512)',end=899) buf
c      write(*,*) buf(1:nblen(buf))
      read(buf,'(a)') cursite
c      write(*,*) cursite

      call strsplit(buf,' ',ntmp,tmpstr)
c     write(*,*) (tmpstr(i),i=1,6)
      read(tmpstr(2),*) ll(1)
c     write(*,*) ll(1)
      read(tmpstr(3),*) ll(2)
      read(tmpstr(4),*) xyz(1)
      read(tmpstr(5),*) xyz(2)
      read(tmpstr(6),*) xyz(3)
c         write(*,'(a20,i6)') tmpstr(6),ntmp
      if (ll(1).ge.lon(1).and.ll(1).le.lon(2).and.
     &     ll(2).ge.lat(1).and.ll(2).le.lat(2)) then
         ns=ns+1
         sites(ns)=cursite
c         write(*,'(a10,a4)') 'In range:',cursite
      endif
c      write(*,'(6f11.5)') ll,lon,lat
      goto 800
 899  continue
      close(fid)
c      write(*,*) '#total found:',ns
      return
      end
