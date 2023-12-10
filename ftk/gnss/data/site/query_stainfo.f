CTITLE
       SUBROUTINE query_stainfo(stafile,site_in,obstime)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--
c     CREATE MAY-07-2008 Tian
c       +Return the first & last dates of observations for one site.
c         Called by get_rnx program.

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      character*(*) stafile,site_in
      integer*4 obstime(10)

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 nblen

c     --Local Parameters--
      integer*4 fid,ioerr,pos
      character*1024 tmpstr,site*4

c     <<VAR_DEC

      obstime(1)=-1
      site=site_in
c     If change the input parameter, the value in the caller will also change.
c     Thus, we assign the SITE variable to a local variable.
c     Bye the way, the caller use an element of an array.
      call uppers(site)
c      write(*,*) 'searching ',site(1:nblen(site))

      call getlun(fid)
      open(file=stafile,unit=fid,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,*) 'Error in opening file '//stafile(1:nblen(stafile))
         stop
      endif
 800  read(fid,'(a)',end=801) tmpstr
c      write(*,*) tmpstr(1:nblen(tmpstr))
      pos=index(tmpstr,site)
      if (pos.gt.0) then
         if (obstime(1).eq.-1) then
            read(tmpstr(26:29),*) obstime(1)
            read(tmpstr(31:33),*) obstime(2)
            read(tmpstr(45:48),*) obstime(6)
            read(tmpstr(50:52),*) obstime(7)
         else
            read(tmpstr(45:48),*) obstime(6)
            read(tmpstr(50:52),*) obstime(7)
         endif
      endif
         
      goto 800
 801  continue
      close(fid)

c      if (obstime(1).eq.-1) then
c         write(*,*) 'not found:',site(1:nblen(site))
c      else
c         write(*,*) 'found:',site(1:nblen(site)),obstime
c      endif

      RETURN
      END
