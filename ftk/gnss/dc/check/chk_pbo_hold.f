CTITLE
      PROGRAM chk_pbo_hold

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1024 file,rpath,ofile

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 iargc,nblen

c     --Local Parameters--
      integer*4 fid,fido,ioerr
      character*1024 buf,line,rfile
      character*14 rdzfile
      character*4 site
      integer*4 year,yr
      character*3 doy
      logical exi


c     <<VAR_DEC

      if (iargc().lt.3) then
         write(*,'(a)') 'Usage: chk_pbo_hold file rnx_path ofile'
         stop
      endif

      call getarg(1,file)
      call getarg(2,rpath)
      call getarg(3,ofile)

      call getlun(fid)
      open(unit=fid,file=file,status='old',iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,'(3a)') '[lfl]ERROR: cannot open input file [',
     +           file(1:nblen(file)),']!'
         stop
      endif

      call getlun(fido)
      open(unit=fido,file=ofile,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,'(3a)') '[lfl]ERROR: cannot open output file [',
     +           ofile(1:nblen(ofile)),']!'
         close(fid)
         stop
      endif

 700  read(fid,'(a1024)',end=999) line
      call strtrim(line,buf)
      if (nblen(line).eq.0.or.buf(1:1).eq.'#') then
c         write(*,*) 'comments:',buf
         write(fido,'(a)') buf(1:nblen(buf))
         goto 700
      endif
      rdzfile=buf((nblen(buf))-13:nblen(buf))
c      write(*,*) rdzfile
      read(rdzfile,801) site,doy,yr
      if (yr.gt.50) then
         year=yr+1900
      else
         year=yr+2000
      endif
c      write(*,*) site,doy,yr,year
 801  format(a4,a3,2x,i2)
      write(rfile,802) rpath(1:nblen(rpath)),year,doy,
     +     rdzfile
 802  format(a,"/",i4,"/",a3,"/",a)
c      write(*,*) rfile(1:nblen(rfile))
      inquire(file=rfile,exist=exi)
c      write(*,*) 'exist:',exi
      if (exi) then
         goto 700
      endif
      write(fido,'(a)') buf(1:nblen(buf))

c      goto 999
      goto 700

 999  continue
      close(fid)
      close(fido)

      STOP
      END
