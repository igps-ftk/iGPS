      PROGRAM newm
c     --PURPOSE--

c     --INPUT--
c     ofile: output filename

c     --OUTPUT--

c     --OUTPUT--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --Global Variables--
      IMPLICIT NONE

c     --Command-line Parameters--

c     --Local Parameters--
      character*1000 file,ofile
      integer*4 fid,fido,ioerr
      integer iargc,nblen,ind
      character*512 strbuf,strtmp,home,progname,prognameU,timestr
      character*10 ymds(3)
      integer*4 i,j,k
      integer*4 year,mm,dd,ymd(3)

c     <<VAR_DEC

c     check command-line parameters

c     check help
c      write(*,*) '#param:',iargc()
      if (iargc().gt.2) goto 800
      if (iargc().lt.1) goto 801
      do i=1,iargc()
         call getarg(i,strbuf)
         if (strbuf(1:2).eq.'-h'.or.strbuf(1:6).eq.'--help') goto 800
      enddo
      goto 801
 800  write(*,*) 'Syntax: newm [filename] [--file=~/.../doc/prog.f]'
      write(*,*) '        newm [--file=~/.../doc/prog.f] [filename]'
      stop

 801  continue
c     in Solaris x86 10, the ~ (home directory) substitute does not work.
c     f90/Sun Studio11

      call getenv('HOME',home)
      file=home(1:nblen(home))//'/iGPS/ftk/doc/manmod.1'
      fido=6
      ofile=''
      do i=1,iargc()
         call getarg(i,strbuf)
         if (index(strbuf,'--file=').gt.0) then
            strtmp=strbuf(index(strbuf,'--file=')+7:)
            read(strtmp,*) file
         else
            read(strbuf,*) ofile
         endif
      enddo
      
c      write(*,*) file(1:nblen(file))
c      write(*,*) ofile(1:nblen(ofile))

c     open files
      call getlun(fid)
      open(unit=fid,file=file,iostat=ioerr,status='old')
      if (ioerr.ne.0) then
         write(*,*) 'NEWM: Error in opening ',
     &        file(1:nblen(file))
         stop
      endif

      if (nblen(ofile).gt.0) then
         call getlun(fido)
         open(unit=fido,file=ofile,iostat=ioerr)
        
         if (ioerr.ne.0) then
            write(*,*) 'NEWM: Error in opening output: ',
     &           ofile(1:nblen(ofile))
            close(fid)
            stop
         endif
      endif

      progname='YOUR_PROG_NAME'
      progname='your_prog_name'
      if (fido.ne.6) then
         call getfilename(ofile,strtmp)
         call desuffix(strtmp,progname)
      endif

c      call idate(mm,dd,year)
c      write(*,*) year,mm,dd
      timestr='MON-DD-YEAR'
c      write(timestr,701),'MON',dd,year
 701  format(I2.2,'/',I2.2,'/',I4.4)
      call idate(ymd)
      write(timestr,701),ymd(2),ymd(1),ymd(3)
c      call date_and_time(ymds)
c      write(*,*) ymds

 802  read(fid,'(a512)',end=899) strbuf
c      write(*,'(a)') strbuf(1:nblen(strbuf))

      ind=index(strbuf,'progname')
      if (ind.ge.1) then
         call strrep(strbuf,'progname',progname,strtmp)
c         write(*,*) 'here',progname
         call blank(strbuf)
         strbuf=strtmp
      endif
      ind=index(strbuf,'PROGNAME')
      if (ind.ge.1) then
c         write(*,*) '1'
         prognameU=progname
         call casefold(prognameU)
c         write(*,*) progname,prognameU
         call strrep(strbuf,'PROGNAME',prognameU,strtmp)
c         write(*,*) '2',strtmp
         call blank(strbuf)
         strbuf=strtmp
      endif
      ind=index(strbuf,'TIMESTR')
      if (ind.ge.1) then
         call strrep(strbuf,'TIMESTR',timestr,strtmp)
         call blank(strbuf)
         strbuf=strtmp
      endif
c     output to file
      write(fido,'(a)') strbuf(1:nblen(strbuf))
c      write(*,*) strbuf(1:nblen(strbuf))
      goto 802

 899  continue
      
c     close files
      close(fid)
      if (fido.ne.6) then
         close(fido)
      endif

c     That's all.
      END
