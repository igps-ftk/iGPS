CTITLE
      PROGRAM lhl

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--
c     List H-files List

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1023 file

c     --OUTPUT--
      character*1023 ofile

c     --EXTERNAL--
      integer*4 iargc,nblen

c     --Local Parameters--
      integer*4 fid,fido,ioerr
      integer*4 NMAX
      parameter(NMAX=10000)
      character*1023 lines(NMAX),line,strs,tmp
      integer*4 i,n,yr,doyr,year,pyr,pdoyr,pyear,tyr,tdoy,tsec,tyear
      real*8 jd,pjd,tmpd

c     <<VAR_DEC
      fid=5
      if (iargc().ge.1) then
         call getarg(1,file)
         call getlun(fid)
         open(unit=fid,file=file,status='old',iostat=ioerr)
         if (ioerr.ne.0) then
            write(*,'(3a)') '[lfl]ERROR: cannot open input file [',
     +           file(1:nblen(file)),']!'
            goto 999
         endif
      endif

      fido=6
      if (iargc().ge.2) then
         call getarg(2,ofile)
         call getlun(fido)
         open(unit=fido,file=ofile,iostat=ioerr)
         if (ioerr.ne.0) then
            write(*,'(3a)') '[lfl]ERROR: cannot open output file [',
     +           ofile(1:nblen(ofile)),']!'
            goto 998
         endif
      endif
c      write(*,*) 'output to ',fid,fido

      pjd=0
 700  read(fid,'(a1023)',END=998) line
      if(nblen(line).lt.1) goto 700
c      write(*,*) line(1:nblen(line))
c      call strrep(line,'/',' ',tmp)
c      write(*,*) 'tmp:',tmp(1:nblen(tmp))
      call getfilename(line,tmp)
      read(tmp(8:9),*) yr
      read(tmp(10:13),*) doyr
      if (yr.gt.50.and.yr.lt.100) then
         year=yr+1900
      elseif(yr.le.50) then
         year=yr+2000
      endif
      call yds_to_jd(year,doyr,0d0,jd)
c      write(*,*) tmp(1:nblen(tmp)),yr,year,doyr,jd

      if (pjd.ne.0) then
         if ((jd-pjd).gt.1.1) then
            do i=1,dnint(jd-pjd-1)
               call jd_to_yds(pjd+i,tyr,tdoy,tsec)
               if (tyr.gt.50.and.tyr.lt.100) then
                  tyear=tyr+1900
               elseif(tyr.le.50) then
                  tyear=tyr+2000
               endif
               write(fido,'(i4.4,1x,i03.3)') tyear,tdoy
            enddo
         endif
      endif
      write(fido,800) year,doyr,line(1:nblen(line))
 800  format(i4.4,1x,i3.3,1x,a)
      pjd=jd

      goto 700


      if (fid.ne.6) then
         close(fido)
      endif

 998  continue
      if (fid.ne.5) then
         close(fid)
      endif

 999  continue

      STOP
      END
