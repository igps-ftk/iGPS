CTITLE chk_rnx_local
      PROGRAM chk_rnx_local

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--
c     To check the file whether was downloaded completely from IGS data center

c     --ALGORITHM--
c     #   1.The uncompress cannot detect incomplete downloads; it reports no error.
c     #   2.The sh_crx2rnx/crx2rnx give error information like:
c     #
c     #       ERROR : The file seems to be trancated in the middle.
c     #               The conversion is interrupted after reading the line 27605 :
c     #             start>                3<end
c     #
c     #    Thus, by checking if the crx2rnx process return ERROR, 
c     #    we can determine that the file is complete or not.
c     #    Steps:
c     #      + Download one file from FTP server
c     #      + Check the validation of the file
c     #      + Output results to a local file.
c     # Output:
c     #   Format of LOG file:
c     #     chk-yymmdd-HHMMSS.log
c     #     STATUS:SITE:YEAR:DOY:TYPE

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      integer yr,doy,ndays

c     --OUTPUT--

c     --Local Parameters--
      character*512 dir,dir_cur,tmpstr,cmdstr
      integer*4 ndaysofyr
      integer*4 i,j

      character*512 files(1000),file,ptn,fileun,filez
      integer*4 nf

      logical exi
      
      integer iargc
      integer*4 ndoyr,nblen
      integer*4 status,system



c     <<VAR_DEC

      if (iargc().lt.3) then
         write(*,*) 'Usage: chk_rnx_local YEAR DOY NDAYS [ROOT_DIR]'
         write(*,*) '  ROOT_DIR is the parent '//
     .        'directory of rinex/nav/met/...'
         stop
      endif
      
      call getarg(1,tmpstr)
      read(tmpstr,*) yr
      call getarg(2,tmpstr)
      read(tmpstr,*) doy
      call getarg(3,tmpstr)
      read(tmpstr,*) ndays

      ndaysofyr=ndoyr(yr)

      dir='/cygdrive/h/gps/igs/pub/rinex'
      if (yr.ge.2002) then
         dir='/cygdrive/i/data.server/pub/rinex'
      endif

c     for debug in solaris
      dir='/export/home/tianyf/tmp'

      ptn='*.Z'

      do i=1,ndays
c         write(*,700) dir(1:nblen(dir)),yr,doy
         write(dir_cur,700) dir(1:nblen(dir)),yr,doy
 700     format(A,"/",I4,"/",I3.3)
c         write(*,*) yr,doy,i
c     How to get a listing a current files?
         inquire (file=dir_cur, exist=exi)
c         write(*,*) 'EX:',exi
         if (.NOT.exi) goto 800


         write(*,*) ' working in ',dir_cur(1:nblen(dir_cur))

c         status=system('ls')
         call ffind(dir_cur,files,ptn,nf,1)
         write(*,*)'NF:',nf
         if (nf.le.0) goto 800
         do j=1,nf
            file=files(j)
            write(*,*) file(1:nblen(file))
            cmdstr='ln -sf '//file(1:nblen(file))//' .'
c            write(*,*) cmdstr(1:nblen(cmdstr))
            status=system(cmdstr)
            if (status.ne.0) stop
            call getfilename(file,filez)
            cmdstr='\\uncompress -f '//filez
            status=system(cmdstr)
            if (status.ne.0) stop
c            
            call desuffix(filez,fileun)
            cmdstr='crx2rnx '//fileun
            status=system(cmdstr)
            if (status.ne.0) then
               write(*,*) 'BAD', fileun(1:nblen(fileun))
c               stop
            endif
            cmdstr='\\rm -f '//fileun
c            write(*,*) cmdstr(1:nblen(cmdstr))
            status=system(cmdstr)
            if (status.ne.0) stop
         enddo

 800     doy=doy+1         
         if (doy.gt.ndaysofyr) then
            doy=1
            yr=yr+1
            ndaysofyr=ndoyr(yr)
            if (yr.ge.2002) then
               dir='/cygdrive/i/data.server/pub/rinex'
            endif
         endif
      enddo

      STOP
      END
