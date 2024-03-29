CTITLE SIO2CATS
      PROGRAM run_cats

      IMPLICIT NONE
      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--


c     --EXAMPLE--

c     --MODIFICATIONS--
c     APR-19-2008 Tian:
c       Created.

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1024 path,opath
      character*100 cats_version

c     --OUTPUT--
c     NONE

c     --Local Parameters--
      character*20 filter,dt
      character*1023 files(nmax_site),file,ofile,headers(nmax_head)
      character*1023 tmpstr,cmdstr,ostr,sys,redirstr,tmpdir,tfile
      character*1023 outext,outext_in

c     offset and post-seismic decay definition file name
      character*1023 opfile
      
      integer*4 noff,nps,offi,psi,tdate(5),tyr,tdoy,tsecod
c     offset & psdecay read from definition file.
      real*8 offs(100),pss(100)
c     offset & psdecay in time axis range
      integer*4 noffU,npsU
      real*8 offsU(100),pssU(100)
c     That is, maximum number of offsets: 100.

      character*4 site,tmplock
      character*1 neustr(3),neu1
      data neustr/'n','e','u'/
      integer*4 fido,ioerr,fidt
c     number of sites found
      integer*4 n
c     array index
      integer*4 i,j,fi,neui
      integer*4 isWn,isTrend,isfn,isrwn,isfpln,npln,isgm,isbp
      integer*4 isAnn,isSemi
c     data matrix
      real*8 data(nmax_row,nmax_col)
      integer*4 nrow,ncol,nhead
c     for date conversion
      integer*4 idate(5),ymd(3),doy,ys,ye,ds,de
      real*8 jd,secr8,yr
c     starting & ending time
      real*8 dyrs,dyre,fdoy,dyr
     

      integer*4 isOver
      logical exi

      integer*4 pid,getpid
      
c     external functions
      integer*4 iargc,nblen,status,system,hostnm
      character*128 hostname
      integer*4 ndoyr

c     <<VAR_DEC

       if (iargc().lt.2) then
         write(*,*) 'Syntax: run_cats path opath'
         write(*,*) '          [--trend=y|n]'
         write(*,*) '          [--ann=y|n]'
         write(*,*) '          [--semi=y|n]'
         write(*,*) '          [--fn=y|n]'
         write(*,*) '          [--wn=y|n]'
         write(*,*) '          [--rwn=n|y]'
         write(*,*) '          [--fpln=n|y]'
         write(*,*) '          [--gm=n|y]'
         write(*,*) '          [--overwrite=n|y]'
c         write(*,*) '          [--offps=offset_ps_file(.def)]'
         write(*,*) '          [--ptn=pattern(.cats)]'
         write(*,*) '          [--cats_version=CATS_VERSION]'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
      pid=getpid()
      status=hostnm(hostname)
c$$$c     create tmporary output directory ${opath}/.tmp_
c$$$      cmdstr='mkdir -p '//opath(1:nblen(opath))//pathsep//'.tmp_'
c$$$      cmdstr=cmdstr(1:nblen(cmdstr))//hostname(1:nblen(hostname))
c$$$      write(tmpdir,700) opath(1:nblen(opath)),pathsep,
c$$$     &     hostname(1:nblen(hostname)),pid
c$$$ 700  format(a,a,'.tmp_',a,'_',i20.20,'/')
c$$$      cmdstr='mkdir -p '//tmpdir(1:nblen(tmpdir))
c$$$      write(*,'(a)') '[run_cats] executing '//cmdstr(1:nblen(cmdstr))
c      status=system(cmdstr)
c      stop
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      filter='*.cats'
c     get extra command-line-parameters
c     Default initials
      isWn=1
      isfn=1
      isrwn=0
      isfpln=0
      isOver=0
      isTrend=1
      isAnn=1
      isSemi=1
      opfile=''
      cats_version='cats_v312_icc_m32'
      outext_in='_mle'

c     parse command-line input options
      if (iargc().ge.3) then
         do i=3,iargc()
            call getarg(i,tmpstr)
c            write(*,*) i,tmpstr(1:nblen(tmpstr))
            if (tmpstr(1:8).eq.'--trend=') then
               if (tmpstr(9:9).eq.'y') then
                  isTrend=1
               else
                  isTrend=0
               endif
            else if (tmpstr(1:6).eq.'--ann=') then
               if (tmpstr(7:7).eq.'y') then
                  isAnn=1
               else
                  isAnn=0
               endif
            else if (tmpstr(1:7).eq.'--semi=') then
               if (tmpstr(8:8).eq.'y') then
                  isSemi=1
               else
                  isSemi=0
               endif
            else if (tmpstr(1:5).eq.'--wn=') then
               if (tmpstr(6:6).eq.'y') then
                  iswn=1
               elseif (tmpstr(6:6).eq.'n') then
                  iswn=0
               else
                  write(*,'(4a)') '[run_cats]ERROR: wrong value for ',
     +                 '--wn option {',tmpstr(6:nblen(tmpstr)),'}.'
                  stop
               endif
            else if (tmpstr(1:5).eq.'--fn=') then
               if (tmpstr(6:6).eq.'y') then
                  isfn=1
               else
                  isfn=0
               endif
            else if (tmpstr(1:6).eq.'--rwn=') then
               if (tmpstr(7:7).eq.'y') then
                  isrwn=1
               else
                  isrwn=0
               endif
            else if (tmpstr(1:7).eq.'--fpln=') then
               if (tmpstr(8:8).eq.'y') then
                  isfpln=1
               else
                  isfpln=0
               endif
            else if (tmpstr(1:5).eq.'--gm=') then
               if (tmpstr(6:6).eq.'y') then
                  isgm=1
               else
                  isgm=0
               endif
            else if (tmpstr(1:5).eq.'--bp=') then
               if (tmpstr(6:6).eq.'y') then
                  isbp=1
               else
                  isbp=0
               endif
            else if (tmpstr(1:12).eq.'--overwrite=') then
               if (tmpstr(13:13).eq.'y') then
                  isOver=1
               else
                  isOver=0
               endif
c$$$            else if (tmpstr(1:8).eq.'--offps=') then
c$$$               if (nblen(tmpstr).le.8) then
c$$$                  write(*,'(a)') 'Not valid offset definition file.'
c$$$                  stop
c$$$               endif
c$$$               opfile=tmpstr(9:nblen(tmpstr))
            else if (tmpstr(1:15).eq.'--cats_version=') then
               if (nblen(tmpstr).le.15) then
                  write(*,'(a)') 'Not valid CATS version option.'
                  stop
               endif
               cats_version=tmpstr(16:nblen(tmpstr))
            else if (tmpstr(1:6).eq.'--ptn=') then
               if (nblen(tmpstr).le.6) then
                  write(*,'(a)') 'Not valid file filter.'
                  stop
               endif
               filter=tmpstr(7:nblen(tmpstr))
            else
               write(*,*) 'Error: invalid command line parameters!',
     &              tmpstr(1:nblen(tmpstr))
               stop
            endif
         enddo
      endif
      write(*,'(3(a,i2))') '[run_cats] wn:',iswn, ' fn:',isfn,
     &     ' rwn:',isrwn,' fpln:',isfpln
      npln=isfn+isrwn

c     searching files to process
      filter='*'//filter(1:nblen(filter))
      write(*,'(2a)') '[run_cats] file filter: ',filter(1:nblen(filter))
      call ffind(path,files,filter,n,1) 
      write(*,'(a,i10)') '[run_cats] #total files:',n
      if (n.le.0) then
         write(*,'(a)') '[run_cats] no files found.'
         stop
      endif



      redirstr=' >& '
c     get system type
c     How to guess the OS type?
c     How to call `uname` and return the results?
c      call getenv('HOME',sys)
c      write(*,*) 'OS Type:',sys(1:nblen(sys))
c      if (sys(1:nblen(sys)).ne.'/export/home/tianyf') then
c         sys='other'
c      else
c         sys='SunOS'
c      endif

c     method 2
      cmdstr='uname > /tmp/uname0'
      status=system(cmdstr)
      call getlun(fido)
      open(unit=fido,file='/tmp/uname0')
      read(fido,*) sys
      close(fido,status='delete')
      write(*,'(2a)') '[run_cats] OS Type: ',sys(1:nblen(sys))
      if (sys(1:nblen(sys)).ne.'SunOS') then
         sys='other'
      else
         sys='SunOS'
      endif

c     get a file handler for temporary file (*.lock)
      call getlun(fidt)

c     loop for each file to be processed
      do fi=1,n
         file=files(fi)
         call getfilename(file,tmpstr)
         site=tmpstr(1:4)
         write(*,'(a)') '[run_cats] processing '//site(1:nblen(site))

         
c$$$         ofile=file
c$$$         call desuffix(ofile,file)
c$$$         ofile=file
c$$$c     read in data
c$$$         write(*,'(a)') '[run_estn] << '//file(1:nblen(file))
c     

c     create output file name
         ofile=opath(1:nblen(opatH))//pathsep//
     &        tmpstr(1:nblen(tmpstr))
         write(*,*) 'iswn:',iswn
         outext=outext_in
         if (iswn.eq.1) then
            outext='wh'//outext(1:nblen(outext))
         endif
         if (isfn.eq.1) then
            outext='fn'//outext(1:nblen(outext))
         endif
         if (isrwn.eq.1) then
            outext='rwn'//outext(1:nblen(outext))
         endif
         if (isfpln.eq.1) then
            outext='fpln'//outext(1:nblen(outext))
         endif
         if (isgm.eq.1) then
            outext='gm'//outext(1:nblen(outext))
         endif
         if (isbp.eq.1) then
            outext='bp'//outext(1:nblen(outext))
         endif
         ofile=ofile(1:nblen(ofile))//'.'//outext(1:nblen(outext))
         ostr=ofile

c$$$         write(*,'(2a)') '[run_cats]>>ofile:',ofile(1:nblen(ofile))
c$$$         stop


c     check existing
         inquire(file=ofile,exist=exi)
c     write(*,*) isOver,exi
         write(*,'(a,i2,a,l2)') '[run_cats] overwrite? ', isOver,
     &        '  exist? ',exi
         if (isOver.eq.0.and.exi) then
            write(*,'(a)'),'[run_cats] !!!exist. skip..'//
     &           ofile(1:nblen(ofile))
            goto 801
         endif


c     create temporary lock file
         tfile=ofile(1:nblen(ofile))//'.lock'
         open(unit=fidt,file=tfile)
         read(fidt,*,iostat=ioerr) tmplock
c     write(*,*) 'ioerr:',ioerr
         if (ioerr.eq.0.and.tmplock(1:4).eq.'lock') then
            write(*,'(2a)') '[run_estn] locked. skip ',
     &           file(1:nblen(file))
            close(fidt)
            goto 801
         endif
         write(fidt,'(a4,1x,a)') 'lock',hostname(1:nblen(hostname))
         write(*,'(a)') '[run_cats] >> '//ofile(1:nblen(ofile))

c         cmdstr='chmod +x '//ofile(1:nblen(ofile))
c         status=system(cmdstr)
c     cats_v312_icc_m32 /home/tianyf/tmp/test_cats/2007_cln.deoffps.cats/yssk.neu.cats --sinusoid 1y1 --model pl:k-1 --model wh:  --verbose --output yssk.neu.cats.fnwh_mle --psdfile yssk.neu.cats.fnwh_mle.psd

         cmdstr=''//cats_version
         cmdstr=cmdstr(1:nblen(cmdstr))//' '//file(1:nblen(file))
         if (isann.eq.1) then
            cmdstr=cmdstr(1:nblen(cmdstr))//' --sinusoid 1y'
            if (isSemi.eq.1) then
               cmdstr=cmdstr(1:nblen(cmdstr))//'1'
            endif
         endif
         if (isTrend.eq.0) then
            cmdstr=cmdstr(1:nblen(cmdstr))//' --notrend'
         endif
         if (iswn.eq.1) then
            cmdstr=cmdstr(1:nblen(cmdstr))//' --model wh:'
         endif
         if (isfn.eq.1) then
            cmdstr=cmdstr(1:nblen(cmdstr))//' --model pl:k-1'
         endif
         if (isrwn.eq.1) then
            cmdstr=cmdstr(1:nblen(cmdstr))//' --model pl:k-2'
         endif
         cmdstr=cmdstr(1:nblen(cmdstr))//' --verbose --output'
         cmdstr=cmdstr(1:nblen(cmdstr))//' '//ofile(1:nblen(ofile))
         
         write(*,'(a)') '[run_cats] executing cats ...'
         
c     stop
         
         write(*,'(a)') '[run_cats] '//cmdstr(1:nblen(cmdstr))
         status=system(cmdstr)
c     stop

c     delete temporary lock file
         close(fidt,status='delete')
         cmdstr='/bin/rm -rf '//tfile(1:nblen(tfile))
         status=system(cmdstr)
         write(*,'(2a)') '[run_cats] done! Lock file cleared: ',
     &        tfile(1:nblen(tfile))
c     stop
 801     continue
c         stop
      enddo

c      cmdstr='/bin/rm -rf '//tmpdir(1:nblen(tmpdir))
c     write(*,*) cmdstr(1:nblen(cmdstr))
c     status=system(cmdstr)

      STOP
      END
