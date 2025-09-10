CTITLE get_orb
      PROGRAM get_rnx

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--
c     Download GPS RINEX observation files from IGS Data Center.

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      integer year,doy,ndays
      character*128 ftp_prog

c     --OUTPUT--

c     --Local Parameters--
      integer*4 NMAX
      parameter(NMAX=5000)
      character*1023 dir,dir_cur,dir_remote,tmpstr,cmdstr,host,tmpstr2
      character*1023 gsac_dir,gsac_file,gsac_filez,url
      character*1023 rstat_dir,rstat_file,rstat_file3
      character*10 archive
      character*4 site,sites(NMAX),sitesref(NMAX),sitesarv(NMAX),siteU
      character*4 sitesGet(NMAX)
      integer*4 nsite,nsiteref,nsitearv,nsiteGet
      integer*4 ndaysofyr, yr,gpsw,gpsd
      integer*4 i,j,k
      character*1 path_sep

      character*1023 files(NMAX),file,ptn,file_cmd,file_log,dir_log
      character*1023 file_remote,file_local,file_site,file_archive
      character*1023 file2,file3,file4
      character*1023 files_exi(10), filter_exi
      integer*4 nf_exi
      integer*4 nf,fid,ioerr

      logical exi,exi2,exi3,exi4
      
      integer iargc
      integer*4 ndoyr,nblen
      integer*4 status,system
      integer*4 today(3),now(3)

c     HOME directory
      character*1023 home

c     check if the request data is available (using station.info)
      character*1023 stafile
      real*8 dyrs(NMAX),dyre(NMAX),dyr
      integer*4 obstime(10)
      integer*4 isStaiChk

c     whether use server holding or not (default: 0/1).
      integer*4 isHoldChk
      data isHoldChk /0/

c     holding file check
      character*1023 hdfile
      integer*4 is_in_holding,isinhd,nhdsit
      character*4 hdsites(NMAX)
c     samples:
c     trom 2008 001 d y /gpsdc/pub/rinex//2008/001/trom0010.08d.Z
c     reso 2008 001 d y /gpsdc/pub/rinex//2008/001/reso0010.08d.Z
c     bahr 2008 001 d y /gpsdc/pub/rinex//2008/001/bahr0010.08d.Z
c     tash 2008 001 d y /gpsdc/pub/rinex//2008/001/tash0010.08d.Z

c     resume downloading?
      integer*4 isResume
      character*10 resumstr
c     

c     whether download file now
      integer*4 isDown
      character*100 downstr
      integer*4 getpid,pid


c     <<VAR_DEC

      if (iargc().lt.4) then
         write(*,*) 'Usage: get_rnx YEAR|YR DOY NDAYS '
         write(*,*) '               --site=sit1,...|--sitefile=sfile'
         write(*,*) '                 e.g.: --site=bjfs,shao,kunm'
         write(*,*) '                       --sitefile=/home/sio.sit'
         write(*,*) '                         Sample of SITEFILE:'
         write(*,*) '                          bjfs lhas'
         write(*,*) '                          wuhn #urum'
         write(*,*) '                          shao'
         write(*,*) '                 *non-blank first char or '//
     &        'sites starting with # will be skipped'
         write(*,*) '                 *full filename is recommended'
         write(*,*) '               [--dir=output_directory ]'
         write(*,*) '                  Default: `pwd`'
         write(*,*) '                  *Output path is the parent '
     .        ,'directory of rinex files.'
         write(*,*) '               [--dirlog=log_directory]'
         write(*,*) '               [--archive=sopac|cddis|kasi]'
         write(*,*) '                  Default:sopac'
         write(*,*) '               [--ftp_prog=wget|ftp|ncftp]'
         write(*,*) '                  Default:ftp'
         write(*,*) '               [--gsac_dir=gsac_directory]'
         write(*,*) '               [--download=y|n]'
         write(*,*) '                  Default:y'
         write(*,*) '               [--holding=HOLDING_FILE]'
         write(*,*) '               [--rstat_dir=RNX_STATUS_DIR]'
         write(*,*) '               [--resume=y|n]'
         write(*,*) '                  Default:n'
         write(*,*) ''
         write(*,*) 'Examples:'
         write(*,*) '  get_rnx 2008 1 2 --site=bjfs,shao,kunm,lhaz,guao'
         write(*,*) '  get_rnx 99 1 3 --sitefile=/dc/cddis.sit '//
     &        '--ftp_prog=wget --dir=/gps/pub/rinex'
         write(*,*) ''
         write(*,*) ' (c) Copyright by Tian 2007-209.'
         write(*,*) '  Please report bugs or send suggestions to '//
     &        'tianyf@gmail.com'
         write(*,*) '  License: free to use/edit for academic purposes.'
         stop
      endif

c     default setting
      archive='sopac'

c     default not resume downloads
      isResume=0
      resumstr=' -nc '

c     Default to download files
      isDown=1

c     default do not check holding file
      hdfile=''

c     At least 4 parameters need: yr,doy,ndays,sites
      call getarg(1,tmpstr)
      read(tmpstr,*) year
      if (year.lt.60) then
         year=year+2000
      else if (year.lt.1960) then
         year=year+1900
      endif
      call getarg(2,tmpstr)
      read(tmpstr,*) doy
      call getarg(3,tmpstr)
      read(tmpstr,*) ndays

c     for debug in solaris
      dir='/export/home/tianyf/tmp/rinex'
      dir_log='/tmp'
      dir_log='./'
      gsac_dir=''
      rstat_dir=''


c     default parameters
      call getenv('HOME',home)
      ftp_prog='ftp'
      isStaiChk=0
      stafile=home(1:nblen(home))//path_sep()//
     &     'gpsf/cgps/conf/setup/station.info'
c*SITE  Station Name      Session Start      Session Stop       Ant Ht   HtCod  Ant N    Ant E    Receiver Type         Vers                  SwVer  Receiver SN           Antenna Type     Dome   Antenna SN          
c 019B  TOBISHIMA         1996 072 00 00 00  2003 160 00 00 00   0.0000  DHPAB   0.0000   0.0000  TRIMBLE 4000SSI 



      if (iargc().eq.4) then
         call getarg(4,tmpstr)
         if (tmpstr(1:6).ne.'--site') then
            write(*,*) 'Error: sites information must be present.'
            stop
         endif
c     sites/site_file?
         if (tmpstr(1:7).eq.'--site=') then
            write(*,*) '#using sites:'//tmpstr(8:nblen(tmpstr))
            cmdstr=tmpstr(8:nblen(tmpstr))
            call strsplit(cmdstr,',',nsite,sites)
         else if (tmpstr(1:11).eq.'--sitefile=') then
            write(*,*) '#using site file:'//tmpstr(12:nblen(tmpstr))
            file_site=tmpstr(12:nblen(tmpstr))
            call site_read(file_site,sitesref,sites,5000,
     &           nsiteref,nsite)
            write(*,*) '#sites:',nsite
         else
            write(*,*) 'Error: invalid site information input!'
            stop
         endif
      else
         do i=4,iargc()
            call getarg(i,tmpstr)
c            write(*,*) tmpstr(1:nblen(tmpstr))
            if (tmpstr(1:7).eq.'--site=') then
               write(*,*) '#using sites:'//tmpstr(8:nblen(tmpstr))
                cmdstr=tmpstr(8:nblen(tmpstr))
                call strsplit(cmdstr,',',nsite,sites)
                write(*,*) '#sites:',nsite
c                do j=1,nsite
c                   write(*,*) '  ',sites(j)
c                enddo
            else if (tmpstr(1:11).eq.'--sitefile=') then
               write(*,*) '#using site file:'//tmpstr(12:nblen(tmpstr))
               file_site=tmpstr(12:nblen(tmpstr))
               call site_read(file_site,sitesref,sites,5000,
     &              nsiteref,nsite)
               write(*,*) '#sites:',nsite
               if (nsite.lt.1) then
                  write(*,*) 'WARNING: no sites found.'
                  stop
               endif
c$$$               do j=1,nsite
c$$$                  write(*,*) sites(j)
c$$$               enddo
c$$$               stop
            else if (tmpstr(1:6).eq.'--dir=') then
               write(*,*) '#using dir:'//tmpstr(7:nblen(tmpstr))
               dir=tmpstr(7:nblen(tmpstr))
            else if (tmpstr(1:10).eq.'--archive=') then
               archive=tmpstr(11:nblen(tmpstr))
               write(*,*) '#using archive:'//archive(1:nblen(archive))
            else if (tmpstr(1:10).eq.'--holdchk=') then
c               write(*,*) nblen(tmpstr)
               if (nblen(tmpstr).le.10) then
                  write(*,*) 'Error: invalid parameter ',
     &                 tmpstr(1:nblen(tmpstr))
                  stop
               endif
               tmpstr2=tmpstr(11:11)
               read(tmpstr2,*) isHoldChk
            else if (tmpstr(1:9).eq.'--dirlog=') then
c               write(*,*) nblen(tmpstr)
               if (nblen(tmpstr).le.9) then
                  write(*,*) 'Error: invalid parameter ',
     &                 tmpstr(1:nblen(tmpstr))
                  stop
               endif
               dir_log=tmpstr(10:nblen(tmpstr))
               write(*,*) ' #INFO: temporary path is ',
     &              dir_log(1:nblen(dir_log))
            else if (tmpstr(1:11).eq.'--ftp_prog=') then
               if (nblen(tmpstr).le.11) then
                  write(*,*) 'Error: invalid parameter ',
     &                 tmpstr(1:nblen(tmpstr))
                  stop
               endif
               ftp_prog=tmpstr(index(tmpstr,'=')+1:nblen(tmpstr))
               write(*,*) ' #INFO: using ',ftp_prog(1:nblen(ftp_prog))
            else if (tmpstr(1:11).eq.'--gsac_dir=') then
               if (nblen(tmpstr).le.11) then
                  write(*,*) 'Error: invalid parameter ',
     &                 tmpstr(1:nblen(tmpstr))
                  stop
               endif
               gsac_dir=tmpstr(index(tmpstr,'=')+1:nblen(tmpstr))
               write(*,*) '#INFO:gsac path ',gsac_dir(1:nblen(gsac_dir))
            else if (tmpstr(1:12).eq.'--rstat_dir=') then
               if (nblen(tmpstr).le.12) then
                  write(*,*) 'Error: invalid parameter ',
     &                 tmpstr(1:nblen(tmpstr))
                  stop
               endif
               rstat_dir=tmpstr(index(tmpstr,'=')+1:nblen(tmpstr))
               write(*,'(2a)') '#INFO:rinex status path ',
     +              rstat_dir(1:nblen(rstat_dir))
            else if (tmpstr(1:11).eq.'--download=') then
               if (nblen(tmpstr).le.11) then
                  write(*,*) 'Error: invalid parameter ',
     &                 tmpstr(1:nblen(tmpstr))
                  stop
               endif
               downstr=tmpstr(index(tmpstr,'=')+1:nblen(tmpstr))
               if (downstr(1:nblen(downstr)).eq.'y') then
                  isDown=1
               else if (downstr(1:nblen(downstr)).eq.'n') then
                  isDown=0
               else
                  write(*,*) ' Invalid parameter:',tmpstr
                  stop
               endif
            else if (tmpstr(1:10).eq.'--holding=') then
               if (nblen(tmpstr).le.10) then
                  write(*,*) 'Error: invalid parameter ',
     &                 tmpstr(1:nblen(tmpstr))
                  stop
               endif
               hdfile=tmpstr(index(tmpstr,'=')+1:nblen(tmpstr))
               write(*,'(2a)') '#INFO:use holding file ',
     &              hdfile(1:nblen(hdfile))
            else if (tmpstr(1:9).eq.'--resume=') then
               if (nblen(tmpstr).le.9) then
                  write(*,*) 'Error: invalid parameter ',
     &                 tmpstr(1:nblen(tmpstr))
                  stop
               endif
               if(tmpstr(index(tmpstr,'=')+1:nblen(tmpstr)).eq.'y') then
                  isResume=1
                  resumstr=' -c '
               else
                  isResume=0
                  resumstr=' -nc '
               endif
            else
               write(*,'(3a)') 
     +              '[get_rnx]ERROR:invalid parameter input[',
     +              tmpstr(1:nblen(tmpstr)),']!!'
               stop
            endif
         enddo
      endif
     
      if (archive.eq.'sopac') then
         host='garner.ucsd.edu'
c         host='132.239.152.113'
      else if (archive.eq.'cddis') then
         host='cddis.gsfc.nasa.gov'
      else if (archive.eq.'kasi') then
         host='nfs.kasi.re.kr'
      else if (archive.eq.'pbo') then
         host='data-out.unavco.org'
      else if (archive.eq.'bkg') then
         host='igs.bkg.bund.de'
      else if (archive.eq.'olg') then
         host='olggps.oeaw.ac.at'
      else if (archive.eq.'olgarv') then
         host='olggps.oeaw.ac.at'
      else if (archive.eq.'whu') then
c     igs.gnsswhu.cn (ip changes)
         host='123.57.234.5'
      else if (archive.eq.'esa') then
         host='gssc.esa.int'
      else
         write(*,*) 'Error: wrong archive [',
     &        archive(1:nblen(archive)),']'
         stop
      endif

      if (isHoldChk.eq.1) then
c     archive file hardwired.         
         file_archive=tmpstr(1:nblen(tmpstr))
     &        //'/gpsf/cgps/conf/sites.holding.'//archive
         write(*,*) 'using archive file:'//
     &        file_archive(1:nblen(file_archive))
         call site_read(file_archive,sitesref,sitesarv,5000,
     &        nsiteref,nsitearv)
         write(*,*) '#sites archive:',nsitearv
c      do i=1,nsitearv
c         write(*,*) sitesarv(i)
c      enddo

         call strarr_and(sites,nsite,sitesArv,nsiteArv,
     &        sitesGet,nsiteGet)
      else
c     NOT perform server holding check.
         do i=1,nsite
            sitesGet(i)=sites(i)
         enddo
         nsiteGet=nsite
      endif
      write(*,*) '#site to download:',nsiteGet

      if (isStaiChk.eq.1) then
c     find the data span for each site
         do i=1,nsiteGet
c         write(*,*) 'calling query_stainfo',stafile,sitesGet(i),obstime
            call query_stainfo(stafile,sitesGet(i),obstime)
c         write(*,*) 'query_stainfo called'
c     If the starting & ending dates not found for site, then no constraints.
            if (obstime(1).eq.-1) then
               dyrs(i)=-1
               dyre(i)=9999
            else
               dyrs(i)=obstime(1)+obstime(2)/ndoyr(obstime(1))
               dyre(i)=obstime(6)+obstime(7)/ndoyr(obstime(6))
            endif
c         write(*,*) sitesGet(i)
         enddo
      endif
c      stop

      ndaysofyr=ndoyr(year)
      if (year.ge.2000) then
         yr=year-2000
      else if (year.gt.1950) then
         yr=year-1900
      endif

c      stop

c     *********************************************************************
c     if gsac checking required, then
      if (gsac_dir.ne.'') then
c         write(*,*) '#checking gsac ...'
         write(gsac_filez,710) gsac_dir(1:nblen(gsac_dir)),
     &        path_sep(),
     &        archive(1:nblen(archive)),year,doy
 710     format(a,a1,a,".",i4.4,".",i3.3,".full.dhf")
c         write(*,*) 'gsac_filez:',gsac_filez(1:nblen(gsac_filez))
         call getfilename(gsac_filez,tmpstr)
c     write(*,*) tmpstr
         call desuffix(tmpstr,tmpstr2)
c     write(*,*) gsac_file
         pid=getpid()
         write(gsac_file,711) '/tmp',path_sep(), 
     &        tmpstr2(1:nblen(tmpstr2)),pid
 711     format(a,a1,a,".",i20.20)
c         write(*,*) 'gsac_file:',gsac_file(1:nblen(gsac_file))
c     uncompress the file
         write(cmdstr,712) gsac_filez(1:nblen(gsac_filez)),
     &        gsac_file(1:nblen(gsac_file))
 712     format("cat",1x,a," > ",a)
c         write(*,*) cmdstr
         status=system(cmdstr)
c         write(*,*) '#gsac checked.'
      endif



c     Loop for each day
c     nf=0
      do i=1,ndays
         nf=0
c     stop
c     open command file
         call getlun(fid)
         call idate(today)
         call itime(now)
         write(file_cmd,703) dir_log(1:nblen(dir_log)),today,now,'.cmd'
 703     format(A,"/get-rnx-",I2.2,I2.2,I4.4,"-",3I2.2,a)
c      write(*,*) file_cmd,today,now
c         write(file_log,703) dir_log(1:nblen(dir_log)),today,now,'.log'
         write(file_log,703) dir_log(1:nblen(dir_log)),today,now,'.log'
         

c         write(*,700) dir(1:nblen(dir)),yr,doy
c     Calculate GPSW/GPSD
c         call doygwk(doy,year,gpsw,gpsd)
         write(dir_cur,700) dir(1:nblen(dir)),year,doy
 700     format(A,"/",I4.4,"/",I3.3)
c         write(*,*) yr,doy,i
c         write(*,*) dir_cur(1:nblen(dir_cur)



         if (archive.eq.'sopac') then
            write(dir_remote,704) year,doy
 704        format("/pub/rinex/",I4.4,"/",I3.3)
         else if (archive.eq.'cddis') then
            write(dir_remote,707) year,doy,yr
c     707           format("/pub/gps/data/daily/",I4.4,"/",I3.3,"/",I2.2,"d")  
 707        format("/gps/data/daily/",I4.4,"/",I3.3,"/",I2.2,"d") 
         else if (archive.eq.'kasi') then
            write(dir_remote,708) year,doy,yr
 708        format("/gps/data/daily/",I4.4,"/",I3.3,"/",I2.2,"d")  
         else if (archive.eq.'pbo') then
            write(dir_remote,709) year,doy
 709        format("/pub/rinex/obs/",I4.4,"/",I3.3)
         else if (archive.eq.'bkg') then
            write(dir_remote,719) year,doy
 719        format("/EUREF/obs/",I4.4,"/",I3.3)
         else if (archive.eq.'olg') then
c     ftp://olggps.oeaw.ac.at/pub/            
c            write(dir_remote,721) site
 721        format("/pub/outdata/",a4,"/")
         else if (archive.eq.'olgarv') then
c     ftp://olggps.oeaw.ac.at/pub/            
            write(dir_remote,722) year,doy
 722        format("/pub/",I4.4,"/",I3.3)
         else if (archive.eq.'whu') then   
            write(dir_remote,723) year,doy,yr
 723        format("/pub/gps/data/daily/",I4.4,"/",I3.3,"/",I2.2,"d")
         else if (archive.eq.'esa') then   
            write(dir_remote,724) year,doy
 724        format("/cddis/gnss/data/daily/",I4.4,"/",I3.3)
         endif

         if (ftp_prog(1:nblen(ftp_prog)).ne.'wget') then
            open(file=file_cmd,unit=fid,iostat=ioerr)
            if (ioerr.ne.0) then
               write(*,*) 'Error: cannot open temporary command file'
               stop
            endif
            if (ftp_prog(1:nblen(ftp_prog)).eq.'ftp') then
               write(fid,*) 'user anonymous tianyf@gmail.com'
            endif
            write(fid,*) 'binary'

            write(fid,*) 'lcd '//dir_cur(1:nblen(dir_cur))       
            write(fid,*) 'cd '//dir_remote(1:nblen(dir_remote)) 
 
         endif

c     Get what sites in holding file?
         if (nblen(hdfile).gt.0) then
            call query_hd(hdfile,year,doy,hdsites,nmax,nhdsit)
            write(*,*) 'sites in holding: ',(hdsites(k)//' ',k=1,nhdsit)
         endif

c     *********************************************************************
c     if IGS YYDOY.status checking required, then
         rstat_file=''
         rstat_file3=''
         if (rstat_dir.ne.'') then
c     write(*,*) '#checking KASI/CDDIS YYDOY.status files ...'
            write(rstat_file,730) rstat_dir(1:nblen(rstat_dir)),
     &           path_sep(),
     &           archive(1:nblen(archive)),year,yr,doy
 730        format(a,a1,a,"/",i4.4,"/",i2.2,i3.3,".status")
            write(*,*) 'rstat_file: ',rstat_file(1:nblen(rstat_file))
            write(rstat_file3,732) rstat_dir(1:nblen(rstat_dir)),
     &           path_sep(),
     &           archive(1:nblen(archive)),year,yr,doy
 732        format(a,a1,a,"/",i4.4,"/",i2.2,i3.3,".status3")
            write(*,*) 'rstat_file3: ',rstat_file3(1:nblen(rstat_file3))
         endif
            
c     ///LOOP for SITE///
         do j=1,nsiteGet
            site=sitesGet(j)
            write(*,*) site
            

            if (archive.eq.'olg') then
c     ftp://olggps.oeaw.ac.at/pub/
               write(dir_remote,720) site
 720           format("/pub/outdata/",a4,"")
            endif

c     check whether the current file is in holdings.
            if (nblen(hdfile).gt.0) then
c$$$               write(*,*) 'checking holding...'
c$$$               isinhd=is_in_holding(hdfile,site,year,doy)
c$$$               write(*,*) 'holding checked...'
c$$$               if (isinhd.eq.1) then
c$$$                  write(*,*) '11:',is_in_holding(hdfile,site,year,doy)
c$$$                  write(*,'(2a)') site(1:nblen(site)),
c$$$     &                 ' is already in holding!'
c$$$                  goto 801
c$$$               endif
c$$$               write(*,*) 'not in holding...'
               write(*,*) 'checking holding...'
               do k=1,nhdsit
                  if (site.eq.hdsites(k)) then
                     write(*,'(2a)') site(1:nblen(site)),
     &                    ' is already in holding!'
                     goto 801
                  endif
               enddo
               write(*,*) 'not in holding...'
            endif
c            write(*,*) '12:',is_in_holding(hdfile,site,year,doy)

c            write(*,*) 'downloading file for ',
c     &           site(1:nblen(site)), doy,'/',year
            dyr=year+doy/ndoyr(year)
            if (isStaiChk.eq.1) then
               if (dyr.lt.dyrs(j).or.dyr.gt.dyre(j)) then
                  write(*,*) '#out of time range:',
     &                 site(1:nblen(site))
                  goto 801
               endif
            endif
            write(file,701) dir_cur(1:nblen(dir_cur)),
     &           site,doy,yr
c 701        format(A,"/",a4,I3.3,"0.",I2.2,"d.Z")
 701        format(A,"/",a4,I3.3,"0.",I2.2,"d.gz")

            write(file2,731) dir_cur(1:nblen(dir_cur)),
     &           site,doy,yr
 731        format(A,"/",a4,I3.3,"0.",I2.2,"d.Z")
            write(file3,735) dir_cur(1:nblen(dir_cur)),
     &           site,doy,yr
 735        format(A,"/",a4,I3.3,"0.",I2.2,"o.Z")
            write(file4,736) dir_cur(1:nblen(dir_cur)),
     &           site,doy,yr
 736        format(A,"/",a4,I3.3,"0.",I2.2,"o.gz")


c            write(*,*) file(1:nblen(file))
c     ,orbType
c     , year,doy,gpsw,gpsd
c            goto 800
            inquire(file=file, exist=exi)
            inquire(file=file2, exist=exi2)
            inquire(file=file3, exist=exi3)
            inquire(file=file4, exist=exi4)


            siteU=site
            call uppers(siteU)
            write(filter_exi,733) siteU(1:4)
 733        format(a4,"*.crx.gz")
            write(*,*) 'filter_exi:',filter_exi(1:nblen(filter_exi))
            write(*,*) 'dir_cur:',dir_cur(1:nblen(dir_cur))

            call ffind(dir_cur,files_exi,filter_exi,nf_exi,1) 
            write(*,'(a,i10)') '#number of exising file:s',nf_exi
c            stop

            write(*,*) 'EX:',exi,exi2,exi3,exi4
            if (exi.or.nf_exi.gt.0) then
               write(*,*) '#already exist: '//
     &              file(1:nblen(file))
               goto 801
c     Here, checking the validation of existing files is not performed.
            endif

            if (exi2) then
               write(*,*) '#already exist: '//
     &              file2(1:nblen(file2))
               goto 801
            endif
            if (exi3) then
               write(*,*) '#already exist: '//
     &              file3(1:nblen(file3))
               goto 801
            endif
            if (exi4) then
               write(*,*) '#already exist: '//
     &              file4(1:nblen(file4))
               goto 801
            endif




c     check for gsac files
c            if (gsac_file.ne.'') then
            if (nblen(gsac_file).gt.0) then
               write(*,*) 'checking gsac for ',year,doy
               write(*,*) 'gsac_file:',gsac_file(1:nblen(gsac_file))
               call query_gsac_dhf(gsac_file,site,url)
               if (url.eq.'') then
c                  write(*,'(5a)') '#[]',site(1:nblen(site)),
c     &                 ' not available on ',
c     &                 archive(1:nblen(archive)),'.'
                  goto 801
               endif
c               write(*,*) 'gsac checked__'
            endif
            
c     check for KASI/CDDIS YYDOY.status holding files
            if (nblen(rstat_file).gt.0) then
               write(*,*) 'checking rinex v2 files for ',year,doy
               write(*,*) 'rstat_file:',rstat_file(1:nblen(rstat_file))
               call query_rnx_status(rstat_file,site,url)
               write(*,*) 'url:',url(1:nblen(url))
               if (url.eq.'') then
c check rinex 3 files               
               write(*,*) 'checking rinex v3 files for ',year,doy
               write(*,*) 'rstat_file3:',
     +               rstat_file3(1:nblen(rstat_file3))
c               siteU=site
c               call uppers(siteU)
               call query_rnx_status(rstat_file3,siteU,url)
               write(*,*) 'url:',url(1:nblen(url))
                  if (url.eq.'') then
c                  write(*,'(5a)') '#[]',site(1:nblen(site)),
c     &                 ' not available on ',
c     &                 archive(1:nblen(archive)),'.'
                    goto 801
                  endif
               endif
c               write(*,*) 'gsac checked__'
            endif

c
c     -->kasi
c     queue for the kasi structure

            write(file,706) dir_cur(1:nblen(dir_cur)),yr,
     &           site,doy,yr
c 706        format(A,"/",I2.2,"d/",a4,I3.3,"0.",I2.2,"d.Z")
 706        format(A,"/",I2.2,"d/",a4,I3.3,"0.",I2.2,"d.gz")

c            write(*,*) file(1:nblen(file))
c     ,orbType
c     , year,doy,gpsw,gpsd
c            goto 800
            inquire (file=file, exist=exi)
c     write(*,*) 'EX:',exi
            if (exi) then
               write(*,*) '#already exist: '//
     &              file(1:nblen(file))
               goto 801
c     Here, checking the validation of existing files is not performed.
c     
            endif
c     <--kasi
            
c            write(*,*) '#queuing ',file(1:nblen(file))
c     first, create the target directory.
            cmdstr='mkdir -p '//dir_cur(1:nblen(dir_cur))
            status=system(cmdstr)
            if (status.ne.0) then
               write(*,*) 'Error: cannot create output directory [',
     &              dir_cur(1:nblen(dir_cur)),'], skipping'
               goto 800
            endif

            if (url.eq.'') then
              write(file_remote,702) site,doy,yr
c 702        format(A4,I3.3,"0.",I2.2,"d.Z")
 702          format(A4,I3.3,"0.",I2.2,"d.gz")
            else
              file_remote=url(6:)
            endif

c     append the file to cmd_file
c     if using ncftp
            if (ftp_prog(1:nblen(ftp_prog)).eq.'ncftp') then
               if (archive.eq.'olg') then
                  write(fid,*) 'cd '//dir_remote(1:nblen(dir_remote))
               endif
c               write(fid,*) 'lcd '//dir_cur(1:nblen(dir_cur))       
c               write(fid,*) 'cd '//dir_remote(1:nblen(dir_remote))  
               write(fid,*) 'get -z ',
     &              file_remote(1:nblen(file_remote)),
     &              ' '//file_remote(1:nblen(file_remote))
            endif
   
c     if using ftp
            if (ftp_prog(1:nblen(ftp_prog)).eq.'ftp') then
               if (archive.eq.'olg') then
                  write(fid,*) 'cd '//dir_remote(1:nblen(dir_remote))
               endif
c               write(fid,*) 'lcd '//dir_cur(1:nblen(dir_cur))       
c               write(fid,*) 'cd '//dir_remote(1:nblen(dir_remote))  
               write(fid,*) 'get  ',
     &              file_remote(1:nblen(file_remote))
            endif
   
c     if using wget ...
            if (ftp_prog(1:nblen(ftp_prog)).eq.'wget') then
               open(file=file_cmd,unit=fid,iostat=ioerr)
               if (ioerr.ne.0) then
                  write(*,*) 'Error: cannot open temporary command file'
                  stop
               endif
               write(fid,*) 'cd '//dir_cur(1:nblen(dir_cur))
               write(fid,*) 'pwd'
               if (archive.eq.'sopac') then
                  write(fid,'(6a)') 'wget '//
     &                 resumstr(1:nblen(resumstr))//
     &                 ' --http-user=anonymous '//
     &                 ' --http-passwd=tianyf@gmail.com '//
     &                 host(1:nblen(host))//
c     &              '/'
     &                 dir_remote(1:nblen(dir_remote))//
     &                 '/',
     &                 file_remote(1:nblen(file_remote))
               else
                  write(fid,'(6a)') 'wget '//
     &                 resumstr(1:nblen(resumstr))//
c     &                 '--ftp-passwd=tianyf@gmail.com ',
     &                 ' ftp://'//host(1:nblen(host)),
c     &              '/'
     &                 dir_remote(1:nblen(dir_remote)),
     &                 '/',
     &                 file_remote(1:nblen(file_remote))
                  
               endif
               close(fid)
               cmdstr='sh  '//file_cmd(1:nblen(file_cmd))//' >'//
     &              file_log(1:nblen(file_log))
c               cmdstr='cat '//file_cmd(1:nblen(file_cmd))
               status=system(cmdstr)
               cmdstr='rm -f '//file_cmd(1:nblen(file_cmd))
c     cannot use \rm ?
c               status=system(cmdstr)
               cmdstr='rm -f '//file_log(1:nblen(file_log))
               status=system(cmdstr)
c               write(*,*) host(1:nblen(host))
c               write(*,*) path_sep()
c               write(*,*) file_remote(1:nblen(file_remote))
c               stop
            endif
c     stop
            
            nf=nf+1
 801        continue
         enddo
            
         if (ftp_prog(1:nblen(ftp_prog)).ne.'wget') then
            write(fid,*) 'quit'
            close(fid)
         write(*,*) 'file_cmd:',file_cmd(1:nblen(file_cmd))
c         stop

c     download the file
            if (nf.ge.1) then
               write(*,*) '#sites to be download:',nf
               
c               cmdstr='/usr/sfw/bin/ncftp '//host(1:nblen(host))//
               if (ftp_prog(1:nblen(ftp_prog)).eq.'ncftp') then
                  cmdstr='ncftp -u anonymous -p tianyf@gmail.com '
     &                 //host(1:nblen(host))//
     &                 ' < '//file_cmd(1:nblen(file_cmd))
               else if (ftp_prog(1:nblen(ftp_prog)).eq.'ftp') then
                  cmdstr='ftp -inpv  '
     &                 //host(1:nblen(host))//
     &                 ' < '//file_cmd(1:nblen(file_cmd))
               endif
c     &           //' | grep -v "^230"'
c     &              //' > '//file_log(1:nblen(file_log))
c     &              //' 2>&1 '
c               write(*,*) '#executing ... '//cmdstr(1:nblen(cmdstr))
               if (isDown.eq.1)then
                  status=system(cmdstr)
               else
                  write(*,*) cmdstr(1:nblen(cmdstr))
               endif
            endif
c     delete temporary files
            cmdstr='rm -f '//file_cmd(1:nblen(file_cmd))
c            status=system(cmdstr)
            cmdstr='rm -f '//file_log(1:nblen(file_log))
            status=system(cmdstr)
         endif

      

         nf=0
         
 800     doy=doy+1         
         if (doy.gt.ndaysofyr) then
            doy=1
            year=year+1
            if (year.ge.2000) then
               yr=year-2000
            else if (year.gt.1950) then
               yr=year-1900
            endif
            ndaysofyr=ndoyr(year)
c            if (year.ge.2002) then
c               dir='/cygdrive/i/data.server/pub/rinex'
c            endif
         endif
c         write(*,*) year
c         write(*,*) 'i:',i,ndays
         if (i.lt.ndays) then
c     if gsac checking required, then
            if (gsac_dir.ne.'') then
               write(gsac_filez,710) gsac_dir(1:nblen(gsac_dir)),
     &              path_sep(),
     &              archive(1:nblen(archive)),year,doy
c     710        format(a,a1,a,".",i4.4,".",i3.3,".full.dhf.Z")
c     write(*,*) 'gsac_filez:',gsac_filez(1:nblen(gsac_filez))
               call getfilename(gsac_filez,tmpstr)
c     write(*,*) tmpstr
               call desuffix(tmpstr,tmpstr2)
c     write(*,*) gsac_file
               pid=getpid()
               write(gsac_file,711) '/tmp',path_sep(), 
     &              tmpstr2(1:nblen(tmpstr2)),pid
c     711        format(a,a1,a,".",i20.20)
c     write(*,*) 'gsac_file:',gsac_file(1:nblen(gsac_file))
c     uncompress the file
               write(cmdstr,712) gsac_filez(1:nblen(gsac_filez)),
     &              gsac_file(1:nblen(gsac_file))
c     712        format("gzcat",1x,a," > ",a)
c     write(*,*) cmdstr
               status=system(cmdstr)
            endif
         endif

      enddo

      STOP
      END


      function is_in_holding(hdfile,site,year,doy)
      character*(*) hdfile,site
      integer*4 year,doy,is_in_holding
      integer*4 fid,ioerr,lyear,ldoy
      character*1023 str
      character*4 lsite
      integer*4 nblen

c      write(*,*) '1'
      call getlun(fid)
      open(unit=fid,file=hdfile,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,'(a)') 'Error open file '//hdfile(1:nblen(hdfile))
         stop
      endif
c      write(*,*) 'open ok'
 890  read(fid,'(a102)',end=899) str

c      write(*,*) '2'
c      write(*,*) str(1:nblen(str))
      if (str(1:1).eq.' ') then
         goto 890
      endif
      read(str,*) lsite,lyear,ldoy
c      write(*,*) '3'
      call lowers(lsite)
c      write(*,*) '4'
      if (lsite(1:4).eq.site(1:4).and.year.eq.lyear.and.doy.eq.doy) then
         is_in_holding=1
         goto 899
      endif
      goto 890
      is_in_holding=0
 899  continue
      
      close(fid)
      

      end


      subroutine query_hd(hdfile,year,doy,hdsites,nmax,nhdsit)
      character*(*) hdfile
      integer*4 nhdsit,nmax
      character*4 hdsites(nmax)
      
      integer*4 year,doy,fid,ioerr,lyear,ldoy
      character*1023 str
      character*4 lsite
      integer*4 nblen

      call getlun(fid)
      nhdsit=0
      open(unit=fid,file=hdfile,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,'(a)') 'Error open file '//hdfile(1:nblen(hdfile))
         stop
      endif
 891  read(fid,'(a102)',end=898) str
      if (str(1:1).eq.' ') then
         goto 891
      endif
      read(str,*) lsite,lyear,ldoy
      if (lyear.lt.year) goto 891
      if (lyear.gt.year) goto 898
      if (ldoy.lt.doy) goto 891
      if (ldoy.gt.doy) goto 898
      call lowers(lsite)
      nhdsit=nhdsit+1
      hdsites(nhdsit)=lsite
      goto 891
 898  continue
      
      close(fid)
      
      end
