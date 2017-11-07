CTITLE get_orb
      PROGRAM get_hfile

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--
c     Download SP3/gfile from IGS Data Center.
c$$$ncftp /pub/hfiles/2007/001 > ls
c$$$hakd1a.07001.Z     hebr1a.07001.Z     hnet3a.07001.Z     hnwu1a.07001.Z
c$$$hakd2a.07001.Z     hemeda.07001.Z     hnet4a.07001.Z     hpan1a.07001.Z
c$$$hban1a.07001.Z     heuraa.07001.Z     hnet5a.07001.Z     hpan2a.07001.Z
c$$$hban2a.07001.Z     higs1a.07001.Z     hnet6a.07001.Z     hpan3a.07001.Z
c$$$hban3a.07001.Z     higs2a.07001.Z     hnet7a.07001.Z     hpan4a.07001.Z
c$$$hbar1a.07001.Z     higs3a.07001.Z     hnet8a.07001.Z     hseu1a.07001.Z
c$$$hbar2a.07001.Z     higs4a.07001.Z     hnet9a.07001.Z     hswu1a.07001.Z
c$$$hbar3a.07001.Z     higs5a.07001.Z     hnetaa.07001.Z
c$$$hbar4a.07001.Z     hnet1a.07001.Z     hnetba.07001.Z
c$$$hbar5a.07001.Z     hnet2a.07001.Z     hneu1a.07001.Z


c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      integer year,doy,ndays

c     --OUTPUT--

c     --Local Parameters--
      character*512 dir,dir_cur,dir_remote,tmpstr,cmdstr,host,tmpstr2
      character*10 archive
      character*4 site,sites(5000),sitesref(5000),sitesarv(5000)
      character*4 sitesGet(5000)
      integer*4 nsite,nsiteref,nsitearv,nsiteGet
      integer*4 ndaysofyr, yr,gpsw,gpsd
      integer*4 i,j

      character*512 files(1000),file,ptn,file_cmd,file_log,dir_log
      character*512 file_remote,file_local,file_site,file_archive
      integer*4 nf,fid,ioerr

      logical exi
      
      integer iargc
      integer*4 ndoyr,nblen
      integer*4 status,system
      integer*4 today(3),now(3)

c     whether use server holding or not (default: 0/1).
      integer*4 isHoldChk
      data isHoldChk /0/
c     



c     <<VAR_DEC

      if (iargc().lt.4) then
c         write(*,*) 'Usage: get_rnx YEAR DOY NDAYS SITEFILE [ROOT_DIR]'
         write(*,*) 'Usage: get_hfile YEAR|YR DOY NDAYS '
         write(*,*) '               --net=net1,...|--netfile=nfile'
         write(*,*) '                 absolute path should be used'
         write(*,*) '               [--dir=output_directory ]'
         write(*,*) '                      Default: `pwd`'
         write(*,*) '               [--archive=sopac|cddis|kasi]'
         write(*,*) '                      Default:sopac'
         write(*,*) ''
         write(*,*) '    Sample of SITEFILE:'
         write(*,*) '      igs1 igs2'
         write(*,*) '      igs3 #igs4'
         write(*,*) '      ++++'
         write(*,*) '      igs5'
         write(*,*) ''
         write(*,*) '    Output path is the parent '//
     .        'directory of h-file files.'
         write(*,*) ''
         write(*,*) '(c) Copyright by Tian 2008.'
         write(*,*) 'License: free to use/edit for academic purpose.'
         stop
      endif

c     default setting
      archive='sopac'

c     At least 4 parameters need: yr,doy,ndays,sites
      call getarg(1,tmpstr)
      read(tmpstr,*) year
      call getarg(2,tmpstr)
      read(tmpstr,*) doy
      call getarg(3,tmpstr)
      read(tmpstr,*) ndays

c     for debug in solaris
      dir='/export/home/tianyf/tmp/rinex'
      dir_log='/tmp'
      dir_log='./'



      if (iargc().eq.4) then
         call getarg(4,tmpstr)
         if (tmpstr(1:5).ne.'--net') then
            write(*,*) 'Error: network information must be present.'
            stop
         endif
c     net/net_file?
         if (tmpstr(1:6).eq.'--net=') then
            write(*,*) 'using net:'//tmpstr(7:nblen(tmpstr))
            cmdstr=tmpstr(7:nblen(tmpstr))
            call strsplit(cmdstr,',',nsite,sites)
         else if (tmpstr(1:10).eq.'--netfile=') then
            write(*,*) 'using net file:'//tmpstr(11:nblen(tmpstr))
            file_site=tmpstr(11:nblen(tmpstr))
            call site_read(file_site,sitesref,sites,5000,
     &           nsiteref,nsite)
            write(*,*) '#net:',nsite
         else
            write(*,*) 'Error: invalid net information input!'
            stop
         endif
      else
         do i=4,iargc()
            call getarg(i,tmpstr)
c            write(*,*) tmpstr(1:nblen(tmpstr))
            if (tmpstr(1:6).eq.'--net=') then
               write(*,*) 'using net:'//tmpstr(7:nblen(tmpstr))
                cmdstr=tmpstr(7:nblen(tmpstr))
                call strsplit(cmdstr,',',nsite,sites)
                write(*,*) '#net:',nsite
c                do j=1,nsite
c                   write(*,*) '  ',sites(j)
c                enddo
            else if (tmpstr(1:10).eq.'--netfile=') then
               write(*,*) 'using net file:'//tmpstr(11:nblen(tmpstr))
               file_site=tmpstr(11:nblen(tmpstr))
               call site_read(file_site,sitesref,sites,5000,
     &              nsiteref,nsite)
               write(*,*) '#net:',nsite
               if (nsite.lt.1) then
                  write(*,*) 'WARNING: no sites found.'
                  stop
               endif
c               do j=1,nsite
c                  write(*,*) sites(j)
c               enddo
            else if (tmpstr(1:6).eq.'--dir=') then
               write(*,*) 'using dir:'//tmpstr(7:nblen(tmpstr))
               dir=tmpstr(7:nblen(tmpstr))
            else if (tmpstr(1:10).eq.'--archive=') then
               archive=tmpstr(11:nblen(tmpstr))
               write(*,*) 'using archive:'//archive(1:nblen(archive))
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
               write(*,*) 'INFO: temporary path is ',
     &              dir_log(1:nblen(dir_log))
            else
               write(*,*) 'Error: invalid net information input!'
               stop
            endif
         enddo
      endif
     
      if (isHoldChk.eq.1) then
c     archive file hardwired.
         call getenv('HOME',tmpstr)
         file_archive=tmpstr(1:nblen(tmpstr))
     &        //'/gpsf/cgps/conf/sites.holding.'//archive
         write(*,*) 'using archive file:'//
     &        file_archive(1:nblen(file_archive))
         call site_read(file_archive,sitesref,sitesarv,5000,
     &        nsiteref,nsitearv)
         write(*,*) '#net archive:',nsitearv
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
      write(*,*) '#net to download:',nsiteGet
c      do i=1,nsiteGet
c         write(*,*) sitesGet(i)
c      enddo
c      stop

      ndaysofyr=ndoyr(year)
      if (year.ge.2000) then
         yr=year-2000
      else if (year.gt.1950) then
         yr=year-1900
      endif

c      stop


c     Loop for each day
c     nf=0
      do i=1,ndays
         nf=0

c     open command file
         call getlun(fid)
         call idate(today)
         call itime(now)
         write(file_cmd,703) dir_log(1:nblen(dir_log)),today,now,'.cmd'
 703     format(A,"/get-hfile-",I2.2,I2.2,I4,"-",3I2.2,a)
c      write(*,*) file_cmd,today,now
         write(file_log,703) dir_log(1:nblen(dir_log)),today,now,'.log'
         
         open(file=file_cmd,unit=fid,iostat=ioerr)
         if (ioerr.ne.0) then
            write(*,*) 'Error: cannot open temporary command file'
            stop
         endif
         write(fid,*) 'binary'
c         write(*,700) dir(1:nblen(dir)),yr,doy
c     Calculate GPSW/GPSD
c         call doygwk(doy,year,gpsw,gpsd)
         write(dir_cur,700) dir(1:nblen(dir)),year,doy
 700     format(A,"/",I4.4,"/",I3.3)
c         write(*,*) yr,doy,i
c         write(*,*) dir_cur(1:nblen(dir_cur)

         do j=1,nsiteGet
            site=sitesGet(j)
            write(file,701) dir_cur(1:nblen(dir_cur)),
     &           site,doy,yr
 701        format(A,"/",a4,I3.3,"0.",I2.2,"d.Z")
c            write(*,*) file(1:nblen(file))
c     ,orbType
c     , year,doy,gpsw,gpsd
c            goto 800
            inquire (file=file, exist=exi)
c     write(*,*) 'EX:',exi
            if (exi) then
               write(*,*) 'already exist: '//
     &              file(1:nblen(file))
               goto 801
c     Here, checking the validation of existing files is not performed.
c     
            endif
c
c     -->kasi
c     queue for the kasi structure

            write(file,706) dir_cur(1:nblen(dir_cur)),
     &           site,yr,doy
 706        format(A,"/h",a4,"a.",I2.2,I3.3,".Z")
c            write(*,*) file(1:nblen(file))
c     ,orbType
c     , year,doy,gpsw,gpsd
c            goto 800
            inquire (file=file, exist=exi)
c     write(*,*) 'EX:',exi
            if (exi) then
               write(*,*) 'already exist: '//
     &              file(1:nblen(file))
               goto 801
c     Here, checking the validation of existing files is not performed.
c     
            endif
c     <--kasi
            
            write(*,*) 'queuing ',file(1:nblen(file))
c     first, create the target directory.
            cmdstr='mkdir -p '//dir_cur(1:nblen(dir_cur))
            status=system(cmdstr)
            if (status.ne.0) then
               write(*,*) 'Error: cannot create output directory [',
     &              dir_cur(1:nblen(dir_cur)),'], skipping'
               goto 800
            endif

c     append the file to cmd_file
            write(fid,*) 'lcd '//dir_cur(1:nblen(dir_cur))

            if (archive.eq.'sopac') then
               write(dir_remote,704) year,doy
 704           format("/pub/hfiles/",I4.4,"/",I3.3)
            else if (archive.eq.'cddis') then
               write(dir_remote,707) year,doy,yr
c 707           format("/pub/gps/data/daily/",I4.4,"/",I3.3,"/",I2.2,"d")  
 707           format("/gps/data/daily/",I4.4,"/",I3.3,"/",I2.2,"d") 
            else if (archive.eq.'kasi') then
               write(dir_remote,708) year,doy,yr
 708           format("/gps/data/daily/",I4.4,"/",I3.3,"/",I2.2,"d")  
            else
               write(*,*) 'Error: wrong archive [',
     &              archive(1:nblen(archive)),']'
               stop
            endif

            write(fid,*) 'cd '//dir_remote(1:nblen(dir_remote))
            
            write(file_remote,702) site,yr,doy
 702        format("h",A4,"a.",I2.2,I3.3,".Z")
c 702        format(A4,I3.3,"0.",I2.2,"d.Z")
            write(fid,*) 'get -z '//file_remote(1:nblen(file_remote))//
     &           ' '//file_remote(1:nblen(file_remote))
            nf=nf+1
 801        continue
         enddo
            
         write(fid,*) 'quit'

         close(fid)

c     download the file
         if (nf.ge.1) then
            write(*,*) '#networks to be download:',nf
            if (archive.eq.'sopac') then
               host='garner.ucsd.edu'
            else if (archive.eq.'cddis') then
               host='cddis.gsfc.nasa.gov'
            else if (archive.eq.'kasi') then
               host='nfs.kasi.re.kr'
            endif
            cmdstr='ncftp '//host(1:nblen(host))//
     &           ' < '//file_cmd(1:nblen(file_cmd))
c     &           //' | grep -v "^230"'
     &           //' > '//file_log(1:nblen(file_log))
     &           //' 2>&1 '
            write(*,*) 'executing ... '//cmdstr(1:nblen(cmdstr))
            status=system(cmdstr)
         endif

      
c     delete temporary files
         cmdstr='rm -f '//file_cmd(1:nblen(file_cmd))
c         status=system(cmdstr)
         
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
      enddo

    



      STOP
      END
