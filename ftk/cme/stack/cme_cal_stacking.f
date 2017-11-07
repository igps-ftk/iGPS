c234567890
c
c  Steps:
c     (1).Read in all residual time series;
c     (2).Get sites names used for CME calcualtions;
c     (3).Calculate CME;
c     (4).Perform CME correction to all time series.
c     (5).Output the results to files.
      program cme_cal_stacking
c     --
      IMPLICIT NONE
      include '../../inc/cgps.h'
c     --
      character*1023 path,opath,ofile,sitefile
c     path - input GNSS residual position time sereis localtion
c     opath - output filtered time series (optional)
c     ofile - output CME time series (if not given, written to the screen;
c             use "grep '^ ' " to extract the CME).
c     sitefile - file contains sites list to derive CME
c
c     --
      integer*4 yr,doy,ndays
      character*512 filter,file,files(nmax_sites)
      character*4 sites(nmax_sites),sites_cme(nmax_sites)
c     dummy variable
      character*4 sites_nul(nmax_sites)
      integer*4 nsite_nul,ndays_cme,ioerr,fid

      integer*4 nf,i,j,fi,k,l,m,n
      integer*4 nrow, ncol, nhead
      character*512 headers(nmax_head)
c
c     store all data when reading time series
      real*8 data_all(nmax_row,nmax_col,nmax_proc)
c     store arranged all data (i.e. same row index)
      real*8 dataAll(nmax_row,nmax_col,nmax_proc),mjdAll(nmax_row)
c
      real*8 data(nmax_row,nmax_col),odata(nmax_row,nmax_col)
      integer*4 nrows(nmax_sites),ncols(nmax_sites)
      character*4,site
c     number of used sites
      integer*4 nsite,ind

c     mjd for all observations
      real*8 mjd_all(nmax_row,nmax_sites),dyrs(nmax_row)
      integer*4 years(nmax_row),doyrs(nmax_row)
      real*8 mjd,mjd_starts(nmax_sites),mjd_ends(nmax_sites)
      real*8 mjd_start,mjd_end
      integer*4 jdyr,jddoy,jdsec
      integer*4 dates(5),year,doyr,ymd(3)
      real*8 secs,dyr,sod
      
c     CME sites index, number of CME sites in CME-site-file
      integer*4 cmeis(nmax_sites),nsite_cme

c     actual number of CME sites in data
      integer*4 nsite_cme_used

c     current day CME correction,CME correction for all days/all components
      real*8 cmec,cmecs(nmax_row,3),oCMEs(nmax_row,6)

c$$$c     array index for N/E/U,current NEU index
c$$$      integer*4 neuis(3),neui
c$$$      integer*4 neueis(3)

c     temporary variables
      real*8 tmp,tmp1,tmp2,tmp3
      real*8 tmp1a,tmp1b,tmp2a,tmp2b,tmp3a,tmp3b
      integer*4 tmp_ncme
c     home directory
      character*1023 path_home,tmpstr,tmpstr2,prog

c     method
      integer*4 mi
c     mi:
c       1 ~ Wdowinski 1997 (JGR-SE)
c       2 ~ Nikolaidis 2002 (Ph.D. Dissertation)
c       3 ~ M...
c
c     external functions
      integer*4 where
      integer iargc,nblen

c     
c     ---
      prog='cme_cal_stacking'


C     Initial Parameters
c     stacking method, currently only mi=1 regional stacking [Wdowinksi, et al., 1997]
      mi=1
c     get cme-sites names
      sitefile='~/gpsf/cgps/conf/site.cme.test'
      sitefile='/home/tianyf/tmp/tibet/cme.sit'
      sitefile=''
      nsite_cme=0
      ofile=''
c     if void, write CME to the screen
      opath=''
c     if not void, write the filtered time series

      if (iargc().lt.1) then
         write(*,'(a)') 'Usage: cme_cal_stacking path',
     &        ' [--opath=OUT_FILTERED_PATH]',
     &        ' [--ofile=OUT_FILE]',
     &        ' [--cmesites=SIT1,SIT2,SIT3,SIT4,SIT5,SIT6,...]',
     &        ' [--cmesitefile=CME_SITE_FILE]'
         write(*,'(10a)') 'For more information, please see',
     &        ' cme_cal_stacking man page.'
         stop
      endif

c     get input path
      call getarg(1,path)
      write(*,'(2a)') '*working in: ',path(1:nblen(path))
      filter='*.neu'
      nf=-1
      call ffind (path,files,filter,nf,1) 
      write(*,'(a,i9)') '#INFO:#files:',nf
      if (nf.lt.3) then
         write(*,'(a)') '#ERROR:too few sites.'
         stop
      endif

c     handling optional parameters
      do i=2,iargc() 
         call getarg(i,tmpstr)
c            write(*,*) tmpstr(1:nblen(tmpstr))
c     --sites list used to derive CME
            if (tmpstr(1:11).eq.'--cmesites=') then
               write(*,'(a)') '#using cme sites:'//
     +              tmpstr(12:nblen(tmpstr))
                tmpstr2=tmpstr(12:nblen(tmpstr))
                call strsplit(tmpstr2,',',nsite_cme,sites_cme)
                write(*,'(a)') '#cme sites:',nsite_cme
c     --sites-list file contains site names to derive CME (iGPS *.sit format)
            else if (tmpstr(1:14).eq.'--cmesitefile=') then
               sitefile=tmpstr(15:nblen(tmpstr))  
               write(*,'(a)') '#using site file:'//
     +              sitefile(1:nblen(sitefile)) 
               if (sitefile(1:1).eq.'~') then
                  call getenv('HOME',path_home)
                  sitefile=path_home(1:nblen(path_home))//
     .                 sitefile(2:nblen(sitefile))
               endif
               call rdsit_(sitefile,sites_cme,nmax_sites,nsite_cme)
               write(*,'("*",8a5)') (sites_cme(j),j=1,nsite_cme)
               if (nsite_cme.lt.1) then
                  write(*,'(a)') '#WARNING: no CME sites found.'
                  stop
               endif
c     --output path where filtered time series generated
            else if (tmpstr(1:8).eq.'--opath=') then
               write(*,'(a)') '#using dir:'//tmpstr(7:nblen(tmpstr))
               opath=tmpstr(9:nblen(tmpstr))
c     --output CME file
            else if (tmpstr(1:8).eq.'--ofile=') then
               ofile=tmpstr(9:nblen(tmpstr))  
               write(*,'(a)') '#out CME file:'//
     +              ofile(1:nblen(ofile))
           else
               write(*,'(3a)') 
     +              '#ERROR:invalid parameter input[',
     +              tmpstr(1:nblen(tmpstr)),']!!'
               stop
            endif
         enddo

c     First, read all data
      nsite=0
      mjd_start=0
      mjd_end=0
c     loop read data of each file
      do fi=1,nf
         file=files(fi)
         write(*,702) fi,nf,file(1:nblen(file))
 702     format('reading.. ',i5,'/',i5,1x,a)
c     goto 801
c$$$  #
c$$$  ##################################################################
c$$$  2006.7740 2006 283 -0.0019 -0.0106  0.0036  0.0035  0.0026  0.0050
c$$$  2006.7767 2006 284  0.0033 -0.0075 -0.0024  0.0035  0.0025  0.0049
c$$$  2006.7795 2006 285 -0.0033 -0.0046 -0.0084  0.0033  0.0026  0.0047
c$$$  2006.7822 2006 286  0.0003 -0.0084 -0.0151  0.0033  0.0026  0.0046
         call read_sio(file, data, nrow, ncol, nhead, headers)
c     only process time series whose length is longer than 1000
c$$$         if (nrow.lt.3000) then
c$$$            write(*,*) ' !! too short. Skipping...'
c$$$            goto 801
c$$$         endif
         nsite=nsite+1
         write(*,'("*",4x,a,2(1x,i9))') '#row,col:',nrow,ncol
         nrows(nsite)=nrow
         ncols(nsite)=ncol
         do i=1,nrow
            do j=1,ncol
               data_all(i,j,nsite)=data(i,j)
            enddo
c     convert year/doy to mjd
            jdyr=data(i,2)
            jddoy=data(i,3)
            jdsec=0
            call yds_to_jd(jdyr,jddoy,jdsec,mjd)
            mjd=mjd-2.4d6
            mjd_all(i,nsite)=mjd
c     write(*,*) mjd_all(i,nsite),i,nsite
c     write(*,'(9f11.5)') (data(i,j),j=1,ncol)
         enddo
         jdyr=data(1,2)
         jddoy=data(1,3)
         jdsec=0
         call yds_to_jd(jdyr,jddoy,jdsec,mjd)
c     mjd=mjd-2400000.5d0
         mjd=mjd-2.4d6
         mjd_starts(nsite)=mjd
c     write(*,*) 'MJD:',mjd_start,'~',mjd_end,':',mjd,data(1,3)
         if (nsite.eq.1) then
            mjd_start=mjd
         endif
         if (mjd.lt.mjd_start) then
            mjd_start=mjd
         endif
         jdyr=data(nrow,2)
         jddoy=data(nrow,3)
         call yds_to_jd(jdyr,jddoy,jdsec,mjd)
c     mjd=mjd-2400000.5d0
c     Error?
c     Should be:?
         mjd=mjd-2.4d6
         mjd_ends(nsite)=mjd
c         write(*,*) mjd_ends(nsite)
         if (nsite.eq.1) mjd_end=mjd
         if (mjd.gt.mjd_end) mjd_end=mjd
c     write(*,*) 'MJD:',mjd_start,' ~',mjd_end,':',mjd

         call getfilename(file, site)
         sites(nsite)=site(1:4)
c     write(*,'(a)') site(1:4)
c     do i=nrow-5,nrow
c     write(*,'(9f11.5)') (data(i,j),j=1,ncol)
c     enddo
 801     continue
      enddo
      write(*,'(a,i9)') '*#sites:',nsite
      write(*,'(2(a,f20.8))') '*MJD:',mjd_start,' ~',mjd_end
      ndays=int(mjd_end-mjd_start+1)
c     create date information for dataAll
      do i=0,ndays-1
         mjdAll(i)=mjd_start+i
         call mjd_to_ymdhms(mjdAll(i),dates,secs)
         ymd(1)=dates(1)
         ymd(2)=dates(2)
         ymd(3)=dates(3)
         call ymd_to_doy(ymd,doyr)
         call jd_to_decyrs(mjdAll(i)+2400000.5d0,dyr)
         dyrs(i)=dyr
         years(i)=dates(1)
         doyrs(i)=doyr
      enddo
c     
      write(*,'(10a)') '*Reshape data matrix ...'
      do i=1,nsite
         do j=1,nrows(i)
            ind=where(mjdAll,nmax_row,ndays,mjd_all(j,i))
            do k=1,ncol
               dataAll(ind,k,i)=data_all(j,k,i)
            enddo
c            write(*,'(10(1x,f10.5))') (dataAll(ind,k,i),k=1,ncol)
         enddo
c         stop
      enddo
c      stop


c     if no CME sites given, then use all sites to calcualte CME.
      if(nsite_cme.eq.0) then
         do i=1,nsite
            sites_cme(i)=sites(i)
         enddo
         nsite_cme=nsite
      endif

      write(*,'(a)') '*sites:'
c     lower case the site names
      do i=1,nsite
         tmpstr=sites(i)
         call lowers(tmpstr)
         sites(i)=tmpstr
      enddo
      do i=1,nsite_cme
         tmpstr=sites_cme(i)
         call lowers(tmpstr)
         sites_cme(i)=tmpstr
      enddo
c     echo sites
      do i=1,nsite,8
         write(*,'("*",8a5)'),(sites(j),j=i,i+7)
      enddo
      write(*,'(a)') '*CME sites:'
      do i=1,nsite_cme,8
         write(*,'("*",8a5)'),(sites_cme(j),j=i,i+7)
      enddo

      nsite_cme_used=0
      do i=1,nsite_cme
         do j=1,nsite
            if (sites_cme(i).eq.sites(j)) then
               nsite_cme_used=nsite_cme_used+1
               cmeis(nsite_cme_used)=j
               goto 803
            endif
         enddo
 803     continue
      enddo
      write(*,'(a)') '*CME site index:'
      do i=1,nsite_cme_used,8
         write(*,'("*",8(1x,i6))') (cmeis(j),j=i,i+7)
      enddo

c     calculate CME for each day
c$$$      neuis(1)=4
c$$$      neuis(2)=5
c$$$      neuis(3)=6
c$$$      neueis(1)=7
c$$$      neueis(2)=8
c$$$      neueis(3)=9
c     loop for each day
      do i=1,ndays
         tmp1a=0d0
         tmp1b=0d0
         tmp2a=0d0
         tmp2b=0d0
         tmp3a=0d0
         tmp3b=0d0
         tmp1=0d0
         tmp2=0d0
         tmp3=0d0
         tmp_ncme=0
c     write(*,*) '#cme site:',nsite_cme_used
         do k=1,nsite_cme_used
c            write(*,*) 'dataAll:',dataAll(i,cmeis(k),1)
            if(dataAll(i,1,cmeis(k)).le.1d-6) goto 804
            tmp1=tmp1+dataAll(i,4,cmeis(k))
c            write(*,*) tmp1,dataAll(i,4,cmeis(k)),dataAll(i,1,cmeis(k))
            tmp2=tmp2+dataAll(i,5,cmeis(k))
            tmp3=tmp3+dataAll(i,6,cmeis(k))
            tmp_ncme=tmp_ncme+1
c            write(*,*) 'dataAll:',dataAll(i,1,cmeis(k)),i,
c     +           dataAll(i,4,cmeis(k))
c            stop
 804        continue
         enddo
         if (tmp_ncme.lt.3) then
            write(*,'(a,f20.5,i6)') '*Warning: no CME derived for day ',
     +           mjdAll(i),tmp_ncme
c            stop
            cmecs(i,1)=0
            cmecs(i,2)=0
            cmecs(i,3)=0
         else
c            write(*,*) tmp1,tmp2,tmp3,tmp_ncme
c            stop
            if (mi.eq.1) then
               cmecs(i,1)=tmp1/tmp_ncme
               cmecs(i,2)=tmp2/tmp_ncme
               cmecs(i,3)=tmp3/tmp_ncme
            endif
            if (mi.eq.2) then
               cmecs(i,1)=tmp1a/tmp1b
               cmecs(i,2)=tmp2a/tmp2b
               cmecs(i,3)=tmp3a/tmp3b
            endif
         endif
c         stop
c         write(*,*) 'tmp1b:',tmp1b
c          write(*,701) mjd,mjd_end,(cmecs(i,j),j=1,3),tmp_ncme
 701     format('cme correction for',f10.2,'/',f10.2,':',3f10.5,' #',i5)

c         write(*,703) dyr,dates(1),doyr,(cmecs(i,j),j=1,3)
 703     format(1x,f10.5,1x,i4.4,1x,i3.3,1x,3f10.5)

      enddo
      write(*,'(a,i10)') '#days:',ndays
      if(nblen(ofile).gt.0) then
         call getlun(fid)
         open(unit=fid,file=ofile,iostat=ioerr)
         if(ioerr.ne.0) then
            write(*,'(10a)') '*Error: cannot open output CME file',
     +           ofile(1:nblen(ofile)),'!!!'
            stop
         endif
      else
         fid=6
      endif
      do i=1,ndays
         write(fid,703) dyrs(i),years(i),doyrs(i),(cmecs(i,j),j=1,3)
      enddo
      if(fid.ne.6) close(fid)

      
c     perform CME corrections for each site/ each day
      if(nblen(opath).le.0) goto 999
      do i=1,nsite
         site=sites(i)
         ofile=opath(1:nblen(opath))//'/'//site(1:nblen(site))
     +        //'Flt.neu'
         write(*,'(1000a)') '*out to ',ofile(1:nblen(ofile)),' ...' 
         do j=1,ndays
c     first, skip no data days
            if(dataAll(j,1,i).le.1d-6) then
              odata(j,1)=0
              goto 802
            endif
            odata(j,1)=dyrs(j)
            odata(j,2)=years(j)
            odata(j,3)=doyrs(j)
            odata(j,4)=dataall(j,4,i)-cmecs(j,1)
            odata(j,5)=dataall(j,5,i)-cmecs(j,2)
            odata(j,6)=dataall(j,6,i)-cmecs(j,3)
            do k=7,ncol
               odata(j,k)=dataAll(j,k,i)
            enddo
 802        continue
         enddo
c     write filtered time series
         call getlun(fid)
         open(unit=fid,file=ofile,iostat=ioerr)
         if(ioerr.ne.0) then
            write(*,'(10a)') '*Error: cannot open out filtered file',
     +           ofile(1:nblen(ofile)),'!!!'
            stop
         endif
         do j=1,ndays
            if(odata(j,1).le.1d-6) goto 805
            year=odata(j,2)
            doyr=odata(j,3)
            write(fid,704) odata(j,1),year,doyr,(odata(j,k),k=4,ncol)
 704        format(1x,f10.5,1x,i4,1x,i3,1x,3f12.7,1000(1x,f12.7))
 805        continue
         enddo
         close(fid)
c         stop
      enddo
 999  continue
      write(*,'(a)'), '['//prog(1:nblen(prog))//']Normal end.'

      end
