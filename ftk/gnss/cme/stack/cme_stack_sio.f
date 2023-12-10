CTITLE cme_stack_sio
c234567890	
C     Algorithm
c     (1).Read in all raw time series;
c     (2).Get sites names used for CME calcualtions;
c     (3).Derive the residual time series for CME sites;
c     (4).Calculate CME based upon residual time seris and estiamtes errors;
c     (5).Perform CME correction to all raw time series.
c     (6).Output the results to files.
      program cme_stack_sio
c     ---
c     Inputs:
c     path - 
c     opath - 
c     sitefile - 
c     yr -
c     doy -
c     ndays -
c     --
      IMPLICIT NONE
      include '../../../inc/ftk.h'
c     --
      integer*4 nmax_sites,nmax_proc
      parameter(nmax_sites=2000)
      parameter(nmax_proc=200)
c     --
      character*512 path,opath,sitefile
      integer*4 yr,doy,ndays
c     --
      integer iargc,nblen
      character*512 filter,file,files(nmax_sites)
      character*4 sites(nmax_sites),sites_cme(nmax_sites)
c     dummy variable
      character*4 sites_nul(nmax_sites)
      integer*4 nsite_nul

      integer*4 nf,i,j,fi,k,l,m,n
      integer*4 nrow, ncol, nhead
      character*512 headers(nmax_head)
      real*8 data_all(nmax_row,nmax_col,nmax_proc)
      real*8 data(nmax_row,nmax_col)
      integer*4 nrows(nmax_sites),ncols(nmax_sites)
      character*4,site
c     number of used sites
      integer*4 nsite

c     mjd for all observations
      real*8 mjd_all(nmax_row,nmax_sites)
      real*8 mjd,mjd_starts(nmax_sites),mjd_ends(nmax_sites)
      real*8 mjd_start,mjd_end
      integer*4 jdyr,jddoy,jdsec
      
c     CME sites index, number of CME sites in CME-site-file
      integer*4 cmeis(nmax_sites),nsite_cme

c     actual number of CME sites in data
      integer*4 nsite_cme_used

c     current day CME correction,CME correction for all days/all components
      real*8 cmec,cmecs(nmax_row,3)

c     site current day index
      integer*4 sitedi(nmax_sites)

c     array index for N/E/U,current NEU index
      integer*4 neuis(3),neui
      integer*4 neueis(3)

c     temporary variables
      real*8 tmp,tmp1,tmp2,tmp3
      real*8 tmp1a,tmp1b,tmp2a,tmp2b,tmp3a,tmp3b
      integer*4 tmp_ncme
c     home directory
      character*512 path_home

c     method
      integer*4 mi
c     mi:
c       1 ~ Wdowinski 1997 (JGR-SE)
c       2 ~ Nikolaidis 2002 (Ph.D. Dissertation)
c       3 ~ M...

c     
c     ---
C     Initial Parameters
      mi=1

      if (iargc().lt.2) then
         write(*,*) 'Syntax: cme_stack_sio path opath'
         write(*,*) '  Optional:[site_file [yr [doy [ndays]]]]'
         stop
      endif

      call getarg(1,path)
      call getarg(2,opath)
      write(*,*) 'path: ',path(1:nblen(path))
      if (iargc().ge.3) then
         call getarg(3,sitefile)
      endif

c     find files
      filter='*.neu'
      nf=-1
      call ffind (path,files,filter,nf,1) 
      write(*,*) 'total files:',nf

c     get cme-sites names
c      sitefile='~/gpsf/cgps/conf/network_scign_cme_sites.txt'
c      sitefile='~/gpsf/cgps/conf/network_scign_cme_sites_test'
c      sitefile='/home/tianyf/gpsf/cgps/conf/network_scign_cme_sites.txt'
      sitefile=
     . '/export/home/tianyf/gpsf/cgps/conf/network_scign_cme_sites_test'
      sitefile=
     . '/export/home/tianyf/gpsf/cgps/conf/network_stacktest_sites_test'
      sitefile=
     . '/export/home/tianyf/gpsf/cgps/conf/network_pbo_cme_sites.txt'
      sitefile='~/gpsf/cgps/conf/network_pbo_cme_sites.txt'
      sitefile='~/gpsf/cgps/conf/site.cme.test'

      if (sitefile(1:1).eq.'~') then
         call getenv('HOME',path_home)
         sitefile=path_home(1:nblen(path_home))//
     .        sitefile(2:nblen(sitefile))
      endif

      call site_read(sitefile,sites_nul,sites_cme,nmax_sites,nsite_nul,
     .     nsite_cme)
      do i=1,nsite_cme,8
         write(*,'(8a6)') (sites_cme(j),j=i,i+7)
      enddo

c     goto 802
c     read in all data
      nsite=0
      mjd_start=0
      mjd_end=0
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
         if (nrow.lt.3000) then
            write(*,*) ' !! too short. Skipping...'
            goto 801
         endif
         nsite=nsite+1
         write(*,*) '#row,col:',nrow,ncol
         nrows(nsite)=nrow
         ncols(nsite)=ncol
c     goto 801
         do i=1,nrow
c     one record/ one day
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
c     write(*,*) 'MJD:',mjd_start,'~',mjd_end,':',mjd

         call getfilename(file, site)
         sites(nsite)=site(1:4)
c     write(*,'(a)') site(1:4)
c     do i=nrow-5,nrow
c     write(*,'(9f11.5)') (data(i,j),j=1,ncol)
c     enddo
 801     continue
      enddo
      write(*,*) '#sites:',nsite
      write(*,*) 'MJD:',mjd_start,'~',mjd_end
      write(*,*) 'sites:'
      do i=1,nsite,8
         write(*,'(8a5)'),(sites(j),j=i,i+7)
      enddo
      write(*,*) 'CME sites:'
      do i=1,nsite_cme,8
         write(*,'(8a5)'),(sites_cme(j),j=i,i+7)
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

      write(*,*) 'CME site index:',(cmeis(i),i=1,nsite_cme_used)

      do i=1,nsite
c     write(*,*) i, mjd_st61arts(i),mjd_ends(i)
      enddo

c     initiate site day index (start from 1)
      do i=1,nsite
         sitedi(i)=1
      enddo

c     calculate CME for each day
      neuis(1)=4
      neuis(2)=5
      neuis(3)=6
      neueis(1)=7
      neueis(2)=8
      neueis(3)=9
      do i=1,mjd_end-mjd_start
         mjd=mjd_start+i-1
c     write(*,*) 'current doy:',mjd
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
c     write(*,*) '#cme site:',nsite_cme
         do k=1,nsite_cme
c     write(*,*) sitedi(cmeis(k)),cmeis(k)
c     write(*,*) mjd_all(1,1)
c     write(*,*) mjd_all(sitedi(cmeis(k)),cmeis(k))
c     write(*,*) mjd_all(1,1)
c            write(*,*) mjd,mjd_all(sitedi(cmeis(k)),cmeis(k))
c     &           ,mjd_ends(cmeis(k))
 804        if (mjd.gt.mjd_all(sitedi(cmeis(k)),cmeis(k)).and.
     &           mjd.lt.mjd_ends(cmeis(k))) then
c     write(*,*) 'add up for ',cmeis(k)
               sitedi(cmeis(k))=sitedi(cmeis(k))+1
               goto 804
            endif

            if (mjd.eq.mjd_all(sitedi(cmeis(k)),cmeis(k))) then
c     for each component
            if (data_all(sitedi(cmeis(k)),neueis(1),cmeis(k)).le.0) then
               goto 805
            endif

            if (mi.eq.1) then
               tmp1=tmp1+data_all(sitedi(cmeis(k)),neuis(1),cmeis(k))
               tmp2=tmp2+data_all(sitedi(cmeis(k)),neuis(2),cmeis(k))
               tmp3=tmp3+data_all(sitedi(cmeis(k)),neuis(3),cmeis(k))
            endif
            if (mi.eq.2) then
               tmp1a=tmp1a+data_all(sitedi(cmeis(k)),neuis(1),cmeis(k))/
     $              (data_all(sitedi(cmeis(k)),neueis(1),cmeis(k))**2)
      tmp1b=tmp1b+1d0/(data_all(sitedi(cmeis(k)),neueis(1),cmeis(k))**2)
               tmp2a=tmp2a+data_all(sitedi(cmeis(k)),neuis(2),cmeis(k))/
     $              (data_all(sitedi(cmeis(k)),neueis(2),cmeis(k))**2)
      tmp2b=tmp2b+1d0/(data_all(sitedi(cmeis(k)),neueis(2),cmeis(k))**2)
               tmp3a=tmp3a+data_all(sitedi(cmeis(k)),neuis(3),cmeis(k))/
     $              (data_all(sitedi(cmeis(k)),neueis(3),cmeis(k))**2)
      tmp3b=tmp3b+1d0/(data_all(sitedi(cmeis(k)),neueis(3),cmeis(k))**2)
c       write(*,*) data_all(sitedi(cmeis(k)),neueis(1),cmeis(k)),tmp1b
            endif
            tmp_ncme=tmp_ncme+1
            endif
 805        continue
         enddo
         if (tmp_ncme.lt.3) then
            cmecs(i,1)=0
            cmecs(i,2)=0
            cmecs(i,3)=0
         else
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
c         write(*,*) 'tmp1b:',tmp1b
          write(*,701) mjd,mjd_end,(cmecs(i,j),j=1,3),tmp_ncme
 701     format('cme correction for',f10.2,'/',f10.2,':',3f10.5,' #',i2)
c     do k=1,nsite_cme
c     tmp=data_all(sitedi(k),neui,cmeis(k))+tmp
c     enddo
      enddo
      stop
c     enddo
      write(*,*) '#days:',mjd_end-mjd_start+1

      
c     perform CME corrections for each site/ each day

c$$$  do i=1,nsite
c$$$  do j=1,mjd_end-mjd_start+1
c$$$  mjd=mjd_start+i-1
c$$$  806	      if (mjd.gt.mjd_all(j,i)) then
c$$$  j=j+1
c     ifort compilation errors:
c$$$  fortcom: Error: cme_stack_sio.f, line 262: An assignment to a DO variable within a DO body is invalid.   [J]
c$$$  j=j+1
c$$$  -----------------^
c     However, this code can compile and run in SUNOS 5.8 (f77).

c     Changed to ...
      goto 802
      do i=1,nsite
c     do j=1,mjd_end-mjd_start+1
         j=1
         if (j.lt.(mjd_end-mjd_start+1)) then
            mjd=mjd_start+j-1
 806        if (mjd.gt.mjd_all(j,i).and.mjd.lt.mjd_ends(i)) then
               j=j+1	 
               goto 806
            endif
            if (mjd.eq.mjd_all(j,i)) then
               write(*,*) data_all(j,neuis(1),i),
     $              data_all(j,neuis(1),i)-cmecs(j,1)
            endif
         endif
      enddo
 802  continue



      print *, '<<END'

      end
