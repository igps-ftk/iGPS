      program simul_corr_length
C     Input:
c     path - 
c     ofile - 
C     ---
      IMPLICIT NONE
      include '../../inc/cgps.h'
c      integer*4 nmax_sites,nmax_proc
c      parameter(nmax_sites=2000)
c      parameter(nmax_proc=700)
C     ---
      character*1023 path,opath,ofile,ofilebd,ofilebk,tmpstr
      character*1023 files(nmax_sites),sites(nmax_sites)
      character*4,site,site1,site2
      real*8 ts1(nmax_row),ts2(nmax_row)
      real*8 mjd1(nmax_row),mjd2(nmax_row)
      integer*4 nrows(nmax_sites),ncols(nmax_sites)
      
c     maximum number of epochs to calculate correlation
      integer*4 nmax_cal,nmax_cal_out
      
      
      character*1023 filter,file
      real*8 mat_corr(nmax_sites,nmax_sites)
      real*8 mat_corrs(nmax_sites,nmax_sites,3)
      real*8 mat_blen_km(nmax_sites,nmax_sites)
      real*8 mat_blen_deg(nmax_sites,nmax_sites)
      integer*4 nsames(nmax_sites,nmax_sites),nsame
      integer*4 fid,fido,fidobd,fidobk,ioerr
      integer*4 nf,i,j,fi,k,l,m,n,nline
      integer*4 nrow, ncol, nhead
      character*512 headers(nmax_head)
      real*8 data_all(nmax_row,nmax_col,nmax_proc)
      real*8 data(nmax_row,nmax_col)
      real*8 tmpjd,tmpmjd
      real*8 epoch, fraction, mjd, seconds
      integer*4 day, date(5)
      integer*4 yr, doy, sec
      real tmp
      character*100 fmtstr
      integer nblen,nrem
      real*8 corr_xy,corr_xy2,corr_xy3
      real*8 lin_corr_xy,lin_corr_xy2,lin_corr_xy3
      real*8 dist,xyz1(3),xyz2(3),ll1(3),ll2(3),llh(5000,3)
      character*1024 coords_file,snxfile,home
c     data index
      integer*4 ind_time,ind_neu(3),nneu,neui,nfmax

c     for storing number of the same days
      integer*4 fidoe
      character*1023 ofilee,fmtstre

      integer*4 iargc,intlen
c     function name ilen cannot work in RHEL-5.
c     is there already a funtion called ilen in RHEL-5?
      
c     data type: network, agency
      character*80 prext_net,prext_org
      
      character*1 neustr(3)
c      real*8 ceiling
      integer*4 perc

c     for map_2points
      integer isradian,ismeter,ismile
      real*8 radius

C     ---
      if (iargc().lt.2) then
         write(*,*) 'Usage: simul_corr_length input_path output_path'
         write(*,*) '         [--snxfile=snxfile]'
         write(*,*) '         [--nfmax=NFMAX]'
         write(*,*) '         [--netext=NETEXT]'
         write(*,*) '         [--orgext=ORGEXT]'
         write(*,*) '         [--filter=FILTER]'
         write(*,*) '         [--nmaxcal=NMAXCAL]'
         write(*,*) '         [--coordsfile=COORDS_FILE]'
         write(*,*) ' Program limits:'
         write(*,*) '   Max number of sites: ',nmax_sites
         write(*,*) '   Max number of processing:',nmax_proc
         write(*,*) '   Max number of rows:',nmax_row
         write(*,*) '   Max number of columns:',nmax_col
         write(*,*) '   Max number of headers:',nmax_head
         write(*,*) 'Please edit those variables in ${gpsf}/inc/cgps.h.'
         stop
      endif

c     **The first TWO command-line parameters must be INPUT and OUTPUT paths.
      call getarg(1,path)
      write(*,'(2a)') ' [ts_corr] working in: ',path(1:nblen(path))
      call getarg(2,opath)
      write(*,'(2a)') ' [ts_corr] output to:',opath(1:nblen(opath))
      
      nfmax=0
      snxfile=''
      prext_net='UNKNOWN'
      prext_org='UNKNOWN'
      filter='*.neu'
      call getenv("HOME",home)
      coords_file=home(1:nblen(home))//pathsep//
     &     'gpsf/cgps/conf/sio.cmonoc.bjgps.llhxyz'
c     default to use all epochs in calculating correlation
      nmax_cal=-1
c     

c     **Get extra command-line parameters.
      do i=3,iargc()
         call getarg(i,tmpstr)
         if (tmpstr(1:10).eq.'--snxfile=') then
            write(*,*) 'Output SINEX:'//tmpstr(8:nblen(tmpstr))
            snxfile=tmpstr(11:nblen(tmpstr))
         else if (tmpstr(1:8).eq.'--nfmax=') then
            read(tmpstr(9:nblen(tmpstr)),*) nfmax
         else if (tmpstr(1:9).eq.'--netext=') then
            prext_net=tmpstr(10:nblen(tmpstr))
         else if (tmpstr(1:9).eq.'--orgext=') then
            prext_org=tmpstr(10:nblen(tmpstr))  
         else if (tmpstr(1:10).eq.'--nmaxcal=') then
            read(tmpstr(11:nblen(tmpstr)),*) nmax_cal
         else if (tmpstr(1:9).eq.'--filter=') then
            filter=tmpstr(10:nblen(tmpstr))
         else if (tmpstr(1:13).eq.'--coordsfile=') then
            coords_file=tmpstr(14:nblen(tmpstr))  
         else
            write(*,'(2a)') ' [ts_corr]ERROR: invalid parameter [',
     &           tmpstr(1:nblen(tmpstr))
            stop
         endif
      enddo
            
c      write(*,*) nmax_cal
c      stop

      if (snxfile.eq.'') then    
         if (nmax_cal.eq.-1) then
            write(tmpstr,'(i5.5)') 0
         else
            write(tmpstr,'(i5.5)') nmax_cal
         endif
         snxfile=opath(1:nblen(opath))//pathsep//
     &        prext_net(1:nblen(prext_net))//'_'//
     &        prext_org(1:nblen(prext_org))//'_'//
     &        'neu'//'_'//tmpstr(1:nblen(tmpstr))//'.snx'
      endif

      ind_time=1
      ind_neu(1)=4
      ind_neu(2)=5
      ind_neu(3)=6
      nneu=3

      neustr(1)='N'
      neustr(2)='E'
      neustr(3)='U'

c      pathsep='/'
      call ffind (path,files,filter,nf,1) 
      write(*,'(a,i5,a)') ' [ts_corr] found ',nf,' sites.'
      if (nf.gt.nmax_proc) then
         nf=nmax_proc
c         stop
      endif

      if (nfmax.gt.0.and.nf.gt.nfmax) then
         nf=nfmax
      endif

c     read all files
      write(fmtstr,'(a,i1,a)')  '(" [ts_corr] reading ",a,"(",i',
     &     intlen(nf),
     &     ',"/",i3,"%)..")' 
      do fi=1,nf
         file=files(fi)
         call getfilename(file,tmpstr)
         perc=int(fi*1.d0/nf*100)
         write(*,fmtstr) 
     &        tmpstr(1:nblen(tmpstr)),
     &        fi,
     &        perc
         call read_sio(file,data,nrow,ncol,nhead,headers)
         nrows(fi)=nrow
         ncols(fi)=ncol
         do i=1,nrow
            do j=1,ncol
               data_all(i,j,fi)=data(i,j)
            enddo
c            write(*,'(9f11.5)') (data(i,j),j=1,ncol)
         enddo
         call getfilename(file, site)
         sites(fi)=site(1:4)
c         write(*,'(a)') site(1:4),sites(fi)
c         do i=nrow-5,nrow
c            write(*,'(9f11.5)') (data(i,j),j=1,ncol)
c         enddo
      enddo

      write(*,'(a)') ' [ts_corr] calculating distances...'      
      write(*,'(2a)') ' [ts_corr] coords_file:',
     &     coords_file(1:nblen(coords_file))
c     read in all apriori coordinates
      do i=1,nf
         site1=sites(i)
c         call read_neuxyz(coords_file,site1,xyz1,ll1)
         call read_net(coords_file,site1,ll1)
         if (ll1(1).eq.-9999) then
            write(*,'(4a)') '[ts_corr]WARNING: no apriori position',
     &           ' found for site "',site1,'"!'
         endif
         llh(i,1)=ll1(1)
         llh(i,2)=ll1(2)
         llh(i,3)=ll1(3)
      enddo

c      write(*,*) 'sites:',(sites(i),i=1,30)
      isradian=0
      ismeter=0
      ismile=0
      radius=0d0
c     === First, calculate distances.
      do i=1,nf
         site1=sites(i)
c         call site_coords_query(coords_file,site1,xyz1,ll1)
c$$$         call read_neuxyz(coords_file,site1,xyz1,ll1)
c$$$         llh(i,1)=ll1(2)
c$$$         llh(i,2)=ll1(1)
c$$$         llh(i,3)=ll1(3)
         ll1(1)=llh(i,1)
         ll1(2)=llh(i,2)
         ll1(3)=llh(i,3)
         do j=i+1,nf
            site2=sites(j)
c            call site_coords_query(coords_file,site2,xyz2,ll2)
c$$$            call read_neuxyz(coords_file,site2,xyz2,ll2)
            ll2(1)=llh(j,1)
            ll2(2)=llh(j,2)
            ll2(3)=llh(j,3)
c            write(*,*) site1,' ',site2,ll1,ll2
c            isRadian=0
            isMeter=1
c            call map_2points(ll1(2),ll1(1),ll2(2),ll2(1),
c     &           isRadian,isMeter,isMile,radius,dist)
            call map_2points(ll1(1),ll1(2),ll2(1),ll2(2),
     &           isRadian,isMeter,isMile,radius,dist)
            mat_blen_km(i,j)=dist/1000
            mat_blen_km(j,i)=dist/1000
c            isRadian=1
            isMeter=0
c            write(*,'(2(a4,3f,1x),f)') site1,ll1,site2,ll2,dist/1d3
c     A bug was found on Tue Sep  1 11:52:43 CST 2009
c     The order of longitude and latitude of map_2points input was wrong.
c            call map_2points(ll1(2),ll1(1),ll2(2),ll2(1),
c     &           isRadian,isMeter,isMile,radius,dist)
c     The RIGHT order is:
            call map_2points(ll1(1),ll1(2),ll2(1),ll2(2),
     &           isRadian,isMeter,isMile,radius,dist)
c            write(*,'(2(a4,3f,1x),f)') site1,ll1,site2,ll2,dist
            mat_blen_deg(i,j)=dist
            mat_blen_deg(j,i)=dist
c            stop
         enddo
c         write(*,'(10f6.1)') (mat_blen_deg(i,j),j=1,nf)
c$$$         write(fidobd,fmtstr) sites(i),(mat_blen_deg(i,j),j=1,nf)
c$$$         write(fidobk,fmtstr) sites(i),(mat_blen_km(i,j),j=1,nf)
c         stop
      enddo

c     write distance files
      call getlun(fidobd)
      ofilebd=opath(1:nblen(opath))//pathsep//
     &     prext_net(1:nblen(prext_net))//'_'//
     &     prext_org(1:nblen(prext_org))//'_'//
     &     'baseline_deg.bln'
      open(unit=fidobd,file=ofilebd)
      call desuffix(ofile,ofilebk)
      ofilebk=opath(1:nblen(opath))//pathsep//
     &     prext_net(1:nblen(prext_net))//'_'//
     &     prext_org(1:nblen(prext_org))//'_'//
     &     'baseline_km.bln'
      call getlun(fidobk)
      open(unit=fidobk,file=ofilebk)
      write(fmtstr,'(a,i5,a)') '(a4,1x,',nf,'(8x,a4))'
      write(*,'(" [ts_corr] blen/deg: ",a)') ofilebd(1:nblen(ofilebd))
      write(*,'(" [ts_corr] blen/km: ",a)') ofilebk(1:nblen(ofilebk))
      write(fidobd,fmtstr) ' ',(sites(i),i=1,nf)
      write(fidobk,fmtstr) ' ',(sites(i),i=1,nf)
      write(fmtstr,'(a,i5,a)') '(a4,1x,',nf,'f12.3)'
      do i=1,nf
         write(fidobd,fmtstr) sites(i),(mat_blen_deg(i,j),j=1,nf)
         write(fidobk,fmtstr) sites(i),(mat_blen_km(i,j),j=1,nf)
      enddo
      close(fidobd)
      close(fidobk)


C     === Do correlation analysis
      write(*,'(a)') 
     &     ' [ts_corr] calculating correlation coefficients...'
      call getlun(fido)
      fidoe=fido+1
      do neui=1,nneu
c     open file for writing
         if (fido.ne.6) then   
            if (nmax_cal.eq.-1) then
               write(tmpstr,'(i5.5)') 0
            else
               write(tmpstr,'(i5.5)') nmax_cal
            endif
            ofile=opath(1:nblen(opath))//pathsep//
     &           prext_net(1:nblen(prext_net))//'_'//
     &           prext_org(1:nblen(prext_org))//'_corr_'//
     &           neustr(neui)//'_'//tmpstr(1:nblen(tmpstr))//'.tcm'
c            call desuffix(ofile,tmpstr)
c            write(ofile,'(a,i1,".corr")') 
c     &           tmpstr(1:nblen(tmpstr)),neui
c            write(*,*) ofile(1:nblen(ofile))
c            goto 899
            open(unit=fido,file=ofile)    
       
            write(*,'(" [ts_corr] corr/",a1,": ",a)') neustr(neui), 
     &           ofile(1:nblen(ofile))
c            goto 899
            
            
            ofilee=opath(1:nblen(opath))//pathsep//
     &           prext_net(1:nblen(prext_net))//'_'//
     &           prext_org(1:nblen(prext_org))//'_corr_'//
     &           neustr(neui)//'_'//tmpstr(1:nblen(tmpstr))//'.nse'
            open(unit=fidoe,file=ofilee)
  

c     nepoch output
c            ofileN=opath(1:nblen(opath))//pathsep//
c     &           prext_net(1:nblen(prext_net))//'_'//
c     &           prext_org(1:nblen(prext_org))//'_corr_'//
c     &           neustr(neui)//'_'//tmpstr(1:nblen(tmpstr))//'.nepoch'
c            call getlun(fidoN)
c            open(unit=fidoN,file=ofileN) 

         endif
c     output file header (sites names)
c$$$c      write(fmtstr,'("(a4,1x,",i,"a7)")') nf
c$$$      write(fmtstr,'(a,i5,a)') '(a4,1x,',nf,'a12)'
c$$$c      write(*,*) fmtstr(1:nblen(fmtstr))
c$$$      write(fido,fmtstr) ' ',(sites(i),i=1,nf)
      write(fmtstr,'(a,i5,a)') '(a4,1x,',nf,'(8x,a4))'
      write(fido,fmtstr) ' ',(sites(i),i=1,nf)
      write(fidoe,fmtstr) ' ',(sites(i),i=1,nf)
c      write(fidoN,fmtstr) ' ',(sites(i),i=1,nf)
      
c     write(fmtstr,'("(a4,1x,",i5,"f11.5)")') nf
      write(fmtstr,'(a,i5,a)') '(a4,1x,',nf,'f12.5)'  
      write(fmtstre,'(a,i5,a)') '(a4,1x,',nf,'i12)' 
c      write(fmtstrN,'(a,i5,a)') '(a4,1x,',nf,'i12)'       

      sec=0
      do i=1,nf
         site1=sites(i)
         do k=1,nrows(i)
            ts1(k)=data_all(k,ind_neu(neui),i)
c            call decyrs_to_jd ( data_all(k,ind_time,i), tmpmjd )
c            mjd1(k)=tmpmjd-0.5
            yr=int(data_all(k,2,i))
            doy=data_all(k,3,i)
c            call YMDHMS_to_MJD ( date, seconds, epoch )
            call yds_to_jd (yr,doy,sec,tmpmjd )
            mjd1(k)=tmpmjd
c            write(*,'(i5,1x,3f21.5)') k,ts1(k),mjd1(k),data_all(k,1,i)
         enddo
         mat_corr(i,i)=1d0
         mat_corrs(i,i,neui)=1d0
         do j=i+1,nf
            site2=sites(j)
            do k=1,nrows(j)
               ts2(k)=data_all(k,ind_neu(neui),j)
c               write(*,*) ts2(k)
c               mjd2(k)=data_all(k,1,j)
               yr=int(data_all(k,2,j))
               doy=data_all(k,3,j)
               call yds_to_jd (yr,doy,sec,tmpmjd )
               mjd2(k)=tmpmjd
            enddo
            if (nmax_cal.ne.-1.and.
     &           nmax_cal.lt.nrows(i).and.nmax_cal.lt.nrows(j)) then
c               tmp=corr_xy3(mjd1,ts1,nmax_row,nrows(i),
               tmp=lin_corr_xy3(mjd1,ts1,nmax_row,nrows(i),

     &              mjd2,ts2,nmax_row,nrows(j),
     &              nsame,nmax_cal,nmax_cal_out)
               nsames(i,j)=nmax_cal_out
               nsames(j,i)=nmax_cal_out
            else
c               tmp=corr_xy2(mjd1,ts1,nmax_row,nrows(i),
               tmp=lin_corr_xy2(mjd1,ts1,nmax_row,nrows(i),
     &              mjd2,ts2,nmax_row,nrows(j),
     &              nsame)
               nsames(i,j)=nsame
               nsames(j,i)=nsame
            endif
c            write(*,*) tmp
            mat_corr(i,j)=tmp
            mat_corr(j,i)=tmp
            mat_corrs(i,j,neui)=tmp
            mat_corrs(j,i,neui)=tmp
c            nepochs(i,j,neui)=nepoch
c            nepochs(j,i,neui)=nepoch
c            write(*,*) site1,' ',site2,':',tmp
c            stop
         enddo         
         write(fido,fmtstr) sites(i),(mat_corr(i,j),j=1,nf)
         write(fidoe,fmtstre) sites(i),(nsames(i,j),j=1,nf)
c         write(fidoN,fmtstrN) sites(i),(nepochs(i,j,neui),j=1,nf)

      enddo


      close(fido)
      close(fidoe)
c      close(fidoN)
   

c     -->Write results into a SINEX-like file.
      open(unit=fido,file=snxfile)
      write(fido,'(a)') '+SITE/ID'
      write(fido, 800) '*Code','Longitude','Latitude','Height'
 800  FORMAT(A5,1X,3(A20,1X),:)
      do i=1, nf 
         write(FIDo,801) SITES(I),(llh(i,j),j=1,3)
 801     FORMAT(A4,2X,3(F20.8,1X),:)
      enddo
      write(FIDo,'(a)') '-SITE/ID'
    
      dO i=1, 3 
         write(fido,'(a)') '+CORR/'//neustr(i)
         write(fmtstr,'("(2(I",i1,",1x),3(e20.12,1x),:))")') intlen(nf)
c         write(*,*) fmtstr

         do j=1,nf
            nline=int(nf/3.d0)
            if (mod(nf,3).ne.0) then
               nline=nline+1
            endif
            do k=1,nline
               if (k*3.gt.nf) then
                  nrem=nf-(k-1)*3
               else
                  nrem=3
               endif
c               write(*,*) j,k*3-2,nrem
               write(fido,fmtstr),j,k*3-2, 
     &              (mat_corrs(j,k*3-2+l-1,i),l=1,nrem)
            enddo
            
         enddo
         write(FIDo,'(a)')  '-CORR/'//neustr(i)
      enddo
      
      write(fido,'(a)') '+BLEN/DEG'
      write(fmtstr,'("(2(I",i1,",1x),3(e20.12,1x),:))")') intlen(nf)
c      write(*,*) fmtstr

      do j=1,nf
         nline=int(nf/3.d0)
         if (mod(nf,3).ne.0) then
            nline=nline+1
         endif
         do k=1,nline
            if (k*3.gt.nf) then
               nrem=nf-(k-1)*3
            else
               nrem=3
            endif
c            write(*,*) j,k*3-2,nrem
            write(fido,fmtstr),j,k*3-2, 
     &           (mat_blen_deg(j,k*3-2+l-1),l=1,nrem)
         enddo
            
      enddo
      write(FIDo,'(a)')  '-BLEN/DEG'

      write(fido,'(a)') '+BLEN/KM'
c      write(fmtstr,'("(2(I",i,",1x),3(f20.3,1x)))")') 5
      write(fmtstr,'("(2(I",i1,",1x),3(e20.12,1x),:))")') intlen(nf)
c      write(*,*) fmtstr
       
      do j=1,nf
         nline=int(nf/3.d0)
         if (mod(nf,3).ne.0) then
            nline=nline+1
         endif
         do k=1,nline
            if (k*3.gt.nf) then
               nrem=nf-(k-1)*3
            else
               nrem=3
            endif
c            write(*,*) j,k*3-2,nrem
            write(fido,fmtstr),j,k*3-2, 
     &           (mat_blen_km(j,k*3-2+l-1),l=1,nrem)
         enddo
            
      enddo
      write(FIDo,'(a)')  '-BLEN/KM'

      close(fido)
      
 899  continue
      enddo
      end
