      program ts_corr_mat
C     Input:
c     path - 
c     ofile - 
C     ---
      IMPLICIT NONE
      include '../../inc/cgps.h'
      integer*4 nmax_sites,nmax_proc
      parameter(nmax_sites=2000)
      parameter(nmax_proc=200)
C     ---
      character*512 path,opath,ofile,ofilebd,ofilebk,tmpstr
      character*512 files(nmax_sites),sites(nmax_sites)
      character*4,site,site1,site2
      real*8 ts1(nmax_row),ts2(nmax_row)
      real*8 mjd1(nmax_row),mjd2(nmax_row)
      integer*4 nrows(nmax_sites),ncols(nmax_sites)
      
      character*512 filter,file
      real*8 mat_corr(nmax_sites,nmax_sites)
      real*8 mat_blen_km(nmax_sites,nmax_sites)
      real*8 mat_blen_deg(nmax_sites,nmax_sites)
      integer*4 fid,fido,fidobd,fidobk,ioerr
      integer*4 nf,i,j,fi,k,l,m,n
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
      integer nblen
      real*8 ts_corr
      real*8 dist,xyz1(3),xyz2(3),ll1(2),ll2(2)
      character*512 coords_file
c     data index
      integer*4 ind_time,ind_neu(3),nneu,neui

      integer iargc
      
c     data type: network, agency
      character*80 prext_net,prext_org
      
      character*1 neustr(3)

C     ---
      if (iargc().lt.1) then
         write(*,*) 'Usage: ts_corr_mat input_path [output_path]'
         write(*,*) '  Omit [...] = standard output device' 
         write(*,*) ' *Note: absolute path name should be used.'
         stop
      endif
      call getarg(1,path)
      write(*,'(2a)') 'Working in: ',path(1:nblen(path))
      if (iargc().lt.2) then
         fido=6
         fidobd=fido
         fidobk=fido
      else
         fido=81
         fidobd=82
         fidobk=83
         call getarg(2,opath)
         write(*,'(2a)') 'Output to:',opath(1:nblen(opath))
      endif

      ind_time=1
      ind_neu(1)=4
      ind_neu(2)=5
      ind_neu(3)=6
      nneu=3
      prext_net='PBO'
      prext_org='SIO'
      filter='*.neu'
      neustr(1)='N'
      neustr(2)='E'
      neustr(3)='U'
c      pathsep='/'
      call ffind (path,files,filter,nf,1) 
      write(*,*) 'Found ',nf,' sites.'
      if (nf.gt.nmax_proc) then
         nf=nmax_proc
c         stop
      endif
      
      do fi=1,nf
         file=files(fi)
         write(*,'("reading.. ",i5,1x,a)') fi,file(1:nblen(file))
c         call read_sio_bin(file, data, nrow, ncol, nhead, headers)
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
c         write(*,'(a)') site(1:4)
c         do i=nrow-5,nrow
c            write(*,'(9f11.5)') (data(i,j),j=1,ncol)
c         enddo
      enddo

C     === Do correlation analysis

      do neui=1,nneu
c     open file for writing
         if (fido.ne.6) then
            ofile=opath(1:nblen(opath))//pathsep//
     &           prext_net(1:nblen(prext_net))//'_'//
     &           prext_org(1:nblen(prext_org))//'_'//
     &           neustr(neui)//'.corr'
c            call desuffix(ofile,tmpstr)
c            write(ofile,'(a,i1,".corr")') 
c     &           tmpstr(1:nblen(tmpstr)),neui
c            write(*,*) ofile(1:nblen(ofile))
c            goto 899
            open(unit=fido,file=ofile)
            call desuffix(ofile,ofilebd)
            ofilebd=ofilebd(1:nblen(ofilebd))//'_deg.blen'
            open(unit=fidobd,file=ofilebd)
            call desuffix(ofile,ofilebk)
            ofilebk=ofilebk(1:nblen(ofilebk))//'_km.blen'
            open(unit=fidobk,file=ofilebk)
            write(*,'(a)') ofile(1:nblen(ofile))
            write(*,'(a)') ofilebd(1:nblen(ofilebd))
            write(*,'(a)') ofilebk(1:nblen(ofilebk))
c            goto 899
         endif
c      write(fmtstr,'("(a4,1x,",i,"a7)")') nf
      write(fmtstr,'(a,i5,a)') '(a4,1x,',nf,'a12)'
c      write(*,*) fmtstr(1:nblen(fmtstr))
      write(fido,fmtstr) ' ',(sites(i),i=1,nf)
      if (fido.ne.6) then
         write(fidobd,fmtstr) ' ',(sites(i),i=1,nf)
         write(fidobk,fmtstr) ' ',(sites(i),i=1,nf)
      endif
c     write(fmtstr,'("(a4,1x,",i5,"f11.5)")') nf
      write(fmtstr,'(a,i5,a)') '(a4,1x,',nf,'f12.3)'

      
      coords_file='/export/home/tianyf/gpsf/cgps/conf/sites.xyz.sio'
      sec=0
      do i=1,nf
         site1=sites(i)
         call site_coords_query(coords_file,site1,xyz1,ll1)
         do k=1,nrows(i)
            ts1(k)=data_all(k,ind_neu(neui),i)
c            call decyrs_to_jd ( data_all(k,ind_time,i), tmpmjd )
c            mjd1(k)=tmpmjd-0.5
            yr=nint(data_all(k,2,i))
            doy=data_all(k,3,i)
c            call YMDHMS_to_MJD ( date, seconds, epoch )
            call yds_to_jd (yr,doy,sec,tmpmjd )
            mjd1(k)=tmpmjd
c            write(*,'(i5,1x,3f21.5)') k,ts1(k),mjd1(k),data_all(k,1,i)
         enddo
         do j=i,nf
            site2=sites(j)
            call site_coords_query(coords_file,site2,xyz2,ll2)
            do k=1,nrows(j)
               ts2(k)=data_all(k,ind_neu(neui),j)
c               write(*,*) ts2(k)
c               mjd2(k)=data_all(k,1,j)
               yr=nint(data_all(k,2,j))
               doy=data_all(k,3,j)
               call yds_to_jd (yr,doy,sec,tmpmjd )
               mjd2(k)=tmpmjd
            enddo
            tmp=ts_corr(mjd1,ts1,nmax_row,nrows(i),
     &           mjd2,ts2,nmax_row,nrows(j))
c            write(*,*) tmp
            mat_corr(i,j)=tmp
C     calculate distances
c            call eval_dist(xyz1,xyz2,dist)
            call map_2points(ll1(1),ll1(2),ll2(1),ll2(2),0,1,0,0d0,dist)
            mat_blen_km(i,j)=dist/1000
            call map_2points(ll1(1),ll1(2),ll2(1),ll2(2),0,0,0,0d0,dist)
            mat_blen_deg(i,j)=dist
c            write(*,*) dist
         enddo

         
         write(fido,fmtstr) sites(i),(mat_corr(i,j),j=1,nf)
         if (fido.ne.6) then
            write(fidobd,fmtstr) sites(i),(mat_blen_deg(i,j),j=1,nf)
            write(fidobk,fmtstr) sites(i),(mat_blen_km(i,j),j=1,nf)
         endif
      enddo

c      call mat_transfill(mat_corr,nmax_sites,nf)
c      write(*,*) 'Trans-filled:'
c      do i=1,nf
c         write(*,fmtstr) sites(i),(mat_corr(i,j),j=1,nf)
c      enddo

      if (iargc().ge.2) then
         close(fido)
         close(fidobd)
         close(fidobk)
      endif
      
 899  continue
      enddo
      end
