      program test_site_read
c     -
      character*512 file
      integer*4 nmax
      parameter(nmax=1000)
      character*4 sites(nmax),sitesref(nmax)
      integer*4 nsite,nsiteref
      integer*4 i,j
      real*8 xyz(3),ll(2),lat(2),lon(2)
      real*8 data(nmax,9)
c     ---
      nsite=0
      nsiteref=0
      file='/export/home/jlm/gps/gpsi/conf/setup/glb_igs2.list.txt'
      file='/home/tianyf/gpsi/conf/setup/glb_igs2.list.txt'
      call site_read(file,sites,sitesref,nmax,nsite,nsiteref)
      write(*,*) '#Reference_Sites:',nsiteref
      do i=1,nsiteref,8
         write(*,'(">",8a5,"<")') (sitesref(j),j=i,i+7)
      enddo
      write(*,*) '#Sites:',nsite
      do i=1,nsite,8
         write(*,'(8a6)') (sites(j),j=i,i+7)
      enddo
c      write(*,'(8a5)') sites

c      sites(1)='ac27'
      
c      stop
      file='/export/home/jlm/gps/gpsi/conf/sites.xyz.sio'
      file='/home/tianyf/gpsi/conf/sites.xyz.sio'
      call site_coords_query(file,sites(10),xyz,ll)
      write(*,*) sites(10),xyz,ll

      lon(1)=-150
      lon(2)=0
      lat(1)=23
      lat(2)=66
      call site_search_byrect(file,lat,lon,sites,nsite)
      write(*,*) '#Sites in RECT:',nsite
      do i=1,nsite,8
         write(*,'(8a5)') (sites(j),j=i,i+7)
      enddo

      file='/home/tianyf/data/jpl/mbh/filtered/7ODM.lat'
c      call l_l_r_read(file,data,nmax,9,
      end
