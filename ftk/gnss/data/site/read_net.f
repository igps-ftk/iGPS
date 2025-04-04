c     Read apriori coordinates for site from QOCA Network File (*.net)
c     Return -9999d0 when not found.

      subroutine read_net(file,site,llh)
c
C     ---
      character*(*) file,site
      real*8 llh(3), lon_ss, lat_ss,ht,u,v,w,epoch,t1,t2
      integer*4 lon_dd,lon_mm,lat_dd,lat_mm
      character*512 buf,tmpstr(7),tmps
      character*4 cursite,siteupper
      character*1 ew,ns
      integer*4 fid,ioerr
C     ---
      siteupper=site
      call casefold(siteupper)
c      fid=11
      call getlun(fid)
      open(unit=fid,file=file,status='old',iostat=ioerr)
      if (ioerr.gt.0) then
         write(*,'(3a)') '[read_net]ERROR: cannot open file "',
     &        file(1:nblen(file)),'" for reading!!!'
         stop
      endif

 800  read(fid,'(a512)',end=898) buf
      if (buf(1:1).ne.' ')  goto 800
c      write(*,*) buf(1:nblen(buf))
      call trimlead(buf)
      read(buf,'(a)') cursite
      call casefold(cursite)
c      write(*,*) cursite, siteupper
c      stop
      if (cursite.eq.siteupper(1:4)) then
c*  site    full-name      latitude        longitude         height       u       v       w    epoch       t1        t2
c ALBH_GPS  site_log  N48 23 23.213242 W123 29 14.892299     31.75972  0.0000  0.0000  0.0000 2001.0470 1900.0000 2500.0000
c         write(*,*) buf
         read(buf,700) cursite,tmps,ns,lat_dd,lat_mm,lat_ss,
     &        ew,lon_dd,lon_mm,lon_ss,ht,u,v,w,epoch,t1,t2
 700     format(a4,4x,2X,a8,2X,A1,I2,1X,I2,1X,F9.6,1X,A1,I3,
     &        1X,I2,1X,F9.6,1X,F12.5, 3(1X,F7.4),3(1X,F9.4))
c         write(*,*) cursite,tmpstr,ns,lat_dd,lat_mm,lat_ss,
c     &        ew,lon_dd,lon_mm,lon_ss,ht,u,v,w,epoch,t1,t2
         llh(1)=lon_dd+lon_mm/60d0+lon_ss/3600.
         llh(2)=lat_dd+lat_mm/60d0+lat_ss/3600.
         llh(3)=ht
         
         if (ew.eq.'W'.or.ew.eq.'w') then
            llh(1)=llh(1)*(-1)
         endif
         if (ns.eq.'s'.or.ns.eq.'S') then
            llh(2)=llh(2)*(-1)
         endif

         
         goto 899
      endif
      goto 800
 898  continue
c     if not found, then void result
      llh(1)=-9999
      llh(2)=-9999
      llh(3)=-9999

 899  continue
      close(fid)
      return
      end
