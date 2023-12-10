c     JPL lat/lon/rad
      program l_l_r2llrs_panga
C     ---
c     Input:
c     path:
c     opath
c     ---
      IMPLICIT NONE
      include '../../../inc/ftk.h'
      character*1000 file,ofile,tmpstr,path,opath,files(nmax_site)
      character*1000 filter,tmpstr2
c     ---
      integer*4 fid,fido,ioerr,ioerro,i,j,fi
      real*8 lats(nmax_row,nmax_col),lons(nmax_row,nmax_col)
      real*8 rads(nmax_row,nmax_col),lat,lon,rad
      integer*4 nrow,ncol,n,tmpi(3)
      integer*4 nrow1,nrow2,nrow3,ncol1,ncol2,ncol3
      integer*4 nhead1,nhead2,nhead3

      integer nblen,iargc
c     header information for PANGA sites
      character*512 headers(nmax_head)
      integer*4 nhead
c     data index for lat/lon/rad
      integer*4 i1,i2,i3
c     start and end times
      real*8 yrs,yre,jds,jde,jd,yr,secr8,jdt
      integer*4 year,doy,day,mon,seci4
c     output number of observations
      integer*4 nrowo,ncolo
      integer*4 idate(5)

c     temp line data
      real*8 tmpline(nmax_col)

c     data line valid (no zero)?
      logical isval

c     ---
       if (iargc().lt.2) then
         write(*,*) 'Syntax: l_l_r2llrs path opath'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      n=0
      fido=11
      filter='*.lat'
      call ffind ( path, files, filter,n,1 ) 
      write(*,*) '#total files:',n
      do fi=1,n
         file=files(fi)

c     the fido cannot be 90
c     fido=90
c     Because the subroutines use file unit 90.
c     Thus, we should use file unit < 30 in the main program,
c     and > 30 in the subroutines.
c     But, it seems that conflicts cannot be ruled out completely.
C     What is the best method?

c     open output file for writing headers
c      write(*,*) 'writing...'
         call desuffix(file,tmpstr)
         call getfilename(tmpstr,tmpstr2)
         ofile=opath(1:nblen(opath))//pathsep//
c     &        tmpstr2(1:nblen(tmpstr2))//'.llr'
     &        tmpstr2(1:nblen(tmpstr2))//'.neu'
C     Tian Mod APR-23-2008
c     The output file extention is changed to .neu, because it is
c     actually in SIO/NEU ascii format.
         write(*,*) '> ',ofile(1:nblen(ofile))
         open(unit=fido,file=ofile)

c     read lat file
         if (debug) write(*,*) 'lat file:', file(1:nblen(file))
         call read_l_l_r_panga(file,lats,nrow1,ncol1,nhead1,headers)
         if (debug) write(*,*) 'lat:',nrow1,ncol1,nhead1
c     write lat header
         do i=1,nhead1
            tmpstr=headers(i)
            write(fido,'(a)') tmpstr(1:nblen(tmpstr))
         enddo

         call desuffix(file,tmpstr)
         file=tmpstr(1:nblen(tmpstr))//'.lon'
         if (debug) write(*,*) 'lon file:', file(1:nblen(file))

         call read_l_l_r_panga(file,lons,nrow2,ncol2,nhead2,headers)
         if (debug) write(*,*) 'lon:',nrow2,ncol2,nhead2
c     write lon header
         do i=1,nhead2
            tmpstr=headers(i)
            write(fido,'(a)') tmpstr(1:nblen(tmpstr))
         enddo
         
         call desuffix(file,tmpstr)
         file=tmpstr(1:nblen(tmpstr))//'.rad'
         if (debug) write(*,*) 'rad file:', file(1:nblen(file))
         call read_l_l_r_panga(file,rads,nrow3,ncol3,nhead3,headers)
         if (debug) write(*,*) 'rad:',nrow3,ncol3,nhead3
c     write rad header
         do i=1,nhead3
            tmpstr=headers(i)
            write(fido,'(a)') tmpstr(1:nblen(tmpstr))
         enddo
 

         yrs=lats(1,1)
         yre=lats(nrow1,1)

         if (yrs.gt.lons(1,1)) yrs=lons(1,1)
         if (yre.lt.lons(nrow2,1)) yre=lons(nrow2,1)

         if (yrs.gt.rads(1,1)) yrs=rads(1,1)
         if (yre.lt.rads(nrow3,1)) yre=rads(nrow3,1)

         if (debug) write(*,*) 'yrs:',yrs,' yre:',yre
c         call decyrs_to_jd(yrs,jds)
c         call decyrs_to_jd(yre,jde)
c         write(*,*) 'jds:',jds,' jde:',jde


CCC   panga time are not start with 0:00 or 12:00, but are random.
cCC   Set to 0:00 of the day
         call decyrs_to_ydhms(yrs,idate)
c         idate(4)=0
c         idate(5)=0
c         write(*,*) 'date s:',idate
         call yds_to_jd(idate(1),idate(2),0,jds)

c         write(*,*) 'yre:',yre,idate
         call decyrs_to_ydhms(yre,idate)
c         write(*,*) 'yre:',yre,idate
c         idate(4)=0
c         idate(5)=0
c         write(*,*) 'date e:',idate
         call yds_to_jd(idate(1),idate(2),0,jde)

c         call decyrs_to_jd(yrs,jds)
c         call decyrs_to_jd(yre,jde)
         nrowo=jde-jds+1
         if (debug)  write(*,*) 'jds:',jds,' jde:',jde,' #days:',nrowo
c         goto 800
         
c     blank file:
         if (nrowo.lt.1) then
            goto 800
         endif

         i1=1
         i2=1
         i3=1
         do i=1,nrowo
            jd=jds+i-1
            do j=1,nmax_col
               tmpline(j)=0d0
            enddo
            isval=.true.

c     call jd_to_ymdhms(jd,idate,secr8)
            call jd_to_yds(jd,year,doy,seci4)
            if (year.gt.70.and.year.lt.100) year=year+1900
            if (year.lt.70) year=year+2000
c            write(*,*) 'idate:',idate
c            tmpline(1)=jd
ccc            tmpline(2)=idate(1)
ccc            tmpline(3)=idate(2)
c            tmpline(2)=year
c            tmpline(3)=doy

            call jd_to_decyrs(jd,yr)

            tmpline(1)=yr
            tmpi(2)=year
            tmpi(3)=doy

 801        call decyrs_to_ydhms(lats(i1,1),idate)
            call yds_to_jd(idate(1),idate(2),0,jdt)
            if (jd.gt.jdt) then
               i1=i1+1
c               if (debug) write(*,*) 'add up for ',i1,jd,jdt
               isval=.false.
               if (i1.lt.nrowo) goto 801
            else if (jd.eq.jdt) then
               tmpline(4)=lats(i1,2)
               tmpline(7)=lats(i1,3)
               i1=i1+1
            else
c               if (debug) write(*,*) i,' blank'
               isval=.false.
            endif

c     for longitude
 802        call decyrs_to_ydhms(lons(i2,1),idate)
            call yds_to_jd(idate(1),idate(2),0,jdt)
            if (jd.gt.jdt) then
               i2=i2+1
c               if (debug) write(*,*) 'add up for ',i1,jd,jdt
               isval=.false.
               if (i2.lt.nrowo) goto 802
            else if (jd.eq.jdt) then
               tmpline(5)=lons(i2,2)
               tmpline(8)=lons(i2,3)
               i2=i2+1
            else
c               if (debug)  write(*,*) i,' blank'
               isval=.false.
            endif

c     for radius
 803        call decyrs_to_ydhms(rads(i3,1),idate)
            call yds_to_jd(idate(1),idate(2),0,jdt)
            if (jd.gt.jdt) then
               i3=i3+1
c               if (debug) write(*,*) 'add up for ',i1,jd,jdt
               isval=.false.
               if (i3.lt.nrowo) goto 803
            else if (jd.eq.jdt) then
               tmpline(6)=rads(i3,2)
               tmpline(9)=rads(i3,3)
               i3=i3+1
            else
c               if (debug) write(*,*) i,' blank'
               isval=.false.
            endif

            if (isval) write(fido,702) tmpline(1),tmpi(2),tmpi(3),
     .           (tmpline(j),j=4,9)
c            write(*,*) tmpline(1) tmpline(2)
c         (tmpi(j),j=1,3) =(lats(i,k),k=4,6)
c         tmpi =(lats(i,k),k=4,6)
c         write(fido,700) lats(i,1),lats(i,4),lats(i,5),lats(i,6),
c         write(fido,700) lats(i,1),(tmpi(j),j=1,3),
c     &        lats(i,2),lons(i,2),rads(i,2),
c     .        lats(i,3),lons(i,3),rads(i,3)
c            write(fido,700) lats(i,1),lats(i,4),lats(i,5),lats(i,6),
c     &           lats(i,2),lons(i,2),rads(i,2)
c         write(*,700) lats(i,1),lats(i,6),lats(i,5),lats(i,4),
c     &        lats(i,2),lons(i,2),rads(i,2)
         enddo
c 700     format(f9.4,3f4.0,3f20.12)
 700  format(f9.4,3i3.2,3f20.12,3f20.12)
 701  format(f10.5,i5,i4,3f10.5,3f10.5)
 702  format(f10.5,i5,i4,1x,3(f20.5,1x),1x,3(f10.5,1x))
c      write(*,*) 'wrote ok'


         close(fido)
 800     continue
      enddo
      
      if (debug) write(*,*) '<<end of l_l_r2llrs_panga'
      stop
      end
