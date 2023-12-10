CTITLE
      PROGRAM hold_rnx_stat

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1023 file,sfile,ofile
      integer*4 yrs,doys,yre,doye

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 nblen,iargc,isSiteIn

c     --Local Parameters--
      integer*4 nmax_site,nmax_day
      parameter(nmax_site=5000,nmax_day=5000)
      integer*4 i,j,nsite,stat(nmax_day,nmax_site),year,doy,fid,ioerr
      integer*4 dayi,pos,year_all(nmax_day),doy_all(nmax_day),fido
      real*8 jds,jd,obs_percent(nmax_site)
      character*4 sites(nmax_site),site
      character*1023 bufline,fmtstr

c     <<VAR_DEC

      if (iargc().lt.1) then
         write(*,*) 'Usage: hold_rnx_stat hold_file [--ofile=OUT_FILE]'
         write(*,*) '         [--yrs=YEAR_START]'
         write(*,*) '         [--yre=YEAR_END]'
         write(*,*) '         [--doys=DAY_OF_START_YEAR]'
         write(*,*) '         [--doye=DAY_OF_END_YEAR]'
         stop
      endif

      call getarg(1,file)
      fido=6

      do i=2,iargc()
         call getarg(i,bufline)
         if (bufline(1:11).eq.'--sitefile=') then
            if (nblen(bufline).le.11) then
               write(*,*) 'Incomplete site file input',
     &              bufline(1:nblen(bufline)),' .'
               stop
            endif
            sfile=bufline(12:nblen(bufline))
            write(*,*) 'input site file:', sfile(1:nblen(sfile))
            call rdsit_(sfile,sites,nmax_site,nsite)
            write(*,*) nsite,(sites(j)//' ',j=1,nsite)
         elseif (bufline(1:8).eq.'--ofile=') then
            if (nblen(bufline).le.8) then
               write(*,*) 'Incomplete output file name ',
     &              bufline(1:nblen(bufline)),' .'
               stop
            endif
            ofile=bufline(9:nblen(bufline))
            write(*,*) 'output file:', ofile(1:nblen(ofile))
            call getlun(fido)
            open(unit=fido,file=ofile,iostat=ioerr)
            if (ioerr.ne.0) then
               write(*,*) 'Error when openning output file ',
     &              ofile(1:nblen(ofile)),' .'
               stop
            endif
         elseif (bufline(1:6).eq.'--yrs=') then            
            if (nblen(bufline).le.6) then
               write(*,*) 'Incomplete input',
     &              bufline(1:nblen(bufline)),' .'
               stop
            endif
            read(bufline(7:nblen(bufline)),*) yrs
         elseif (bufline(1:6).eq.'--yre=') then            
            if (nblen(bufline).le.6) then
               write(*,*) 'Incomplete input',
     &              bufline(1:nblen(bufline)),' .'
               stop
            endif
            read(bufline(7:nblen(bufline)),*) yre
         elseif (bufline(1:7).eq.'--doys=') then            
            if (nblen(bufline).le.7) then
               write(*,*) 'Incomplete input',
     &              bufline(1:nblen(bufline)),' .'
               stop
            endif
            read(bufline(8:nblen(bufline)),*) doys
         elseif (bufline(1:7).eq.'--doye=') then            
            if (nblen(bufline).le.7) then
               write(*,*) 'Incomplete input',
     &              bufline(1:nblen(bufline)),' .'
               stop
            endif
            read(bufline(8:nblen(bufline)),*) doye
         else
            write(*,*) 'Invalid input: ',
     &           bufline(1:nblen(bufline)),' .'
            stop
         endif

      enddo
      write(*,*) yrs,doys,yre,doye
      call yds_to_jd(yrs,doys,0,jds)
      write(*,*) 'jd start:',jds


      do i=1,nsite
         obs_percent(i)=0
      enddo

c      stop
      call getlun(fid)
      open(unit=fid,file=file,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,*) 'Error when openning input file ',
     &        file(1:nblen(file)),' .'
         stop
      endif
 801  read(fid,'(a)',end=800) bufline
c$$$       /data0/igs0/pub/rinex/2008/001
c$$$  working in /data0/igs0/pub/rinex/2008/001
c$$$  NF:  165
c$$$albh 2008 001 d y /data0/igs0/pub/rinex/2008/001/albh0010.08d.Z
c$$$algo 2008 001 d y /data0/igs0/pub/rinex/2008/001/algo0010.08d.Z
c$$$alic 2008 001 d y /data0/igs0/pub/rinex/2008/001/alic0010.08d.Z
      if (bufline(1:1).eq.' ') then
         goto 801
      endif
      read(bufline,*) site,year,doy
c      write(*,*) year,doy
      if (year.lt.yrs) goto 801
      if (year.gt.yre) goto 800

      if (yrs.eq.yre) then
         if (doy.lt.doys) goto 801
         if (doy.gt.doye) goto 800
         call yds_to_jd(year,doy,0,jd)
         dayi=jd-jds+1
         year_all(dayi)=year
         doy_all(dayi)=doy
         do j=1,nsite
            pos=isSiteIn(sites,nmax_site,nsite,site)            
            if (pos.gt.0) then
               stat(dayi,pos)=1
c               write(*,*) obs_percent(pos),pos,year,doy,site
               obs_percent(pos)=obs_percent(pos)+1
c               write(*,*) ' ',obs_percent(pos),pos,year,doy,site
               goto 801
            endif
         enddo
      else
         if (year.eq.yrs) then
            if (doy.lt.doys) goto 801       
            call yds_to_jd(year,doy,0,jd)
            dayi=jd-jds+1
            year_all(dayi)=year
            doy_all(dayi)=doy
c            write(*,*) year,doy,site,nsite
            do j=1,nsite
               pos=isSiteIn(sites,nmax_site,nsite,site)            
               if (pos.gt.0) then
                  stat(dayi,pos)=1
c                  write(*,*) obs_percent(pos),pos,year,doy,site
                  obs_percent(pos)=obs_percent(pos)+1
c                  write(*,*) ' ',obs_percent(pos),pos,year,doy,site
                  goto 801
               endif
            enddo
         endif

         if (year.gt.yrs.and.year.lt.yre) then
            call yds_to_jd(year,doy,0,jd)
            dayi=jd-jds+1
            year_all(dayi)=year
            doy_all(dayi)=doy
            do j=1,nsite
               pos=isSiteIn(sites,nmax_site,nsite,site)        
               if (pos.gt.0) then
                  stat(dayi,pos)=1
                  obs_percent(pos)=obs_percent(pos)+1
                  goto 801
               endif
            enddo
         endif
      
         if (year.eq.yre) then
            if (doy.gt.doye) goto 800
c     return
            call yds_to_jd(year,doy,0,jd)
            dayi=jd-jds+1
            year_all(dayi)=year
            doy_all(dayi)=doy
            do j=1,nsite
               pos=isSiteIn(sites,nmax_site,nsite,site)        
               if (pos.gt.0) then
                  stat(dayi,pos)=1
                  obs_percent(pos)=obs_percent(pos)+1
                  goto 801
               endif
            enddo
         endif
      endif
      goto 801
 800  continue
      close(fid)

c     sites title
      write(fmtstr,'("(4x,1x,3x,1x,",i9,"a5)")'),nsite
      write(fido,fmtstr) (sites(i),i=1,nsite)
      write(fmtstr,'("(i4.4,1x,i3.3,1x,",i9,"i5)")'),nsite
      do i=1,dayi
         write(fido,fmtstr) year_all(i),doy_all(i),(stat(i,j),j=1,nsite)
      enddo

      do i=1,nsite
         obs_percent(i)=obs_percent(i)/dayi*100
      enddo
      write(fmtstr,'("(a8,1x,"i9,"f5.0)")') nsite
      write(fido,fmtstr) '%',(obs_percent(i),i=1,nsite)
c     sites title again
      write(fmtstr,'("(4x,1x,3x,1x,",i9,"a5)")'),nsite
      write(fido,fmtstr) (sites(i),i=1,nsite)

      if (fido.ne.6) close(fido)

      write(*,*) 'Done.'
      STOP
      END


      function isSiteIn(sites,nmax_site,nsite,site)
      integer*4 isSiteIn,nmax_site,nsite
      character*4 sites(nmax_site),site
      integer*4 i
    
      isSiteIn=0
      do i=1,nsite
         if (sites(i).eq.site) then
            isSiteIn=i
            goto 900
         endif
      enddo
 900  continue
c      return(isSiteIn)
      end
