CTITLE
      PROGRAM extract_stinfo

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--
c       extract_stinfo site_lit_file(*.sit) station.info_file [ofile]

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1024 sitfile, stinfofile

c     --OUTPUT--
      character*1024 ofile

c     --EXTERNAL--
      integer*4 iargc,nblen
      character*1 path_sep

c     --Local Parameters--
      integer*4 fid,fido,i,ioerr
      integer*4 nmaxsit,nsit
      parameter(nmaxsit=10000)
      character*4 sites(nmaxsit),site,sitmp,sites_l(nmaxsit)
      integer*4 isgot(nmaxsit),nfound,nmissing
      character*1024 line

c     <<VAR_DEC
      
      if (iargc().lt.2) then
         write(*,'(2a)') 'Usage: extract_stinfo site_file stinfo_file',
     &        ' [ofile]'
         stop
      endif
      
      call getarg(1,sitfile)
      write(*,'("*<",a)') sitfile(1:nblen(sitfile))
      call getarg(2,stinfofile)
      write(*,'("*<",a)') stinfofile(1:nblen(stinfofile))
      fido=6
      ofile=''
      if (iargc().ge.3) then
         call getarg(3,ofile)
         write(*,'("*>",a)') ofile(1:nblen(ofile))
      endif

      call rdsit_(sitfile,sites,nmaxsit,nsit)
      write(*,'("#sites:",i5)') nsit
      write(*,'("*",8a5)') (sites(i),i=1,nsit)
      do i=1,nsit
         sitmp=sites(i)
         call lowers(sitmp)
         sites_l(i)=sitmp
         isgot(i)=0
      enddo


c     open station.info file and loop line-by-line
      call getlun(fid)
      open(unit=fid,file=stinfofile,status='old',iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,'(3a)') 'EXTRACT_STINFO[FATAL]:error open input file ',
     &        stinfofile(1:nblen(stinfofile)),' !!!'
         stop
      endif

      if (ofile.ne.'') then
         call getlun(fido)
         open(unit=fido,file=ofile,iostat=ioerr)
         if (ioerr.ne.0) then
            write(*,'(4a)') 'EXTRACT_STINFO[FATAL]:error open',
     &           ' output file ',
     &           ofile(1:nblen(ofile)),' !!!'
            close(fid)
            stop
         endif
      endif
      

 801  read(fid,'(a1024)',end=999) line
      if (line(1:1).ne.' ') then
         write(fido,'(a)') line
      endif
      read(line,*) sitmp
      call lowers(sitmp)
      do i=1,nsit
         if (sitmp.eq.sites_l(i)) then
            write(fido,'(a)') line
            isgot(i)=1
         endif
      enddo   
       
      goto 801

 999  continue
      close(fid)
      if (fido.ne.6) then
         close(fido)
      endif

      nmissing=0
      do i=1,nsit
         if (isgot(i).eq.0) then
            nmissing=1
            goto 802
         endif
      enddo
 802  continue

      fido=6
      if (nmissing.gt.0) then
         call getpathname(ofile,line)
         write(ofile,'(3a)') line(1:nblen(line)),
     &     path_sep(),'missing_stinfo.sit'
c         ofile='missing.sit'
         call getlun(fido)
         open(unit=fido,file=ofile)
         write(*,'(2a)') 'Missing sites are saved to ',
     &        ofile(1:nblen(ofile))
      endif
         
      do i=1,nsit
         if (isgot(i).eq.0) then
            write(*,'(2a)') "*Missing station.info for site: ",sites(i)
            if (fido.ne.6) then
               write(fido,'(" ",a4)') sites(i)
            endif
         endif
      enddo
      if (fido.ne.6) then
         close(fido)
      endif

      STOP
      END
