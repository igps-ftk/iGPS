c$$$;Return sites list from sites files.
c$$$;Conform to SOPAC site file format (by guess):
c$$$;	Site start with # is commented out.
c$$$;	?? What does ++++ stands for?

      subroutine rdldsite(file,sites,nsite,ll)
C     TITLE site_read
CCC   Description:

CCC   MOD JUL-17-2007 TIAN@BEIJING.CHINA
CCC   site_read cannot deal with Tab separated sites names now.
CCC   trimlead subroutine can delete the leading blanks/spaces; however,
CCC   it cannot delete TABs.
CCC   Thus, it is important to avoid using TAB in sites file.

C     ---
      implicit none
c      integer*4 nmax,n1site,nsiteref
c      integer*4 n,ntmp,isref,ntest2
      integer*4 nsite
      character*(*) file
      character*4 site
      character*(*) sites(nsite)
      real*8 ll(nsite,2)
c      character*4 sitesref(5000)
c     ---
      character*512 bufl,bufstr,tmpsites(5000)
c      character*1 sep
      integer*4 fid, i,ioerr
      real*8 lat,lon
      integer nblen,iargc
c     ---
c      if (iargc().lt.1) then
c         write(*,*) 'Usage: rdsite site_file'
c         stop
c      endif

c      n1site=0
c      nsiteref=0
c      isref=1
c      sep=' '
c      ntest2=0
c      call getarg(1,file)
c      call getlun(fid)
c      fid=90
      open(unit=fid,file=file,iostat=ioerr,status='old')
      if (ioerr.ne.0) then
         write(*,*) ' [rdldsite] FATAL: cannot open input file ['//
     &        file(1:nblen(file))//'].'
         stop
      endif
      nsite=0
 800  read(fid,'(a512)',end=899) bufl
c      write(*,*) bufline(1:nblen(bufline))
      if (bufl(1:1).ne.' ') then
         goto 800
      endif
      if (nblen(bufl).lt.5) then
         goto 800
      endif
      nsite=nsite+1
      read(bufl,*) site,lat,lon
      sites(nsite)=site
      ll(nsite,1)=lon
      ll(nsite,2)=lat
c      ntmp=0
c      write(*,*) ntest2,n1site
c      goto 800
c      call strsplit(bufline,sep,ntmp,tmpsites)
c     the above code changed the value of nsite !!!
c      write(*,*) ntmp,bufline(1:nblen(bufline))
      
c      goto 800
c      do i=1,ntmp
c         write(*,'(a4,i5)') tmpsites(i),len(tmpsites(i))
         
c        bufstr=tmpsites(i)
c        call trimlead(bufstr)
c         if (bufstr(1:1).eq.'#') then
c            goto 801
c         endif
c         if (bufstr(1:4).eq.'++++') then
c            isref=0
c            goto 801
c         endif       
c         if (isref.eq.1) then
c            nsiteref=nsiteref+1
c            call lowers(bufstr)
c            sitesref(nsiteref)=bufstr(1:4)
c            write(*,*) '|',sitesref(nsiteref),'|',nsiteref
c            write(*,'(3a)') '>',bufstr(1:4),'<'
c         else
c            n1site=n1site+1
c            ntest2=ntest2+1
c            write(*,*) nblen(tmpsites(i))
c            call lowers(bufstr)
c            sites(ntest2)=bufstr
c            write(*,*) sites(ntest2),'(',ntest2,')'
c         endif
c         write(*,*),nsiteref,ntest2,sitesref(nsiteref)
c 801     continue
c      enddo

      goto 800
 899  continue
      close(fid)

c      write(*,*) '#ref_sites:',nsiteref,n1site
c      do i=1,nsiteref
c         write(*,'(a4)') sitesref(i)
c      enddo
c      do i=1,n1site
c         write(*,'(a4)') sites(i)
c      enddo
c      stop

      end
