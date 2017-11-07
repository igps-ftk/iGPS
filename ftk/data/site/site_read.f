c$$$;Return sites list from sites files.
c$$$;Conform to SOPAC site file format (by guess):
c$$$;	Site start with # is commented out.
c$$$;	?? What does ++++ stands for?

      subroutine site_read(file,sites,sitesref,nmax,n1site,nsiteref)
CTITLE site_read
CCC   Description:

CCC   MOD JUL-17-2007 TIAN@BEIJING.CHINA
CCC   site_read cannot deal with Tab separated sites names now.
CCC   trimlead subroutine can delete the leading blanks/spaces; however,
CCC   it cannot delete TABs.
CCC   Thus, it is important to avoid using TAB in sites file.

C     ---
      implicit none
      integer*4 nmax,n1site,nsiteref
      integer*4 n,ntmp,isref,ntest2
      character*(*) file
      character*(*) sites(*)
      character*(*) sitesref(*)
c     ---
      character*512 bufline,bufstr,tmpsites(5000)
      character*1 sep
      integer*4 fid, i
      integer nblen
c     ---
      n1site=0
      nsiteref=0
      isref=1
      sep=' '
      ntest2=0

      fid=90
      open(unit=fid,file=file)
 800  read(fid,'(a512)',end=899) bufline
c      write(*,*) bufline(1:nblen(bufline))
      if (nblen(bufline).lt.2.or.bufline(1:1).ne.' ') then
         goto 800
      endif
      ntmp=0
c      write(*,*) ntest2,n1site
c      goto 800
      call strsplit(bufline,sep,ntmp,tmpsites)
c     the above code changed the value of nsite !!!
c      write(*,*) ntmp,bufline(1:nblen(bufline))
      
c      goto 800
      do i=1,ntmp
c         write(*,'(a4,i5)') tmpsites(i),len(tmpsites(i))
         
         bufstr=tmpsites(i)
         call trimlead(bufstr)
         if (bufstr(1:1).eq.'#') then
            goto 801
         endif
         if (bufstr(1:4).eq.'++++') then
            isref=0
            goto 801
         endif       
         if (isref.eq.1) then
            nsiteref=nsiteref+1
            call lowers(bufstr)
            sitesref(nsiteref)=bufstr(1:4)
c            write(*,*) '|',sitesref(nsiteref),'|',nsiteref
c            write(*,'(3a)') '>',bufstr(1:4),'<'
         else
            n1site=n1site+1
            ntest2=ntest2+1
c            write(*,*) nblen(tmpsites(i))
            call lowers(bufstr)
            sites(ntest2)=bufstr
c            write(*,*) sites(ntest2),'(',ntest2,')'
         endif
c         write(*,*),nsiteref,ntest2,sitesref(nsiteref)
 801     continue
      enddo

      goto 800
 899  continue
      close(fid)

c      write(*,*) '#ref_sites:',nsiteref
c      do i=1,nsiteref
c         write(*,'("debug:",i3,a5)') i, sitesref(i)
c      enddo
      n1site=ntest2
      end
