CTITLE
      SUBROUTINE read_offps(file,site,nmax,offs,noff,pss,nps,neu)
c     --PURPOSE--

c     --ALGORITHM--
c$$$  # OFFSETS ADDED BY TIAN
c$$$   OFFSET XIAA 2000.17620 2000 064 U
c$$$   OFFSET BJSH 2000.14620 2000 054 U
c$$$   OFFSET ZHNZ 2005.68900 2005 251 N

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      character*(*) file,site
      integer*4 nmax
      character*1 neu

c     --OUTPUT--
      real*8 offs(nmax),pss(nmax)
      integer*4 noff,nps

c     --EXTERNAL--
      integer*4 nblen

c     --Local Parameters--
      integer*4 fid,ioerr,n,ns,i,j
      character*1023 bufl
      character*4 siteI,siteF
      character*100 strs(10),optype
      character*1 enu
      real*8 op,tmpr

c     <<VAR_DEC
      
      call getlun(fid)
      open(unit=fid,file=file,status='old',iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,'(2a)') 'Error when open file',
     &        file(1:nblen(file))//'.'
         stop
      endif

      siteI=site
      call uppers(siteI)

      noff=0
      nps=0
 800  read(fid,'(a1023)',end=899) bufl
c      write(*,*) 'bufl:',bufl(1:nblen(bufl))
      if (bufl(1:1).ne.' ') goto 800
      call strsplit(bufl,' ',ns,strs)
      if (ns.lt.3) goto 800
      if (ns.ge.4) then
         read(bufl,*) optype,siteF,op,enu
         call lowers(enu)
c         write(*,*) neu(1:1),'-',enu(1:1)
         if (enu(1:1).ne.neu(1:1)) goto 800
      else
         read(bufl,*) optype,siteF,op
      endif
      call uppers(siteF)
      if (siteI.ne.siteF) goto 800
      call uppers(optype)
      if (optype.eq.'OFFSET') then
c         write(*,*) ' []'//bufl(1:nblen(bufl))
         read(bufl,*) optype,siteF,op
         noff=noff+1
         offs(noff)=op
         goto 800
      else if (optype.eq.'PSDECAY') then
         read(bufl,*) optype,siteF,op
         nps=nps+1
         pss(nps)=op
         goto 800
      else
         goto 800
      endif

      goto 800

 899  continue
      close(fid)

      do i=1,noff-1
         do j=i+1,noff
            if (offs(i).gt.offs(j)) then
               tmpr=offs(j)
               offs(j)=offs(i)
               offs(i)=tmpr
            endif
         enddo
      enddo

      do i=1,nps-1
         do j=i+1,nps
            if (pss(i).gt.pss(j)) then
               tmpr=pss(j)
               pss(j)=pss(i)
               pss(i)=tmpr
            endif
         enddo
      enddo

      RETURN
      END
