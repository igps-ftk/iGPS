CTITLE
      PROGRAM usgs_ts2sio
c     --PURPOSE--

c     --INPUT--

c     --OUTPUT--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --Global Variables--
      IMPLICIT NONE
      INCLUDE '../../inc/cgps.h'

c     --Command-line Parameters--
      character*1000 file,ofile

c     --Local Parameters--

c     <<VAR_DEC
      character*1000 hfile,home
      character*1000 buf1000
      character*1000 headers(nmax_head),tmpstr
      real*8 data(nmax_row,nmax_col),datao(nmax_row,nmax_col)
      real*8 dataline(nmax_col)
      integer*4 nrow,ncol,nhead,nrowo,ncolo
      integer*4 i,j,di
      integer nblen,iargc
      logical allr8

      if (iargc().lt.2) then
         write(*,*) 'Syntax:usgs_ts2sio file [ofile]'
         if (.not.debug) stop
         write(*,*) 'Use debug settings:'
         call getenv('HOME',home)
         file=home(1:nblen(home))//'/data/usgs/fullseries/7ODM.ts'
         ofile=home(1:nblen(home))//'/tmp/7ODM.ts'
         ofile='7ODM.ts'
      endif
      
      if (iargc().ge.1) then
         call getarg(1,file)
      endif
      if (iargc().ge.2) then
         call getarg(2,ofile)         
      endif

      if (debug) then
         write(*,*) file(1:nblen(file))
         write(*,*) ofile(1:nblen(ofile))
      endif

      call read_usgs(file,data,nrow,ncol,nhead,headers)

      if (debug) then
         write(*,*) nrow,ncol,nhead
         do i=1,nhead
            tmpstr=headers(i)
            write(*,*) tmpstr(1:nblen(tmpstr))
         enddo
      endif

c     form the output format data
      ncolo=9
      di=0
      do i=1,nrow
c$$$         datao(i,1)=data(i,1)
c$$$         datao(i,2)=nint(data(i,1))
c$$$         datao(i,3)=data(i,2)
c$$$         datao(i,4)=data(i,3)
c$$$         datao(i,5)=data(i,5)
c$$$         datao(i,6)=data(i,7)
c$$$         datao(i,7)=data(i,4)
c$$$         datao(i,8)=data(i,6)
c$$$         datao(i,9)=data(i,8)
         dataline(1)=data(i,1)
         dataline(2)=nint(data(i,1))
         dataline(3)=data(i,2)
         dataline(4)=data(i,3)
         dataline(5)=data(i,5)
         dataline(6)=data(i,7)
         dataline(7)=data(i,4)
         dataline(8)=data(i,6)
         dataline(9)=data(i,8)
         if (.not.allr8(dataline,nmax_col,ncolo)) goto 801
         di=di+1
         do j=1,ncolo
            datao(di,j)=dataline(j)
         enddo         
 801     continue
      enddo
      nrowo=di

c     outpu the result
      call write_sio(ofile,datao,nrowo,ncolo,headers,nhead)

      END
