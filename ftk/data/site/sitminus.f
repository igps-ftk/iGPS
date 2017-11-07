CTITLE
      PROGRAM sitminus

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1023 file1,file2

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 iargc

c     --Local Parameters--
      integer*4 nmaxsite,nsite1,nsite2,i,j
      parameter(nmaxsite=5000)
      character*4 sites1(nmaxsite),sites2(nmaxsite),sites(nmaxsite)

      integer*4 ind1(nmaxsite),ind2(nmaxsite)
      integer*4 nind1,nind2,nsite,isFound

c     <<VAR_DEC

      if (iargc().lt.2) then
         write(*,'(a)') 'Usage: sitcomm file1 file2'
         write(*,'(2a)') '  file1 and file2 are iGPS site list ',
     +        'file (*.sit)'
         write(*,'(2a)') '  sitcomm returns the common sites in ',
     +        'file1 and file2.'
         stop
      endif

      call getarg(1,file1)
      call getarg(2,file2)

      call rdsit_(file1,sites1,nmaxsite,nsite1)
     
      call rdsit_(file2,sites2,nmaxsite,nsite2)
c      write(*,*) nsite1
c      write(*,701) (sites1(i),i=1,nsite1)
c      write(*,*) nsite2
c      write(*,701) (sites2(i),i=1,nsite2)
 701  format(8(1x,a4))

c$$$      call set_intersect_s(sites1,nmaxsite,nsite1,
c$$$     &     sites2,nmaxsite,nsite2,
c$$$     &     ind1,nmaxsite,nind1,ind2,nmaxsite,nind2, 
c$$$     &     sites,nmaxsite,nsite)

      do i=1,nsite1
         isFound=0
         do j=1,nsite2
            if (sites1(i).eq.(sites2(j))) then
               isFound=1
               goto 801
            endif
         enddo
         write(*,'(1x,a4)') sites1(i)
 801     continue
      enddo

c      write(*,*) nsite
      write(*,702) (sites(i),i=1,nsite)
 702  format(1(1x,a4))

      STOP
      END
