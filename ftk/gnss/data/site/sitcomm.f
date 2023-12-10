CTITLE
      PROGRAM sitcomm

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

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
      integer*4 nmaxsite,nsite1,nsite2,i
      parameter(nmaxsite=5000)
      character*4 sites1(nmaxsite),sites2(nmaxsite),sites(nmaxsite)

      integer*4 ind1(nmaxsite),ind2(nmaxsite)
      integer*4 nind1,nind2,nsite

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

      call set_intersect_s(sites1,nmaxsite,nsite1,
     &     sites2,nmaxsite,nsite2,
     &     ind1,nmaxsite,nind1,ind2,nmaxsite,nind2, 
     &     sites,nmaxsite,nsite)

c      write(*,*) nsite
      write(*,702) (sites(i),i=1,nsite)
 702  format(1(1x,a4))

      STOP
      END
