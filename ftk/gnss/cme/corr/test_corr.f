CTITLE
      PROGRAM test_corr

      IMPLICIT NONE
      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--

c     --OUTPUT--

c     --EXTERNAL--

c     --Local Parameters--
      character*1023 file
      character*4 sites(nmax_sites)
      integer*4 nsit,i,j
      real*8 corr(nmax_sites,nmax_sites,3),llh(3,nmax_sites)
      real*8 blen_deg(nmax_sites,nmax_sites)
      real*8 blen_km(nmax_sites,nmax_sites)

c     <<VAR_DEC

      file='/home/tianyf/tmp/neu.cmc/corr/NEU_CMC_neu.snx'
      file='/home/tianyf/tmp/neu.cmc/corr/pbo_sio_neu.snx'
      call read_corr_snx(file,sites,nsit,corr,blen_deg,blen_km,llh)
c$$$      write(*,*) 'Corr North:'
c$$$      do i=1,nsit
c$$$         write(*,'(1000(1x,f5.2))') (corr(i,j,1),j=1,nsit)
c$$$      enddo
c$$$      write(*,*) 'Corr East:'
c$$$      do i=1,nsit
c$$$         write(*,'(1000(1x,f5.2))') (corr(i,j,2),j=1,nsit)
c$$$      enddo
c$$$      write(*,*) 'Corr Up:'
c$$$      do i=1,nsit
c$$$         write(*,'(1000(1x,f5.2))') (corr(i,j,3),j=1,nsit)
c$$$      enddo
c$$$      write(*,*) 'Blen Deg:'
c$$$      do i=1,nsit
c$$$         write(*,'(1000(1x,f5.2))') (blen_deg(i,j),j=1,nsit)
c$$$      enddo
c$$$      write(*,*) 'Blen Km:'
c$$$      write(*,'(a4,1000(1x,a8))') '',sites(1:nsit)
c$$$      do i=1,nsit
c$$$         write(*,'(a4,1000(1x,f8.2))') sites(i),(blen_km(i,j),j=1,nsit)
c$$$      enddo
      STOP
      END
