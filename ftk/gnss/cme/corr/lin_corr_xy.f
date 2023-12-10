     
      real*8 function lin_corr_xy(mjd1,ts1,nmax1,nrow1,
     &     mjd2,ts2,nmax2,nrow2,nsame)
C     ---
C     Input:
c     mjd1,mjd2:
c     ts1,ts2:
      integer*4 nmax1,nmax2
      real*8 mjd1(nmax1),mjd2(nmax2)
      real*8 ts1(nmax1),ts2(nmax2)
      integer*4 nrow1,nrow2,nrow,nsame
      real*8 sd1,sd2,cov12,tmp,mean1,mean2,tmp1,tmp2
      real*8 i,j,i1_start,i1,i2,i2_start
      real*8 mjds(nmax1),ts1s(nmax1),ts2s(nmax1),inds(nmax1)
      real*8 mjd_start,mjd_end
C     ---
C     >>
c     get the same eopchs
      if (mjd1(nrow1).lt.mjd2(1).or.mjd2(nrow2).lt.mjd1(1)) then
         lin_corr_xy=0
         write(*,'(a)') '[lin_corr_xy]WARNING: no common epochs!'
c         write(*,*) mjd1(nrow1),mjd1(1),mjd2(nrow2),mjd2(1)
         goto 39
      endif

      if (mjd1(1).gt.mjd2(1)) then
         mjd_start=mjd1(1)
      else
         mjd_start=mjd2(1)
      endif
      if (mjd1(nrow1).lt.mjd2(nrow2)) then
         mjd_end=mjd1(nrow1)
      else
         mjd_end=mjd2(nrow2)
      endif
c      write(*,*) 'mjds:',mjd_start,mjd_end,mjd1(nrow1),mjd2(nrow2)

      i1_start=1
      do j=1,nrow1
         if (mjd1(j).ge.mjd_start) then
            goto 30
         endif
         i1_start=i1_start+1
      enddo
 30   continue
      i2_start=1
      do j=1,nrow1
         if (mjd2(j).ge.mjd_start) then
            goto 31
         endif
         i2_start=i2_start+1
      enddo
 31   continue

      nsame=0
      i2=i2_start
c      write(*,*) 'start:',i1_start,i2_start,nrow1,mjd1(1),mjd_end
      do j=i1_start,nrow1
 34      if (mjd1(j).gt.mjd_end) then
            goto 33
         endif
         i1=j
         if (mjd1(i1).eq.mjd2(i2)) then
            nsame=nsame+1
            mjds(nsame)=mjd1(i1)
            ts1s(nsame)=ts1(i1)
            ts2s(nsame)=ts2(i2)
c            write(*,'(2f20.5)') mjd1(i1),mjd2(i2)
            i2=i2+1
            goto 32
         endif
         if (mjd1(i1).gt.mjd2(i2)) then
            i2=i2+1
c            write(*,'(f20.5,a20)') mjd1(i1),' - '
            goto 34
         endif
         if (mjd1(i1).lt.mjd2(i2)) then
c            write(*,'(a20,f20.5)')  '-', mjd2(i2)
            goto 32
         endif
 32      continue
      enddo
 33   continue
c      write(*,*) '#common epochs:',nsame,nrow1,nrow2
      if (nsame.eq.0) then
         write(*,'(a)') '[lin_corr_xy]WARNING: no common epochs!'
         lin_corr_xy=0
         goto 39
      endif
      
c     means
      tmp1=0
      tmp2=0
      do i=1,nsame
         tmp1=tmp1+ts1s(i)
         tmp2=tmp2+ts2s(i)
      enddo
      mean1=tmp1/nsame
      mean2=tmp2/nsame

c     deviations
      sd1=0
      sd2=0
      cov12=0
      do i=1,nsame
         sd1=sd1+(ts1s(i)-mean1)**2
         sd2=sd2+(ts2s(i)-mean2)**2
         cov12=cov12+(ts1s(i)-mean1)*(ts2s(i)-mean2)
c         write(*,'(2f9.4)') ts1s(i),ts2s(i)
      enddo
c      stop

c     Pearson's correlation
c       corr_xy=cov12/(dsqrt(sd1)*dsqrt(sd2))

      sd1=sd1/(nsame)
      sd2=sd2/(nsame)
      cov12=cov12/(nsame)
c     Lin's concordance correlation coefficient
      lin_corr_xy=2*cov12/(sd1+sd2+(mean1-mean2)**2)
      
 39   continue
      return
      end
