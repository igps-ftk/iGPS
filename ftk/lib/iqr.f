      function iqr(ts,nmax,n,ii,win)
      
c     Algorithm:
c     IQR calculation is based upon *de-trended* time series.
c     Nikolaidis [2002] used a sliding window of 365 days.
c     Outliers: residual > 3 * IQR

c     Inputs:
      integer*4 nmax
      real*8 ts(nmax)
      integer*4 n,ii,win
      
c     Output:
      real*8 iqr

c     Locals
      real*8 ts_sub(win)
      integer*4 i,hwin

c     External
      real*8 stddev

c     win should not be even
      if (mod(win,2).eq.0) then
         write(*,*) 'Window Size of IQR should not be EVEN!'
         stop
      endif
      
c     Half of the lenght of window
c     say, if 365
c     then
c     hwin=365/2=182 days
      hwin=int(win/2)

      if (ii-hwin.lt.1) ii=win/2
      if (ii+hwin.gt.n) ii=n-win/2

      do i=1,win
         ts_sub(i)=ts(ii-hwin+i-1)
      enddo

      iqr=stddev(ts_sub,win)
      iqr=iqr*1.35
      
      return
      end

