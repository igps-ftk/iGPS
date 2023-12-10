      real*8 function correl(ts1,ts2,nmax,n)
C     ---
      integer*4 namx,n
      real*8 ts1(nmax),ts2(nmax)
C     ---
      

      correl=0d0
      return
      end

      real*8 function cov_mtrx(x,nmax,
