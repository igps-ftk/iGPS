      PROGRAM xperiod
C     driver for routine period
      INTEGER NP,NPR
      REAL TWOPI
      PARAMETER(NP=90,NPR=11,TWOPI=6.2831853)
      INTEGER idum,j,jmax,n,nout
      REAL gasdev,prob,x(NP),y(NP),px(2*NP),py(2*NP)
      idum=-4
      j=0
      do 11 n=1,NP+10
        if (n.ne.3.and.n.ne.4.and.n.ne.6.and.n.ne.21.and.
     *       n.ne.38.and.n.ne.51.and.n.ne.67.and.n.ne.68.and.
     *       n.ne.83.and.n.ne.93) then
          j=j+1
          x(j)=n
          y(j)=0.75*cos(0.6*x(j))+gasdev(idum)
        endif
c        write(*,*) x(n),y(n)
11    continue
      call period(x,y,j,4.,1.,px,py,2*NP,nout,jmax,prob)
      write(*,*) 'PERIOD results for test signal (cos(0.6x) + noise):'
      write(*,*) 'NOUT,JMAX,PROB=',nout,jmax,prob
      do 12 n=max(1,jmax-NPR/2),min(nout,jmax+NPR/2)
        write(*,*) n,TWOPI*px(n),py(n)
12    continue
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ,4-#.
