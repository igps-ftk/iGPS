      PROGRAM xlfit
C     driver for routine lfit
      INTEGER NPT,NTERM
      REAL SPREAD
      PARAMETER(NPT=100,SPREAD=0.1,NTERM=5)
      INTEGER i,idum,j,ia(NTERM)
      REAL chisq,gasdev
      REAL x(NPT),y(NPT),sig(NPT),a(NTERM),covar(NTERM,NTERM)
      EXTERNAL funcs
      idum=-911
      do 12 i=1,NPT
        x(i)=0.1*i
        call funcs(x(i),a,NTERM)
        y(i)=0.
        do 11 j=1,NTERM
          y(i)=y(i)+j*a(j)
11      continue
        y(i)=y(i)+SPREAD*gasdev(idum)
        sig(i)=SPREAD
12    continue
      do 13 i=1,NTERM
        ia(i)=1
13    continue
      call lfit(x,y,sig,NPT,a,ia,NTERM,covar,NTERM,chisq,funcs)
      write(*,'(/1x,t4,a,t22,a)') 'Parameter','Uncertainty'
      do 14 i=1,NTERM
        write(*,'(1x,t5,a,i1,a,f8.6,f11.6)') 'A(',i,') = ',
     *       a(i),sqrt(covar(i,i))
14    continue
      write(*,'(/3x,a,e12.6)') 'Chi-squared = ',chisq
      write(*,'(/3x,a)') 'Full covariance matrix'
      do 15 i=1,NTERM
        write(*,'(1x,6e12.2)') (covar(i,j),j=1,NTERM)
15    continue
      write(*,'(/1x,a)') 'press RETURN to continue...'
      read(*,*)
C     now check results of restricting fit parameters
      do 16 i=2,NTERM,2
        ia(i)=0
16    continue
      call lfit(x,y,sig,NPT,a,ia,NTERM,covar,NTERM,chisq,funcs)
      write(*,'(/1x,t4,a,t22,a)') 'Parameter','Uncertainty'
      do 17 i=1,NTERM
        write(*,'(1x,t5,a,i1,a,f8.6,f11.6)') 'A(',i,') = ',
     *       a(i),sqrt(covar(i,i))
17    continue
      write(*,'(/3x,a,e12.6)') 'Chi-squared = ',chisq
      write(*,'(/3x,a)') 'Full covariance matrix'
      do 18 i=1,NTERM
        write(*,'(1x,6e12.2)') (covar(i,j),j=1,NTERM)
18    continue
      END

      SUBROUTINE funcs(x,afunc,ma)
      INTEGER i,ma
      REAL x,afunc(ma)
      afunc(1)=1.
      afunc(2)=x
      do 11 i=3,ma
        afunc(i)=sin(i*x)
11    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ,4-#.
