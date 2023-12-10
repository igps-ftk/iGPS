      SUBROUTINE avevar(data,n,ave,var)
      INTEGER*4 n
      REAL*8 ave,var,data(n)
      INTEGER*4 j
      REAL*8 s,ep
      ave=0.0
      do 11 j=1,n
        ave=ave+data(j)
11    continue
      ave=ave/n
      var=0.0
      ep=0.0
      do 12 j=1,n
        s=data(j)-ave
        ep=ep+s
        var=var+s*s
12    continue
      var=(var-ep**2/n)/(n-1)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ,4-#.
