      PROGRAM xamoeba
C     driver for routine amoeba
      INTEGER NP,MP
      REAL FTOL
      PARAMETER(NP=3,MP=4,FTOL=1.0E-6)
      INTEGER i,iter,j,ndim
      REAL famoeb,p(MP,NP),x(NP),y(MP)
      EXTERNAL famoeb
      DATA p/0.0,1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0/
      ndim=NP
      do 12 i=1,MP
        do 11 j=1,NP
          x(j)=p(i,j)
11      continue
        y(i)=famoeb(x)
12    continue
      call amoeba(p,y,MP,NP,ndim,FTOL,famoeb,iter)
      write(*,'(/1x,a,i3)') 'Number of iterations: ',iter
      write(*,'(/1x,a)') 'Vertices of final 3-D simplex and'
      write(*,'(1x,a)') 'function values at the vertices:'
      write(*,'(/3x,a,t11,a,t23,a,t35,a,t45,a/)') 'I',
     *     'X(I)','Y(I)','Z(I)','FUNCTION'
      do 13 i=1,MP
        write(*,'(1x,i3,4f12.6)') i,(p(i,j),j=1,NP),y(i)
13    continue
      write(*,'(/1x,a)') 'True minimum is at (0.5,0.6,0.7)'
      END

      REAL FUNCTION famoeb(x)
      REAL bessj0,x(3)
      famoeb=0.6-bessj0((x(1)-0.5)**2+(x(2)-0.6)**2+(x(3)-0.7)**2)
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ,4-#.
