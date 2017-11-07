      PROGRAM xspline
C     driver for routine spline
      INTEGER N
      REAL PI
      PARAMETER(N=20,PI=3.141593)
      INTEGER i
      REAL yp1,ypn,x(N),y(N),y2(N)
      write(*,*) 'Second-derivatives for sin(x) from 0 to PI'
C     generate array for interpolation
      do 11 i=1,20
        x(i)=i*PI/N
        y(i)=sin(x(i))
11    continue
C     calculate 2nd derivative with SPLINE
      yp1=cos(x(1))
      ypn=cos(x(N))
      call spline(x,y,N,yp1,ypn,y2)
C     test result
      write(*,'(t19,a,t35,a)') 'spline','actual'
      write(*,'(t6,a,t17,a,t33,a)') 'angle','2nd deriv','2nd deriv'
      do 12 i=1,N
        write(*,'(1x,f8.2,2f16.6)') x(i),y2(i),-sin(x(i))
12    continue
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ,4-#.
