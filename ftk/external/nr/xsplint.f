      PROGRAM xsplint
C     driver for routine splint, which calls spline
      INTEGER NP
      REAL PI
      PARAMETER(NP=10,PI=3.141593)
      INTEGER i,nfunc
      REAL f,x,y,yp1,ypn,xa(NP),ya(NP),y2(NP)
      do 14 nfunc=1,2
        if (nfunc.eq.1) then
          write(*,*) 'Sine function from 0 to PI'
          do 11 i=1,NP
            xa(i)=i*PI/NP
            ya(i)=sin(xa(i))
11        continue
          yp1=cos(xa(1))
          ypn=cos(xa(NP))
        else if (nfunc.eq.2) then
          write(*,*) 'Exponential function from 0 to 1'
          do 12 i=1,NP
            xa(i)=1.0*i/NP
            ya(i)=exp(xa(i))
12        continue
          yp1=exp(xa(1))
          ypn=exp(xa(NP))
        else
          stop
        endif
C     call SPLINE to get second derivatives
        call spline(xa,ya,NP,yp1,ypn,y2)
C     call SPLINT for interpolations
        write(*,'(1x,t10,a1,t20,a4,t28,a13)') 'x','f(x)','interpolation'
        do 13 i=1,10
          if (nfunc.eq.1) then
            x=(-0.05+i/10.0)*PI
            f=sin(x)
          else if (nfunc.eq.2) then
            x=-0.05+i/10.0
            f=exp(x)
          endif
          call splint(xa,ya,y2,NP,x,y)
          write(*,'(1x,3f12.6)') x,f,y
13      continue
        write(*,*) '***********************************'
        write(*,*) 'Press RETURN'
        read(*,*)
14    continue
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ,4-#.
