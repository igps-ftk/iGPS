      PROGRAM xqrdcmp
C     driver for routine qrdcmp
      INTEGER NP
      PARAMETER(NP=20)
      INTEGER i,j,k,l,m,n
      REAL con,a(NP,NP),c(NP),d(NP),q(NP,NP),qt(NP,NP),r(NP,NP),
     *     x(NP,NP)
      CHARACTER txt*3
      LOGICAL sing
      open(7,file='MATRX1.DAT',status='old')
      read(7,*)
10    read(7,*)
      read(7,*) n,m
      read(7,*)
      read(7,*) ((a(k,l), l=1,n), k=1,n)
      read(7,*)
      read(7,*) ((x(k,l), k=1,n), l=1,m)
C     print out a-matrix for comparison with product of Q and R
C     decomposition matrices.
      write(*,*) 'Original matrix:'
      do 11 k=1,n
        write(*,'(1x,6f12.6)') (a(k,l), l=1,n)
11    continue
C     perform the decomposition
      call qrdcmp(a,n,NP,c,d,sing)
      if (sing) write(*,*) 'Singularity in QR decomposition.'
C     find the Q and R matrices
      do 13 k=1,n
        do 12 l=1,n
          if (l.gt.k) then
            r(k,l)=a(k,l)
            q(k,l)=0.0
          else if (l.lt.k) then
            r(k,l)=0.0
            q(k,l)=0.0
          else
            r(k,l)=d(k)
            q(k,l)=1.0
          endif
12      continue
13    continue
      do 21 i=n-1,1,-1
        con=0.0
        do 14 k=i,n
          con=con+a(k,i)**2
14      continue
        con=con/2.0
        do 17 k=i,n
          do 16 l=i,n
            qt(k,l)=0.0
            do 15 j=i,n
              qt(k,l)=qt(k,l)+q(j,l)*a(k,i)*a(j,i)/con
15          continue
16        continue
17      continue
        do 19 k=i,n
          do 18 l=i,n
            q(k,l)=q(k,l)-qt(k,l)
18        continue
19      continue
21    continue
C     compute product of Q and R matrices for comparison with original matrix.
      do 24 k=1,n
        do 23 l=1,n
          x(k,l)=0.0
          do 22 j=1,n
            x(k,l)=x(k,l)+q(k,j)*r(j,l)
22        continue
23      continue
24    continue
      write(*,*) 'Product of Q and R matrices:'
      do 25 k=1,n
        write(*,'(1x,6f12.6)') (x(k,l), l=1,n)
25    continue
      write(*,*) 'Q matrix of the decomposition:'
      do 26 k=1,n
        write(*,'(1x,6f12.6)') (q(k,l), l=1,n)
26    continue
      write(*,*) 'R matrix of the decomposition:'
      do 27 k=1,n
        write(*,'(1x,6f12.6)') (r(k,l), l=1,n)
27    continue
      write(*,*) '***********************************'
      write(*,*) 'Press RETURN for next problem:'
      read(*,*)
      read(7,'(a3)') txt
      if (txt.ne.'END') goto 10
      close(7)
      END
