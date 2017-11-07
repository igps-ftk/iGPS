      PROGRAM xqrsolv
C     driver for routine qrsolv
      INTEGER NP
      PARAMETER (NP=20)
      REAL a(NP,NP),ai(NP,NP),b(NP,NP),c(NP),d(NP),x(NP)
      INTEGER j,k,l,m,n
      CHARACTER txt*3
      LOGICAL sing
      open(7,file='MATRX1.DAT',status='old')
      read(7,*)
10    read(7,*)
      read(7,*) n,m
      read(7,*)
      read(7,*) ((a(k,l), l=1,n), k=1,n)
      read(7,*)
      read(7,*) ((b(k,l), k=1,n), l=1,m)
C     save matrix a for later testing
      do 12 l=1,n
        do 11 k=1,n
          ai(k,l)=a(k,l)
11      continue
12    continue
C     do qr decomposition
      call qrdcmp(a,n,NP,c,d,sing)
      if (sing) write(*,*) 'Singularity in QR decomposition.'
C     solve equations for each right-hand vector
      do 16 k=1,m
        do 13 l=1,n
          x(l)=b(l,k)
13      continue
        call qrsolv(a,n,NP,c,d,x)
C     test results with original matrix
        write(*,*) 'Right-hand side vector:'
        write(*,'(1x,6f12.6)') (b(l,k), l=1,n)
        write(*,*) 'Result of matrix applied to sol''n vector'
        do 15 l=1,n
          b(l,k)=0.0
          do 14 j=1,n
            b(l,k)=b(l,k)+ai(l,j)*x(j)
14        continue
15      continue
        write(*,'(1x,6f12.6)') (b(l,k), l=1,n)
        write(*,*) '***********************************'
16    continue
      write(*,*) 'Press RETURN for next problem:'
      read(*,*)
      read(7,'(a3)') txt
      if (txt.ne.'END') goto 10
      close(7)
      END
