      SUBROUTINE Splint1(XA,YA,Y2A,NZ,N,X,Y)
      INTEGER*4 NZ,N
      real*8  XA(NZ),YA(NZ),Y2A(NZ),X,Y
      real*8  H,A,B
      INTEGER*4 KLO,KHI,K
      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.0e0) THEN
        print*,'Splint: XA(KHI) ',KHI,XA(KHI)
        PAUSE 'Bad XA input.'
      ENDIF
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     &       ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      RETURN
      END
