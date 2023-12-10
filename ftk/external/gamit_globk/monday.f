      SUBROUTINE MONDAY(IDOY,IM,IDAY,IYR)
C
C     CONVERT A DAY OF YEAR TO MONTH AND DAY
C     RICK ABBOT - NOVEMBER 1984
C
      integer marray(12),idoy,iday,im,iyr,isum

      DATA MARRAY/31,28,31,30,31,30,31,31,30,31,30,31/
C
c     reset this to avoid problem with previous calls
      marray(2) = 28
      IF (MOD(IYR,4).EQ.0) MARRAY(2)=29
      ISUM=0
      DO 12 IM=1,12
      ISUM=ISUM+MARRAY(IM)
      IF (IDOY.GT.ISUM) GO TO 12
      IDAY=IDOY-ISUM+MARRAY(IM)
      GO TO 14
 12   CONTINUE
C
 14   RETURN
      END
