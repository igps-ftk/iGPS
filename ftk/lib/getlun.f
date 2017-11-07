c     http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/901d78c2b1a11a84/6b60c7fccff6f4e8?lnk=gst&q=getlun&rnum=2&hl=en#6b60c7fccff6f4e8
*     +GETLUN  Returns number of a logical unit which is free for use.
      SUBROUTINE GETLUN(LUN)
      INTEGER*4 LUN
*     LUN  Returns logical unit number to use, or -1 if none available
*-
*     Allocates first available unit in range 8 to 99, since units up to 7
*     are pre-connected on some systems, numbers >99 not always permitted.
      LOGICAL OPEN, EXISTS
*
      DO 100, LUN = 8, 99
          INQUIRE(UNIT=LUN, EXIST=EXISTS, OPENED=OPEN)
          IF(EXISTS .AND. .NOT. OPEN) GO TO 999
100   CONTINUE
      PRINT *, 'getlun: no more I/O units free'
      LUN = -1      
999   CONTINUE
      END 
