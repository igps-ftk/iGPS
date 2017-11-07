CTITLE
      SUBROUTINE dir_test(dir_cur,exi)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      character*(*) dir_cur

c     --OUTPUT--
      logical exi

c     --EXTERNAL--

c     --Local Parameters--

c     <<VAR_DEC
      inquire (file=dir_cur, exist=exi)

      RETURN
      END
