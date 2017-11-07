CTITLE
      integer*4 FUNCTION intlen(n)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 n

c     --OUTPUT--

c     --EXTERNAL--

c     --Local Parameters--
      character*1000 tmpstr
      integer*4 i,istart,nblen

c     <<VAR_DEC
      write(tmpstr,'(i99)') n
      istart=1
      do i=1,nblen(tmpstr)
         if (tmpstr(i:i).ne.' ') then
            istart=i
            goto 700
         endif
      enddo
 700  continue

      intlen=nblen(tmpstr)-istart+1

      RETURN
      END
