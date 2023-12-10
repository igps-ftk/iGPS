CTITLE
       FUNCTION path_sep()
c     --PURPOSE--
c     Return the path separator of current OS.

c     --ALGORITHM--
c     call `uname -a` to get the information.

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
c     None.

c     --OUTPUT--
      character*1 path_sep

c     --EXTERNAL--
      integer*4 system

c     --Local Parameters--
      character*1024 cmdstr,tmpstr
      integer*4 status
      

c     <<VAR_DEC
      cmdstr='uname -a > /dev/null'
      status=system(cmdstr)
c     write(*,*) status
      if (status.eq.0) then
c     Linux/Unix/MacOS/Cygwin
         path_sep='/'
      else
c     Windows
         path_sep='\\'
c     At present, we do not consider other OS.
      endif

      RETURN
      END
