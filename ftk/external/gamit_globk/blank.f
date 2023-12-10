      subroutine blank(string)
c
c     blank a character string
c
      integer ls,i,nblen
      character*(*) string
      character*1 blk
      data blk/' '/
c
      ls = nblen(string)
      if (ls .eq. 0) return
      do i = 1,ls
         string(i:i) = blk
      enddo

      return
      end
c
c
