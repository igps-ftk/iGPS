
c+
c     trim trailing blanks
c-
      subroutine trimright(strin, pos)
      character *(*) strin
      integer*4 i,pos,lenin 

      lenin=LEN(strin)
      do i=Len(strin),1,-1
         if (strin(i:i).ne.' ') go to 620
      end do
 620  continue
      pos=i

      return
      end
