      subroutine lowers(string)
      character*(*) string
      integer*4 i,j,ichar

c     return a lower case string

      j = ichar('a')-ichar('A')
      do 10 i=1,len(string)
         if (lge(string(i:i),'A') .and. lle(string(i:i),'Z')) then
            string(i:i)=char(ichar(string(i:i))+j)
         endif
  10  continue

      return
      end

