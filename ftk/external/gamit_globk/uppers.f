      subroutine uppers(string)
      character*(*) string
      integer*4 i,j,ichar

c     return an upper case string

      j = ichar('A')-ichar('a')
      do 10 i=1,len(string)
         if (lge(string(i:i),'a') .and. lle(string(i:i),'z')) then
            string(i:i)=char(ichar(string(i:i))+j)
         endif
  10  continue

      return
      end



