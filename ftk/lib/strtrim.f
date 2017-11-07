c+
c     delete two wides blanks
c-
      subroutine strtrim(strin,strout)
      character*(*) strin,strout
      character*512 strtmp
      integer i,j, nblen

c     delete right blanks 
c      strtmp=strin(1:nblen(strin))
c     delete left blanks
c      j=1
      do i=1,len(strin)
         if (strin(i:i).ne.' ') then
            goto 61
         endif
      enddo      
 61   strout=strin(i:nblen(strin))
c      write(*,*) i, strin(1:nblen(strin))
      return
      end
