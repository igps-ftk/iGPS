       program nblen 
c
c     given a character string, nblen returns the length of the string
c     to the last non-blank character, presuming the string is left-
c     justified, i.e. if string = '   xs  j   ', nblen = 8.
c
c     called non-library routines: none
c     language: standard fortran 77
c
      integer ls,i
      character*1024 string
      character*1 blank,null
      data blank /' '/
      integer*4 iargc
c
      if (iargc().lt.1) then
         write(*,'(i1)') 0
         stop
      endif

      call getarg(1,string)
      
      null = char(0)
c      nblen = 0
      ls = len(string)
      if (ls .eq. 0) go to 2
      do 1 i = ls, 1, -1
         if (string(i:i) .ne. blank .and. string(i:i) .ne. null) go to 2
    1    continue
c      return
c    2 nblen = i
 2    continue
      write(*,'(I10)') i
      end
c
c
