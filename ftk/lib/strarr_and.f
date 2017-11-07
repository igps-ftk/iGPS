CTITLE strarr_and
      SUBROUTINE strarr_and(arr1,n1,arr2,n2,arr,n)
c     --PURPOSE--
c     The AND operation of two string arrays

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      character*(*) arr1(*),arr2(*)
      integer*4 n1,n2

c     --OUTPUT--
      character*(*) arr(*)
      integer*4 n

c     --Local Parameters--
      integer*4 i,j

c     <<VAR_DEC
      n=0
      do i=1,n1
         do j=1,n2
            if (arr1(i).eq.arr2(j)) then
               n=n+1
               arr(n)=arr1(i)
               goto 800
            endif
         enddo
 800     continue
      enddo

      RETURN
      END
