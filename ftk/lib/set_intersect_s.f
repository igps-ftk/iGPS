CTITLE
      SUBROUTINE set_intersect_s(set0,nmax0,n0,set1,nmax1,n1,
     &     ind0,nmaxind0,nind0,ind1,nmaxind1,nind1, 
     &     set01,nmaxset01,nset01)

c     --PURPOSE--

c     --ALGORITHM--
c     The intersection of A and B is written "A ? B". Formally:
c
c    x is an element of A ? B if and only if
c
c        * x is an element of A and
c        * x is an element of B.
c
c    For example:
c
c        * The intersection of the sets {1, 2, 3} and {2, 3, 4} is {2, 3}.
c        * The number 9 is not in the intersection of the set of prime 
c     numbers {2, 3, 5, 7, 11, ?} and the set of odd numbers 
c     {1, 3, 5, 7, 9, 11, ?}.


c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '.../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 nmax0,n0,nmax1,n1,nmaxind0,nmaxind1,nmaxset01
      character*(*) set0(nmax0),set1(nmax1)
      integer*4 ind0(nmaxind0),ind1(nmaxind1)

c     --OUTPUT--
      integer*4 nind0,nind1,nset01
      character*(*) set01(nmaxset01)

c     --EXTERNAL--
      integer*4 nblen

c     --Local Parameters--
      integer*4 i,j,k,ji
      character*1023 buf1,buf0

c     <<VAR_DEC
      if (n0.le.0.or.n1.le.0) then
         nset01=0
         return
      endif

c      write(*,*) nmax0,n0,nmax1,n1,nmaxind0,nind0,nmaxind1,nind1,
c     &     nmaxset01,nset01
      nset01=0
      nind0=0
      nind1=0
c      ji=1
      do i=1,n0
c         write(*,*) 'i:',i
         do j=1,n1
            buf0=set0(i)
            buf1=set1(j)
            if (buf0(1:nblen(buf0)).eq.buf1(1:nblen(buf1))) then
              nset01=nset01+1
              set01(nset01)=set0(i)
              nind0=nind0+1
              nind1=nind1+1
              ind0(nind0)=i
              ind1(nind1)=j
              goto 800
           endif
c           ji=ji+1
        enddo
 800    continue
      enddo

      RETURN
      END
