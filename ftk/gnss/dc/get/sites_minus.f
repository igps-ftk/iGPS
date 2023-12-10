CTITLE
      program sites_minus
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      character*10230 sitestr1,sitestr2

c     --OUTPUT--

c     --EXTERNAL--

c     --Local Parameters--
      character*4 sites1(10000),sites2(10000)
      integer*4 n1,n2,i,pos,iargc

c     <<VAR_DEC
      
      if (iargc().lt.2) then
         write(*,*) 'Usage: sites_minus sites sites_minus'
         stop
      endif

      call getarg(1,sitestr1)
      call getarg(2,sitestr2)

      call strsplit(sitestr1,',',n1,sites1)
c      call strsplit(sitestr2,',',n2,sites2)

      do i=1,n1
         pos=index(sitestr2,sites1(i))
         if (pos.gt.0) goto 800
         write(*,'(a4)') sites1(i)
 800     continue
      enddo
      


      

c      RETURN
      END
