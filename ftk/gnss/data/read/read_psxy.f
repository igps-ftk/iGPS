CTITLE
      SUBROUTINE read_psxy(file,xys,npts,names,n)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
      INCLUDE '../../../inc/ftk.h'

c     >>VAR_DEC
c     --INPUT--
      character*(*) file

c     --OUTPUT--
      real*8 xys(2,nmax_row)
      integer*4 npts(nmax_row)
      character*(*) names(nmax_row)
      integer*4 n
      

c     --EXTERNAL--
      integer*4 nblen

c     --Local Parameters--
      integer*4 fid,nline,i,npt,ntotal
      character*1023 line,line1,lines(nmax_row)
      real*8 xi,yi

c     <<VAR_DEC

      call read_txt(file,lines,nline)
      write(*,*) 'nline:',nline
      
      n=0
      npt=0
      ntotal=0
      do i=1,nline
        line=lines(i)
        call strtrim(line,line1)
c        write(*,*) 'line1:',line1(1:nblen(line1))
        if (line1(1:1).eq.'>') then
          n=n+1
          names(n)=line1(2:)
          if (n.gt.1) then
            npts(n-1)=npt
          endif
          npt=0
          goto 601
        else
          npt=npt+1
          ntotal=ntotal+1
          read(line1,*) xi,yi
          xys(1,ntotal)=xi
          xys(2,ntotal)=yi
        endif
        
601   continue
      enddo
      npts(n)=npt
      
      write(*,*) n,npts(1:n),ntotal

      RETURN
      END
