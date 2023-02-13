CTITLE
      PROGRAM fdiff

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1023 file1, file2

c     difference mode:
      integer*4 dmd
c       0 - output common lines in both files (default);      
c       1 - output unique lines in 1st file;
c       2 - output unique lines in 2nd file.

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 nblen

c     --Local Parameters--
      integer*4 nmax
      parameter(nmax=1000000)
      character*1023,line1,line2,lines1(nmax),lines2(nmax)
      integer*4 nl1,nl2,i,j,inds1(nmax),inds2(nmax),nind

c     <<VAR_DEC

      if (iargc().lt.2) then
        write(*,'(3a)') 'Usage: fdiff file1 file2 [mode]'
        write(*,'(3a)') '  mode:'
        write(*,'(3a)') '    0-common lines in both (default)'   
        write(*,'(3a)') '    1-unique lines in 1st file'
        write(*,'(3a)') '    2-unique lines in 2nd file'
        stop
      endif

      call getarg(1,file1)
      call getarg(2,file2)
      dmd=0
      if (iargc().ge.3) then
        call getarg(3,line1)
        read(line1,*) dmd
c        write(*,*) line1
      endif
      
      write(*,*) '[]INFO:file1: ',file1(1:nblen(file1))
      write(*,*) '[]INFO:file2: ',file2(1:nblen(file2))
      write(*,*) '[]INFO:mode:',dmd
c      stop
      
      call read_txt(file1,lines1,nl1)
      write(*,*) '[]INFO:#file1:',nl1
      if (nl1.ge.nmax) then
        write(*,*) '[]ERROR: 1st file has too many lines (',nl1,
     +    ' vs.',nmax,')!!'
        stop
      endif
c701   format(a,i,a,i,a)      
      call read_txt(file2,lines2,nl2)
      write(*,*) '[]INFO:#file2:',nl2
      if (nl2.ge.nmax) then
        write(*,*) '[]ERROR: end file has too many lines (',nl2,
     +    ' vs.',nmax,')!!'
        stop
      endif
      
      
      nind=0
      
      if (dmd.eq.0.or.dmd.eq.2)  goto 902
      
c     handle mode 1      
      if (dmd.ne.1) then 
        write(*,*) '[]ERROR: wrong mode(',dmd,')!!'
        stop
      endif
      do i=1,nl1
        line1=lines1(i)
        inds1(i)=0
        do j=1,nl2
          line2=lines2(j)
          if (line1.eq.line2) then
            inds1(i)=1
            goto 903
          endif
        enddo
        if (inds1(i).eq.0) then
          write(*,'(a)') line1(1:nblen(line1))
c          stop
          nind=nind+1
        endif
903     continue
      enddo      
      goto 904
      
c     mode 0/2      
902   continue      
      do i=1,nl2
        line2=lines2(i)
        inds2(i)=0
        do j=1,nl1
          line1=lines1(j)
          if (line1.eq.line2) then
            inds2(i)=1
            goto 901
          endif
        enddo
901     continue
        if (dmd.eq.2.and.inds2(i).eq.0) then
          write(*,'(a)') line2(1:nblen(line2))
c          stop
          nind=nind+1
        endif
        if (dmd.eq.0.and.inds2(i).eq.1) then
          write(*,'(a)') line2(1:nblen(line2))
c          stop
          nind=nind+1
        endif
      enddo
      
904   continue     
      if (dmd.eq.0) then
        write(*,*) '[]INFO:#common lines in both files:',nind
      endif 
      if (dmd.eq.1) then
        write(*,*) '[]INFO:#unique lines in 1st file:',nind
      endif
      if (dmd.eq.2) then
        write(*,*) '[]INFO:#unique lines in 2nd file:',nind
      endif
      
      
999   STOP
      END
