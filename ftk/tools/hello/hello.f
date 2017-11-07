      program hello
C     Input:
c     ofile:
c     ---
      IMPLICIT NONE
      character*512 ofile
      integer*4 fid,ioerr,iargc
c     ---
      if (iargc().lt.1) then
         write(*,*) "Usage: hello ofile.name"
         write(*,*) "hello: test whether the compiled "//
     &        "FORTRAN77 code support large(>99) file unit."
         stop
      endif

      call getarg(1,ofile)
      fid=101
      
      
      open(unit=fid,file=ofile,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,'(a)') 'Error when open file for writing!'
         write(*,'(a)') 'Check your FORTRAN 77 compiler.'
         stop
      endif
      write(fid,'(a)') 'hello world'
      close(fid)
      write(*,'(3a)') 'Test PASSED.'

      stop
      end
