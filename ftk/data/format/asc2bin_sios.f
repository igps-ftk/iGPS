      program asc2bin_sios
C     Input:
c     path: input file name
c     opath: output file name
      integer*4 nmax_head
      parameter(nmax_head=100)
      character*1000 file, ofile, hfile,files(10000),path,opath
      integer*4 fid,ioerr,fido
      character*1000 buf1000, filter
      character*100 headers(nmax_head)
      real*8 data(10000,30)
      integer*4 nrow, ncol, nhead
      integer*4 i,j,n
      integer nblen,iargc

      if (iargc().lt.2) then
         write(*,*) 'Syntax: exe_data_asc2bin_sio path opath'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      n=0
      filter='*.neu'
      call ffind ( path, files, filter,n,1 ) 

      do i=1,n
         file=files(i)
         nrow=10000
         ncol=30
         nhead=100
         call data_read_sio(file, data, nrow, ncol, nhead, headers)
         call getfilename(file, ofile)
         ofile=opath(1:nblen(opath))//ofile
         call write_sio_bin(ofile, data, nrow, ncol, nhead, headers)
c         hfile=ofile(1:nblen(ofile))//'.hdr'
         call desuffix(ofile,hfile)
         hfile=hfile(1:nblen(hfile))//'.hdr'
         write(*,*) hfile(1:nblen(hfile))
c         stop
         call write_sio_hdr(hfile, nhead, headers, nmax_head,nrow,ncol)
         write(*,'(2a)') '--ASC2BIN.. ',file(1:nblen(file))
c         write(*,*) ofile(1:nblen(ofile))
c         write(*,*) hfile(1:nblen(hfile))
      enddo
      
      end
