      program asc2bin_sio
C     Input:
c     file: input file name
c     ofile: output file name
      integer*4 nmax_head
      parameter(nmax_head=100)
      character*1000 file, ofile, hfile
      integer*4 fid,ioerr,fido
      character*1000 buf1000
      character*100 headers(nmax_head)
      real*8 data(10000,30)
      integer*4 nrow, ncol, nhead
      integer*4 i,j
      integer nblen,iargc

      if (iargc().lt.2) then
         write(*,*) 'Syntax: exe_data_asc2bin_sio file ofile'
         stop
         write(*,*) 'Use debug settings:'
         file='/home/tianyf/data/sio/bjfsCleanUnf.neu'
         ofile='/home/tianyf/data/sio/bjfsCleanUnf.neu.bin'
      endif
      call getarg(1,file)
      call getarg(2,ofile)
      write(*,*) file(1:nblen(file))
      write(*,*) ofile(1:nblen(ofile))
      nrow=10000
      ncol=30
      nhead=100
      call data_read_sio(file, data, nrow, ncol, nhead, headers)
c      do i=1, nrow
c         write(*,*) data(i,1)
c      enddo
c      write(*,*) '#nrow:', nrow, ncol, nhead
      call write_sio_bin(ofile, data, nrow, ncol, nhead, headers)
      hfile=ofile(1:nblen(ofile))//'.hdr'
      write(*,*) hfile(1:nblen(hfile))
      call write_sio_hdr(hfile, nhead, headers, nmax_head, nrow, ncol)
      nhead=0
      nrow=0
      ncol=0
      call read_sio_hdr(hfile, nhead, headers, nmax_head, nrow, ncol)
c$$$      do i=1, nhead
c$$$         write(*,'(a80)') headers(i)
c$$$      enddo
c      write(*,*) nhead, nrow, ncol

c      call getpathname(file,hfile)
c      write(*,*) hfile(1:nblen(hfile))

      end
