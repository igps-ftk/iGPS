      subroutine read_qmap(file,data,nrow,ncol,nhead,headers,
     +     site_name,long,lati)

C     Inputs:
c     file: character*256
c     firstepoch: int(2)
c     lastepoch: int(2)
c     xyzref: double(3)
c     neuref: double(3)
c     headers: character*256 n
c     data: double (M*N)

c     
      implicit none
      include '../../../inc/ftk.h'
c     --

      character*(*) file
      character*1000 buf

      integer*4 fid,ioerr
      integer*4 nrow,ncol,nhead
 
      character*(*) headers(nmax_head)
      real*8 data(nmax_row,nmax_col)

      character*8 site_name
      real*8 long,lati

      integer*4 i,j
      integer*4 nblen

      
      nhead=0

      call getlun(fid)
      open(unit=fid,file=file)  
      nrow=0
      ncol=10
 30   read(fid,'(a1000)', iostat=ioerr, end=90) buf

      if (buf(1:1).ne.' ') then
         nhead=nhead+1
         headers(nhead)=buf
      endif

      nrow=nrow+1
      
c 1998.65620000  -207.90    76.13     3.29     3.05  0.1785    -4.99     8.34 -0.2420 -0.2464  BJFS_GPS  115.8925   39.6086

      read(buf,*) (data(nrow,j),j=1,ncol),site_name,long,lati
      
      goto 30

 90   continue
      
      close(fid)

      if (nrow.gt.nmax_row) then
         write(*,'(2a,i6,a,i6,a)') '[read_qmap]ERROR: number of ',
     +        'lines (' ,nrow,
     +        ') exceeds program limit (',nmax_row,')!!!'
         write(*,'(17x,2a)') 'Please edit the nmax_row item in',
     +        ' $GPSF/inc/cgps.h .'
         stop
      endif

      return
      end


