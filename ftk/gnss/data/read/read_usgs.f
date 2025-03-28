      SUBROUTINE read_usgs(file,data,nrow,ncol,nhead,headers)
c     --PURPOSE--

c     --INPUT--

c     --OUTPUT--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --Global Variables--
      IMPLICIT NONE
      include '../../../inc/ftk.h'

c     --Command-line Parameters--

c     --Local Parameters--

      character*(*) file
      character*1000 buf

      integer*4 fid,ioerr
      integer*4 nrow,ncol,nhead
 
      character*(*) headers(nmax_head)
      real*8 data(nmax_row,nmax_col)

      integer*4 i,j
      integer*4 nblen

      integer tmpncol
      character*512 tmpstrs(nmax_head)

      integer*4 yr,doy,hh,mm,ss
      character*512 tmpstr

      character*512 fmt

c      logical debug
     

c     <<VAR_DEC
c      debug=.false.

      call file_info_usgs(file,nrow,ncol,nhead,headers)
      if (debug) then
         write(*,*) nrow,ncol,nhead
      endif

      call getlun(fid)
      open(unit=fid,file=file)  
      i=0   
 30   read(fid,'(a1000)', iostat=ioerr, end=90) buf
      i=i+1
c      if (buf(1:1).eq.'#') then
c         write(*,*) buf(1:nblen(buf))
      if (i.ge.nhead) goto 801
      goto 30

801   continue
c     read the data line by line
      do i=1,nrow
c         write(*,*) i
c     fist, read the current into line buffer
         read(fid,'(a1000)') buf
c         write(*,*) buf(1:nblen(buf))
c         write(*,*) nblen(buf),i,nrow
         if (nblen(buf).lt.1) goto 802
c     second, decompose the line buffer into splits by calling str_split
         call strsplit(buf,',',tmpncol,tmpstrs)
c     read the first column
         read(tmpstrs(1),*) data(i,1)
c         write(*,*) data(i,1)
c     skip the second column
c     read year/doy/hour/min/sec 
         tmpstr=tmpstrs(2)
c         tmpstr=tmpstr(index(tmpstr,' '):)
         call trimlead(tmpstr)
c         write(*,*) tmpstr(1:nblen(tmpstr))
         read(tmpstr,700) yr,doy,hh,mm,ss
 700     format(i4,1x,i3,1x,i2,1x,i2,1x,i2)
         data(i,2)=doy

c     read the rest columns
         do j=3,ncol
            read(tmpstrs(j),*) data(i,j)
c            write(*,*) data(i,j)
         enddo
c         read(fid,*,end=90) (data(i,j),j=1,ncol)
c         write(*,'(9f15.4)') (data(i,j),j=1,ncol)
 802     continue
      enddo
      
      if (debug) then
         write(*,*) 'The last 5 lines of data:'
         write(fmt,'("(""Data Line #"",i10,",i5,"(F15.4))")') ncol
         write(*,*) fmt(1:nblen(fmt))
         do i=nrow-5,nrow
            write(*,fmt) i,(data(i,j),j=1,ncol)
         enddo
      endif


 90   continue
      close(fid)

c      write(*,*) 'debug:',debug
      return
      END
