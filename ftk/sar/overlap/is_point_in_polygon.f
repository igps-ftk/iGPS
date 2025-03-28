CTITLE
      PROGRAM is_point_in_polygon

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--


c

c     --EXAMPLE--

c     --MODIFICATIONS--
c     + On Mon Jan  4 09:42:36 CST 2021 by tianyf
c     .
c
c

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--

c     1 x : e.g., longtitude
      real*8 x
c     2 y : e.g., latitude
      real*8 y
c     3 file : polgyon coordinates in GMT psxy format
      character*1023 file

c     --OUTPUT--
c     output reults (1: in; 0: out)
      integer*4 is_in

c     --EXTERNAL--
      integer*4 iargc,nblen,is_point_inside_polygon
      character*1 path_sep

c     --Local Parameters--

c     maximum number of vertices of polygon
      integer*4 NMax
      parameter(NMAX=100000)


c     for reading polygon file
      real*8 xys(2,NMAX),xi,yi


      character*1023 tmpstr,buf
      integer*4 np,i
      integer*4 fid,ioerr


c     <<VAR_DEC
      if (iargc().lt.3) then
         write(*,'(3a)') 'Usage: is_point_in_polygon x y polygon_file'
         stop
      endif
  

c      write(*,'(a)') '[i]Starting ...________________'
      call getarg(1,tmpstr)
      read(tmpstr(1:nblen(tmpstr)),*) x
      write(*,*) '[]x:',x
      call getarg(2,tmpstr)      
      read(tmpstr(1:nblen(tmpstr)),*) y
      write(*,*) '[]y:',y
      call getarg(3,tmpstr)
      file=tmpstr(1:nblen(tmpstr))
      write(*,'(3a)') '[]file:',file(1:nblen(file))
      
      

      np=0
      call getlun(fid)
c      write(*,*) 'fid is ',fid, 'for ', file(1:nblen(file))

      open(unit=fid,file=file,iostat=ioerr,status='old')
      if (ioerr.ne.0) then
         write(*,'(3a)') '[]ERROR:cannot open file ',
     .        file(1:nblen(file)),' for reading!!'
         stop
      endif

 30   read(fid,'(a1023)', iostat=ioerr, end=90) buf
      if (buf(1:1).eq.">") then
        goto 30
      endif
      read(buf,*) xi,yi
      np=np+1
      xys(1,np)=xi
      xys(2,np)=yi
      
      goto 30

 90   continue
      close(fid)

      do i=1, np
        write(*,*) i,xys(1,i),xys(2,i)
      enddo
      
C     if the fisrt and last points are the same, remove the last one      
      if (xys(1,1).eq.xys(1,np).and.xys(2,1).eq.xys(2,np)) then
        np=np-1
      endif
      
      is_in=is_point_inside_polygon(xys,nmax,np,x,y)
      write(*,*) "is_in",is_in


      STOP
      END
