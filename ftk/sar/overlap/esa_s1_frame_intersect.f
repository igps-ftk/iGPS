CTITLE
      PROGRAM esa_s1_frame_intersect

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

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

c     file : polgyon coordinates in GMT psxy format
      character*1023 file1
c     file : polgyon coordinates in GMT psxy format
      character*1023 file2

c     --OUTPUT--
c     output reults (1: in; 0: out)
      integer*4 is_in

c     --EXTERNAL--
      integer*4 iargc,nblen,is_point_inside_polygon
      character*1 path_sep

c     --Local Parameters--
c     1 x : e.g., longtitude
      real*8 x
c     2 y : e.g., latitude
      real*8 y

c     maximum number of vertices of polygon
      integer*4 NMax
      parameter(NMAX=100000)


c     for reading polygon file
      real*8 xys1(2,NMAX),xys2(2,NMAX),xi,yi,xys(2,nmax)


      character*1023 tmpstr,buf,prog,frame_ids(nmax),frame_id
      integer*4 np1,np2,np,i,j,nps1(nmax),inds1(nmax),nl,pos
      integer*4 fid,ioerr


c     <<VAR_DEC
      prog='esa_s1_frame_intersect'
      if (iargc().lt.2) then
         write(*,'(3a)') 'Usage: ',prog(1:nblen(prog)),
     +    ' vector_file1 vector_file2'
         stop
      endif
  

c      write(*,'(a)') '[i]Starting ...________________'
      call getarg(1,tmpstr)
      file1=tmpstr(1:nblen(tmpstr))
      write(*,'(3a)') '[]file1: ',file1(1:nblen(file1))
      call getarg(2,tmpstr)
      file2=tmpstr(1:nblen(tmpstr))
      write(*,'(3a)') '[]file2: ',file2(1:nblen(file2))
      
      

c     read vector file 2 (only read point coordinates (x,y))
      np2=0
      call getlun(fid)
c      write(*,*) 'fid is ',fid, 'for ', file(1:nblen(file))
      open(unit=fid,file=file2,iostat=ioerr,status='old')
      if (ioerr.ne.0) then
         write(*,'(3a)') '[]ERROR:cannot open file2 ',
     .        file2(1:nblen(file2)),' for reading!!'
         stop
      endif
 31   read(fid,'(a1023)', iostat=ioerr, end=91) buf
      if (buf(1:1).eq.">") then
        goto 31
      endif
      read(buf,*) xi,yi
      np2=np2+1
      xys2(1,np2)=xi
      xys2(2,np2)=yi      
      goto 31
 91   continue
      close(fid)
c      do i=1, np2
c        write(*,*) i,xys2(1,i),xys2(2,i)
c      enddo
c

c     read vector file 1 (polygon-by-polygon)
      do i=1,NMAX
        nps1(i)=0
        inds1(i)=0
      enddo
      call getlun(fid)
c      write(*,*) 'fid is ',fid, 'for ', file1(1:nblen(file1))
      open(unit=fid,file=file1,iostat=ioerr,status='old')
      if (ioerr.ne.0) then
         write(*,'(3a)') '[]ERROR:cannot open file1 ',
     .        file1(1:nblen(file1)),' for reading!!'
         stop
      endif
      np1=0
      nl=1
 30   read(fid,'(a1023)', iostat=ioerr, end=90) buf
c      write(*,*) buf(1:nblen(buf))
      if (buf(1:1).eq.">") then
        np1=np1+1
        inds1(np1)=nl
        pos=index(buf,'L')
        frame_id=buf(pos+1:pos+9)
        frame_ids(np1)=frame_id
        goto 30
      endif
      read(buf,*) xi,yi
      xys1(1,nl)=xi
      xys1(2,nl)=yi    
      nl=nl+1  
      goto 30
 90   continue
      close(fid)
      inds1(np1+1)=nl
      
      do i=1, np1
        frame_id=frame_ids(i)
c        write(*,*) i,inds1(i),inds1(i+1)-1,frame_id(1:nblen(frame_id))
        np=0
        do j=inds1(i),inds1(i+1)-1 
          np=np+1
          xys(1,np)=xys1(1,j)
          xys(2,np)=xys1(2,j)
c          write(*,*) xys1(1,j),xys1(2,j)
        enddo
        
C     if the fisrt and last points are the same, remove the last one      
        if (xys(1,1).eq.xys(1,np).and.xys(2,1).eq.xys(2,np)) then
          np=np-1
        endif    
c        do j=1,np
c          write(*,*) j,xys(1,j),xys(2,j)
c        enddo
        do j=1,np2
          x=xys2(1,j)
          y=xys2(2,j)  
          is_in=is_point_inside_polygon(xys,nmax,np,x,y)
          
c          write(*,*) "is_in",is_in,x,y,np
          if (is_in.eq.1) then
            write(*,*) frame_id(1:nblen(frame_id)),' is_in'
            goto 92
          endif
        enddo        
 92   continue
      enddo
c      write(*,*) 'np1,nl:',np1,nl
      
      stop
      



      STOP
      END
