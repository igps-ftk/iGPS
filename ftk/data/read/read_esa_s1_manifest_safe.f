      subroutine read_esa_s1_manifest_safe(file,orbtype,track,xys)

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
      include '../../inc/cgps.h'
c     --

      character*(*) file
      character*1023 buf,buf1,buf2,orbtype
      integer*4 track
      real*8 xys(2,4)

      integer*4 fid,ioerr
 

      integer*4 i,j,p1,p2
      integer*4 nblen


      call getlun(fid)
c      write(*,*) 'fid is ',fid, 'for ', file(1:nblen(file))

      open(unit=fid,file=file,iostat=ioerr,status='old')
      if (ioerr.ne.0) then
         write(*,'(3a)') '[]ERROR:cannot open file ',
     .        file(1:nblen(file)),' for reading!!'
         stop
      endif

 30   read(fid,'(a1023)', iostat=ioerr, end=90) buf

      if (index(buf,'pass').gt.1) then
         p1=index(buf,'>')
         p2=index(buf,'</')
         orbtype=buf(p1+1:p2-1)
c         write(*,*) ''
         goto 30
      endif


      if (index(buf,'relativeOrbitNumber').gt.1.and.
     .     index(buf,'start').gt.1) then
         p1=index(buf,'>')
         p2=index(buf,'</')
         read( buf(p1+1:p2-1),*) track
c         write(*,*) 'track buf ',buf(1:nblen(buf))
         goto 30
      endif

      if (index(buf,'coordinates').gt.1) then
         p1=index(buf,'>')
         p2=index(buf,'</')
         buf1=buf(p1+1:p2-1)
         call strrep(buf1,',',' ',buf2)
c         write(*,*) 'xys buf ',buf2(1:nblen(buf2))
         read( buf2,*) xys
         goto 30
      endif


      goto 30

 90   continue
      close(fid)
      return
      end


