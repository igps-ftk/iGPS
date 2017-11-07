c+
c Name:
c     rawnew
c Purpose:
c     create a new RAW image file
c Input:
c     file - filename
c     ns -
c     nl -
c     nb -
c     dt -
c-
      subroutine rawnew(file,ns,nl,nb,dt)


c     variables-->
      character*(*) file
      integer*4 ns,nl,nb,dt,i,j,k
      integer fid 

c     <--|


c     executables
      fid=90
      write(*,'(2A)') 'output to:',file
      write(*,*) 'ns:',ns,' nl:',nl,' nb:',nb,' dt:',dt
c      nl=2 
      open(unit=fid,file=file,
     .   access='direct',recl=4,status='replace')
      do i=1,nl
         do j=1,ns
c            do k=1,nb
               write(fid,rec=(i-1)*ns+j) (i-1)*ns+j
c               write(*,*)  (i-1)*ns+j
c            enddo
         enddo
      enddo
      close(fid)


      end
