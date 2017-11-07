c     JPL lat/lon/rad
      program l_l_r2llr
C     ---
c     Input:
c     file:
c     ofile
c     ---
      IMPLICIT NONE
      include '../../inc/cgps.h'
      character*1000 file,ofile,tmpstr
c     ---
      integer*4 fid,fido,ioerr,ioerro,i,j,k
      real*8 lats(nmax_row,nmax_col),lons(nmax_row,nmax_col)
      real*8 rads(nmax_row,nmax_col),lat,lon,rad
      integer*4 nrow,ncol,tmpi(3)
      integer nblen,iargc
c     ---

      if (iargc().lt.1) then 
         write(*,*) 'Syntax: l_l_r2llr file [ofile]'
         stop
      endif
      call getarg(1,file)
      if (iargc().ge.2) then
         call getarg(2,ofile)
c         write(*,*) 'out:',ofile(1:nblen(ofile))
         fido=11
c     the fido cannot be 90
c     fido=90
c     Because the subroutines use file unit 90.
c     Thus, we should use file unit < 30 in the main program,
c     and > 30 in the subroutines.
c     But, it seems that conflicts cannot be ruled out completely.
C     What is the best method?
         open(unit=fido,file=ofile)
      else
         fido=6
      endif

      call l_l_r_read(file,lats,nrow,ncol)
      call desuffix(file,tmpstr)
      file=tmpstr(1:nblen(tmpstr))//'.lon'
c      write(*,*) 'lon file:', file(1:nblen(file))
      call l_l_r_read(file,lons,nrow,ncol)
    
      file=tmpstr(1:nblen(tmpstr))//'.rad'
c      write(*,*) 'rad file:', file(1:nblen(file))
      call l_l_r_read(file,rads,nrow,ncol)
 
c      write(*,*) 'writing...'
      do i=1,nrow
         tmpi(1)=lats(i,4)
         tmpi(2)=lats(i,5)
         tmpi(3)=lats(i,6)
c         (tmpi(j),j=1,3) =(lats(i,k),k=4,6)
c         tmpi =(lats(i,k),k=4,6)
c         write(fido,700) lats(i,1),lats(i,4),lats(i,5),lats(i,6),
         write(fido,700) lats(i,1),(tmpi(j),j=1,3),
     &        lats(i,2),lons(i,2),rads(i,2)
c         write(*,700) lats(i,1),lats(i,6),lats(i,5),lats(i,4),
c     &        lats(i,2),lons(i,2),rads(i,2)
      enddo
 700  format(f9.4,3i3.2,3f20.12)
c      write(*,*) 'wrote ok'

      if (fido.ne.6) then
c         write(fido,*) 'hello world'
         close(fido)
c         write(*,*) 'closed'
      endif

      stop
      end
