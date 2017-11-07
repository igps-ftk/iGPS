c     JPL lat/lon/rad
      program l_l_r2llrs
C     ---
c     Input:
c     path:
c     opath
c     ---
      IMPLICIT NONE
      include '../../inc/cgps.h'
      character*512 file,ofile,tmpstr,path,opath,files(3000)
      character*512 filter,tmpstr2
c     ---
      integer*4 fid,fido,ioerr,ioerro,i,j,fi
      real*8 lats(nmax_row,nmax_col),lons(nmax_row,nmax_col)
      real*8 rads(nmax_row,nmax_col),lat,lon,rad
      integer*4 nrow,ncol,n,tmpi(3)
      integer nblen,iargc
      integer*4 doy,year,seci4
c     external function
      integer idoy
c     ---
       if (iargc().lt.2) then
         write(*,*) 'Syntax: l_l_r2llrs path opath'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
      write(*,*) 'input:',path(1:nblen(path))
      write(*,*) 'output:',opath(1:nblen(opath))

      n=0
      fido=11
      filter='*.lat'
      call ffind (path, files, filter,n,1 ) 
      write(*,*) '#total files:',n
      do fi=1,n
         file=files(fi)

c     the fido cannot be 90
c     fido=90
c     Because the subroutines use file unit 90.
c     Thus, we should use file unit < 30 in the main program,
c     and > 30 in the subroutines.
c     But, it seems that conflicts cannot be ruled out completely.
C     What is the best method?
    

         call l_l_r_read(file,lats,nrow,ncol)
         call desuffix(file,tmpstr)
         file=tmpstr(1:nblen(tmpstr))//'.lon'
c      write(*,*) 'lon file:', file(1:nblen(file))
         call l_l_r_read(file,lons,nrow,ncol)
    
         file=tmpstr(1:nblen(tmpstr))//'.rad'
c      write(*,*) 'rad file:', file(1:nblen(file))
         call l_l_r_read(file,rads,nrow,ncol)
 
c      write(*,*) 'writing...'
         call desuffix(file,tmpstr)
         call getfilename(tmpstr,tmpstr2)
         write(*,*) 'output:',opath(1:nblen(opath))
         ofile=opath(1:nblen(opath))//pathsep//
     &        tmpstr2(1:nblen(tmpstr2))//'.neu'
         write(*,*) '> ',ofile(1:nblen(ofile))
c         goto 800
         
c     blank file:
         if (nrow.lt.1) then
            goto 800
         endif

         open(unit=fido,file=ofile)
         do i=1,nrow
         tmpi(1)=lats(i,4)
         tmpi(2)=lats(i,5)
         tmpi(3)=lats(i,6)
c         (tmpi(j),j=1,3) =(lats(i,k),k=4,6)
c         tmpi =(lats(i,k),k=4,6)
c         write(fido,700) lats(i,1),lats(i,4),lats(i,5),lats(i,6),
c         write(fido,700) lats(i,1),
         write(fido,701) lats(i,1),
     &        int(lats(i,1)),idoy(int(lats(i,1)),tmpi(2),tmpi(3)),
c     &        (tmpi(j),j=1,3),
     &        lats(i,2)/100,lons(i,2)/100,rads(i,2)/100,
     .        lats(i,3)/100,lons(i,3)/100,rads(i,3)/100
c            write(fido,700) lats(i,1),lats(i,4),lats(i,5),lats(i,6),
c     &           lats(i,2),lons(i,2),rads(i,2)
c         write(*,700) lats(i,1),lats(i,6),lats(i,5),lats(i,4),
c     &        lats(i,2),lons(i,2),rads(i,2)
         enddo
c 700     format(f9.4,3f4.0,3f20.12)
c 700  format(f9.4,3i3.2,3f20.12,3f20.12)
 701  format(f9.4,i5,i4,3f20.12,3f20.12)
c      write(*,*) 'wrote ok'


         close(fido)
 800     continue
      enddo
      

      stop
      end
