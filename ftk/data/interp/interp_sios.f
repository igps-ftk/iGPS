      program interp_sios
C     Input:
c     path: input file name
c     opath: output file name
c     --
      implicit none
      include '../../inc/cgps.h'
c     --
      character*1000 file, ofile, hfile,files(nmax_site),path,opath
      integer*4 fid,ioerr,fido
      character*1000 buf1000, filter
      character*100 headers(nmax_head)
      real*8 data(nmax_row,nmax_col),datao(nmax_row,nmax_col)
      integer*4 ind_time,ind_neu(3)
      integer*4 nrow,ncol,nhead
      integer*4 i,j,k,m,n,nf,ci

      real*8 t(nmax_row),x(nmax_row),tintp(nmax_row),xintp(nmax_row)
      integer*4 nintp
      real*8 tmin,tmax,tstep,tmpr81
      
      integer*4 year,ydoy,date(5),doy
      real*8 jd,decyr
      real*8 sec,mjd

      real*8 ot(nmax_row),ox(nmax_row)
      integer*4 onrow

      integer nblen,iargc

c      integer*4 date(5),sec
      
c     ---

      if (iargc().lt.2) then
         write(*,*) 'Syntax: interp_sios path opath'
         stop
      endif
      call getarg(1,path)
      call getarg(2,opath)
c      write(*,*) path(1:nblen(path))
c      write(*,*) opath(1:nblen(opath))

      n=0
      filter='*.neu'

      call ffind (path,files,filter,nf,1 ) 

      do i=1,nf
         file=files(i)
         nrow=10000
         ncol=30
         nhead=100
         write(*,*) 'process ..',file(1:nblen(file))
         call read_sio(file,data,nrow,ncol,nhead,headers)
c         write(*,*) nrow,ncol
c         tmin=data(1,1)
c         tmax=data(nrow,1)
c         step=0.0027

         do j=1,nrow
            year=data(j,2)
            ydoy=data(j,3)
            call yds_to_jd(year,ydoy,sec,mjd)
c     start from 00:00 hrs
c            t(j)=mjd-2400000.5d0
c     start from 12:00 hrs
            t(j)=mjd-2400000.d0
c            write(*,*) t(j),x(j),year,ydoy
c            write(*,'(9f12.5)') (data(j,k),k=1,ncol)
c            datao(j,1)=data(j,1)
c            datao(j,2)=data(j,2)
c            datao(j,3)=data(j,3)
         enddo

         do ci=4,ncol
            do j=1,nrow
               x(j)=data(j,ci)
            enddo
            
            call interp_tx(t,x,nmax_row,nrow,ot,ox,onrow)
            
            do j=1,onrow
               call mjd_to_ymdhms(ot(j),date,sec)
               call ymd_to_doy(date,doy)
               datao(j,2)=date(1)
               datao(j,3)=doy
               jd=ot(j)+2400000.5d0
               call jd_to_decyrs(jd,decyr)
c               write(*,*) decyr,jd,mjd
               datao(j,1)=decyr
               datao(j,ci)=ox(j)
            enddo
         enddo

         call getfilename(file, ofile)
         ofile=opath(1:nblen(opath))//pathsep//ofile
         write(*,*) '  out:',ofile(1:nblen(ofile))
         call write_sio(ofile,datao,onrow,ncol,headers,nhead)
c         write(*,*) onrow,ncol,nhead
c         stop
      enddo
      
      end
