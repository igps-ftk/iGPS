c$$$;APR09 2007 Tian
c$$$;  +A little progress for loop, much faster now;
c$$$;
c$$$;Data Sample:
c$$$;	PBO Station Position Time Series
c$$$;	Format Version: 1.0.1
c$$$;	4-character ID: AB11
c$$$;	Station name  : Nome_AnvilAK2006
c$$$;	First Epoch   : 20060721 120000
c$$$;	Last Epoch    : 20070310 120000
c$$$;	Release Data  : 20070404 073140
c$$$;	XYZ Reference position :  -2658010.23252  -693674.79470  5737338.58385
c$$$;	NEU Reference position :    64.5644967742  194.6265414893  349.44237
c$$$; 	20060721 120000 53937.5000 -2658010.23592  -693674.79682  5737338.58572  0.00407  0.00260  0.00736  0.505 -0.677 -0.528
c$$$;		      64.5644967558  194.6265415143  349.44631    -0.00264   0.00119   0.00333    0.00282  0.00219  0.00805 -0.006 -0.003  0.295 suppf
c$$$
      subroutine read_pos(file,data,nrow,ncol,nhead,headers)

c  ;
 
c     ---
      include '../../../inc/ftk.h'
      character*(*) file
      integer*4 nrow,ncol,nhead
      integer*4 nmax_col_local
      parameter(nmax_col_local=39)
      real*8 data(nmax_row,nmax_col_local)
      character*(*) headers(nmax_head)


      integer fid,ioerr,i,j
      character*8 firstepoch(2),lastepoch(2)
      real*8 xyzref(3),neuref(3)
      character*1000 buf,tmpstr,tmpstrs(30)
      integer nblen
      real*8 tmpval
      
   
c     ---
      
      call query_pos(file,nrow,ncol,nhead,firstepoch,lastepoch,
     &     xyzref,neuref)
      if (nrow.gt.nmax_row) then
         write(*,'(2a,i6,a,i6,a)') '[read_pos]ERROR: number of ',
     +        'lines (' ,nrow,
     +        ') exceeds program limit (',nmax_row,')!!!'
         write(*,'(17x,2a)') 'Please edit the nmax_row item in',
     +        ' $GPSF/inc/cgps.h .'
         stop
      endif

c      write(*,*) 'data size:',nrow,ncol,nhead
c      write(*,*) 'first:',firstepoch
c      write(*,*) ' last:', lastepoch
c      write(*,*) 'xyzref:',xyzref
c      write(*,*) 'neuref:',neuref
c     fid=90
      call getlun(fid)
      open(unit=fid,file=file)
c     first, skip head lines
      do i=1,nhead         
         read(fid,'(a1000)',end=899) buf
c         write(*,'(a80)') buf(1:nblen(buf))
         headers(i)=buf
      enddo

 801  continue
      
c      next, read data matrix
      write(*,*) "read data section..."
      i=1
 802  read(fid,'(a1000)',end=899) buf
c      goto 803
c$$$      call strsplit(buf,' ',ncol,tmpstrs)
c$$$      do j=1,ncol-1
c$$$         read(tmpstrs(j),*) tmpval
c$$$         data(i,j)=tmpval
c$$$c         write(*,'(a20,f20.5)') tmpstrs(j),tmpval
c$$$      enddo
c     test single line read
 803  read(buf,*) (data(i,j),j=1,ncol-1)
c     test OK
c      write(*,*) i
c      write(*,*) (data(i,j),j=1,5)
c      write(*,'(a80)') buf(1:nblen(buf))
      i=i+1
      goto 802

 899  continue

c$$$      do i=1,nrow
c$$$         write(*,*) (data(i,j),j=1,3)
c$$$      enddo
      
      end
      
c$$$;Appendix A.
c$$$;Table 2: PBO GPS Station Position Time Series Format
c$$$;Entry Definition
c$$$;YYYY	 4-digit year for the given position epoch
c$$$;MM	 	2-digit month of year for the given position epoch
c$$$;DD 	2-digit day of month for the given position epoch
c$$$;HH 	2-digit hour for the given position epoch
c$$$;MM 	2-digit minute for the given position epoch
c$$$;SS	 2-digit second for the given position epoch
c$$$;JJJJJ	 Modified Julian day for the given position epoch
c$$$;X Y Z 	ITRF Cartesian coordinates, meters
c$$$;xx 	Standard deviation of the X position, meters
c$$$;yy	 Standard deviation of the Y position, meters
c$$$;zz	 Standard deviation of the Z position, meters
c$$$;xy	 Correlation of the X and Y position
c$$$;xz	 Correlation of the X and Z position
c$$$;yz	 Correlation of the Y and Z position
c$$$;N	 North latitude, decimal degrees, relative to WGS-84 ellipsoid
c$$$;E 	East longitude, decimal degrees, relative to WGS-84 ellipsoid
c$$$;U	 Elevation, meters, relative to WGS-84 ellipsoid
c$$$;Ndel	 Change in North component relative to NEU reference position, meters. If the
c$$$;		station moves northward, Ndel is positive.
c$$$;Edel	Change in East component relative to NEU reference position, meters. If the station
c$$$;		moves eastward, Ndel is positive.
c$$$;Udel 	Change in vertical component relative to NEU reference position, meters. If the
c$$$;		station moves upward, Ndel is positive.
c$$$;nn		Standard deviation of Ndel, meters
c$$$;ee 	Standard deviation of Edel, meters
c$$$;uu 	Standard deviation of Udel, meters
c$$$;ne 	Correlation of Ndel and Edel
c$$$;nu		 Correlation of Ndel and Udel
c$$$;eu		 Correlation of Edel and Udel
c$$$;<quality>		 'final' or 'rapid', corresponding to products generated from final or rapid orproducts
c$$$;See the PBO web page for a reference for the Modified Julian date.
