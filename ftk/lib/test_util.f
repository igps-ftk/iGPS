c+
c Name:
c     periodogram
c Purpose:
c     Test least square solution for equations: p=p0+r*t+e
c     To estimate:
c       +rate
c       +rate uncertainty
c Inputs:
c     +site position time series (each component: north, east, up)
c Outputs:
c     +periodogram
c     +amplitude of power law noise
c     +spectral index (kappa)
c-
      program periodogram


c     variables declaration
c     -->

c      implicit none

c     define the maximum number of observations
      integer NMAX
      parameter(NMAX=5000)

c     parameters
      real*8 na,nb,nk
c     loop variable
      integer i,j
c     design matrix
c      real*8 A(NMAX,2),AT(2,NMAX),tmp(NMAX,NMAX)
c     data covariance matrix
c      real*8 cx(NMAX,NMAX)
c     classical periodogram: P(*) R*8
      real*8 Pw(NMAX)
c     FT
      real*8 FT(NMAX)
c     time series
      real*8 xi(NMAX)
c     time
      real*8 ti(NMAX)
c     length of time series
      integer n0
c     frequency
      real*8 w(NMAX),fn,fi(NMAX)
c     temp var
      real*8 tmpcos,tmpsin
      integer*4 ind(2)

c     complex number i sqrt(-1)
      complex ci
c      data ci/(0,-1)
c     How to get the virtural number *i*?

c     pi
      parameter(pi=3.14159265)
c     file names
      character*256 file
      character*256 tfile
      integer ioerr

c     complex test
      complex test1

c     imaginary i (ii) = -i = sqrt(-1)
      complex ii

c     <--
c     end of variables declaration

c     test variables
      integer*4 write
      real*8 age_of

c     read the data file
      file='/home/tianyf/gpsf/ls/bjfsRaw.xyz'
      file='/home/tianyf/gpsf/periodogram/bjfs.n'
      file='/home/tianyf/gpsf/periodogram/ts_res'
c      write(*,*) 'using file:', file(1:nblen(file))
c      stop
      write(*,'("Using time series: ",a)') file(1:index(file,' '))
      call getfilename(file(1:nblen(file))//'.txt',tfile)
      write(*,'("getfilename:",a)') tfile
      call getpathname(file(1:nblen(file))//'.txt',tfile)
      write(*,'(a)') tfile
      call desuffix(file(1:nblen(file))//'.txt',tfile)
      write(*,'("desuffix:",a,"|")') tfile(1:nblen(tfile))
      write(*,*) 'create new raw file...'
      tfile='/usr/home/tianyf/gpsf/periodogram/i1.raw'
      call rawnew(tfile,512,256,1,2)
      open(unit=500,file=file)
      i=1
 600  read(500,700,iostat=ioerr) ti(i),xi(i)
 700  format(F9.4,1X,F12.4)
c      write(*,"(i5,2f20.8)") i,t(i),x(i)
c      print *, ioerr
      i=i+1
      if (ioerr.eq.-1) go to 601
      if (i.le.NMAX) go to 600
      close(500)
 601  continue
      n0=i-2
      write(*,*) 'total number of observation (N0):', n0

c     demean
      tmpmean=mean1(xi,n0)
      write(*,*) 'x(i) time series mean:', tmpmean
      do i=1,n0
         xi(i)=xi(i)-tmpmean
      enddo

c     test for complex variable
      test1=(1,2)
c      test1=sqrt(-1.)
      write(*,*) test1,real(test1),imag(test1)

      n=n0/2
      write(*,*) 'number of frequency:',n

c     calculate frequency (w)
c     number of observations: N0
c     unit: yr 
c     sampling frequency: 365 /yr
c     period: T=1/f (yr) = 1/365 (yr) =delta(T)
c
c     fundermental period: T   TT      
      deltaT=1/365D0
      write(*,*) 'fundermental period (deltaT):',deltaT,' yr'
      fNy=0.5/deltaT
      write(*,*) 'Nyquist frequency:', fNy, ' yr^-1'
      T=deltaT*N0
      write(*,*) 'total time span:', T, ' yr', 
     .  (ti(n)-ti(1))/deltaT,ti(n),ti(1),n,n0,deltaT
c     total= ... yr
c     w = 2*pi*f
c      write(*,*) mod(n,2)
c      write(*,*) 'omega(w):'
c      do j=1,n0/2
c         w(j)=2D0*pi*fNy*(j-1)
c         w(j)=2D0*pi*(j-1)/(n0*deltaT)
c         fi(j)=(j-1)/(n0*deltaT)
c         write(*,'(2F10.3)'),w(j),(j-1)/(N0*deltaT)
c      enddo

c     calculate classical periodogram
      do i=1,n
         w(i)=2D0*pi*i/(n0*deltaT)
         fi(i)=i/(n0*deltaT)
         tmpcos=0
         tmpsin=0
         do j=1,n0
            ti(j)=j*deltaT
            tmpcos=tmpcos+xi(j)*cos(w(i)*ti(j))
            tmpsin=tmpsin+xi(j)+sin(w(i)*ti(j))
         enddo         
         Pw(i)=(1D0/n0)*(tmpcos**2+tmpsin**2)
c         write(*,*) fi(i),Pw(i)
      enddo

c     calculate classical periodogram: method 2
      do i=1,n
         w(i)=2D0*pi*i/(n0*deltaT)
         tmp=0
         do j=1,n0
            ti(j)=j*deltaT
            tmp=tmp+xi(j)*exp(-(w(i)*ti(j)))
         enddo         
         Pw(i)=(1D0/n0)*(tmp**2)
         write(*,*) fi(i),Pw(i)
      enddo

c     testes
c     |-->
      write(unit=6,FMT='(a)'),'''hello the  world"'
      write(unit=6,FMT='(a)'),"""hello the  world'"

      write=12*35
      write(*,*) write
c      do i=0,126
c         write(*,*),i,' - ', char(i)
c      enddo
      write(*,*) 'hello world    ', len_trim('hello world    ')
c      write(*,*) file
c      write(*,*) trim(file)
c      write(*,*) trim("hello world     ")
c     no trim function
      write(*,'(/,3(/,E12.4,:,","))') (pw(i),i=1,10)
      write(*,'(3(D12.4,:,","))') (pw(i),i=1,10)
      age_of=30.
      write(*,*) age_of
c     -->|
c
c     

c     form the design matrix
c      write(*,*) 'forming the design matrix ...'
c      do 602 i=1,n,1
c         print*,i
c         A(i,1)=1
c         A(i,2)=t(i)
c 602  continue
c      open(unit=13,file='a.out')
c      do i=1,n
c         write(13,'(2F12.4)') (A(i,j),j=1,2)
c      end do
c      close(13)
c
c     calling tranpose A -> A(T)
c      write(*,*) 'calling transpose..'
c      call transpose(A,AT,n,2)
c     check
c      write(*,700) AT
c      open(unit=13,file='at.out')
c      do i=1,2
c         write(13,'(1944F12.4)') (AT(i,j),j=1,n)
c      end do
c      close(13)
c      
c      write(*,*) 'calling multiply..'
c      call matrix_multiply(AT,A,tmp,2,n,n,2)
c      write(*,700) tmp
c      write(*,*) 'end of multiply'
c      open(unit=11,file='ply.out')
c      do i=1,n
c         write(11,'(1994F12.4)') (tmp(i,j),j=1,n)
c      end do
c      close(11)
c      do i=1,2
c         write(*,'(F20.4)') (tmp(i,j),j=1,2)
c      enddo


c     create classical periodogram[Scargle, 1982]
     


     
      return
      end



c+
c     transpose
c-
      subroutine transpose(a, at, m,n)
      parameter(NMAX=2000)
      integer  m,n
      real*8 a(NMAX,2),at(2,NMAX)
c     loop variables
      integer i,j
      write(*,*),m,n
      do i=1,n
         do j=1,m
            at(i,j)=a(j,i)
c            print*,i,j,' hah ',at(i,j)
         enddo
      enddo
      

      end



c+
c     matrix multiplication
c-
      subroutine matrix_multiply(aa,bb,cc,ma,na,mb,nb)


      parameter(NMAX=2000)
      integer ma,mb,na,nb
      real*8 bb(NMAX,2),aa(2,NMAX),cc(NMAX,NMAX)

c     loop variables
      integer i,j,k

c      open(unit=131,file='am.out')
c      do i=1,ma
c         write(131,'(2F12.4)') (aa(i,j),j=1,2)
c      end do
c      close(131)
c      open(unit=132,file='atm.out')
c      do i=1,2
c         write(132,'(1944F12.4)') (bb(i,j),j=1,ma)
c      end do
c      close(132)

      write(*,*) ma,na,mb,nb
c      do i=1,na
c         write(*,*) i, aa(i,1)
c      enddo
c      stop
      open(unit=12,file='c.out')
      do 603  i=1, ma
         do 604 j=1, nb
            cc(i,j)=0
            do 605  k=1,na
               cc(i,j)=cc(i,j)+aa(i,k)*bb(k,j)
c               write(*,*) i,j,k,aa(i,k),bb(k,j),cc(i,j)
c               write(*,*) i,k,aa(i,k),k,j,bb(k,j)
 605        continue
c            write(12,*) c(i,j),i,j
 604     continue
 603  continue
      close(12)

      end


