      program test
      
      implicit none
      integer*4 n,i
      character*100 str, tmpstr
      character*1 sep
      character*100 strs(100)

      real*8 xyz(3),neu(3),lamda,theta

c     for covj_1
      real*8 j_1(5,5)

c     for determ
      real*8 da(3,3)
      data da/2,1,1,4,-6,0,-2,7,2/
      data da/2,4,-2,1,-6,7,1,0,2/
c     for cholesky decomposition
      data da/6.0, 15.0, 55.0,15.0, 55.0, 225.0,55.0, 225.0, 979.0/
      integer*4 dn,dnp
      real*8 determ

c     for cholesky decomposition
      integer info,job
      integer lda,p,jpvt(3)
      real*8 work(3)

c     for drand
      real*8 drand,d_sin
      integer nblen

      integer*4 rindex,lnblnk

c     for putc
      integer*4 status,putc
      character char

c     for dgetrf
      data da/1,2,1,4,10,15,3,7,1/
      integer m,ipivot(3)
c     for dgetri
      integer ldwork
c      real*8 work(3)

c     for large matrix
      integer nl
      parameter(nl=2000)
      integer ipivot_l(nl)
      real*8 work_l(nl),al(nl,nl)

c     for M*N transpose
      character io
      integer tm,tn,tmn(3,2),tmno(2,3)
      data tmn/1,2,3,4,5,6/

c     for path_sep()
      character*1 path_sep


      integer*4 j

c     for testing map_2points
      real*8 ll1(2),ll2(2),radius,dist
      integer radian,meter,mile


c     test for map_2points

      ll1(1)=-112.7285492d0
      ll1(2)=44.4855680d0
      ll2(1)=-115.5874836d0
      ll2(2)=36.3195915d0
c      call map_2points(ll1(1),ll1(2),ll2(1),ll2(2),
c     &     isRadian,isMeter,isMile,radius,dist)
c      write(*,*) ll1,ll2,
c     &     isRadian,isMeter,isMile,radius,dist
c      stop

      write(*,*) 'test for ilen() ...'
      write(*,*) 8, ilen(8)
      write(*,*) 19,ilen(19)
      write(*,*) 199,ilen(199)
      write(*,*) 1299,ilen(1299)

      write(*,*) 'test started'
      str='ab c defg hi j kl  mhin'
      str='   1    1 0.10000E+01  -0.368631694E+00   0.170166902244E-01'
      sep=' '
      n=100
      write(*,*) 'strsplit2:'
      call strsplit2(str,sep,n,strs)
      write(*,*) 'strsplit:'
c      call strsplit(str,sep,n,strs)
      write(*,*) 'total:', n
      do i=1, n
         tmpstr=strs(i)
         write(*,*) tmpstr(1:index(tmpstr,' '))
      enddo
      
      stop

      xyz(1)=4581691.012d0
      xyz(2)=556114.680d0
      xyz(3)=4389360.696d0
c     7ODM
      xyz(1)=-2407750.8914d0
      xyz(2)=-4706536.7309d0
      xyz(3)=3557571.3976d0

c     PBO AB06
      xyz(1)=-3524499.47955d0
      xyz(2)=-1049128.16162d0
      xyz(3)=5194460.04495d0

      lamda=54.8853229575d0
      theta=196.5765461953d0
c$$$PBO Station Position Time Series
c$$$Format Version: 1.0.1
c$$$4-character ID: AB06
c$$$Station name  : FalsePass_AK2005
c$$$First Epoch   : 20060724 120000
c$$$Last Epoch    : 20061103 120000
c$$$Release Data  : 20070217 053652
c$$$XYZ Reference position :  -3524499.47955 -1049128.16162  5194460.04495
c$$$NEU Reference position :    54.8853229362  196.5765461929  500.40832
c$$$ 20060724 120000 53940.5000 -3524499.47159 -1049128.15941  5194460.03608  0.00275  0.00152  0.00381  0.553 -0.782 -0.534      54.8853229575  196.5765461953  500.39682     0.00164   0.00015  -0.01201    0.00149  0.00121  0.00455 -0.023 -0.003  0.143 suppf

c     BLYT
      xyz(1)=-2223206.56880d0
      xyz(2)=-4830299.77500d0
      xyz(3)=3510587.64020 d0
      
      lamda=245.285d0
      theta=33.610d0
c$$$Unc. BLYT_GPS -2223206.58450 -4830299.79980  3510587.66317  -0.00270   0.00080   0.00080 2000.092  0.0026  0.0049  0.0035
c$$$Apr. BLYT_GPS -2223206.58450 -4830299.79980  3510587.66317  -0.00270   0.00080   0.00080 2000.092  0.0026  0.0049  0.0035
c$$$Loc.  BLYT_GPS N coordinate  (m)           3741494.36638      0.00314      0.00137
c$$$Loc.  BLYT_GPS E coordinate  (m)          22740425.44062     -0.00391      0.00154
c$$$Loc.  BLYT_GPS U coordinate  (m)                85.90025      0.03694      0.00626

      call xyz2neu(xyz,lamda,theta,neu)
      write(*,*) 'xyz:',xyz
      write(*,*) 'neu:',neu

c     test for covj_1
      call covj_1(j_1,5)
      write(*,'(5f10.4)') j_1

c     test for determ
      write(*,*) 'Det[A]:'
c     dn=dnp=5
      dn=3
      dnp=3
      
      do i=1,3
c         do j=1,3
c            da(i,j)=(i+j)
c         enddo
         write(*,'(5f10.4)') (da(i,j),j=1,3)
      enddo
c      write(*,*) 'det:',determ(da,dn,dnp)


c     cholesky decomposition
      lda=dn
      p=dnp
      job=0
      do i=1,3
c         do j=1,3
c            da(i,j)=(i+j)
c         enddo
         write(*,'(5f10.4)') (da(i,j),j=1,3)
      enddo
c$$$c      call DCHDC(dA,lda,P,WORK,JPVT,JOB,INFO)
c$$$      write(*,*) 'dchdc:'
c$$$      write(*,'(3f)') da
c$$$
c$$$      write(*,*) 'dtrans...'
c$$$      call dtrans('I',1d0,da,3,3,da)
c$$$      write(*,'(3f)') da
c$$$
c$$$c     test for drand
c$$$      write(*,*) 'drand:'
c$$$      do i=1,3
c$$$         do j=1,3
c$$$c            da(i,j)=d_sin(drand(0))
c$$$         enddo
c$$$         write(*,'(3f)') (da(i,j),j=1,3)
c$$$      enddo

c$$$c     test for getlog
c$$$      call getlog(str)
c$$$      call hostnm(tmpstr)
c$$$      write(*,*) 'You are ',str(1:index(str,' ')),' on ',
c$$$     .     tmpstr(1:nblen(tmpstr))
c$$$      write(*,*) 'You are ',str(1:index(str,' ')),' on ',
c$$$     .     tmpstr(1:(lnblnk(tmpstr)))
c$$$
c$$$      call perror('perror..')
c$$$      write(*,*) ' end-of-perror'
c$$$      do i=1,lnblnk(str)
c$$$         char=str(i:i)
c$$$         status=putc(char)   
c$$$         status=putc('.')
c$$$      enddo
c$$$      status=putc('\n')
c$$$
c$$$c     lapack - LU decomposition -> invert
c$$$c     dgetrf,dgetri
c$$$      m=3
c$$$      n=3
c$$$      lda=3
c$$$
c$$$      write(*,*) 'original:'
c$$$      do i=1,n
c$$$         write(*,'(3f)') (da(i,j),j=1,3)
c$$$      enddo
c$$$      call dgetrf(m,n,da,lda,ipivot,info)
c$$$      write(*,*) 'infor of dgetrf:',info
c$$$      ldwork=3
c$$$      call dgetri(n,da,lda,ipivot,work,ldwork,info)
c$$$      write(*,*) 'infor of dgetri:',info
c$$$      write(*,*) 'invert of A:'
c$$$      do i=1,n
c$$$         write(*,'(3f)') (da(i,j),j=1,3)
c$$$      enddo
c$$$c     
c$$$c     test large matrix invert
c$$$      do i=1,nl
c$$$         do j=1,nl
c$$$c            al(i,j)=drand(0)
c$$$         enddo
c$$$      enddo
c$$$      write(*,*) 'large dgetrf..'
c$$$c      call dgetrf(nl,nl,al,nl,ipivot_l,info)
c$$$c      write(*,*) 'infor of dgetrf:',info
c$$$c      write(*,*) 'large dgetri..'
c$$$      ldwork=nl
c$$$c      call dgetri(nl,al,nl,ipivot_l,work_l,ldwork,info)
c$$$c      write(*,*) 'infor of dgetri:',info
c$$$
c$$$
c$$$c     transpose M*N
c$$$      write(*,*) 'transpose M*N:'
c$$$      io='o'
c$$$      tm=3
c$$$      tn=2
c$$$      do i=1,tm
c$$$         write(*,*) (tmn(i,j),j=1,tn)
c$$$      enddo
c$$$      call dtrans(io,1d0,tmn,tm,tn,tmno)
c$$$      do i=1,tn
c$$$         write(*,*) (tmno(i,j),j=1,tm)
c$$$      enddo

      write(*,*) 'Current OS path separator is:', path_sep()

      STOP
      end
