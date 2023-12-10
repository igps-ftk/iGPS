      subroutine interp_tx(t,x,nmax,nrow,ot,ox,ndays)
c     ---
      implicit none
c     --
      integer*4 nmax,nrow
      real*8 t(nmax),x(nmax),ot(nmax),ox(nmax)
c     --
      integer*4 i,j,k,l,m,n

      INTEGER*4 NP
      REAL*8 PI
      PARAMETER(PI=3.141593)
      INTEGER*4 nfunc
      integer*4 szwin
      parameter(szwin=20)
      REAL*8 f,x1,y1,yp1,ypn,xa(szwin),ya(szwin),y2(szwin)    
c     --
      integer*4 ndays
      real*8 mjds(nmax),mjdmin,mjdmax

      real*8 dy

c     ---
c      np=nrow
      mjdmin=t(1)
      mjdmax=t(nrow)
      ndays=mjdmax-mjdmin+1

c      yp1=sin(xa(1))
c      ypn=sin(xa(np))
      yp1=1d30
      ypn=1d30

     

      do i=1,ndays
         ot(i)=mjdmin+i-1
      enddo

 
      k=1
      do i=1,nrow
c         write(*,*) t(i),ot(k)
 800     if (t(i).gt.ot(k)) then
c     fill gap
c            write(*,*) 'fill gap'
            if ((i-szwin/2).gt.0) then
               do j=1,szwin
                  l=i-szwin/2+j
                  xa(j)=t(l)
                  ya(j)=x(l)
c                  write(*,*) xa(j),ya(j),i,i-szwin/2+j
               enddo
            else if ((i+szwin/2).gt.nrow) then
               do j=nrow-szwin,nrow
                  xa(j)=t(j)
                  ya(j)=x(j)
               enddo
            else
               do j=1,szwin
                  xa(j)=t(j)
                  ya(j)=x(j)
               enddo
            endif

            x1=ot(k)
            call spline1(xa,ya,szwin,szwin,yp1,ypn,y2)
            call splint1(xa,ya,y2,szwin,szwin,x1,y1)
c            write(*,700) 'interp(spline):',x1,y1
c            call polint(xa,ya,szwin,x1,y1,dy)
        
            ox(k)=y1
c            write(*,700) 'interp(polint):',x1,y1
            k=k+1
            goto 800
         endif
 700     format(a20,2f20.8)
         
         if (t(i).eq.ot(k)) then
            ox(k)=x(i)
            k=k+1
         endif
      enddo
         
c      write(*,*) 'ndays:',ndays,mjdmin,mjdmax

      end
      
