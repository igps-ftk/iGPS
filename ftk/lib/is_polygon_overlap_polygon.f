CTITLE
       real*8 FUNCTION is_polygon_overlap_polygon(xys1,nmax1,npt1,
     +     xys2,nmax2,npt2,xstep,ystep)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      integer*4 nmax1,npt1,nmax2,npt2
      real*8 xys1(2,nmax1),xys2(2,nmax2),xstep,ystep

c     --OUTPUT--

c     --Local Parameters--
      integer*4 date0(5),date1(5)
      real*8 xmin,xmax,ymin,ymax,rect(4)
      real*8 xmin1,xmax1,ymin1,ymax1,xmin2,xmax2,ymin2,ymax2

      integer*4 nmax
      parameter(nmax=10000)
      integer*4 data1(nmax,nmax),data2(nmax,nmax),data3(nmax,nmax)

      integer*4 i,j,nx,ny,np1,np3

c     <<VAR_DEC

c     the range of the frame
      xmin1=xys1(1,1)
      xmax1=xys1(1,1)
      ymin1=xys1(2,1)
      ymax1=xys1(2,1)
      do i=2,npt1
         if (xys1(1,i).lt.xmin1) xmin1=xys1(1,i)
         if (xys1(1,i).gt.xmax1) xmax1=xys1(1,i)
         if (xys1(2,i).lt.ymin1) ymin1=xys1(2,i)
         if (xys1(2,i).gt.ymax1) ymax1=xys1(2,i)
      enddo
      xmin2=xys2(1,1)
      xmax2=xys2(1,1)
      ymin2=xys2(2,1)
      ymax2=xys2(2,1)
      do i=2,npt2
         if (xys2(1,i).lt.xmin2) xmin2=xys2(1,i)
         if (xys2(1,i).gt.xmax2) xmax2=xys2(1,i)
         if (xys2(2,i).lt.ymin2) ymin2=xys2(2,i)
         if (xys2(2,i).gt.ymax2) ymax2=xys2(2,i)         
      enddo
      if (xmin1.gt.xmax2.or.xmax1.lt.xmin2.or.
     .     ymin1.gt.ymax2.or.ymax1.lt.ymin2) then
         write(*,*) 'out of range'
         is_polygon_overlap_polygon=0d0
         return
      endif
      if (xmin1.lt.xmin2) then 
         xmin=xmin1
      else
         xmin=xmin2
      endif
      if (xmax1.gt.xmax2) then 
         xmax=xmax1
      else
         xmax=xmax2
      endif
      if (ymin1.lt.ymin2) then 
         ymin=ymin1
      else
         ymin=ymin2
      endif
      if (ymax1.gt.ymax2) then 
         ymax=ymax1
      else
         ymax=ymax2
      endif
      write(*,*) 'xmin xmax ymin ymax:',xmin,xmax,ymin,ymax
      write(*,*) 'width,height:',xmax-xmin,ymax-ymin
      rect(1)=xmin
      rect(2)=xmax
      rect(3)=ymin
      rect(4)=ymax

      nx=int( (xmax-xmin)/xstep )
      ny=int( (ymax-ymin)/ystep )
      if (nx.gt.nmax) then
         write(*,*) '[]ERROR: nx is too large (',nx,'>',nmax,')!!'
         stop
      endif
      if (ny.gt.nmax) then
         write(*,*) '[]ERROR: ny is too large (',ny,'>',nmax,')!!'
         stop
      endif
      write(*,*) 'nx, ny:',nx,ny,nx*ny
c$$$      write(*,*) 'xys1:'
c$$$      do i=1,npt1
c$$$         write(*,'(2(1x,f20.15))') (xys1(j,i),j=1,2)
c$$$      enddo
c$$$      write(*,*) 'xys2:'
c$$$      do i=1,npt2
c$$$         write(*,'(2(1x,f20.15))') (xys2(j,i),j=1,2)
c$$$      enddo
c$$$      write(*,*) 'xstep,ystep:',xstep,ystep
      call polygon_rasterize2(xys1,nmax1,npt1,rect,
     +     xstep,ystep,nx,ny,data1)
      call polygon_rasterize2(xys2,nmax2,npt2,rect,
     +     xstep,ystep,nx,ny,data2)
c      write(*,*) 'frame 1 (target):'
c      do j=ny,1,-1
c         write(*,'(100i2)') (data1(i,j),i=1,nx)
c      enddo  
c      write(*,*) 'frame 2:'
c      do j=ny,1,-1
c$$$         do i=1,nx
c$$$            if ( (rect(3)+ystep*j).lt.39.75) then
c$$$               data2(i,j)=3
c$$$            endif
c$$$         enddo
c         write(*,'(100i2)') (data2(i,j),i=1,nx)
c      enddo
c      stop

      np1=0
      np3=0
      do i=1,nx
         do j=1,ny
            data3(i,j)=data1(i,j)+data2(i,j)
            if (data1(i,j).eq.1) np1=np1+1
            if (data3(i,j).eq.2) np3=np3+1
         enddo
      enddo
      
c      stop
c$$$      write(*,*) 'np1,np3:',np1,np3
      if (np1.gt.0) then
         is_polygon_overlap_polygon=1d0*np3/np1
      else
         is_polygon_overlap_polygon=0d0
      endif

      RETURN
      END
