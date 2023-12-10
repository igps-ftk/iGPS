CTITLE
      PROGRAM s1_frame_match

      IMPLICIT NONE
      INCLUDE '../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--


c

c     --EXAMPLE--

c     --MODIFICATIONS--
c     + On Mon Jan  4 09:42:36 CST 2021 by tianyf
c     .
c
c

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--

c     file1, file2 : polgyon coordinates in GMT psxy format
      character*1023 file1, file2

c     --OUTPUT--
c     output reults (1: in; 0: out)
      integer*4 is_in,nout
      character*1023 onames(nmax_row)

c     --EXTERNAL--
      integer*4 iargc,nblen,is_point_inside_polygon
      integer*4 is_line_intersect_polygon
      real*8 is_polygon_overlap_polygon
      character*1 path_sep

c     --Local Parameters--

c     maximum number of vertices of polygon


c     for reading polygon file
      real*8 xys1(2,nmax_row),xys2(2,nmax_row),xi,yi,x1,y1,x2,y2
      real*8 xy1(2,nmax_row),xy2(2,nmax_row)
      real*8 x2s,x2e,y2s,y2e,x1s,x1e,y1s,y1e
      integer*4 npts1(nmax_row),npts2(nmax_row),npt1,npt2
      character*1023 names1(nmax_row),names2(nmax_row),name1,name2
      integer*4 n1,n2,i1s,i1e,i2s,i2e,l2p
      real*8 is_overlap


      character*1023 tmpstr,buf
      integer*4 np1,np2,i,j,k,l
      integer*4 fid,ioerr


c     <<VAR_DEC
      if (iargc().lt.2) then
         write(*,'(3a)') 'Usage: is_vectors_overlap file1 file2'
         stop
      endif
  
c     whether force polyline to be polygon
      l2p=1

c      write(*,'(a)') '[i]Starting ...________________'
      call getarg(1,tmpstr)
      file1=tmpstr(1:nblen(tmpstr))
      write(*,'(3a)') '[]file1:',file1(1:nblen(file1))
      call getarg(2,tmpstr)
      file2=tmpstr(1:nblen(tmpstr))
      write(*,'(3a)') '[]file2:',file2(1:nblen(file2))
      
      call read_psxy(file1,xys1,npts1,names1,n1)
      call read_psxy(file2,xys2,npts2,names2,n2)
      write(*,*) 'n1,n2:',n1,n2
      write(*,*) 'npts2:',(npts2(i),i=1,n2)
c      stop
      
      i2s=0
c      nout=0
      do i=1,n2
        nout=0
        npt2=npts2(i)
        
        if (i.eq.1) then
          i2s=1
        else
          i2s=i2s+npts2(i-1)
        endif
        i2e=i2s+npt2-1
        write(*,*) 'i2s,i2e:',i2s,i2e

        x2s=xys2(1,i2s)
        y2s=xys2(2,i2s)
        x2e=xys2(1,i2e)
        y2e=xys2(2,i2e)

        np2=0
        do j=i2s,i2e
           np2=np2+1
           xy2(1,np2)=xys2(1,j)
           xy2(2,np2)=xys2(2,j)
        enddo

        do k=1,np2
           write(*,*) 'xy2:', (xy2(l,k),l=1,2)
        enddo

c       for point type        
        if (npt2.eq.1) then
           write(*,*) 'input is point'
          i1s=0
          do j=1,n1
            npt1=npts1(j)
            if (j.eq.1) then
              i1s=1
            else
              i1s=i1s+npts1(j-1)
            endif
            i1e=i1s+npts1(j)-1
            write(*,*) 'i1s,i1e:',i1s,i1e
            np1=0
            do k=i1s,i1e
              np1=np1+1
              xy1(1,np1)=xys1(1,k)
              xy1(2,np1)=xys1(2,k)
            enddo
            is_in=is_point_inside_polygon(xy1,nmax_row,np1,x2s,y2s)
            if (is_in.eq.1) then
              nout=nout+1
              name1=names1(j)
              onames(nout)=name1
            endif
          enddo
        endif
        
c       for line type          
        if (npt2.gt.1.and.(x2s.ne.x2e.or.y2s.ne.y2e).and.l2p.ne.1) then
           write(*,*) 'input is line'
          i1s=0
          do j=1,n1
            npt1=npts1(j)
            if (j.eq.1) then
              i1s=1
            else
              i1s=i1s+npts1(j-1)
            endif
            i1e=i1s+npts1(j)-1
            write(*,*) 'i1s,i1e:',i1s,i1e
            np1=0
            do k=i1s,i1e
              np1=np1+1
              xy1(1,np1)=xys1(1,k)
              xy1(2,np1)=xys1(2,k)
            enddo
            
            is_in=is_line_intersect_polygon(xy1,nmax_row,np1,x2,y2)
            write(*,*) i,j,is_in
            
          enddo
        endif

        
        if (l2p.eq.1.and.npt2.gt.1) then
           np2=np2+1
           xy2(1,np2)=xy2(1,1)
           xy2(2,np2)=xy2(2,1)
           x2e=x2s
           y2e=y2s
        endif
c       for polygon type  
        if (npt2.gt.1.and.x2s.eq.x2e.and.y2s.eq.y2e) then
           write(*,*) 'input is polygon ...'
          i1s=0
          do j=1,n1
            npt1=npts1(j)
            write(*,*) 'np1:',np1
            if (j.eq.1) then
              i1s=1
            else
              i1s=i1s+npts1(j-1)
            endif
            i1e=i1s+npts1(j)-1
            write(*,*) 'i1s,i1e:',i1s,i1e
            np1=0
            do k=i1s,i1e
              np1=np1+1
              xy1(1,np1)=xys1(1,k)
              xy1(2,np1)=xys1(2,k)
            enddo
            
            if (xy1(1,1).ne.xy1(1,np1).or.xy1(2,1).ne.xy1(2,np1)) then
               np1=np1+1
               xy1(1,np1)=xy1(1,1)
               xy1(2,np1)=xy1(2,1)
            endif
            name1=names1(j)
            write(*,*) name1(1:nblen(name1))
c$$$            do k=1,np1
c$$$               write(*,*) 'xy1:', (xy1(l,k),l=1,2)
c$$$            enddo
            is_overlap=is_polygon_overlap_polygon(xy1,nmax_row,np1,
     +           xy2,nmax_row,np2,1d-1,1d-1)
            if (is_overlap.gt.0) then
               is_in=1
               nout=nout+1
               onames(nout)=name1
            else
               is_in=0
            endif
            write(*,*) i,j,is_overlap,is_in
            
          enddo

        endif
        
        write(*,*) 'nout:',nout
        if (nout.ge.1) then
           do j=1,nout
              name1=onames(j)
              write(*,'(a,1(1x,a))') 'MATCHED: ',name1(4:12)
           enddo
        endif

      enddo
      stop



      STOP
      END
