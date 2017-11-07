c+
c     matrix multiplication
c-
      subroutine matrix_multiply(aa,bb,cc,ma,na,mb,nb)

      integer ma,mb,na,nb
      real*8 aa(ma,na),bb(mb,nb),cc(ma,nb)

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

c      write(*,*) ma,na,mb,nb
c      do i=1,na
c         write(*,*) i, aa(i,1)
c      enddo
c      stop
c      open(unit=12,file='c.out')
      do 603  i=1,ma
         do 604 j=1,nb
            cc(i,j)=0
            do 605  k=1,na
               cc(i,j)=cc(i,j)+aa(i,k)*bb(k,j)
c               write(*,*) i,j,k,aa(i,k),bb(k,j),cc(i,j)
c               write(*,*) i,k,aa(i,k),k,j,bb(k,j)
 605        continue
c            write(12,*) c(i,j),i,j
 604     continue
 603  continue
c      close(12)

      end



