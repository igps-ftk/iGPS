c+
c     matrix multiplication
c-
      subroutine matply(aa,bb,cc,ma,na,mb,nb)
      
      integer ma,mb,na,nb
      real*8 bb(mb,nb),aa(ma,na),cc(ma,nb)

c     loop variables
      integer i,j,k

c     |->
      if (na.ne.mb) then
         write(*,*) 'Array dimensions do not agree. Check there size'
         stop
      endif

c      write(*,*) ma,na,mb,nb
c      do i=1,na
c         write(*,*) i, aa(i,1)
c      enddo
c      stop
      do 603  i=1, ma
         do 604 j=1, nb
            cc(i,j)=0d0
            do 605  k=1,na
               cc(i,j)=cc(i,j)+aa(i,k)*bb(k,j)
c               write(*,*) i,j,k,aa(i,k),bb(k,j),cc(i,j)
c               write(*,*) i,k,aa(i,k),k,j,bb(k,j)
 605        continue
c            write(12,*) c(i,j),i,j
 604     continue
 603  continue
 
      end



