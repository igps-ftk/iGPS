CTITLE
      SUBROUTINE xyz2neu(xyz,lamda,theta,neu)
c     --PURPOSE--
c     convert from caterian XYZ to local NEU coordinates.

c     --ALGORITHM--
c     From Nikolaidis, 2002, UCSD Dissertation, pp.16 (formula II.5)

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
      real*8 xyz(3),lamda,theta

c     --OUTPUT--
      real*8 neu(3)

c     --Local Parameters--
c     rotation matrix
c     NEU = ROT_MAT * XYZ
      real*8 rot_mat(3,3)
      integer*4 i,j
      real*8 xyz1(3,1),neu1(3,1)

c     <<VAR_DEC

      do i=1,3
         xyz1(i,1)=xyz(i)
         write(*,*) xyz1(i,1)
      enddo

      write(*,*) 'll:',lamda,theta
      
      rot_mat(1,1)=-dsin(lamda)*dcos(theta)
      rot_mat(1,2)=-dsin(lamda)*dsin(theta)
      rot_mat(1,3)=dcos(lamda)
      rot_mat(2,1)=-dsin(theta)
      rot_mat(2,2)=dcos(theta)
      rot_mat(2,3)=0d0
      rot_mat(3,1)=dcos(lamda)*dcos(theta)
      rot_mat(3,2)=dcos(lamda)*dsin(theta)
      rot_mat(3,3)=dsin(lamda)
      write(*,*) 'rot_mat:'
      do i=1,3
         write(*,*) (rot_mat(i,j),j=1,3)
      enddo

c     call matrix multiplication
      write(*,*) 'calling matrix multiplication'
      call matply(rot_mat,xyz1,neu1,3,3,3,1)
      write(*,*) 'neu:',neu1
      write(*,*) 'xyz:',xyz1
      
      do i=1,3
         neu(i)=neu1(i,1)
      enddo

      RETURN
      END
