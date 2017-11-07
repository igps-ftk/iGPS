     subroutine newtonlaw1(m1,m2,l,f)
c     the force between mass points of distanct (l)
c          m1*m2
c     F=G*-------
c           l*l
c                                          -11  3   -1 -2
c     Gravitational constant: G = 6.6742*10    m  kg  s
c     ---
c     Inputs:
      real*8 m1,m2,l
c     Outputs:
      real*8 f
c     Constants:
      real*8 G
      data G /6.6742D-11/
c     --
c     Local variables
     
      f=G*(m1*m2/l**2)
      
      return
      end
