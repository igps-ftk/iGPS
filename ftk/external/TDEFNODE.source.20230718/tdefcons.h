c tdefnode
c global constants 

      parameter (  
c PII
     .  pi   = 3.14159265358979d0, 
     .  pii  = 3.14159265358979d0, 
     .  pi2  = 6.28318530717959d0,
     .  pii2 = 6.28318530717959d0,

c degrees to radians
     .  d2r =  1.7453292519943d-2,

c radians to degrees
     .  r2d =  5.7295779513082d1,

c kilometers to degrees on Earth
     .  x2d =  8.9932200116306d-3,

c degrees to kilometers on Earth
     .  d2x =  1.1119487777534d2,

c Earth radius in kilometers
     .  Erad = 6.3709972d3,
     
c days per year
     .  dpy = 3.6525d2,

c seconds per yr
     .  secperyr = 3.15576d7 )

c material constants
      common /con1/ xmu, xlambda, poisrat

c--- end of constants

