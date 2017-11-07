      integer*4 function is_nan_r8(val)
      real*8 val
      real*8 nan

c      nan=1d0/0
c
c      if (nan.eq.val) then
c         is_nan_r8=1
c      else
c         is_nan_r8=0
c      endif
      is_nan=isnan(val)
      return
      end
