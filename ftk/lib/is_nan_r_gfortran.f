      integer*4 function is_nan_r(val)
      real val
      real nan

c      nan=1./0
c
c      if (nan.eq.val) then
c         is_nan_r=1
c      else
c         is_nan_r=0
c      endif

      is_nan = isnan(val)
      return
      end
