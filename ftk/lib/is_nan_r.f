      integer*4 function is_nan_r(val)
      real val
      real nan

      nan=1./0

      if (nan.eq.val) then
         is_nan_r=1
      else
         is_nan_r=0
      endif

      return
      end
