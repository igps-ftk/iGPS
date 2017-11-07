      integer*4 function is_nan_r8(val)
      real*8 val
      real*8 nan

      nan=1d0/0

      if (nan.eq.val) then
         is_nan_r8=1
      else
         is_nan_r8=0
      endif

      return
      end
