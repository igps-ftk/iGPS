      subroutine ds2hms(iyr,idoy,sod,ihour,iminute,seconds)
c
c     routine to convert from day of year + seconds to
c     hours, minutes, seconds.
c
      implicit none

      real*8 sod,hours,minutes,seconds
      integer*4 iyr,ihour,iminute,idoy
C     seconds per day
      real*8      spd
      logical leapyr

      data        spd/86400.d0/

 100  continue
      if (sod .ge. spd) then
         sod = sod - spd
         idoy = idoy + 1
         goto 100
      else if (sod .lt. 0.d0) then
         sod = sod + spd
         idoy = idoy - 1
         goto 100
      endif
      if(idoy.gt.365 .and. .not.leapyr(iyr)) then
       idoy=idoy-365
       iyr=iyr+1
      endif
      if(idoy.gt.366 .and. leapyr(iyr)) then
       idoy=idoy-366
       iyr=iyr+1
      endif

C     decimal hours
      hours   = sod/3600.d0
C Kludge to get around round off problem in G77 compiler....
c      hours = hours+(1.d-17)
C     integer hours
      ihour   = int(hours)
      minutes = (sod - 3600.d0 * dble(ihour))/60.d0
C Kludge to get around round off problem in G77 compiler....
c      minutes = minutes+(1.d-17)
C     integer minutes
      iminute = int(minutes)
      seconds = sod - 3600.d0*dble(ihour) - 60.d0*dble(iminute)
c
c Check to make sure time is in the correct format....
       if (seconds.eq.60.d0) then
         seconds = 0.d0
         iminute = iminute + 1
         if (iminute.eq.60) then
           iminute = 0
           ihour = ihour + 1
         endif
       endif 
       if (iminute.eq.60) then
         iminute = 0
         ihour = ihour + 1
       endif
       
c      write(*,200)hours,ihour,minutes,iminute,seconds,sod
c200   format('2 hours ',f35.28,'ihour ',i15,'minutes ',f45.38,
c     .       'iminute ',i15,'seconds ',f35.28,'sod ',f35.28)

      return
      end
