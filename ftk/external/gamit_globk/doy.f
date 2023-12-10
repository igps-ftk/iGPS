      program doy
 
*     This is generic program which will read a varierty of
*     date types from the runstring and return the other date
*     types.
 
*   date(5)     - Calender date
*   day_of_year - Day of year
*   rcpar       - Get runstring parameters
*   len_run     - Length of string entry
*   ierr        - IOSTAT error flag
*   num_run     - Number of arguments in the runstring
*   i           - Loop counter
*   gps_week    - GPS week number starts with week zero on 1980/1/5 Midnight
*   gps_dow     - GPS doy of week
*   gps_sow     - GPS seconds into week
*   gps_start_mjd - MJD of the start of the GPS week count (80/1/5)
*   indx        - Postition of character in string

c   Add Decimal Year in output, K. Feigl May 97
c   MOD TAH 970520: Changed gps_dow to run from 0-6 (instead of 1-7).
c   MOD SCM 971029: Changed some of the math from REAL*8 to INTEGER*4 for LINUX.
c   MOD SCM 971115: Fixed more problems with mixing REAL and INTEGER arithmatic.
 
      integer*4 date(5), day_of_year, rcpar, len_run, ierr, num_run, i,
     .          gps_week, gps_dow, gps_sow, gps_start_mjd, indx
 
*   jd          - Julian date
*   mjd         - Modified Julian date.
*   sectag      - Seconds tag
 
      real*8 jd, mjd, sectag,dyear,grace_start_mjd,grace_sec
 
*   runstring(5)    - Five elments of the string.
 
 
      character*40 runstring(5)
      character*4 day_of_week(7)

      data gps_start_mjd / 44243 /
      data grace_start_mjd / 51544.5d0 /
      data day_of_week / 'Sun ', 'Mon ', 'Tue ', 'Wed ', 
     .                   'Thu ', 'Fri ', 'Sat '  /
 
****  Start looping over the runstring to see how many arguments
      i = 0
      len_run = 1
 
      do while ( len_run.gt.0 .and. i.lt.5 )
          len_run = rcpar(i+1,runstring(i+1))
          if( len_run.gt.0 ) i = i + 1
      end do
 
      num_run = i
 
****  See if we got any
      if( num_run.eq.0 ) then 
c          call proper_runstring('doy.hlp','doy',0)

c         time is not GPS (see below)
          indx = 0
       endif
 
****  Based on number, decoade the results
 
*     Set to 0 hr, 0 min
      date(4) = 0
      date(5) = 0
      sectag  = 0.01   ! Add one-hundredth-second to stop 60-sec returns

*     Check to see if the first argument has a W in it.  If it
*     does assume that it is GPS week.
      call casefold( runstring(1) )
      indx = index(runstring(1),'W')
      if( indx.gt.0 ) then

*         GPS week number passed.  Replace the W with a blank
*         and decode
          call sub_char(runstring(1),'W',' ')
          read(runstring(1),*,iostat=ierr) gps_week

*         Now see if gps_dow or gps_sow passed as second runstring
          if( num_run.eq.2 ) then
              read(runstring(2),*,iostat=ierr) gps_dow
*             check the size
              if( gps_dow.gt.6 .or. gps_dow.eq.0 ) then
                  gps_sow = gps_dow
                  gps_dow = gps_sow/86400 
              else
                  gps_sow = (gps_dow)*86400
              end if
          else
              gps_dow = 0
              gps_sow = 0
          end if
	  
*         Now compute the other quantities (add 1 because day of week
*         runs from 1 to 7.
          mjd = dble(gps_start_mjd + gps_week*7 + gps_sow/86400 + 1)
          jd  = mjd + 2400000.5d0
          call jd_to_ymdhms(jd, date, sectag)
          call ymd_to_doy(date, day_of_year)	  	 
          grace_sec = (mjd - grace_start_mjd)*86400.d0

      else
* MOD TAH 041221: See if Y placed making this a deciminal year
          if( index(runstring(1),'Y') .gt. 0 ) then    ! Process as deciminal years
              call sub_char(runstring(1),'Y',' ')
              read(runstring(1),*,iostat=ierr) dyear
              call decyrs_to_jd( dyear, jd)
              call jd_to_ymdhms(jd, date, sectag)
              call ymd_to_doy(date, day_of_year)
	      

* MOD SCM 110304:Check to see if the first argument has a G in it.  
* If it does assume that it is GRACE seconds.

          else if( index(runstring(1),'G') .gt. 0 ) then
              call sub_char(runstring(1),'G',' ')
              read(runstring(1),*,iostat=ierr) grace_sec
*         GRACE ZERO EPOCH is 2000 01 01 12 00 0.0d0
* MOD APP 121004: add small offset to GRACE secs to prevent rounding error
              mjd = grace_start_mjd + (grace_sec + 0.05)/86400.d0
              jd  = mjd + 2400000.5d0
              call jd_to_ymdhms(jd, date, sectag)
              call ymd_to_doy(date, day_of_year)
	            
*         Original conversions.

          else if( num_run.eq.1 ) then
 
*             Take to Julian date
              read(runstring(1),*,iostat=ierr) jd
              if( jd.lt.100000 ) jd = jd + 2400000.5d0
              call jd_to_ymdhms(jd, date, sectag)
              call ymd_to_doy(date, day_of_year)
 
          else if( num_run.eq.2 ) then
              read(runstring(1),*,iostat=ierr) date(1)
*                         ! January
              date(2) = 1
              read(runstring(2),*,iostat=ierr) date(3)
              call ymdhms_to_jd(date, sectag, jd)
              day_of_year = date(3)
              call jd_to_ymdhms(jd, date, sectag)
          else if( num_run.ge.3 ) then
              read(runstring(1),*,iostat=ierr) date(1)
              read(runstring(2),*,iostat=ierr) date(2)
              read(runstring(3),*,iostat=ierr) date(3)
              if( num_run.ge.4 ) 
     .        read(runstring(4),*,iostat=ierr) date(4)
              if( num_run.ge.5 ) 
     .        read(runstring(5),*,iostat=ierr) date(5)
              call ymdhms_to_jd(date, sectag, jd)
              call ymd_to_doy(date, day_of_year)
          else
               write (*,'(a)') '***TODAY*** IS: '
              call systime (date,sectag)
              call ymdhms_to_jd(date, sectag, jd)
              call ymd_to_doy(date, day_of_year)
          end if

*         Now compute the gps date quanities
          mjd = jd - 2400000.5d0
          gps_week = (idint(mjd) - gps_start_mjd - 1)/7
*         This test of if date is before start of gps time.
*         (Usual problem with fortan set int(-0.99) to 0)
          if( mjd-gps_start_mjd-1 .lt.0 ) gps_week = gps_week - 1
C         gps_sow  = (idint(mjd) - (gps_start_mjd+gps_week*7+1))*86400
          gps_sow  = (mjd - (gps_start_mjd+gps_week*7+1))*86400
*         This adjustment is also for negative int values.
          if( mjd-gps_start_mjd-1 .lt.0 .and.
     .        gps_sow.ge. 604800 ) gps_sow = gps_sow - 604800
          gps_dow = gps_sow/86400 
          grace_sec = (mjd - grace_start_mjd)*86400.d0
      end if

c     calculate decimal year, dealing with leap years
      call jd_to_ymdhms(jd,date,sectag)
      call jd_to_decyrs( jd, dyear )
           
****  Now write results
      write(*,100) (date(i),i=1,5), day_of_year, jd, mjd, gps_week,
     .             gps_dow, gps_sow, day_of_week(gps_dow+1),
     .             dyear,grace_sec
 100  format('Date ',i4,'/',i2.2,'/',i2.2,1x,i2,':',i2.2,
     .       ' hrs, DOY ',i3,' JD ',F13.4,' MJD ',F11.4,/,
     .       'GPS Week ',i5,' Day of week ',i2,', GPS Seconds ',i6,
     .       ' Day of Week ',a4,/,
     .       'Decimal Year ',f14.9,' GRACE Seconds ',f11.1)
 
      end
 
 


