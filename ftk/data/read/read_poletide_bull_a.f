      subroutine read_poletide_bull_a(file,mjdall,xpall,ypall,n)
c     ---
      implicit none
      include '../../inc/cgps.h'
      character*(*) file
      real*8 mjdall(nmax_pt),xpall(nmax_pt),ypall(nmax_pt)
      integer*4 n
c     ---
      character*1000 tmpstr,buf
      integer*4 i,j
      integer*4 fid,ioerr
      integer*4 a,b,c
      real*8 xp,yp,xpe,ype,mjd
      integer nblen
      character*2 type
c     ---

c      file = 'D:\phd\Document\thesis.ref\loading\pole.tide\data\finals2000A.all.d'
      if (nblen(file).lt.1) then
         file='/home/tianyf/gpsf/cgps/conf/poletide/finals2000A.all.d'
         write(*,*) 'Using default[pole tide]',file(1:nblen(file))
      endif

      fid=31
      n=0
c      write(*,*) file
c      goto 899
      open(unit=fid,file=file,iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,*) 'Error open pole tide file'
         goto 899
      endif
 800  read(fid,700,end=898,iostat=ioerr) a,b,c,mjd,type,xp,xpe,yp,ype
c 800  read(fid,'(a1000)',end=898) buf
      if (ioerr.ne.0) then
         write(*,*) 'pole tide read error.'
         goto 898
      endif
c      write(*,700) a,b,c,mjd,type,xp,xpe,yp,ype
c      write(*,*) n,nmax_pt
c      write(*,*) buf(1:80)
c      read(buf,700) a,b,c,mjd,type,xp,xpe,yp,ype
c      write(*,700) a,b,c,mjd,type,xp,xpe,yp,ype
      n=n+1
      mjdall(n)=mjd
      xpall(n)=xp
      ypall(n)=yp
c      write(*,*) n
      goto 800
 700  format(i2,i2,i3,f9.2,a2,f10.6,f9.6,f10.6,f9.6)

 898  continue
c      write(*,*) 'read_poletide_bulla_a'
      close(fid)
c      pause "pole read"
 899  return
      end

c$$$;The format of the finals2000A.data, finals2000A.daily, and finals2000A.all files is:
c$$$;Col.# Format Quantity
c$$$;------- ------ -------------------------------------------------------------
c$$$;1-2 I2 year (to get true calendar year, add 1900 for MJD<=51543 or add 2000 for MJD>=51544)
c$$$;3-4 I2 month number
c$$$;5-6 I2 day of month
c$$$;7 X [blank]
c$$$;8-15 F8.2 fractional Modified Julian Date (MJD)
c$$$;16 X [blank]
c$$$;17 A1 IERS (I) or Prediction (P) flag for Bull. A polar motion values
c$$$;18 X [blank]
c$$$;19-27 F9.6 Bull. A PM-x (sec. of arc)
c$$$;28-36 F9.6 error in PM-x (sec. of arc)
c$$$;37 X [blank]
c$$$;38-46 F9.6 Bull. A PM-y (sec. of arc)
c$$$;47-55 F9.6 error in PM-y (sec. of arc)
c$$$;56-57 2X [blanks]
c$$$;58 A1 IERS (I) or Prediction (P) flag for Bull. A UT1-UTC values
c$$$;59-68 F10.7 Bull. A UT1-UTC (sec. of time)
c$$$;69-78 F10.7 error in UT1-UTC (sec. of time)
c$$$;79 X [blank]
c$$$;80-86 F7.4 Bull. A LOD (msec. of time) -- NOT ALWAYS FILLED
c$$$;87-93 F7.4 error in LOD (msec. of time) -- NOT ALWAYS FILLED
c$$$;94-95 2X [blanks]
c$$$;96 A1 IERS (I) or Prediction (P) flag for Bull. A nutation values
c$$$;97 X [blank]
c$$$;98-106 F9.3 Bull. A dX wrt IAU2000A Nutation (msec. of arc), Free Core Nutation NOT Removed
c$$$;107-115 F9.3 error in dX (msec. of arc)
c$$$;116 X [blank]
c$$$;117-125 F9.3 Bull. A dY wrt IAU2000A Nutation (msec. of arc), Free Core Nutation NOT Removed
c$$$;126-134 F9.3 error in dY (msec. of arc)
c$$$;135-144 F10.6 Bull. B PM-x (sec. of arc)
c$$$;145-154 F10.6 Bull. B PM-y (sec. of arc)
c$$$;155-165 F11.7 Bull. B UT1-UTC (sec. of time)
c$$$;166-175 F10.3 Bull. B dX wrt IAU2000A Nutation (msec. of arc)
c$$$;176-185 F10.3 Bull. B dY wrt IAU2000A Nutation (msec. of arc)
