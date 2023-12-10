      Subroutine decyrs_to_ydhms( epoch,idate )

c     Convert a decimal year to yr doy hr min sec 
c     R. King 28 May 2003

      implicit none

      real*8 epoch,xjd,sod,sec

      integer*4 iyr,idoy,ihr,imin,isod,idate(5)

                    
      call decyrs_to_jd( epoch,xjd )
      call jd_to_yds( xjd,iyr,idoy,isod ) 
      sod = float(isod)
      call ds2hms(iyr,idoy,sod,ihr,imin,sec)
      idate(1) = iyr
      idate(2) = idoy
      idate(3) = ihr
      idate(4) = imin
      idate(5) = int(sec)

      return
      end

