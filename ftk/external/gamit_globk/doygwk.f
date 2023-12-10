        subroutine doygwk(idoy,iy,iweek,idow)
c
c purpose:      convert dayofyear to/from gpsweek/dayofweek
c
c idoy=0 gpsweek/dayofweek -> dayofyear/year ,      otherwise reverse
c
c by Peng Fang, SIO, UCSD, pfang@pgga.ucsd.edu    Jan. 28, 93
c
        integer idoys(4),iy,iday,idow,idoy,iweek,i
        data idoys/366,365,365,365/
        if (idoy.eq.0) then
c gpsweek/dayofweek -> dayofyear/year
                idoy=iweek*7+idow+6
c                iy=80
c               Y2K mod
                 iy = 1980
                do while (idoy.gt.idoys(mod(iy,4)+1))
                        idoy=idoy-idoys(mod(iy,4)+1)
                        iy=iy+1
                enddo
        else
c dayofyear/year -> gpsweek/dayofweek
c                i=80
c                Y2K mod
                i = 1980
                iday=idoy-6
                do while (i.lt.iy)
                        iday=iday+idoys(mod(i,4)+1)
                        i=i+1
                enddo
                iweek=(iday)/7
                idow=mod(iday,7)
        endif
        return
        end

