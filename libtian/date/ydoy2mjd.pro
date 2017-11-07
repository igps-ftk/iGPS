;Return the middle time of day: 12:00 h
FUNCTION YDOY2MJD, years, doys
  IF N_ELEMENTS(years) NE N_ELEMENTS(doys) THEN BEGIN
    PRINT, ' [ydoy2mjd]Error: year and doy must be of the same length.'
    RETURN, -1
  ENDIF
  
  years=LONG(years)
  doys=LONG(doys)
  
  ;  mjds=DBLARR(N_ELEMENTS(years))
  ;  FOR i=0ull,N_ELEMENTS(mjds)-1 DO BEGIN
  ;    DOY,years[i],1,doys[i],12,0,dyear=dyr,mjd=mjd
  ;    mjds[i]=mjd
  ;  ENDFOR
  dates=INTARR(5,N_ELEMENTS(years))
  dates[0,*]=years
  dates[1,*]=1
  dates[2,*]=doys
  dates[3,*]=12
  dates[4,*]=0
  seconds=DBLARR(N_ELEMENTS(years))
  seconds[*]=0
  YMDHMS_TO_MJDS , dates, seconds, mjds
  RETURN,mjds
END

PRO YDOY2MJD
  PRINT, ydoy2mjd(REPLICATE(2004,35),INDGEN(035)+1)
;print,ydoy2mjd(2007,002)
END
