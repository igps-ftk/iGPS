
PRO YMD_TO_DOYS, ymds, doys
  IF N_PARAMS() LT 1 THEN BEGIN
    ymds=[[2007,12,31], $
      [2008,12,3],  $
      [2015,1,31] ]
  ENDIF
  
  ;*     Rouitne to convert yy mm dd to yy and day-of-year.  To do
  ;*     this we compute number of days since start of year.
  ;
  ;*   ymd(3)      - yy mm dd in array
  ;*   doy         - day-of-year
  
  ;	ymd = intarr(3)
  
  ;      integer*4 ymd(3), doy
  ;
  ;* LOCAL VARIABLES
  ;
  ;*   date(5)     - Full yy mm dd hh mm
  dates = INTARR(5,N_ELEMENTS(ymds[0,*]))
  
  ;      integer*4 date(5)
  ;
  ;*   sectag      - Seconds tag for jd routine
  ;*   jd          - date convert to juliane date
  ;*   jan1_jd     - JD on January 1
  ;
  ;      real*8 sectag, jd, jan1_jd
  ;
  ;****  START: put ymd into form for ymdhms_to_jd call
  dates(0,*) = ymds(0,*)
  dates(1,*) = ymds(1,*)
  dates(2,*) = ymds(2,*)
  dates(3,*) = 0
  dates(4,*) = 0
  sectags  = 0.d0
  
  ;*     Get julian date
  YMDHMS_TO_JDs, dates, sectags, jds
  
  ;*     Now do Jan 1
  dates(1,*) = 1
  dates(2,*) = 1
  YMDHMS_TO_JDs, dates, sectags, Jan1_jds
  
  ;*     Save the day of year
  doys  = jds - Jan1_jds + 1
  
  ;print,doy
  ;****  Thats all
  
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,ymds
    HELP, ymds, doys
    PRINT,doys
  ENDIF
  RETURN
END

