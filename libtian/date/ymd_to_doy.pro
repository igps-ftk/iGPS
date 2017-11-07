
pro ymd_to_doy, ymd, doy
	if n_params() lt 1 then begin
		ymd=[2007,12,31]
	endif

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
	date = intarr(5)

;      integer*4 date(5)
;
;*   sectag      - Seconds tag for jd routine
;*   jd          - date convert to juliane date
;*   jan1_jd     - JD on January 1
;
;      real*8 sectag, jd, jan1_jd
;
;****  START: put ymd into form for ymdhms_to_jd call
      date(0) = ymd(0)
      date(1) = ymd(1)
      date(2) = ymd(2)
      date(3) = 0
      date(4) = 0
      sectag  = 0.d0

;*     Get julian date
       ymdhms_to_jd, date, sectag, jd

;*     Now do Jan 1
      date(1) = 1
      date(2) = 1
      ymdhms_to_jd, date, sectag, Jan1_jd

;*     Save the day of year
      doy  = jd - Jan1_jd + 1

	;print,doy
;****  Thats all
      return
      end

