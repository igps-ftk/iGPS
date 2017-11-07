pro  ymdhms_to_jd, date, sectag, jd

	if n_params() lt 2 then begin
		date=[1992,1,1,0,0]
		date=[2007,5,23,0,0]
		sectag=0
	endif
	date=fix(date)
	ymdhms_to_mjd,date, sectag,jd
	jd=jd+2400000.5d0
	if n_params() lt 2 then  print,jd, format='(f20.4)'
end
