;
function ymd2ydoy,a,b,c
;# convert year month day to year day_of_year or vise versa
;
if n_params() lt 1 then begin
	print, ""
	print, "usage: ymd2ydoy year month day (e.g. ymd2ydoy 1992 2 3 -> 1992 034) or"
	print, "       ymd2ydoy year day_of_year (e.g. ymd2ydoy 2000 81 -> 2000 03 21)"
	print, ""
	print,''
endif

if n_params() eq 3 then begin
	doyc=strsplit('0 31 59 90 120 151 181 212 243 273 304 334 365',/extract)
	doy=doyc[b-1]+c
	if ( a-1988 mod 4 ) eq 0 and b gt 2 then doy=doy+1
	return,[a,doy]
endif else begin
	mth=fix(strsplit('0 31 28 31 30 31 30 31 31 30 31 30 31',/extract))
	day=b+0
	a=fix(a)
	if ( (a-1988) mod 4) eq 0 then mth[1] = mth[1] +1
	m=1
	while (day gt mth[m]) do begin
		day = day-mth[m]
		m=m+1
	endwhile


	;print,a,m,day,format='(i5,i3,i3)'
	return, [a,m,day]
endelse

end


pro ymd2ydoy
	tdate=[2002,3,28]
	print,tdate
	print, ymd2ydoy(tdate[0],tdate[1],tdate[2])
	print,ymd2ydoy(2002,87)
	print,ymd2ydoy(1996,366)
end