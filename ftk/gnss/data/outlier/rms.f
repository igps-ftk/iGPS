;TO CALCULATE RMS:
;
;   1. SQUARE all the values
;   2. Take the average of the squares
;   3. Take the square root of the average
;

function rms, data
	data=double(data)
	data=reform(data,n_elements(data))
	;help,data
	n_ele = n_elements(data)
	;n_ele -  number of elements
	tmprms=sqrt( total(  (data)^2 /(1d0*n_ele) ) )
	return, tmprms
	;
end

pro rms
	print, rms(sin(dist(12)))
end