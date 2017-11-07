function iqr, data
	data=double(data)
	data=reform(data,n_elements(data))
	;help,data
	tmpstdv=stddev(data,/double)

	return, tmpstdv*1.35
	;
end

pro iqr
	print, iqr(sin(dist(12)))
end