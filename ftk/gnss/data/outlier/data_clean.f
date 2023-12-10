;Name:
;	data_clean
;
;Purpose:
;	Delete outliers from coordinates time series
;
;Rules:
;	(a). error bars > 3 RMS for any component; [Wdowinski et al., 1997]
;	(b). deviate from the mean of the series (? residual?) by more than 3 times the RMS scatter;  [Wdowinski et al., 1997]
;	(c). |v - median(vs)| > 3*IQR(vs) [Nikolaidis, 2002]
;		  where,
;			IQR = Interquartile Range = 75th - 25th
;			vs = 1 year
;	(d). The daily coordinate solutions with formal
;		   uncertainties larger than the chosen thresholds of 50,
;		   50, and 100 mm for east, north, and vertical components,
;		   respectively, are discarded. When the residuals exceed the
;		   thresholds of 100, 100, and 300 mm for east, north, and
;		   vertical components, respectively, they are considered as
;		   outliers and are discarded [Dong et al., 2006].
;
;Inputs:
;	file	-
;	ofile	-
;	ni, ei, ui	-	index positions for N/E/U component, default to be 1, 2, and 3.
;
;
function data_clean, data, ni=ni, ei=ei, ui=ui, verbose=verbose
	;
	if n_elements(data) eq 0 then begin
		error,'No data defined!'
	endif
	if n_elements(ni) eq 0 then ni = 1
	if n_elements(ei) eq 0 then ei = 2
	if n_elements(ui) eq 0 then ui = 3

	neui = [ni, ei, ui]

	;**the most important part is to get the outliers index.

	ndays=n_elements(data[0,*])

	;calculate residual
	data0 = data_modelling(data[2:5,*])

    tmpinds=[-1]
	for ci = 0, 2 do begin	;ci - component index
		;rule (a)
		tmprms= rms(data[neui[ci]+3,*])
		tmpind=where(data[neui[ci]+3,*] ge tmprms)
		if tmpind[0] ne -1 then begin
			;tmpinds=[tmpinds, tmpind]
		endif
		;help, tmpinds, tmpind

		;rule (b) {assumed for residual time series}
		tmpmean = mean(data0[[ci+1],*],/double)
		tmprms = rms(data0[[ci+1],*])
		tmpind=where( (data0[[ci+1],*] - tmpmean) ge 2*tmprms )
		if tmpind[0] ne -1 then begin
			;tmpinds=[tmpinds, tmpind]
		endif
		;help, tmpinds, tmpind

		;rule (c)  {for residual time series}
		tmpmedian = median(data0[[ci+1],*], /double)
		semiiqrlen=366/2
		for di=0, ndays-1 do begin
			;|v - median(vs)| > 3*IQR(vs)
			if di lt semiiqrlen then begin
				dayspan=indgen(semiiqrlen*2)
			endif else begin
				if di gt ndays-semiiqrlen-1 then begin
					dayspan=ndays-indgen(semiiqrlen*2)-1
				endif else begin
					dayspan=di-indgen(semiiqrlen*2)
				endelse
			endelse
			;print,dayspan,format='(366I4)'
			tmpvm = abs(data0[ci+1,di] - median( data0[ci+1,dayspan]))
			tmpiqr = iqr( data0[ci+1,dayspan])
			;print,tmpvm, tmpiqr
			if tmpvm gt 3*tmpiqr then begin
				tmpinds=[tmpinds, di]
			endif
		endfor



		;rule (d)

	endfor

	if n_elements(tmpinds) le 1 then begin
		return, data
	endif else begin
		tmpinds = tmpinds[1:*]
		tmpinds = tmpinds[uniq(tmpinds[sort(tmpinds)])]
		help, tmpinds,data
		return, data[*, inv_ind(tmpinds,top=ndays)]
	endelse

	;print, 'END-of-data_clean'

end

pro data_clean, file, ofile, ni, ei, ui
	if n_params() lt 2 then begin
		 file='C:\data\PBO\data-out.unavco.org\pub\products\position\pbo.final_frame.pos\BTDM.pbo.final_frame.pos'
		 ofile='C:\data\PBO\data-out.unavco.org\pub\products\position\pbo.final_frame.pos_cleaned'+getfilename(file)
		 path='C:\data\PBO\data-out.unavco.org\pub\products\position\pbo.final_frame.pos\'
		 ni = 3
		 ei = 4
		 ui = 5
	endif

	;goto, test_for_files
	data_read_pbo, file, data=data, headers=headers
	;help, data, headers
	;data=data[*,1000:*]
	data0 = data_clean(data, ni=3, ei=4, ui=5)
	window, 0, xsize=1024
	plot, data[2,*], data[3,*], psym=1;,/nodata
	x=data[2,*]
	yu=data[3,*]+data[6,*]
	yl=data[3,*]-data[6,*]
	;oplot,[x,x],[yu,yl]
	oplot, data0[2,*], data0[3,*]

	goto, endit

test_for_files:
	files = file_search(path+path_sep()+'*.pos', count=nf)
	for fi=0, nf-1 do begin
		data_read_pbo, files[fi], data=data, headers=headers
		if n_elements(data[0,*]) lt 365 then continue
		print, files[fi]
		;help, data, headers
		data0 = data_clean(data, ni=3, ei=4, ui=5)
		window, 0, xsize=1024
		plot, data[2,*], data[3,*], psym=1,/nodata
		x=data[2,*]
		yu=data[3,*]+data[6,*]
		yl=data[3,*]-data[6,*]
		;oplot,[x,x],[yu,yl]

		oplot, data0[2,*], data0[3,*]
	endfor

endit:
	return
end