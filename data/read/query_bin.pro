pro query_bin, file, hfile=hfile, ns=ns, nl=nl, headers=lines,	$
		slope_n=slope_n, psdecay_n=psdecay_n, offset_n=offset_n, annual_n=annual_n, semiannual_n=semiannual_n,	$
		slope_e=slope_e, psdecay_e=psdecay_e, offset_e=offset_e, annual_e=annual_e, semiannual_e=semiannual_e,	$
		slope_u=slope_u, psdecay_u=psdecay_u, offset_u=offset_u, annual_u=annual_u, semiannual_u=semiannual_u,	$
		failed = failed


	if n_params() lt 1 then begin
		file='D:\phd\expt\data\sopac\cleanedNeuUnfTimeSeries20070409_bin\ab06CleanUnf.neu'
		file='D:\phd\expt\data\sopac\cleanedNeuUnfTimeSeries20070409_bin\candCleanUnf.neu'
	endif

	failed = 0

	hfile = desuffix(file)+'.hdr'
	lines = read_txt(hfile)

	for li=0, n_elements(lines)-1 do begin
		;
		tmpline=lines[li]

		pos = strpos(tmpline, 'NOT AVAILABLE')
		if pos[0] ne -1 then failed =1

		;n component
		pos = strpos(tmpline, 'n component')
		isin = 1

		if pos[0] ne -1 then begin
			;start n
			islo=0
			ioff=0
			ipsd=0
			while isin eq 1 do begin
				;print,tmpline

				pos = strpos(tmpline,'slope')
				if pos[0] ne -1 then begin
					ii=0
					tmp = strsplit(tmpline,/extract)
					pos0 = strpos(tmpline,'(')
					pos1 = strpos(tmpline,')')
					tmptime = strsplit(strmid(tmpline,pos0+1, pos1-pos0-1),'-',/extract)
					;print,tmptime
					if islo eq 0 then begin
						slope_n = double([tmp([3,5]),tmptime])
						islo=1
					endif else begin
						slope_n = [[slope_n],[ double([tmp([3,5]),tmptime] ) ] ]
					endelse
					;print,slope_n
				endif

				;ps decay
				pos = strpos(tmpline,'ps decay')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					ii=0
					pos0 = strpos(tmpline,'(')
					pos1 = strpos(tmpline,')')
					tmptime = strmid(tmpline,pos0+1, pos1-pos0-1)
					;print,tmptime
					if ipsd eq 0 then begin
						psdecay_n = double([tmp([4,6,10]),tmptime])
						ipsd=1
					endif else begin
						psdecay_n = [[psdecay_n],[ double([tmp([4,6,10]),tmptime] ) ] ]
					endelse
					;print,psdecay_n
				endif

				;detect offsets
				pos = strpos(tmpline,'offset')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					ii=0
					pos=strpos(tmpline,'*')
					if pos[0] ne -1 then tmp=tmp[1:*]	;discard co-seismic information
					pos0 = strpos(tmpline,'(')
					pos1 = strpos(tmpline,')')
					tmptime = strmid(tmpline,pos0+1, pos1-pos0-1)
					;print,tmptime
					if ioff eq 0 then begin
						offset_n = double([tmp([3,5]),tmptime])
						ioff=1
					endif else begin
						;print,'error',[tmp([3,5]),tmptime]
						offset_n = [[offset_n],[ double([tmp([3,5]),tmptime] ) ] ]
					endelse
					;help, offset_n
					;print,offset_n
				endif


				;detect annual, semi-annual
				pos = strpos(tmpline,' annual')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					annual_n = double(tmp([2,4,7]))
					;print,annual_n
				endif
				pos = strpos(tmpline,'semi-annual')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					semiannual_n = double(tmp([2,4,7]))
					;print,semiannual_n
				endif


				li=li+1
				tmpline = lines[li]
				if strtrim(tmpline,2) eq '#' then isin = 0
			endwhile
		endif

		;help, slope_n


		pos = strpos(tmpline, 'e component')
		if pos[0] ne -1 then begin
			;start e
			isin = 1
			islo=0
			ioff=0
			ipsd=0
			while isin eq 1 do begin
				;print,tmpline
				pos = strpos(tmpline,'slope')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					ii=0
					pos0 = strpos(tmpline,'(')
					pos1 = strpos(tmpline,')')
					tmptime = strsplit(strmid(tmpline,pos0+1, pos1-pos0-1),'-',/extract)
					;print,tmptime
					if islo eq 0 then begin
						slope_e = double([tmp([3,5]),tmptime])
						islo=1
					endif else begin
						slope_e = [[slope_e],[ double([tmp([3,5]),tmptime] ) ] ]
					endelse
					;print,slope_e
				endif

				;ps decay
				pos = strpos(tmpline,'ps decay')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					pos0 = strpos(tmpline,'(')
					pos1 = strpos(tmpline,')')
					tmptime = strmid(tmpline,pos0+1, pos1-pos0-1)
					;print,tmptime
					if ipsd eq 0 then begin
						psdecay_e = double([tmp([4,6,10]),tmptime])
						ipsd=1
					endif else begin
						psdecay_e = [[psdecay_e],[ double([tmp([4,6,10]),tmptime] ) ] ]
					endelse
					;print,psdecay_e
				endif

				;detect offsets
				pos = strpos(tmpline,'offset')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					pos=strpos(tmpline,'*')
					if pos[0] ne -1 then tmp=tmp[1:*]	;discard co-seismic information
					pos0 = strpos(tmpline,'(')
					pos1 = strpos(tmpline,')')
					tmptime = strmid(tmpline,pos0+1, pos1-pos0-1)
					;print,tmptime
					if ioff eq 0 then begin
						offset_e = double([tmp([3,5]),tmptime])
						ioff=1
					endif else begin
						;print,'error',[tmp([3,5]),tmptime]
						offset_e = [[offset_e],[ double([tmp([3,5]),tmptime] ) ] ]
					endelse
					;help, offset_e
					;print,offset_e
				endif


				;detect annual, semi-annual
				pos = strpos(tmpline,' annual')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					annual_e = double(tmp([2,4,7]))
					;print,annual_e
				endif
				pos = strpos(tmpline,'semi-annual')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					semiannual_e = double(tmp([2,4,7]))
					;print,semiannual_e
				endif


				li=li+1
				tmpline = lines[li]
				if strtrim(tmpline,2) eq '#' then isin = 0
			endwhile
		endif

		pos = strpos(tmpline, 'u component')
		if pos[0] ne -1 then begin
			;start u
			isin = 1

			islo=0
			ioff=0
			ipsd=0

			ii=0
			while isin eq 1 do begin
				;print,tmpline
				pos = strpos(tmpline,'slope')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					;ii=0
					pos0 = strpos(tmpline,'(')
					pos1 = strpos(tmpline,')')
					tmptime = strsplit(strmid(tmpline,pos0+1, pos1-pos0-1),'-',/extract)
					;print,tmptime
					if islo eq 0 then begin
						slope_u = double([tmp([3,5]),tmptime])
						islo=1
					endif else begin
						slope_u = [[slope_u],[ double([tmp([3,5]),tmptime] ) ] ]
					endelse
					;help,slope_u
				endif

				;ps decay
				pos = strpos(tmpline,'ps decay')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)

					pos0 = strpos(tmpline,'(')
					pos1 = strpos(tmpline,')')
					tmptime = strmid(tmpline,pos0+1, pos1-pos0-1)
					;print,tmptime
					if ipsd eq 0 then begin
						psdecay_u = double([tmp([4,6,10]),tmptime])
						ipsd=1
					endif else begin
						psdecay_u = [[psdecay_u],[ double([tmp([4,6,10]),tmptime] ) ] ]
					endelse
					;help,psdecay_u
				endif

				;detect offsets
				pos = strpos(tmpline,'offset')
				;ii=0
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					pos=strpos(tmpline,'*')
					if pos[0] ne -1 then tmp=tmp[1:*]	;discard co-seismic information
					pos0 = strpos(tmpline,'(')
					pos1 = strpos(tmpline,')')
					tmptime = strmid(tmpline,pos0+1, pos1-pos0-1)
					;print,tmptime
					if ioff eq 0 then begin
						offset_u = double([tmp([3,5]),tmptime])
						ioff=1
					endif else begin
						;print,'error',[tmp([3,5]),tmptime]
						offset_u = [[offset_u],[ double([tmp([3,5]),tmptime] ) ] ]
					endelse
					;help, offset_u
					;print,offset_u
				endif


				;detect annual, semi-annual
				pos = strpos(tmpline,' annual')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					annual_u = double(tmp([2,4,7]))
					;print,annual_u
				endif
				pos = strpos(tmpline,'semi-annual')
				if pos[0] ne -1 then begin
					tmp = strsplit(tmpline,/extract)
					semiannual_u = double(tmp([2,4,7]))
					;print,semiannual_u
				endif


				li=li+1
				tmpline = lines[li]
				if strtrim(tmpline,2) eq '#' then isin = 0
			endwhile
		endif

		;print,tmpline
		pos = strpos(tmpline, 'COL')
		if pos[0] ne -1 then begin
			if n_elements(strsplit(tmpline,':',/extract)) gt 1 then begin
				;A site named "COLB'.
				ns = fix( (strsplit(tmpline,':',/extract))[1] )
			endif
		endif
		pos = strpos(tmpline, 'ROW')
		if pos[0] ne -1 then begin
			if n_elements(strsplit(tmpline,':',/extract)) gt 1 then begin
				nl = fix( (strsplit(tmpline,':',/extract))[1] )
			endif
		endif
		;print,ns,nl
	endfor
	;print,ns,nl
	;help, offset_u, offset_e, offset_n, slope_n, slope_e, slope_u, psdecay_n, psdecay_u, psdecay_e
end