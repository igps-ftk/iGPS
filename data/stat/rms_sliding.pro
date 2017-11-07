function rms_sliding, t, x, szwin
  if n_elements(szwin) eq 0 then begin
    szwin=9
  endif else begin
    if n_elements(szwin) gt 1 || szwin lt 3 then begin
      print,'[]ERROR: wrong input for szwin: ',szwin,'!'
      return,!values.d_nan
    endif
  endelse
  
  if n_elements(x) le szwin then begin
    print,'[]ERROR: too short time series!'
    return,!values.d_nan
  endif
  
  
  np=n_elements(x)
  
  hszwin=szwin/2
  
  y=dblarr(np)
  mjds=dblarr(np)
  for pi=0ull,np-1 do begin
    doy,strtrim(t[pi],2)+'Y',mjd=mjd
    mjds[pi]=long(mjd)+.5d0
    ;stop
  endfor
  
  for pi=0ull,np-1 do begin
    ;stop
    pos=where( mjds ge (mjds[pi]-hszwin) and mjds le (mjds[pi]+hszwin) )
    ;p1=(pi-hszwin) > 0
    ;p2=(pi+hszwin) < (np-1)
    if pos[0] eq -1 then begin
      stop
    endif
    ;stop
    pos2=where(finite(x[pos]) eq 1)
    if pos2[0] eq -1 then begin
      stop
    endif
    y[pi]=rms(x[pos[pos2]])
  endfor
  ;stop
  return,y
end

