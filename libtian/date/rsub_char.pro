function rsub_char, str, substr_to_rep, str_with
	if n_params() lt 2 then begin
		str='abcdefWxdfg'
		substr_to_rep='W'
		print,'Not valid inputs'
	endif

	if n_elements(str_with) eq 0 then str_with=' '

	pos=strpos(str, substr_to_rep)
	if pos[0] eq -1 then return, str

	;help, pos
	for pi=0, n_elements(pos)-1 do begin
		str=strmid(str,0,pos[pi])+str_with+strmid(str,pos[pi]+1+strlen(substr_to_rep))
		;print,str
	endfor

	;print, str
	return, str

end