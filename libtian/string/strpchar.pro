function strpchar, str
  stro=''
  numstrs=strarr(11)
  for i=0,9 do begin
    numstrs[i]=string(i,format='(i1)')
  endfor
  numstrs[10]='.'
      
  for ci=0,strlen(str)-1 do begin
    c=strmid(str,ci,1)
    pos=where(numstrs eq c)
    if pos[0] ne -1 then begin
      stro=stro+c
    endif else begin
      stro=stro+' '
    endelse
  endfor
  
  return, stro
end

pro strpchar
  str='Nav  3.63 / Boot  3 '
  ostr=strpchar(str)
  print,ostr
end