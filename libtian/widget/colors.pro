pro colors, n, color=color, rgbs=rgbs
  if n_params() lt 1 then begin
    n=16
  endif
  
  ;color=lindgen(n+2)*16777215d0/(n+2)
  ind=256d0/n*(indgen(n)+1d0)-1
  ;print,ind
  color=lonarr(n)
  rgbs=intarr(3,n)
  for i=0,n-1 do begin
    r=ind[i]
    if (i mod 2) eq 0 then begin
      g=ind[(N/2-I) > 0]
    endif else begin
      g=ind[(N/2+I) < (N-1)]
    endelse
    b=ind[n-1-i]
    color[i]=rgb(r,g,b)
    rgbs[*,i]=[r,g,b]
    ;PRINT,R,G,B
  endfor
  
  if n_params() lt 1 then begin
    help,color
    print,color
  endif
  
  ;STOP

end