pro clockwise_round_index,n,xs=xs,ys=ys
  if n_elements(n) eq 0 then begin
    n=2
  endif
  
  xs=intarr(999)
  ys=xs
  
  count=0
  
;  if (n mod 2) ne 1 then begin
;    print,'[]ERROR: n must be odd number!!'
;    return
;  endif 
  
  half_width= n / 2
  
  for i=0, n do begin
    xs(count)=n
    ys(count)=i*(-1)
    count=count+1
  endfor
  
  for i=1, n*2 do begin
    xs(count)=n-i
    ys(count)=n*(-1)
    count=count+1
  endfor
  
  
  for i=1, n*2 do begin
    xs(count)=n*(-1)
    ys(count)=n*(-1)+i
    count=count+1
  endfor
  
  
  for i=1, n*2 do begin
    xs(count)=n*(-1)+i
    ys(count)=n*(1)
    count=count+1
  endfor
  
  
  
  for i=1, n-1 do begin
    xs(count)=n*(1)
    ys(count)=n*(1)-i
    count=count+1
  endfor
  
  xs=xs(0:count-1)
  ys=ys(0:count-1)
  ;help,xs,ys
  
  ;print,xs(0:count-1)
  ;print,ys(0:count-1)
  
  
end