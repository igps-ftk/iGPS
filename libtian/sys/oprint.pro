PRO OPRINT,input
  if n_params() lt 1 then begin
    input='hello,world'
  endif
  
  bksp = REPLICATE(8B,16)
  PRINT,format='(a,$)',STRING(bksp)+input
  
END