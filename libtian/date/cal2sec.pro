function cal2sec, year, mon, day, hh, mm, ss
  if n_params() lt 3 then begin
    return,''
  endif
  if n_elements(hh) eq 0 then hh=0
  if n_elements(mm) eq 0 then mm=0
  if n_elements(ss) eq 0 then ss=0d0
  jd2000=julday(1,1,2000,12,0,0)
  jd=julday(mon,day,year,hh,mm,ss)
  ;help,jd2000,jd
  ;print,jd2000,jd,format='(2f30.12)'
  ;print,jd-jd2000,format='(f30.12)'
  sec=(jd-jd2000)*3600d0*24
  return,sec
  

end

pro cal2sec
  print,cal2sec(2010,1,1,12,0),format='(f30.12)'
end