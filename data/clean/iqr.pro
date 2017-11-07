FUNCTION IQR, data
  data=DOUBLE(data)
  data=REFORM(data,N_ELEMENTS(data))
  ;help,data
  tmpstdv=STDDEV(data,/double)
  
  RETURN, tmpstdv*1.35
;
END

PRO IQR
  PRINT, iqr(SIN(DIST(12)))
END