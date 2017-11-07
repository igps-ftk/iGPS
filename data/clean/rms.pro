;TO CALCULATE RMS:
;
;   1. SQUARE all the values
;   2. Take the average of the squares
;   3. Take the square root of the average
;

FUNCTION RMS, data
  data=DOUBLE(data)
  data=REFORM(data,N_ELEMENTS(data))
  ;help,data
  n_ele = N_ELEMENTS(data)
  ;n_ele -  number of elements
  tmprms=SQRT( TOTAL(  (data)^2 /(1d0*n_ele) ) )
  RETURN, tmprms
;
END

PRO RMS
  PRINT, rms(SIN(DIST(12)))
END