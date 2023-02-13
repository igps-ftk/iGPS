FUNCTION SLOPE_TO_STRIKE, slope
  strike=-1*slope+90
  ;print,slope,strike
  
  xs=INDGEN(21)-10
  ys=xs*TAN(slope*!dpi/180)
  
;  WINDOW,3
;  PLOT,xs,ys,background='ffffff'x,color='0'x,psym=2,/ynozero,/iso
;  OPLOT,[-1000,1000],[0,0],color='aaaaaa'x
;  OPLOT,[0,0],[-1000,1000],color='aaaaaa'x
  ;stop
  RETURN,strike
END

PRO SLOPE_TO_STRIKE
  ;  slope=60 ;degrees
  ;  print,slope_to_strike(slope)  ;30 expected
  ;  slope=160 ;degrees
  ;  print,slope_to_strike(slope)  ;30 expected
  ;  slope=-10 ;degrees
  ;  print,slope_to_strike(slope)  ;30 expected
  slope=-100 ;degrees
  PRINT,slope_to_strike(slope)  ;30 expected
  
END