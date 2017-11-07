;+
; :Name:
;   NDOY
;
; :Description:
;   Return the number of days for the given year.
;   
; :Params:
;    yr
;
;
;
; :Examples:
;
; :Modifications:
;   Released on May 11, 2010
;
; :Author:
;   tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION NDOY, yr
  IF yr LT 10 THEN BEGIN
    yr=yr+2000
  ENDIF ELSE BEGIN
    IF yr LT 100 THEN BEGIN
      yr=yr+1900
    ENDIF
  ENDELSE
  ;
  RETURN,JULDAY(12,31,yr,12,0,0)-JULDAY(1,1,yr,12,0,0)+1
  
END

PRO NDOY, yr
  PRINT,'[NDOY]', 2002,':',ndoy(02)
  PRINT,'[NDOY]', 2004,':',ndoy(2004)
END