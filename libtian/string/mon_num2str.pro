;+
; :Name:
;   MON_NUM2STR
;
; :Description:
;
; :Params:
;    moni
;
;
;
; :Examples:
;    Month       1: Jan
;    Month       2: Feb
;    Month       3: Mar
;    Month       4: Apr
;    Month       5: May
;    Month       6: Jun
;    Month       7: Jul
;    Month       8: Aug
;    Month       9: Sep
;    Month      10: Oct
;    Month      11: Nov
;    Month      12: Dec
;
; :Modifications:
;   Released on May 11, 2010
;
; :Author:
;   tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION MON_NUM2STR, moni
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,'Error input! Must be 1-12.'
    RETURN,''
  ENDIF
  monstr=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  RETURN, monstr[moni-1]
END


PRO MON_NUM2STR
  FOR i=1,12 DO BEGIN
    PRINT,'Month',i,': ',mon_num2str(i)
  ENDFOR
END
