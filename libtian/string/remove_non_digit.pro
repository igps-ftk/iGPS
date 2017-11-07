

;+
; :Name
;    remove_non_digit
;    
; :Description:
;    Remove non-digit character in a string.
;
; :Params:
;    in - input string
;
; :Example:
;    in_str='Non-digit-22334'
;    out_str=remove_non_digit(in_str)
;    IDL> print,out_str
;       22334
;       
; :Modification: 
;    + Created on Fri, Feb 20, 2015 10:06:12 AM by tianyf
;-
FUNCTION REMOVE_NON_DIGIT, in
  tmp=str2arr(in)
  out=STRARR(N_ELEMENTS(tmp))
  FOR ci=0,N_ELEMENTS(tmp)-1 DO BEGIN
    tval=BYTE(tmp[ci])
    IF tval LT BYTE('0') || tval GT BYTE('9') THEN CONTINUE
    out[ci]=tmp[ci]
  endfor
  pos=where(out ne '')
  if pos[0] eq -1 then return,''
  ;stop
  RETURN,strjoin(out[pos])
END

PRO REMOVE_NON_DIGIT
  tmp=REMOVE_NON_DIGIT('5dC00')
  HELP,tmp
  print,tmp,format='(a)'
END