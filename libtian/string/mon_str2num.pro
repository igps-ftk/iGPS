;+
; :Name:
;   MON_STR2NUM
;
; :Description:
;
; :Params:
;    MON_STR
;
;
;
; :Examples:
;    JAN           1
;    FEB           2
;    MAR           3
;    APR           4
;    MAY           5
;    JUN           6
;    JUL           7
;    AUG           8
;    SEP           9
;    OCT          10
;    NOV          11
;    DEC          12
;
; :Modifications:
;   Released on May 11, 2010
;
; :Author:
;   tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION MON_STR2NUM, MON_STR
  ;STOP
  MON_STRS=['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
  POS=WHERE(MON_STRS EQ STRUPCASE(MON_STR))
  IF POS[0] EQ -1 THEN RETURN,-1
  RETURN,POS+1  
END

PRO MON_STR2NUM
  MONSTRS=['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
  FOR I=0,N_ELEMENTS(MONSTRS)-1 DO BEGIN
    PRINT,MONSTRS[I],MON_STR2NUM(MONSTRS[I])
  ENDFOR
END