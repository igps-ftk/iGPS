;+
; Name:
;		Tag_Contains
;
; Purpose:
;		Check if a string is a element of a struct.
;
;-
FUNCTION TAG_CONTAINS, struct, tagname
  type=SIZE(struct, /TYPE)
  IF TYPE NE 8 AND TYPE NE 7 THEN RETURN, 0
  IF TYPE EQ 8 THEN names=TAG_NAMES(struct)
  IF TYPE EQ 7 THEN names=struct
  ;
  FOR i=0, N_ELEMENTS(names)-1 DO BEGIN
    IF STRCMP(names(i), tagname, /FOLD_CASE) THEN RETURN, 1
  ENDFOR
  ;
  RETURN, 0
END
;
PRO TAG_CONTAINS
  tst_struct={name:'',age:28}
  PRINT,Tag_Contains(tst_struct,'name')
  PRINT,Tag_Contains(tst_struct,'nam')
  tags=['name','age']
  PRINT,Tag_Contains(tags,'name')
  PRINT,Tag_Contains(tags,'nam')
  
END
