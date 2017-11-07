;+
; Name:
;		Dialog_PickFile1
;
; Purpose:
;		Add remember-selection function to Dialog_PickFile.
;
; Status:
;		Obsolete.
;		Replaced by dp function.
;-
FUNCTION DIALOG_PICKFILE1, _extra=_ex
  ;
  ;cd, current=curpath
  ;fname=dialog_pickfile(_extra=_ex,path=curpath,get_path=retpath)
  ;cd, retpath
  ;
  ;return,fname
  RETURN, dp(_extra=_ex)
END
;
PRO DIALOG_PICKFILE1
  fname1=dialog_pickfile1(title='file 1:',/rec)
  IF fname1 EQ '' THEN RETURN
  fname2=dialog_pickfile1(title='file 2:')
  IF fname2 EQ '' THEN RETURN
  ;
  PRINT,'File 1 is:'+fname1
  PRINT,'File 2 is:'+fname2
END
