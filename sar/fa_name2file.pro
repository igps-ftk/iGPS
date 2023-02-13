 FUNCTION FA_NAME2FILE, fa, vpath=vpath
 
   IF N_ELEMENTS(fa) EQ 0 THEN BEGIN
     fa='fa_xsh'
   ENDIF
   
   
   IF N_ELEMENTS(vpath) EQ 0 THEN vpath='C:\GMT_pub\vector\profile'
   
   
   file=vpath+PATH_SEP()+fa+'.psxy'
   IF FILE_TEST(file,/regular) NE 1 THEN BEGIN
     file=''
   ENDIF
   
   
   RETURN, file
   
 END
 
 PRO FA_NAME2FILE, fa
 
   fa='fa_xsh'
   ;fa='fa_xs1h'
   
   file=FA_NAME2FILE(fa)
   PRINT,'fault name:',fa
   PRINT,'fault file:',file
 END