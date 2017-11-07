
;+
; :Name:
;   CREATE_SITEMAP_INFO
;
; :Description:
;   Create information for psxy/pstext in GMT, to create sites map plots.
; :Params:
;    SITES
;    OFILE
;
; :Keywords:
;    CFILE
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
PRO CREATE_SITEMAP_INFO, SITES, OFILE, CFILE=CFILE
  IF N_PARAMS() LT 2 THEN BEGIN
    PRINT,'[CREATE_SITEMAP_INFO]Usage: create_sitemap_info, sites, ofile [, cfile=cfile]'
    RETURN
  ENDIF
  
  IF N_ELEMENTS(SITES) LE 0 || SITES[0] EQ '' THEN BEGIN
    PRINT,'[CREATE_SITEMAP_INFO]ERROR: no input sites. Program Stopped.'
  ENDIF
  
  IF N_ELEMENTS(CFILE) EQ 0 THEN BEGIN
    CFILE=GET_CFILE()
  ENDIF
  
  OPENW,FID,OFILE,/GET_LUN
  FOR SI=0, N_ELEMENTS(SITES)-1 DO BEGIN
    SITE=SITES[SI]
    ;READ_LLHXYZ, CFILE, SITE=SITE, XYZ=XYZ, LLH=LLH
    READ_NET, CFILE, SITE=SITE, LLH=LLH
    PRINT,SITE,LLH
    PRINTF,FID,LLH[0],LLH[1],"8 0 4 3 "+SITE, $
      FORMAT='(2F20.8,1X,A)'
  ENDFOR
  FREE_LUN,FID
END