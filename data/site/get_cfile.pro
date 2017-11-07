;+
; :Name:
;   GET_CFILE
;
; :Description:
;   Return the default a priori coordinate (*.llhxyz) filename.
;   Please use your own file if your sites are not in this file.
;   To create your own LLHXYZ file, use the menu "DATA - iGPS A Priori Coordnate File (*.llhxyz) - ...".
;
; :Examples:
;
; :Modifications:
;   Released on May 13, 2010
;
; :Author:
;   tianyf
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
FUNCTION GET_CFILE
  ;RETURN,FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['tables'],'itrf2005.apr.updated.net')
  RETURN,FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['tables'],'gps.net')
END

PRO GET_CFILE
  PRINT,GET_CFILE()
END