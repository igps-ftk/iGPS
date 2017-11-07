;+
;NAME:
;  START_IGPS
;PURPOSE:
;  SCRIPT USED TO SETUP iGPS PATH AND START iGPS
;PLATFORM
;  +IDL [OPTIONAL: WITH IMSL]
;    IMSL IS NOT AVAILABLE FOR SOME 64-BIT MODE IDL SESSIONS (SAY, IN MAC OS X 10.6 (intel x64)
;      AND SOLARIS X86 64).
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
PRO START_IGPS
  ;GET_IGPS_ROOT, IGPS_ROOT=IGPS_ROOT

  IF N_ELEMENTS(PROG) EQ 0 THEN BEGIN
    PROG='START_IGPS'
  ENDIF
  
  RELE=FLOAT(!VERSION.RELEASE)
  IF RELE LT 7.0 THEN BEGIN
    HELP,/SOURCE_FILE,OUTPUT=LINES
    FOR I=0,N_ELEMENTS(LINES)-1 DO BEGIN
      POS=STRPOS(LINES[I],PROG)
      IF POS[0] LT 0 THEN CONTINUE
      ;FILE=LAST(STRSPLIT(LINES[I],/EXTRACT))
      FILE=(STRSPLIT(LINES[I],/EXTRACT))[N_ELEMENTS(STRSPLIT(LINES[I],/EXTRACT))-1]
      
      ;IDL> help,lines
      ;LINES           STRING    = Array[6]
      ;IDL> for i=0,5 do print,lines[i]
      ;Compiled Procedures:
      ;$MAIN$
      ;START_IGPS
      ; /export/home/jlm/gps/gpsf/external/iGPS/main/start_igps.pro
      ;
      ;Compiled Functions:
      ;IDL> help,!version,/st
      ;** Structure !VERSION, 8 tags, length=104, data length=100:
      ;   ARCH            STRING    'sparc'
      ;   OS              STRING    'sunos'
      ;   OS_FAMILY       STRING    'unix'
      ;   OS_NAME         STRING    'Solaris'
      ;   RELEASE         STRING    '6.4.1'
      ;   BUILD_DATE      STRING    'Sep 25 2007'
      ;   MEMORY_BITS     INT             64
      ;   FILE_OFFSET_BITS
;   INT             64
      
      
      ;IDL> help,lines
      ;LINES           STRING    = Array[5]
      ;IDL> for i=0,4 do print,lines[i]
      ;Compiled Procedures:
      ;$MAIN$
      ;START_IGPS                /home/tianyf/gpsf/external/iGPS/main/start_igps.pro
      ;
      ;Compiled Functions:
      ;IDL> help,!version,/st
      ;** Structure !VERSION, 8 tags, length=76, data length=76:
      ;   ARCH            STRING    'x86'
      ;   OS              STRING    'linux'
      ;   OS_FAMILY       STRING    'unix'
      ;   OS_NAME         STRING    'linux'
      ;   RELEASE         STRING    '6.4'
      ;   BUILD_DATE      STRING    'Apr 26 2007'
      ;   MEMORY_BITS     INT             32
      ;   FILE_OFFSET_BITS
      ;                   INT             64
      
      BREAK
    ENDFOR
  ENDIF ELSE BEGIN
    FILE=ROUTINE_FILEPATH(PROG) ;NOT AVAILABLE IN IDL VERSION < 7.0
  ENDELSE
  ;STOP
  IF RELE LT 6.4 THEN BEGIN
    POS=STRPOS(FILE,'main'+PATH_SEP()+STRLOWCASE(PROG)+'.pro')
    IGPS_ROOT=STRMID(FILE,0,POS-1)
    IGPS_ROOT=''
    PRINT,'* iGPS has only been tested on IDL 6.4 and IDL 7.'
    PRINT,'* It does not work in IDL v5.5, because some new features are used (checked menu/accelerator/...).'
    PRINT,'* If you still want to give it a try, uncomment or delete the below line.'
    RETURN
  ENDIF ELSE BEGIN
    PATH=FILE_DIRNAME(FILE)
    IGPS_ROOT=FILE_DIRNAME(PATH)
  ENDELSE
  ;PRINT,!PATH
  POS=STRPOS(!PATH,IGPS_ROOT)
  IF POS LT 0 THEN BEGIN
    !PATH=!PATH+PATH_SEP(/SEARCH_PATH)+EXPAND_PATH('+'+IGPS_ROOT)
  ENDIF
  
  DEF_IGPS_SYSV,IGPS_ROOT
  ;START iGPS
  iGPS
END