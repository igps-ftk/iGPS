;;APR09 2007 TIAN
;;  +A LITTLE PROGRESS FOR LOOP, MUCH FASTER NOW;;
;;SIO:
;;	COMMENTS LINES START WITH A '#'
;;


PRO QUERY_ICD, FILE, $
    LINES=LINES, $
    NS=NS, $
    NL=NL, $
    NH=NH, $
    HEADERS=HEADERS,	$
    SLOPE_N=SLOPE_N, PSDECAY_N=PSDECAY_N, OFFSET_N=OFFSET_N, ANNUAL_N=ANNUAL_N, SEMIANNUAL_N=SEMIANNUAL_N,	$
    SLOPE_E=SLOPE_E, PSDECAY_E=PSDECAY_E, OFFSET_E=OFFSET_E, ANNUAL_E=ANNUAL_E, SEMIANNUAL_E=SEMIANNUAL_E,	$
    SLOPE_U=SLOPE_U, PSDECAY_U=PSDECAY_U, OFFSET_U=OFFSET_U, ANNUAL_U=ANNUAL_U, SEMIANNUAL_U=SEMIANNUAL_U,	$
    FAILED = FAILED, $
    XYZ=XYZ
  ;;
  PROG='QUERY_ICD'
  
  IF N_PARAMS() LT 1 THEN BEGIN
    ;
    FILE1=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','formats','iGPS'],'LHAZ.icd.final_igs08.pos.neu')
    ;
    ;FILE=FILE1
  ;OR
  ;    ;test input header lines
  ;    lines1=read_txt(file1)
  ;    lines=lines1
  ;    lines=grepi(lines1,'^#')
  ;    HELP, lines,lines1
    
    
  ENDIF
  ;;
  OFFSET_N=REPLICATE(-9999D0,3)
  OFFSET_E=OFFSET_N
  OFFSET_U=OFFSET_N
  PSDECAY_N=REPLICATE(-9999D0,4)
  PSDECAY_E=PSDECAY_N
  PSDECAY_U=PSDECAY_N
  ;;
  XYZ=DBLARR(3)
  
  IF N_ELEMENTS(FILE) GT 0 THEN BEGIN
    IF FILE_TEST(FILE) EQ 1 THEN BEGIN
      ;PRINT,'['+PROG+']Reading file ...'
      LINES=READ_TXT(FILE)
    ENDIF ELSE BEGIN
      PRINT,'['+PROG+']ERROR:file not exist!'
      RETURN
    ENDELSE
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(LINES) EQ 0 THEN BEGIN
      PRINT,'['+PROG+']ERROR: file and header are both not given!'
      RETURN
    ENDIF ELSE BEGIN
      PRINT,'['+PROG+']WARNING: use given header lines!'
    ENDELSE
  ENDELSE
  
  NLINES=N_ELEMENTS(LINES)
  IF NLINES EQ 1 && STRTRIM(LINES[0],2) EQ '' THEN RETURN
  
  HEADERS=GREPI(LINES,'^#')
  IF HEADERS[0] EQ '' THEN BEGIN
    NH=0ULL
  ENDIF ELSE BEGIN
    NH=N_ELEMENTS(HEADERS)
  ENDELSE
  
  NL=NLINES-NH
  IF NL GT 0 THEN BEGIN
    LINE=LINES[NH+1]
    NS=N_ELEMENTS(STRSPLIT(LINE,/EXTRACT))
  ENDIF
  
  ;HELP, NLINES, NH, NL, NS
  
  ;LOOP FOR EACH HEADER LINE
  IS_NEU=0  ;1-N; 2-E; 3-U
  FOR  LI=0, NH-1 DO BEGIN
    TMPLINE=HEADERS[LI]
    
    ;CHECK IF VALID HEADER EXISTS
    ;pos = STRPOS(tmpline, 'NOT AVAILABLE')
    ;IF pos[0] NE -1 THEN failed =1
    ;STOP
    
    ;;n component (WE USE UPPERCASE TO DIFFER FROM THE SIO HEADER (LOWERCASE)).
    POS = STRPOS(TMPLINE, 'N COMPONENT')
    IF POS[0] NE -1 THEN BEGIN
      IS_NEU=1
      ISLO=0
      IOFF=0
      IPSD=0
      CONTINUE
    ENDIF
    POS = STRPOS(TMPLINE, 'E COMPONENT')
    IF POS[0] NE -1 THEN BEGIN
      IS_NEU=2
      ISLO=0
      IOFF=0
      IPSD=0
      CONTINUE
    ENDIF
    POS = STRPOS(TMPLINE, 'U COMPONENT')
    IF POS[0] NE -1 THEN BEGIN
      IS_NEU=3
      ISLO=0
      IOFF=0
      IPSD=0
      CONTINUE
    ENDIF
    
    CASE IS_NEU OF
      0: BEGIN
      END
      1: BEGIN
      
        pos = STRPOS(tmpline,'slope')
        IF pos[0] NE -1 THEN BEGIN
          ii=0
          tmp = STRSPLIT(tmpline,/extract)
          pos0 = STRPOS(tmpline,'(')
          pos1 = STRPOS(tmpline,')')
          tmptime = STRSPLIT(STRMID(tmpline,pos0+1, pos1-pos0-1),'-',/extract)
          ;;;;print,tmptime
          IF islo EQ 0 THEN BEGIN
            slope_n = DOUBLE([tmp([3,5]),tmptime])
            islo=1
          ENDIF ELSE BEGIN
            slope_n = [[slope_n],[ DOUBLE([tmp([3,5]),tmptime] ) ] ]
          ENDELSE
        ;;;;print,slope_n
        ENDIF
        
        
        ;;;;ps decay
        pos = STRPOS(tmpline,'ps decay')
        pos2=STRPOS(tmpline,'postseismic decay')
        IF pos[0] NE -1 && pos2[0] EQ -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          ii=0
          pos0 = STRPOS(tmpline,'(')
          pos1 = STRPOS(tmpline,')')
          tmptime = STRMID(tmpline,pos0+1, pos1-pos0-1)
          ;;print,tmptime
          IF ipsd EQ 0 THEN BEGIN
            psdecay_n = DOUBLE([tmp([4,6,9]),tmptime])
            ;stop
            ipsd=1
          ENDIF ELSE BEGIN
            psdecay_n = [[psdecay_n],[ DOUBLE([tmp([4,6,9]),tmptime] ) ] ]
          ;print,tmp  ;## ps decay: postseismic decay
          ENDELSE
        ;;;;print,psdecay_n
        ENDIF
        
        ;;;;detect offsets
        pos = STRPOS(tmpline,'offset')
        pos2=STRPOS(tmpline,'coseismic')
        IF pos[0] NE -1 && pos2[0] EQ -1 THEN BEGIN
          tmp = STRSPLIT(STRMID(tmpline,pos),/extract)
          ;print,tmp
          ;stop
          ii=0
          pos=STRPOS(tmpline,'*')
          ;IF pos[0] NE -1 THEN tmp=tmp[1:*]  ;;discard co-seismic information
          pos0 = STRPOS(tmpline,'(')
          pos1 = STRPOS(tmpline,')')
          tmptime = STRMID(tmpline,pos0+1, pos1-pos0-1)
          ;;print,tmptime
          IF ioff EQ 0 THEN BEGIN
            offset_n = DOUBLE([tmp([2,4]),tmptime])
            ioff=1
          ENDIF ELSE BEGIN
            ;;print,'error',[tmp([3,5]),tmptime]
            offset_n = [[offset_n],[ DOUBLE([tmp([2,4]),tmptime] ) ] ]
          ;PRINT,tmp
          ;print,tmpline
          ;stop
          ;* offset 2: 0.0114 +/- 0.0027 m (1999.7904)
          ;## offset 1: 0.0021 +/- 0.0049 m (1999.4397)
          ;* offset 2: 0.0054 +/- 0.0037 m (1999.7904)
          ;## offset 1: 0.0173 +/- 0.0089 m (1999.4397)
          ENDELSE
        ;;;;help, offset_n
        ;;;;print,offset_n
        ENDIF
        
        
        ;;;;detect annual, semi-annual
        pos = STRPOS(tmpline,' annual')
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          annual_n = DOUBLE(tmp([2,4,7]))
        ;;print,annual_n
        ENDIF
        pos = STRPOS(tmpline,'semi-annual')
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          semiannual_n = DOUBLE(tmp([2,4,7]))
        ;;;;print,semiannual_n
        ENDIF
        
      ;STOP
      END
      2: BEGIN
      
        ;;;;print,tmpline
        pos = STRPOS(tmpline,'slope')
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          ii=0
          pos0 = STRPOS(tmpline,'(')
          pos1 = STRPOS(tmpline,')')
          tmptime = STRSPLIT(STRMID(tmpline,pos0+1, pos1-pos0-1),'-',/extract)
          ;;;;print,tmptime
          IF islo EQ 0 THEN BEGIN
            slope_e = DOUBLE([tmp([3,5]),tmptime])
            islo=1
          ENDIF ELSE BEGIN
            slope_e = [[slope_e],[ DOUBLE([tmp([3,5]),tmptime] ) ] ]
          ENDELSE
        ;;;;print,slope_e
        ENDIF
        
        ;;;;ps decay
        pos = STRPOS(tmpline,'ps decay')
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          pos0 = STRPOS(tmpline,'(')
          pos1 = STRPOS(tmpline,')')
          tmptime = STRMID(tmpline,pos0+1, pos1-pos0-1)
          ;;print,tmptime
          IF ipsd EQ 0 THEN BEGIN
            psdecay_e = DOUBLE([tmp([4,6,9]),tmptime])
            ipsd=1
          ENDIF ELSE BEGIN
            psdecay_e = [[psdecay_e],[ DOUBLE([tmp([4,6,9]),tmptime] ) ] ]
          ENDELSE
        ;;;;print,psdecay_e
        ENDIF
        
        ;;;;detect offsets
        pos = STRPOS(tmpline,'offset')
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          pos=STRPOS(tmpline,'*')
          IF pos[0] NE -1 THEN tmp=tmp[1:*]  ;;discard co-seismic information
          pos0 = STRPOS(tmpline,'(')
          pos1 = STRPOS(tmpline,')')
          tmptime = STRMID(tmpline,pos0+1, pos1-pos0-1)
          ;;;;print,tmptime
          IF ioff EQ 0 THEN BEGIN
            offset_e = DOUBLE([tmp([3,5]),tmptime])
            ioff=1
          ENDIF ELSE BEGIN
            ;;;;print,'error',[tmp([3,5]),tmptime]
            offset_e = [[offset_e],[ DOUBLE([tmp([3,5]),tmptime] ) ] ]
          ENDELSE
        ;;;;help, offset_e
        ;;;;print,offset_e
        ENDIF
        
        
        ;;;;detect annual, semi-annual
        pos = STRPOS(tmpline,' annual')
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          annual_e = DOUBLE(tmp([2,4,7]))
        ;;;;print,annual_e
        ENDIF
        pos = STRPOS(tmpline,'semi-annual')
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          semiannual_e = DOUBLE(tmp([2,4,7]))
        ;;;;print,semiannual_e
        ENDIF
        
      END
      3: BEGIN
      
        ;;;;print,tmpline
        pos = STRPOS(tmpline,'slope')
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          ;;;;ii=0
          pos0 = STRPOS(tmpline,'(')
          pos1 = STRPOS(tmpline,')')
          tmptime = STRSPLIT(STRMID(tmpline,pos0+1, pos1-pos0-1),'-',/extract)
          ;;;;print,tmptime
          IF islo EQ 0 THEN BEGIN
            slope_u = DOUBLE([tmp([3,5]),tmptime])
            islo=1
          ENDIF ELSE BEGIN
            slope_u = [[slope_u],[ DOUBLE([tmp([3,5]),tmptime] ) ] ]
          ENDELSE
        ;;;;help,slope_u
        ENDIF
        
        ;;;;ps decay
        pos = STRPOS(tmpline,'ps decay')
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          
          pos0 = STRPOS(tmpline,'(')
          pos1 = STRPOS(tmpline,')')
          tmptime = STRMID(tmpline,pos0+1, pos1-pos0-1)
          ;;;;print,tmptime
          IF ipsd EQ 0 THEN BEGIN
            psdecay_u = DOUBLE([tmp([4,6,9]),tmptime])
            ipsd=1
          ENDIF ELSE BEGIN
            psdecay_u = [[psdecay_u],[ DOUBLE([tmp([4,6,9]),tmptime] ) ] ]
          ENDELSE
        ;;;;help,psdecay_u
        ENDIF
        
        ;;;;detect offsets
        pos = STRPOS(tmpline,'offset')
        ;;;;ii=0
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          pos=STRPOS(tmpline,'*')
          IF pos[0] NE -1 THEN tmp=tmp[1:*]  ;;discard co-seismic information
          pos0 = STRPOS(tmpline,'(')
          pos1 = STRPOS(tmpline,')')
          tmptime = STRMID(tmpline,pos0+1, pos1-pos0-1)
          ;;;;print,tmptime
          IF ioff EQ 0 THEN BEGIN
            offset_u = DOUBLE([tmp([3,5]),tmptime])
            ioff=1
          ENDIF ELSE BEGIN
            ;;;;print,'error',[tmp([3,5]),tmptime]
            offset_u = [[offset_u],[ DOUBLE([tmp([3,5]),tmptime] ) ] ]
          ENDELSE
        ;;;;help, offset_u
        ;;;;print,offset_u
        ENDIF
        
        
        ;;;;detect annual, semi-annual
        pos = STRPOS(tmpline,' annual')
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          annual_u = DOUBLE(tmp([2,4,7]))
        ;;;;print,annual_u
        ENDIF
        pos = STRPOS(tmpline,'semi-annual')
        IF pos[0] NE -1 THEN BEGIN
          tmp = STRSPLIT(tmpline,/extract)
          semiannual_u = DOUBLE(tmp([2,4,7]))
        ;;;;print,semiannual_u
        ENDIF
        
      END
    ENDCASE
    
    
    
  ENDFOR
  
  IF N_PARAMS() LT 1 THEN BEGIN
    ;FOR i=0, N_ELEMENTS(headers)-1 DO PRINT, headers(i)
    HELP, $
      SLOPE_N, PSDECAY_N, OFFSET_N, ANNUAL_N, SEMIANNUAL_N,	$
      SLOPE_E, PSDECAY_E, OFFSET_E, ANNUAL_E, SEMIANNUAL_E,	$
      SLOPE_U, PSDECAY_U, OFFSET_U, ANNUAL_U, SEMIANNUAL_U, $
      NH, NL, NS
    ;PRINT,offset_n
  ;    PRINT, slope_n,  offset_n, annual_n, semiannual_n,	$
  ;      slope_e,  offset_e, annual_e, semiannual_e,	$
  ;      slope_u,  offset_u, annual_u, semiannual_u
  ENDIF
  ;;
  failed=0
  RETURN
  
END
