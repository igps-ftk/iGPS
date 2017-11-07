FUNCTION QRY_SIO_DECODE_OFFSET, TMP
  ;Given an offset line, return the offset size and time
  ;Sample:
  ;#    offset 1:-232.61  +/- 3.59  mm (2011-04-17 [2011.2918])

  ;use the first ":", "+/-", and "mm" to get the size and sigma of offset.
  POS1=STRPOS(TMP, ':')
  POS2=STRPOS(TMP, '+/-')
  POS3=STRPOS(STRLOWCASE(TMP), 'mm')
  POS4=STRPOS(TMP, '(')
  POS5=STRPOS(TMP, '[')
  POS6=STRPOS(TMP, ']')
  POS7=STRPOS(TMP, ')')
  
  RTN_VAL=[STRMID(TMP,POS1+1,POS2-POS1-1), $
    STRMID(TMP,POS2+3,POS3-POS2-3), $
    STRMID(TMP,POS5+1,POS6-POS5-1)]
  ;HELP, RTN_VAL
    
  RETURN,DOUBLE(RTN_VAL)
  
END


;;APR09 2007 TIAN
;;  +A LITTLE PROGRESS FOR LOOP, MUCH FASTER NOW;;
;;SIO:
;;	COMMENTS LINES START WITH A '#'
;;
PRO QUERY_SIO, FILE, $
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
  IF N_PARAMS() LT 1 THEN BEGIN
    FILE=FILEPATH(ROOT_DIR=!IGPS_ROOT,SUBDIRECTORY=['example','sio','cleanedNeuUnf'],'bjfsCleanUnf.neu')
    FILE='D:\Papers.data\Paper.SpatialFltering\garner\WNAM_Clean_ResidNeuTimeSeries_comb_20150318\c24\oksoCleanResid.neu'
    PRINT,'[QUERY_SIO]WARNING: no input parameter. Using the default test example :'+FILE+'!', FORMAT='(A)'
    IF N_ELEMENTS(FILE) EQ 0 || FILE_TEST(FILE) NE 1 THEN FILE=DIALOG_PICKFILE(/READ)
    IF FILE EQ '' THEN RETURN
  ENDIF
  ;;
  offset_n=REPLICATE(-9999d0,3)
  offset_e=offset_n
  offset_u=offset_n
  psdecay_n=REPLICATE(-9999d0,4)
  psdecay_e=psdecay_n
  psdecay_u=psdecay_n
  ;;
  XYZ=DBLARR(3)
  ;XYZ[*]=-9999D0
  OPENR,fid,file,/get_lun, ERROR = err
  
  ; If err is nonzero, something happened. Print the error message to
  ; the standard error file (logical unit -2):
  IF (err NE 0) THEN BEGIN
    PRINTF, -2, !ERROR_STATE.MSG
  ENDIF
  
  tmpstr=''
  READF,fid,tmpstr
  ;print,tmpstr
  
  li=0ull
  headers=''
  
  WHILE NOT EOF(fid) DO BEGIN
    ;STOP
    tmpline=tmpstr
    
    pos = STRPOS(tmpline, 'NOT AVAILABLE')
    IF pos[0] NE -1 THEN failed =1
    
    IF ARG_PRESENT(slope_n) || ARG_PRESENT(psdecay_n) || ARG_PRESENT(offset_n)  $
      || ARG_PRESENT(annual_n) || ARG_PRESENT(semiannual_n) THEN BEGIN
      
      ;;n component
      pos = STRPOS(tmpline, 'n component')
      ;pos = STRPOS(tmpline, 'N COMPONENT')
      isin = 1
      
      IF pos[0] NE -1 THEN BEGIN
      
        ;;start n
        islo=0
        ioff=0
        ipsd=0
        headers=[headers,tmpline]
        WHILE isin EQ 1 DO BEGIN
          ;;;;print,tmpline
        
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
            ;IF pos[0] NE -1 THEN tmp=tmp[1:*]	;;discard co-seismic information
            pos0 = STRPOS(tmpline,'(')
            pos1 = STRPOS(tmpline,')')
            tmptime = STRMID(tmpline,pos0+1, pos1-pos0-1)
            ;;print,tmptime
            IF ioff EQ 0 THEN BEGIN
              offset_n = QRY_SIO_DECODE_OFFSET(tmpline)
              ioff=1
            ENDIF ELSE BEGIN
              offset_n = [[offset_n],[ QRY_SIO_DECODE_OFFSET(tmpline) ] ]
            ENDELSE
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
          
          li=li+1
          READF,fid,tmpstr
          ;stop
          ;print,tmpstr
          headers=[headers, tmpstr]
          IF STRTRIM(TMPLINE,2) EQ '' THEN CONTINUE
          tmpline=tmpstr
          IF STRTRIM(tmpline,2) EQ '#' THEN BEGIN
            isin = 0
            GOTO, NEXT_LINE
          ;headers=[headers,tmpline]
          ENDIF
          IF STRMID(STRTRIM(TMPLINE,2),0,1) NE '#' THEN BEGIN
            HEADERS=HEADERS[0:N_ELEMENTS(HEADERS)-2]
            GOTO, READDATA
          ENDIF
        ENDWHILE
      ENDIF
      
    ;;;;help, slope_n
      
    ENDIF ;END-OF-NORTH-COMPONENT
    
    
    IF ARG_PRESENT(slope_E) || ARG_PRESENT(psdecay_E) || ARG_PRESENT(offset_E)  $
      || ARG_PRESENT(annual_E) || ARG_PRESENT(semiannual_E) THEN BEGIN
      
      pos = STRPOS(tmpline, 'e component')
      ;POS = STRPOS(TMPLINE, 'E COMPONENT')
      IF pos[0] NE -1 THEN BEGIN
        ;;;;start e
        headers=[headers,tmpline]
        isin = 1
        islo=0
        ioff=0
        ipsd=0
        ;STOP
        WHILE isin EQ 1 DO BEGIN
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
            IF pos[0] NE -1 THEN tmp=tmp[1:*]	;;discard co-seismic information
            IF ioff EQ 0 THEN BEGIN
              offset_e = QRY_SIO_DECODE_OFFSET(tmpline)
              ioff=1
            ENDIF ELSE BEGIN
              offset_e = [[offset_e],[ QRY_SIO_DECODE_OFFSET(tmpline) ] ]
            ENDELSE
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
          
          
          li=li+1
          READF, fid, tmpstr
          headers=[headers, tmpstr]
          tmpline = tmpstr
          IF STRTRIM(TMPLINE,2) EQ '' THEN CONTINUE
          IF STRTRIM(tmpline,2) EQ '#' THEN BEGIN
            isin = 0
            GOTO, NEXT_LINE
          ENDIF
          IF STRMID(STRTRIM(TMPLINE,2),0,1) NE '#' THEN GOTO, READDATA
        ENDWHILE
      ENDIF
      
      
    ENDIF ;END-OF-EAST-COMPONENT
    
    IF ARG_PRESENT(slope_U) || ARG_PRESENT(psdecay_U) || ARG_PRESENT(offset_U)  $
      || ARG_PRESENT(annual_U) || ARG_PRESENT(semiannual_U) THEN BEGIN
      
      pos = STRPOS(tmpline, 'u component')
      ;POS = STRPOS(TMPLINE, 'U COMPONENT')
      IF pos[0] NE -1 THEN BEGIN
        ;;;;start u
        headers=[headers,tmpline]
        isin = 1
        
        islo=0
        ioff=0
        ipsd=0
        
        ii=0
        WHILE isin EQ 1 DO BEGIN
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
            IF pos[0] NE -1 THEN tmp=tmp[1:*]	;;discard co-seismic information
            pos0 = STRPOS(tmpline,'(')
            pos1 = STRPOS(tmpline,')')
            tmptime = STRMID(tmpline,pos0+1, pos1-pos0-1)
            ;;;;print,tmptime
            IF ioff EQ 0 THEN BEGIN
              offset_u = QRY_SIO_DECODE_OFFSET(tmpline)
              ioff=1
            ENDIF ELSE BEGIN
              ;;;;print,'error',[tmp([3,5]),tmptime]
              offset_u = [[offset_u],[ QRY_SIO_DECODE_OFFSET(tmpline) ] ]
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
          
          
          li=li+1
          READF,fid,tmpstr
          headers=[headers, tmpstr]
          tmpline = tmpstr
          IF STRTRIM(TMPLINE,2) EQ '' THEN CONTINUE
          IF STRTRIM(tmpline,2) EQ '#' THEN BEGIN
            isin = 0
            GOTO, NEXT_LINE
          ENDIF
          IF STRMID(STRTRIM(TMPLINE,2),0,1) NE '#' THEN GOTO, READDATA
        ENDWHILE
      ENDIF
      
    ENDIF ;END-OF-VERTICAL-COMPONENT
    
    IF STRMID(tmpstr,0,1) NE '#' THEN BEGIN
      li=li-1
      LI=1
      GOTO, readdata
    ENDIF
    
    
    IF li EQ 0 THEN BEGIN
      headers=tmpstr
    ENDIF ELSE BEGIN
      headers=[headers, tmpstr]
    ENDELSE
    
    
    NEXT_LINE:
    pos = STRPOS(tmpline,'Reference_X')
    IF POS[0] NE -1 THEN BEGIN
      XYZ[0]=DOUBLE((STRSPLIT(TMPLINE,':',/EXTRACT))[1])
    ENDIF
    pos = STRPOS(tmpline,'Reference_Y')
    IF POS[0] NE -1 THEN BEGIN
      XYZ[1]=DOUBLE((STRSPLIT(TMPLINE,':',/EXTRACT))[1])
    ENDIF
    pos = STRPOS(tmpline,'Reference_Z')
    IF POS[0] NE -1 THEN BEGIN
      XYZ[2]=DOUBLE((STRSPLIT(TMPLINE,':',/EXTRACT))[1])
    ENDIF
    
    
    
    li=li+1ull
    ;;;;
    
    READF, fid, tmpstr
  ;print,li
  ;print,tmpstr
    
  ENDWHILE
  ;;
  
  READDATA:
  
  ;STOP
  IF HEADERS[0] NE '' THEN BEGIN
    nh=N_ELEMENTS(headers);-1
  ;headers=headers[0:nh-1]
  ENDIF ELSE BEGIN
    NH=0ull
  ENDELSE
  
  datatmp=tmpstr
  datatmp=''
  NS=N_ELEMENTS(STRSPLIT(TMPSTR,/EXTRACT))
  
  ;readf,fid,datatmp
  ;datatmp=strsplit(datatmp,/extract)
  ;ns=n_elements(strsplit(tmpstr,/extract))
  ;;datatmp=datatmp[0:n_elements(datatmp)-2]
  ;nl=0
  NL=LI
  nl=1
  WHILE NOT EOF(fid) DO BEGIN
    READF,fid,tmpstr
    IF nl EQ 0 THEN ns=N_ELEMENTS(STRSPLIT(tmpstr,/extract))
    nl=nl+ULONG64(1)
  ENDWHILE
  
  
  ENDIT:
  FREE_LUN,FID
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,SITE,FIRSTEPOCH,LASTEPOCH,XYZREF,NEUREF,DATA, HEADERS,NS,NL,NH
    ;FOR I=0, N_ELEMENTS(HEADERS)-1 DO PRINT, HEADERS(I)
    HELP,SLOPE_N, PSDECAY_N, OFFSET_N, ANNUAL_N, SEMIANNUAL_N,	$
      SLOPE_E, PSDECAY_E, OFFSET_E, ANNUAL_E, SEMIANNUAL_E,	$
      SLOPE_U, PSDECAY_U, OFFSET_U, ANNUAL_U, SEMIANNUAL_U, $
      NL,NS
  ;print,offset_n
  ;    PRINT, SLOPE_N,  OFFSET_N, ANNUAL_N, SEMIANNUAL_N,	$
  ;      SLOPE_E,  OFFSET_E, ANNUAL_E, SEMIANNUAL_E,	$
  ;      SLOPE_U,  OFFSET_U, ANNUAL_U, SEMIANNUAL_U
  ENDIF
  ;;
  FAILED=0
  RETURN
  
END
