PRO ON_IGPS_VEL_PROFILES_DP_IN_TYPE, EV
  ;HELP, ev,/st
  
  CASE ev.INDEX OF
    0: BEGIN
      input_fmt=3
      t=['3: XYZ (*.xyz)',  $
        ' X          Y         Z',  $
        ' 89.892319  30.813284  2.033756']
    END
    1: BEGIN
      input_fmt=4
      t=['4: XYZe (*.xyze)',  $
        'X          Y         Z     Z_sig',  $
        '89.892319  30.813284  2.033756      2.838231']
    END
    2: BEGIN
      input_fmt=81
      t=['81: GMT psvelo (*.psvelo)',  $
        'lon lat Ve Vn Se Sn Cen Site',  $
    'e.g.,',  $
    '  81.71   28.66   6.7   32.4   0.53   0.53   -0.0108 BMCL',  $
    '  85.31   28.21   8.4   29.0   0.52   0.52    0.0518 CHLM',  $
    '  86.90   30.45  12.6   21.6   0.53   0.52    0.0427 CUOM',  $
    '  ...']
    END
    3: BEGIN
      input_fmt=82
      t=['82: Variant of psvelo (*.psvelo2)',  $
        'Site Long  Lat Vn  Sn  Ve  Se  Cne',  $
        'H095  102.232 27.8745 -10.8 1.3 10.5  1.3 0.0064']
    END
    4: BEGIN
      input_fmt=83
      t=['83: GMT psvelo with multi-text (*.psvelo3)',  $
        '  Long.    Lat.      Ve       Vn     Se     Sn    Cne  Sta.      Source',  $
        ' 74.336  39.842   0.170   13.790  0.560  0.503  0.000  I089  This_study',  $
        ' 78.680  29.848  10.840   32.656  1.550  1.450 -0.002  LAN2  Kreemer et al. [2014] ']
    END
    5: BEGIN
      input_fmt=84
      t=['84: GMT psvelo with Up (*.psveloU)',  $
        'read psvelo+up velocity field',  $
        'lon lat Ve Vn Se Sn Cen Site Vu']
    END
    6: BEGIN
      input_fmt=85
      t=['84: GMT psvelo with Up (*.psveloU)',  $
        'read psvelo+up velocity field',  $
        'lon lat Ve Vn Se Sn Cen Site Vu Su']
    END
    7: BEGIN
      input_fmt=101
      t=['101',  $
        '* Site   Longitude  Latitude   Ve   dVe    Vn   dVn   Cen      Vu  dVu  (mm/yr)',  $
        '1375_GPS  105.8150   33.3400  -0.47 0.80  -1.66 0.70 -0.043   0.00 9.00      ']
    END
    8: BEGIN
      input_fmt=102
      t=['102: BEGIN',  $
        '   long      lat       Ve      dVe       Vn      dVn       Vu      dVn    Tau_h   Tau_v',  $
        '   86.000   26.000  10.5821   0.9956  22.6099   0.8203   0.0000   7.4123  54.0000  54.0000']
    END
    9: BEGIN
      input_fmt=111
      t=['111: BEGIN',  $
        '   long      lat       Ve      dVe       Vn      dVn       Vu      dVn     Cen      Ceu      Cnu',  $
        '   87.650   39.400  -9.3569   0.4007  -3.4889   0.4172   0.7089   3.8793  -0.0002   0.0328   0.0066']
    END
    10: BEGIN
      input_fmt=112
      t=['112: CMM4 Shen (*.cmm4)',  $
        ' site lat lon Ve dVe Vn dVn Cen n_epoch time_span epoch_avg [other]',  $
        'CHAF_GPS 34.3006 -119.3310 -29.13 0.36 28.23 0.32 0.020 5 8.6 1991.5 I034']
    END
    11: BEGIN
      input_fmt=113
      t=['113: *.cmm4u',  $
        '0003_GPS  33.7504 -117.4563   2.36 7.49  0.022 -0.026    2  3.2 1994.1']
    END
    12: BEGIN
      input_fmt=121
      t=['121: *.qmap',  $
        '*Station   Longitude   Latitude Ve_init Ve_incr    Ve     dVe   Vn_init Vn_incr    Vn     dVn   Cen',  $
        ' ARTU_GPS   58.5583   56.4278     0.0     0.0    24.9     0.0     0.0     0.0     6.1     0.0   0.0000   ']
    END
    13: BEGIN
      input_fmt=131
      t=['131: *.vel',  $
        '*   Long.       Lat.         E & N Rate      E & N Adj.      E & N +-   RHO        H Rate   H adj.    +-  SITE',  $
        '*  (deg)      (deg)           (mm/yr)       (mm/yr)       (mm/yr)                 (mm/yr)',  $
        '   90.68400   27.18300     1.41   12.84    1.41   12.84    0.90    0.80 -0.120     -0.00   -0.00    3.00 ZHEM_GPS']
    END
  ENDCASE
  
  id=WIDGET_INFO(ev.TOP, find_by_uname='LBL_TXT_IN')
  WIDGET_CONTROL, id, set_value=t
  id=WIDGET_INFO(ev.TOP, find_by_uname='DP_IN_TYPE')
  WIDGET_CONTROL, id, set_uvalue=input_fmt
END



PRO ON_IGPS_LBL_TXT_FA_TRACK, EV
  LBL_ID = WIDGET_INFO(EV.TOP, FIND_BY_UNAME='LBL_STATUS')
  CASE EV.ENTER OF
    0: BEGIN
      WIDGET_CONTROL,LBL_ID,SET_VALUE='Ready'
    END
    1: BEGIN
      WIDGET_CONTROL,LBL_ID,SET_VALUE='fault trace file in GMT psxy format'
    END
  ENDCASE
END

PRO ON_IGPS_PROFILES_INIT, WWIDGET

  ID = WIDGET_INFO(WWIDGET, FIND_BY_UNAME='RAD_PROFILE_SRC_PROGRAM_AUTO')
  WIDGET_CONTROL, ID, /SET_BUTTON
  
  ID = WIDGET_INFO(WWIDGET, FIND_BY_UNAME='RAD_PROFILE_STRIKE_MEAN')
  WIDGET_CONTROL, ID, /SET_BUTTON
  
  ID = WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CKB_OVERWRITE')
  WIDGET_CONTROL, ID, /SET_BUTTON
  
  ID = WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DP_IN_TYPE')
  WIDGET_CONTROL,id,SET_DROPLIST_SELECT=2
  WIDGET_CONTROL,id,SET_uvalue=81
;help,id
  
END

PRO ON_IGPS_RAD_PROFILE_SRC_USER_INPUT, EV
  id=WIDGET_INFO(ev.TOP,find_by_uname='BASE_PROFILE_SRC_USER')
  WIDGET_CONTROL, id, sensitive=1
  id=WIDGET_INFO(ev.TOP,find_by_uname='B_OPT_LENGTH')
  WIDGET_CONTROL, id, sensitive=0 
  id=WIDGET_INFO(ev.TOP,find_by_uname='B_OPT_WIDTH')
  WIDGET_CONTROL, id, sensitive=0 
  id=WIDGET_INFO(ev.TOP,find_by_uname='B_OPT_STRIKE')
  WIDGET_CONTROL, id, sensitive=0 
  help,id
END

PRO ON_IGPS_RAD_PROFILE_SRC_PROGRAM_AUTO, EV
  id=WIDGET_INFO(ev.TOP,find_by_uname='BASE_PROFILE_SRC_USER')
  WIDGET_CONTROL, id, sensitive=0
  id=WIDGET_INFO(ev.TOP,find_by_uname='B_OPT_LENGTH')
  WIDGET_CONTROL, id, sensitive=1 
  id=WIDGET_INFO(ev.TOP,find_by_uname='B_OPT_WIDTH')
  WIDGET_CONTROL, id, sensitive=1 
  id=WIDGET_INFO(ev.TOP,find_by_uname='B_OPT_STRIKE')
  WIDGET_CONTROL, id, sensitive=1 
END

PRO ON_VEL_PROFILES_UI_BTN_OK,ev
  ;INPUT GPS VELOCITY
  id=WIDGET_INFO(ev.TOP,find_by_uname='TXT_IN')
  WIDGET_CONTROL,id,get_value=tmp
  file_gps=STRTRIM(tmp[0],2)
  IF file_gps EQ '' || FILE_TEST(file_gps,/regular) NE 1 THEN BEGIN
    MSGBOX,'No input file provided, or the file does not exist!!',/error
    RETURN
  ENDIF
  
  ID = WIDGET_INFO(ev.top, FIND_BY_UNAME='DP_IN_TYPE')
  WIDGET_CONTROL,id,gET_uvalue=input_fmt
  help, input_fmt
  ;stop
  
  
  ;FAULT LINE
  id=WIDGET_INFO(ev.TOP,find_by_uname='TXT_FA')
  WIDGET_CONTROL,id,get_value=tmp
  file_fault=STRTRIM(tmp[0],2)
  IF file_fault EQ '' || FILE_TEST(file_fault,/regular) NE 1 THEN BEGIN
    MSGBOX,'No fault file provided, or the file does not exist!!',/error
    RETURN
  ENDIF
  
  
  ;PROFILE OPTION
  ;
  ID = WIDGET_INFO(ev.TOP, FIND_BY_UNAME='RAD_PROFILE_SRC_PROGRAM_AUTO')
  rad_profile_src_program_auto=WIDGET_INFO( ID, /BUTTON_set)
  ;HELP, rad_profile_src_program_auto
  
  IF rad_profile_src_program_auto EQ 1 THEN BEGIN
    id=WIDGET_INFO(ev.TOP,find_by_uname='txt_profile_length')
    WIDGET_CONTROL,id,get_value=tmp
    txt_profile_length=DOUBLE(tmp[0])
    IF txt_profile_length LE 0 THEN BEGIN
      MSGBOX,'Profile length error!!',/error
      RETURN
    ENDIF
    id=WIDGET_INFO(ev.TOP,find_by_uname='txt_profile_width')
    WIDGET_CONTROL,id,get_value=tmp
    txt_profile_width=DOUBLE(tmp[0])
    IF txt_profile_width LE 0 THEN BEGIN
      MSGBOX,'Profile width error!!',/error
      RETURN
    ENDIF
    
    ID = WIDGET_INFO(ev.TOP, FIND_BY_UNAME='RAD_PROFILE_STRIKE_VARYING')
    is_RAD_PROFILE_STRIKE_VARYING=WIDGET_INFO( ID, /BUTTON_set)
    ID = WIDGET_INFO(ev.TOP, FIND_BY_UNAME='RAD_PROFILE_STRIKE_MEAN')
    is_RAD_PROFILE_STRIKE_MEAN=WIDGET_INFO( ID, /BUTTON_set)
    ID = WIDGET_INFO(ev.TOP, FIND_BY_UNAME='RAD_PROFILE_STRIKE_TWO')
    is_RAD_PROFILE_STRIKE_TWO=WIDGET_INFO( ID, /BUTTON_set)
    ;HELP, is_RAD_PROFILE_STRIKE_VARYING,is_RAD_PROFILE_STRIKE_MEAN,is_RAD_PROFILE_STRIKE_TWO
    ;STOP
    IF is_RAD_PROFILE_STRIKE_VARYING EQ 1 THEN BEGIN
      auto_strike=1
    ENDIF
    IF is_RAD_PROFILE_STRIKE_MEAN EQ 1 THEN BEGIN
      auto_strike=2
    ENDIF
    IF is_RAD_PROFILE_STRIKE_TWO EQ 1 THEN BEGIN
      auto_strike=3
    ENDIF
    
  ENDIF ELSE BEGIN
  
    id=WIDGET_INFO(ev.TOP,find_by_uname='TXT_PF')
    WIDGET_CONTROL,id,get_value=tmp
    file_profile=STRTRIM(tmp[0],2)
    IF file_profile EQ '' || FILE_TEST(file_profile,/regular) NE 1 THEN BEGIN
      MSGBOX,'No profile file provided, or the file does not exist!!',/error
      RETURN
    ENDIF
    
  ENDELSE
  
  
  id=WIDGET_INFO(ev.TOP,find_by_uname='txt_search_radius')
  WIDGET_CONTROL,id,get_value=tmp
  txt_search_radius=DOUBLE(tmp[0])
  IF txt_search_radius LE 0 THEN BEGIN
    MSGBOX,'Station search radius width error!!',/error
    RETURN
  ENDIF
  
  
  id=WIDGET_INFO(ev.TOP,find_by_uname='TXT_OUT')
  WIDGET_CONTROL,id,get_value=tmp
  opath=STRTRIM(tmp[0],2)
  IF opath EQ '' || FILE_TEST(opath,/directory) NE 1 THEN BEGIN
    MSGBOX,'No output path, or the path does not exist!!',/error
    RETURN
  ENDIF
  
  
  ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='CKB_OVERWRITE')
  is_OVERWRITE=WIDGET_INFO(ID,/BUTTON_SET)
  
  
  HELP, file_gps, file_fault, file_profile,txt_profile_length, txt_profile_width, txt_search_radius, opath, is_overwrite
  
  
  LBl_ID=WIDGET_INFO(EV.TOP,FIND_BY_UNAME='LBL_STATUS')
  
  IF rad_profile_src_program_auto EQ 1 THEN BEGIN
  
    VEL_PROFILE_CREATE, file_gps, $
      opath, $
      ffile=file_fault,  $
      inputfmt=input_fmt, $
      auto_strike=auto_strike,  $
      spacing_profile=txt_profile_width,  $
      length_profile=txt_profile_length,  $
      search_radius=txt_search_radius,  $
      LBl_ID=LBl_ID
      
  ENDIF ELSE BEGIN
  
    ;stop
    VEL_PROFILE_CREATE, file_gps, $
      pfile=file_profile,  $
      opath, $
      ffile=file_fault,  $
      inputfmt=input_fmt , $
      search_radius=txt_search_radius,  $
      LBl_ID=LBl_ID
      
      
  ENDELSE
  
  
;MSGBOX,'Done!',/info
END

;
;exit. NO warning!!
PRO ON_VEL_PROFILES_UI_BTN_EXIT,ev
  WIDGET_CONTROL,ev.TOP,/destroy
END

;display help information
PRO ON_VEL_PROFILES_UI_BTN_HELP,ev
  dummy=DIALOG_MESSAGE(['Inputs:',$
    '  1). GPS horizontal velocity file in GMT psvelo format;',$
  '  2). Fault line file in GMT psxy format;',  $
  '      Only ONE fault segment allowed!',  $
    '  3). Profiles parameters (length, width, orientations);', $
  '  4). Output directory.',  $
    '', $
    ""],dialog_parent=ev.TOP,/info,$
    title='GPS Velocity Profiles Creator')
END
;
;event manager
PRO VEL_PROFILES_UI_EVENT,ev
  WTARGET = (WIDGET_INFO(EV.ID,/NAME) EQ 'TREE' ?  $
    WIDGET_INFO(EV.ID, /TREE_ROOT) : EV.ID)
    
  WWIDGET =  EV.TOP
  
  ;  HELP, WTARGET,WIDGET_INFO(WWIDGET, FIND_BY_UNAME='LBL_TXT_FA')
  ;  HELP,TAG_NAMES(EV, /STRUCTURE_NAME)
  
  CASE WTARGET OF
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='LBL_TXT_FA'): BEGIN
      ;HELP,TAG_NAMES(EV, /STRUCTURE_NAME)
      IF (TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING') THEN BEGIN
        ON_IGPS_LBL_TXT_FA_TRACK, EV
      ENDIF
    END
    
    
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='RAD_PROFILE_SRC_USER_INPUT'): BEGIN
      IF (TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_BUTTON') THEN BEGIN
        ON_IGPS_RAD_PROFILE_SRC_USER_INPUT, EV
      ENDIF
    END
    
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='RAD_PROFILE_SRC_PROGRAM_AUTO'): BEGIN
      IF (TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_BUTTON') THEN BEGIN
        ON_IGPS_RAD_PROFILE_SRC_PROGRAM_AUTO, EV
      ENDIF
    END
    
    
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='RAD_PROFILE_STRIKE_VARYING'): BEGIN
      IF (TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_BUTTON') THEN BEGIN
        ;ON_IGPS_RAD_PROFILE_STRIKE_VARYING, EV
      ENDIF
    END
    
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='RAD_PROFILE_STRIKE_MEAN'): BEGIN
      IF (TAG_NAMES(EV, /STRUCTURE_NAME) EQ 'WIDGET_BUTTON') THEN BEGIN
        ;ON_IGPS_RAD_PROFILE_STRIKE_MEAN, EV
      ENDIF
    END
    
    
    ELSE:
  ENDCASE
  
END
;
;create GUI
PRO VEL_PROFILES_UI,group_leader=group_leader
  wtitle='Create GPS Velocity Profiles'
  IF N_ELEMENTS(group_leader) EQ 0 THEN BEGIN
    base=WIDGET_BASE(title=wtitle,/column,map=0,space=1,xpad=1,ypad=1,NOTIFY_REALIZE='ON_iGPS_PROFILES_INIT',TAB_MODE = 1)
  ENDIF ELSE BEGIN
    base=WIDGET_BASE(title="iGPS: "+wtitle,/column,GROUP_LEADER=GROUP_LEADER, $
      map=0,space=1,xpad=1,ypad=1,NOTIFY_REALIZE='ON_iGPS_PROFILES_INIT',TAB_MODE = 1)
  ENDELSE
  
  ;frame1
  BASE_1=WIDGET_BASE(BASE,FRAME=0,space=1,xpad=1,ypad=1,/column)
  
  ;LBL=WIDGET_LABEL(base_1,VALUE='(1) Input GPS Velocity File:',/ALIGN_left ) ;,/SUNKEN_FRAME
  VALUE=['InSAR LOS (*.xyz)', $ ;3
    'InSAR LOS+Uncertainty (*.xyze)', $  ;4
    'GMT psvelo (*.psvelo)', $  ;81
    '(*.psvelo2)', $  ;82
    '(*.psvelo3)', $  ;83
    'psvelo+u (*.psvelou)', $ ;84
    'psvelo+u+sig_u (*.psvelou)', $ ;85
    'psvelo2+u (*.psvelo2u)', $ ;101
    'llenu',  $ ;102
    'llenuc', $ ;103
    'CMM4 (*.cmm4)', $  ;112
    'CMM4 Up (*.cmm4u)', $  ;113
    'QOCA Map (*.qmap)', $  ;121
    'GAMIT/GLOBK Velocity (*.vel)' ] ;131 (13 columns)
  DP_IN_TYPE = WIDGET_DROPLIST( base_1 ,  $
    VALUE=VALUE,  $
    UNAME='DP_IN_TYPE', TITLE='(1) Input Velocity File:', $
    EVENT_PRO='ON_IGPS_VEL_PROFILES_DP_IN_TYPE')
    
  TXT_IN = CW_DIRFILE(BASE_1, TITLE = '', UNAME='TXT_IN', $
    value=!igps_root+PATH_SEP()+'example'+PATH_SEP()+'profile'+PATH_SEP()+'wang.min.jgr2020.Table.S4.psvelo', $
    FILTER=[['*.psvelo','*.txt','*'],['GMT psvelo input format (*.psvelo)','Text Files (*.txt)', 'All files (*)']], $
    XSIZE=120,FRAME=0,/ALIGN_RIGHT,STYLE='FILE')
    
    
  strs=['GPS velocity file in GMT psvelo input format :', $
    '  longitude   latitude   velocity_east   velocity_north   uncertainity_east   uncertainity_north   correlation_en   site_name ',  $
    'e.g.,',  $
    '  81.71   28.66   6.7   32.4   0.53   0.53   -0.0108 BMCL',  $
    '  85.31   28.21   8.4   29.0   0.52   0.52    0.0518 CHLM',  $
    '  86.90   30.45  12.6   21.6   0.53   0.52    0.0427 CUOM',  $
    '  ...']
  LBL=WIDGET_TEXT(base_1,VALUE=strs,  $
    ysize=7, xsize=120,  $
    /TRACKING_EVENTS, $
    UNAME='LBL_TXT_IN', $
    /ALIGN_CENTER)
    
    
  ;frame 2
  BASE_2=WIDGET_BASE(BASE,FRAME=0,space=1,xpad=1,ypad=1,/column)
  
  LBL=WIDGET_LABEL(base_2,VALUE='(2) Fault File:',/ALIGN_left)
  TXT_FA=CW_DIRFILE(base_2,TITLE='',XSIZE=120,$
    UNAME='TXT_FA', $
    VALUE=!igps_root+PATH_SEP()+'example'+PATH_SEP()+'profile'+PATH_SEP()+'fa_redriver.psxy',  $
    FILTER=[['*.psxy','*.txt','*'],['GMT psxy input format (*.psxy)','Text Files (*.txt)', 'All files (*)']], $
    STYLE='FILE', SENSITIVE=1,/ALIGN_RIGHT)
    
    
  strs=['GMT psxy input format :', $
    '  longitude1   latitude1  ',  $
    'e.g.,',  $
    '> -L"Kunlun_Fault"', $
    '90.4862004572  36.2108493543',  $
    '102.690253055  33.8288732363',  $
    '...']
  LBL=WIDGET_TEXT(base_2,VALUE=strs,  $
    ysize=9, xsize=120,  $
    /TRACKING_EVENTS, $
    UNAME='LBL_TXT_FA', $
    /ALIGN_CENTER)
    
    
  ;frame 3
  BASE_3=WIDGET_BASE(BASE,FRAME=0,space=1,xpad=1,ypad=1,/column,UNAME='BASE_3')
  lbl=WIDGET_LABEL(base_3,value='(3) Parameters for Profiles:',/align_left)
  
  BASE_RAD_PROFILE_SRC=WIDGET_BASE(base_3,/ROW,SPACE=0, $
    YPAD=0,XPAD=0,FRAME=0,/EXCLUSIVE,/ALIGN_CENTER,/BASE_ALIGN_CENTER)
  RAD_PROFILE_SRC_USER_INPUT=WIDGET_BUTTON(BASE_RAD_PROFILE_SRC,UNAME='RAD_PROFILE_SRC_USER_INPUT', $
    VALUE='User Input',/ALIGN_LEFT)
  RAD_PROFILE_SRC_PROGRAM_AUTO=WIDGET_BUTTON(BASE_RAD_PROFILE_SRC,UNAME='RAD_PROFILE_SRC_PROGRAM_AUTO', $
    VALUE='Generate Profiles Automatically',/ALIGN_LEFT)
      
  BASE_PROFILE_SRC_USER=WIDGET_BASE(BASE_3,FRAME=0,space=1,xpad=1,ypad=1,/column,uname='BASE_PROFILE_SRC_USER',sensitive=0)
  TXT_PF=CW_DIRFILE(BASE_PROFILE_SRC_USER,TITLE='',XSIZE=120,$
    UNAME='TXT_PF', $
    VALUE=!igps_root+PATH_SEP()+'example'+PATH_SEP()+'profile'+PATH_SEP()+'pf_honghe1.psxy',  $
    FILTER=[['*.psxy','*.txt','*'],['GMT psxy input format (*.psxy)','Text Files (*.txt)', 'All files (*)']], $
    STYLE='FILE', SENSITIVE=1,/ALIGN_RIGHT)
    
    
  BASE_3b=WIDGET_BASE(BASE_3,FRAME=0,space=1,xpad=1,ypad=1,/row,/align_center,UNAME='BASE_3B') 
  BASE_PROFILE_SRC_PROG=WIDGET_BASE(BASE_3b,FRAME=0,space=1,xpad=1,ypad=1,/column,uname='BASE_PROFILE_SRC_PROG',sensitive=1)
  
  base_profile_length=WIDGET_BASE(uname='B_OPT_LENGTH',BASE_PROFILE_SRC_PROG,/row,frame=0,space=0,xpad=0,ypad=0,/ALIGN_right)
  lbl=WIDGET_LABEL(base_profile_length,value='Length of Profiles (in km):',/align_left)
  txt_profile_length=WIDGET_TEXT(base_profile_length,uname='txt_profile_length',value='200',/editable,/ALL_EVENTS,/ALIGN_CENTER,xsize=5)
  
  
  base_profile_width=WIDGET_BASE(uname='B_OPT_WIDTH',BASE_PROFILE_SRC_PROG,/row,frame=0,space=0,xpad=0,ypad=0,/ALIGN_right)
  lbl=WIDGET_LABEL(base_profile_width,value='Width of Profiles (in km):',/align_left)
  txt_profile_width=WIDGET_TEXT(base_profile_width,uname='txt_profile_width',value='50',/editable,/ALL_EVENTS,/ALIGN_CENTER,xsize=5)
 
    base_search_radius=WIDGET_BASE(uname='B_OPT_RADIUS',BASE_PROFILE_SRC_PROG,/row,frame=0,space=0,xpad=0,ypad=0,/ALIGN_right)
  lbl=WIDGET_LABEL(base_search_radius,value='Search Radius (in km):',/align_left)
  txt_search_radius=WIDGET_TEXT(base_search_radius,uname='txt_search_radius',value='100',/editable,/ALL_EVENTS,/ALIGN_CENTER,xsize=5)
  
  BASE_PROFILE_STRIKE=WIDGET_BASE(uname='B_OPT_STRIKE',BASE_3b,FRAME=0,space=0,xpad=0,ypad=0,/column,sensitive=1)
  
  BASE_profile_rot=WIDGET_BASE(BASE_PROFILE_STRIKE,/EXCLUSIVE, $
    UNAME='BASE_profile_rot',SPACE=0,XPAD=0,YPAD=0,/column,FRAME=0, /align_center)
  RAD_PROFILE_STRIKE_VARYING=WIDGET_BUTTON(BASE_profile_rot,VALUE='Rotate profiles along strikes of fault segments',TRACKING_EVENTS=1,UNAME='RAD_PROFILE_STRIKE_VARYING')
  RAD_PROFILE_STRIKE_MEAN=WIDGET_BUTTON(BASE_profile_rot,VALUE='Mean strike of all fault segments',TRACKING_EVENTS=1,UNAME='RAD_PROFILE_STRIKE_MEAN')
  RAD_PROFILE_STRIKE_TWO=WIDGET_BUTTON(BASE_profile_rot,VALUE='Line of First-Last Vertices',TRACKING_EVENTS=1,UNAME='RAD_PROFILE_STRIKE_TWO')
  ;RAD_PROFILE_STRIKE_USER=WIDGET_BUTTON(BASE_profile_rot,VALUE='User Input',TRACKING_EVENTS=1,UNAME='RAD_PROFILE_STRIKE_USER')
 

  
  ;frame 4
  BASE_4=WIDGET_BASE(BASE,FRAME=0,space=1,xpad=1,ypad=1,/column)
  lbl=WIDGET_LABEL(base_4,value='(4) Output Path:',/align_left)
  
  TXT_OUT = CW_DIRFILE(base_4, TITLE = "", $
    UNAME='TXT_OUT', $
    VALUE=!igps_root+PATH_SEP()+'example'+PATH_SEP()+'profile'+PATH_SEP()+'p_auto',  $
    XSIZE=120,/align_right)
    
  BASE_OUT_OPT=WIDGET_BASE(base_4,/NONEXCLUSIVE, $
    UNAME='BASE_OUT_OPT',SPACE=0,XPAD=0,YPAD=0,/ROW,FRAME=0, /align_center)
  CKB_OVERWRITE=WIDGET_BUTTON(BASE_OUT_OPT,VALUE='Overwrite Existing Output Files',TRACKING_EVENTS=1,UNAME='CKB_OVERWRITE', sensitive=0)
  
  
  base_btn=WIDGET_BASE(base,/row,space=0,xpad=0,ypad=0,/align_center)
  btn_ok=WIDGET_BUTTON(base_btn,value=' O K ',EVENT_PRO='ON_VEL_PROFILES_UI_BTN_OK',/ALIGN_LEFT)
  btn_exit=WIDGET_BUTTON(base_btn,value='Quit',EVENT_PRO='ON_VEL_PROFILES_UI_BTN_EXIT',/ALIGN_CENTER)
  btn_help=WIDGET_BUTTON(base_btn,value='About',EVENT_PRO='ON_VEL_PROFILES_UI_BTN_HELP',/ALIGN_CENTER)
  
  lbl_staus=WIDGET_LABEL(base,/align_center,value='Ready',uname='LBL_STATUS',/DYNAMIC_RESIZE )
  
  WIDGET_CONTROL,base,/realize
  CENTERBASE,base
  WIDGET_CONTROL,base,map=1
  XMANAGER, 'VEL_PROFILES_UI', base, /no_block
END
