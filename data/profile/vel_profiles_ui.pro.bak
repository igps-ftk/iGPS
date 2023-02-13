
PRO ON_CORRECT_OFFSET_UI_BTN_OK,ev
  id=WIDGET_INFO(ev.TOP,find_by_uname='TXT_IN')
  WIDGET_CONTROL,id,get_value=tmp
  path=STRTRIM(tmp[0],2)
  IF path EQ '' || FILE_TEST(path,/directory) NE 1 THEN BEGIN
    MSGBOX,'No input path, or the path does not exist!!',/error
    RETURN
  ENDIF
  
  id=WIDGET_INFO(ev.TOP,find_by_uname='TXT_DEF')
  WIDGET_CONTROL,id,get_value=tmp
  deffile=STRTRIM(tmp[0],2)
  IF deffile EQ '' || FILE_TEST(deffile,/regular) NE 1 THEN BEGIN
    MSGBOX,'No offset list file, or the file does not exist!!',/error
    RETURN
  ENDIF
  
  id=WIDGET_INFO(ev.TOP,find_by_uname='txt_szwin_left')
  WIDGET_CONTROL,id,get_value=tmp
  szwin_left=FIX(tmp[0])
  IF szwin_left LE 0 THEN BEGIN
    MSGBOX,'Left window width error!!',/error
    RETURN
  ENDIF
  id=WIDGET_INFO(ev.TOP,find_by_uname='txt_szwin_right')
  WIDGET_CONTROL,id,get_value=tmp
  szwin_right=FIX(tmp[0])
  IF szwin_right LE 0 THEN BEGIN
    MSGBOX,'Right window width error!!',/error
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
  OVERWRITE=WIDGET_INFO(ID,/BUTTON_SET)
  
  HELP, path, deffile, szwin_left, szwin_right, opath
  CORRECT_OFFSET, path, opath, deffile, szwin=[szwin_left, szwin_right], $
    overwrite=overwrite
  
  msgbox,'Done!',/info
END

;
;exit
PRO ON_CORRECT_OFFSET_UI_BTN_EXIT,ev
  WIDGET_CONTROL,ev.TOP,/destroy
END
;display help information
PRO ON_CORRECT_OFFSET_UI_BTN_HELP,ev
  dummy=DIALOG_MESSAGE(["Correct the offsets in GPS position time series.", $
    '  * Input/output time series are both in Yice@CEA *.dat format.',$
    '      e.g.',$
    '      1999.20410911      0.00     0.00     0.00      1.45     1.77     5.56     -0.03     0.07     0.03 KMIN',$
    '',$
    '  * Offset list is specified as iGPS Offset/Psdecay definition file (*.def)', $
    '      Specific width of averaging window can now be used. e.g.',$
    '       offset kmin 2005.00137000 N  ;--szwin_left=20 --szwin_right=',$
    '       offset kmin 2010.00137000 n  ;--SZWIN_RIGHT=5',$
    '      * the string following the semi-comma is comment.',$
    '',$
    "Note: use this program at your own risk!"],dialog_parent=ev.TOP,/info,$
    title='Correct Offsets')
END
;
;event manager
PRO CORRECT_OFFSET_UI_EVENT,event

END
;
;create GUI
PRO CORRECT_OFFSET_UI,group_leader=group_leader
  wtitle='Correct Offset in Time Series (for Yice)'
  IF N_ELEMENTS(group_leader) EQ 0 THEN BEGIN
    base=WIDGET_BASE(title=wtitle,/column,map=0,space=0,xpad=0,ypad=0,TAB_MODE = 1)
  ENDIF ELSE BEGIN
    base=WIDGET_BASE(title="iGPS: "+wtitle,/column,GROUP_LEADER=GROUP_LEADER, $
      map=0,space=0,xpad=0,ypad=0,TAB_MODE = 1)
  ENDELSE
  
  TXT_IN = CW_DIRFILE(BASE, TITLE = 'Input Path:', UNAME='TXT_IN', $
    XSIZE=120,FRAME=0,/align_right)
    
  LBL=WIDGET_LABEL(base,VALUE='Offset/Psdecay List File ( iGPS Offset/Psdecay Definition File Format *.def ):',/ALIGN_LEFT)
  TXT_DEF=CW_DIRFILE(base,TITLE='',XSIZE=120,$
    UNAME='TXT_DEF', $
    FILTER=[['*.def','*'],['iGPS Offset/Psdecay Definition File (*.def)','All files (*)']], $
    STYLE='FILE', SENSITIVE=1,/ALIGN_RIGHT)
    
  base_szwin=WIDGET_BASE(base,/row,frame=0,space=0,xpad=0,ypad=0,/ALIGN_left)
  lbl=WIDGET_LABEL(base_szwin,value='Default Window Width for Calculating Offset ( Left; Right, in days ):',/align_left)
  txt_szwin_left=WIDGET_TEXT(base_szwin,uname='txt_szwin_left',value='10',/editable,/ALL_EVENTS,/ALIGN_CENTER,xsize=5)
  txt_szwin_right=WIDGET_TEXT(base_szwin,uname='txt_szwin_right',value='10',/editable,/ALL_EVENTS,/ALIGN_CENTER,xsize=5)
  
  
  TXT_OUT = CW_DIRFILE(base, TITLE = "Output Path:", $
    UNAME='TXT_OUT', $
    XSIZE=120,/align_right)
    
  BASE_OUT_OPT=WIDGET_BASE(base,/NONEXCLUSIVE, $
    UNAME='BASE_OUT_OPT',SPACE=0,XPAD=0,YPAD=0,/ROW,FRAME=0, /align_center)
  CKB_OVERWRITE=WIDGET_BUTTON(BASE_OUT_OPT,VALUE='Overwrite Existing Output Files',TRACKING_EVENTS=1,UNAME='CKB_OVERWRITE')
  
  
  base_btn=WIDGET_BASE(base,/row,space=0,xpad=0,ypad=0,/align_center)
  btn_ok=WIDGET_BUTTON(base_btn,value=' O K ',event_pro='on_correct_offset_ui_btn_ok',/align_left)
  btn_exit=WIDGET_BUTTON(base_btn,value='Quit',event_pro='on_correct_offset_ui_btn_exit',/align_center)
  btn_help=WIDGET_BUTTON(base_btn,value='About',event_pro='on_correct_offset_ui_btn_help',/align_center)
  WIDGET_CONTROL,base,/realize
  CENTERBASE,base
  WIDGET_CONTROL,base,map=1
  XMANAGER, 'correct_offset_ui', base, /no_block
END
