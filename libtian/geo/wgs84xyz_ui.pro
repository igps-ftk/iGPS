;GUI for convert coordinates between WGS84 and geocentric XYZ
;
;update d,m,s when dd changed.
pro wgs84xyz_update_dms,ev
  id=widget_info(ev.top,find_by_uname='txt_lat')
  widget_control,id,get_value=lat
  lat=double(lat[0])
  id=widget_info(ev.top,find_by_uname='txt_lon')
  widget_control,id,get_value=lon
  lon=double(lon[0])
  id=widget_info(ev.top,find_by_uname='txt_h')
  widget_control,id,get_value=h
  h=double(h[0])
  ;print,lat,lon,h
  dlat=fix(lat)
  mlat=fix((lat-dlat)*60.d)
  slat=(lat-dlat-mlat/60.d)*3600d
  ;print,dlat,mlat,slat
  dlon=fix(lon)
  mlon=fix((lon-dlon)*60.d)
  slon=(lon-dlon-mlon/60.d)*3600d
  ;print,dlon,mlon,slon
  id=widget_info(ev.top,find_by_uname='txt_dlat')
  widget_control,id,set_value=strtrim(dlat,2)
  id=widget_info(ev.top,find_by_uname='txt_mlat')
  widget_control,id,set_value=strtrim(mlat,2)
  id=widget_info(ev.top,find_by_uname='txt_slat')
  widget_control,id,set_value=strtrim(string(slat,format='(F20.8)'),2)
  id=widget_info(ev.top,find_by_uname='txt_dlon')
  widget_control,id,set_value=strtrim(dlon,2)
  id=widget_info(ev.top,find_by_uname='txt_mlon')
  widget_control,id,set_value=strtrim(mlon,2)
  id=widget_info(ev.top,find_by_uname='txt_slon')
  widget_control,id,set_value=strtrim(string(slon,format='(F20.8)'),2)
  
end
;
;update dd when dms changed.
pro wgs84xyz_update_dd,ev
  id=widget_info(ev.top,find_by_uname='txt_dlat')
  widget_control,id,get_value=dlat
  dlat=double(dlat[0])
  id=widget_info(ev.top,find_by_uname='txt_mlat')
  widget_control,id,get_value=mlat
  mlat=double(mlat[0])
  id=widget_info(ev.top,find_by_uname='txt_slat')
  widget_control,id,get_value=slat
  slat=double(slat[0])
  id=widget_info(ev.top,find_by_uname='txt_dlon')
  widget_control,id,get_value=dlon
  dlon=double(dlon[0])
  id=widget_info(ev.top,find_by_uname='txt_mlon')
  widget_control,id,get_value=mlon
  mlon=double(mlon[0])
  id=widget_info(ev.top,find_by_uname='txt_slon')
  widget_control,id,get_value=slon
  slon=double(slon[0])
  ;
  lat=dlat+mlat/60.+slat/3600d
  lon=dlon+mlon/60.+slon/3600d
  ;
  id=widget_info(ev.top,find_by_uname='txt_lat')
  widget_control,id,set_value=strtrim(string(lat,format='(F20.8)'),2)
  id=widget_info(ev.top,find_by_uname='txt_lon')
  widget_control,id,set_value=strtrim(string(lon,format='(F20.8)'),2)
  
end
;
;wgs84 -> xyz
pro on_wgs84xyz_ui_btn_wgs842xyz,ev
  id=widget_info(ev.top,find_by_uname='txt_lat')
  widget_control,id,get_value=lat
  lat=double(lat[0])
  id=widget_info(ev.top,find_by_uname='txt_lon')
  widget_control,id,get_value=lon
  lon=double(lon[0])
  id=widget_info(ev.top,find_by_uname='txt_h')
  widget_control,id,get_value=h
  h=double(h[0])
  ;print,lat,lon,h
  geoxyz,lat,lon,h,x,y,z,1
  ;print,x,y,z
  id=widget_info(ev.top,find_by_uname='txt_x')
  widget_control,id,set_value=strtrim(string(x,format='(F20.8)'),2)
  id=widget_info(ev.top,find_by_uname='txt_y')
  widget_control,id,set_value=strtrim(string(y,format='(F20.8)'),2)
  id=widget_info(ev.top,find_by_uname='txt_z')
  widget_control,id,set_value=strtrim(string(z,format='(F20.8)'),2)
end
; xyz -> wgs84
pro on_wgs84xyz_ui_btn_xyz2wgs84,ev
  id=widget_info(ev.top,find_by_uname='txt_x')
  widget_control,id,get_value=x
  x=double(x[0])
  id=widget_info(ev.top,find_by_uname='txt_y')
  widget_control,id,get_value=y
  y=double(y[0])
  id=widget_info(ev.top,find_by_uname='txt_z')
  widget_control,id,get_value=z
  z=double(z[0])
  geoxyz,lat,lon,h,x,y,z,2
  id=widget_info(ev.top,find_by_uname='txt_lat')
  widget_control,id,set_value=strtrim(string(lat,format='(F20.8)'),2)
  id=widget_info(ev.top,find_by_uname='txt_lon')
  widget_control,id,set_value=strtrim(string(lon,format='(F20.8)'),2)
  id=widget_info(ev.top,find_by_uname='txt_h')
  widget_control,id,set_value=strtrim(string(h,format='(F20.8)'),2)
  ;
  wgs84xyz_update_dms,ev
end
;
;exit
pro on_wgs84xyz_ui_btn_exit,ev
  widget_control,ev.top,/destroy
end
;display help information
pro on_wgs84xyz_ui_btn_help,ev
  dummy=dialog_message(["Convert between XYZ and WGS84 for a try.", $
    "* Use this program at your own risk! *"],dialog_parent=ev.top,/info,$
    title='WGS84<->XYZ')
end
;
;event manager
pro wgs84xyz_ui_event,event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
    widget_info(Event.id, /tree_root) : event.id)
    
    
  wWidget =  Event.top
  
  case wTarget of
  
    ;when dd changed, update dms
    Widget_Info(wWidget, FIND_BY_UNAME='txt_lat'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_CH' )then $
        wgs84xyz_update_dms, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_STR' )then $
        wgs84xyz_update_dms, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_DEL' )then $
        wgs84xyz_update_dms, Event
    end
    
    Widget_Info(wWidget, FIND_BY_UNAME='txt_lon'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_CH' )then $
        wgs84xyz_update_dms, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_STR' )then $
        wgs84xyz_update_dms, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_DEL' )then $
        wgs84xyz_update_dms, Event
    end
    
    ;when dms changed, update dd
    Widget_Info(wWidget, FIND_BY_UNAME='txt_dlat'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_CH' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_STR' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_DEL' )then $
        wgs84xyz_update_dd, Event
    end
    
    Widget_Info(wWidget, FIND_BY_UNAME='txt_mlat'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_CH' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_STR' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_DEL' )then $
        wgs84xyz_update_dd, Event
    end
    
    Widget_Info(wWidget, FIND_BY_UNAME='txt_slat'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_CH' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_STR' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_DEL' )then $
        wgs84xyz_update_dd, Event
    end
    
    Widget_Info(wWidget, FIND_BY_UNAME='txt_dlon'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_CH' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_STR' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_DEL' )then $
        wgs84xyz_update_dd, Event
    end
    
    Widget_Info(wWidget, FIND_BY_UNAME='txt_mlon'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_CH' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_STR' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_DEL' )then $
        wgs84xyz_update_dd, Event
    end
    
    
    Widget_Info(wWidget, FIND_BY_UNAME='txt_slon'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_CH' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_STR' )then $
        wgs84xyz_update_dd, Event
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_DEL' )then $
        wgs84xyz_update_dd, Event
    end
    
    
    else:
  endcase
end
;
;create GUI
pro wgs84xyz_ui,group_leader=group_leader
  if n_elements(group_leader) eq 0 then begin
    base=widget_base(title="WGS84 - XYZ",/column,map=0,space=0,xpad=0,ypad=0,TAB_MODE = 1)
  endif else begin
    base=widget_base(title="iGPS: WGS84 - XYZ",/column,GROUP_LEADER=GROUP_LEADER, $
      map=0,space=0,xpad=0,ypad=0,TAB_MODE = 1)
  endelse
  ;wgs84 coordiantes
  ;in degrees
  lbl=widget_label(base,value=' WGS-84 Geographic Coodinates:',/align_left)
  base_wgs84=widget_base(base,/column,frame=0,space=0,xpad=0,ypad=0,/ALIGN_CENTER)
  lbl=widget_label(base_wgs84,value='(in DD)',/ALIGN_CENTER)
  base_dd=widget_base(base_wgs84,column=2,uname='base_dd',space=0,xpad=0,ypad=0,/ALIGN_CENTER)
  lbl_lat=widget_label(base_dd,value='Latitude:',/ALIGN_CENTER)
  txt_lat=widget_text(base_dd,uname='txt_lat',value='38.20',/editable,/ALL_EVENTS,/ALIGN_CENTER)
  lbl_lon=widget_label(base_dd,value='Longitude:',/ALIGN_CENTER)
  txt_lon=widget_text(base_dd,uname='txt_lon',value='115.2',/editable,/ALL_EVENTS,/ALIGN_CENTER)
  ;in d,m,s
  lbl=widget_label(base_wgs84,value='(in DMS)',/ALIGN_CENTER)
  base_dms=widget_base(base_wgs84,row=2,uname='base_dms',space=0,xpad=1,ypad=0)
  lbl_lat=widget_label(base_dms,value='Latitude:',scr_xsize=70,/align_right)
  txt_dlat=widget_text(base_dms,uname='txt_dlat',value='38',/editable,/ALL_EVENTS)
  txt_mlat=widget_text(base_dms,uname='txt_mlat',value='12',/editable,/ALL_EVENTS)
  txt_slat=widget_text(base_dms,uname='txt_slat',value='0',/editable,/ALL_EVENTS)
  lbl_lon=widget_label(base_dms,value='Longitude:',scr_xsize=70,/align_right)
  txt_dlon=widget_text(base_dms,uname='txt_dlon',value='115',/editable,/ALL_EVENTS)
  txt_mlon=widget_text(base_dms,uname='txt_mlon',value='2',/editable,/ALL_EVENTS)
  txt_slon=widget_text(base_dms,uname='txt_slon',value='0',/editable,/ALL_EVENTS)
  ;height
  base_h=widget_base(base_wgs84,/row,space=0,xpad=0,ypad=0)
  lbl_h=widget_label(base_h,value='Height:',scr_xsize=60,/align_right)
  txt_h=widget_text(base_h,uname='txt_h',value='85.2',/editable)
  lbl=widget_label(base_h,value=' meters')
  ;x,y,z values
  lbl=widget_label(base,value='',/align_left)
  lbl=widget_label(base,value=' ECEF XYZ Coodinates (in meters):',/align_left)
  base_xyz=widget_base(base,row=3,frame=0,space=0,xpad=0,ypad=0,/align_center)
  lbl_x=widget_label(base_xyz,value='X:')
  txt_x=widget_text(base_xyz,uname='txt_x',value='-1932937.25975',/editable,/align_right)
  lbl_y=widget_label(base_xyz,value='Y:')
  txt_y=widget_text(base_xyz,uname='txt_y',value='4656319.01278',/editable)
  lbl_z=widget_label(base_xyz,value='Z:')
  txt_z=widget_text(base_xyz,uname='txt_z',value='3894971.98220',/editable)
  lbl=widget_label(base,value='',/align_left)
  base_btn=widget_base(base,/row,space=0,xpad=0,ypad=0,/align_center)
  btn_w2x=widget_button(base_btn,value='WGS84 -> XYZ',event_pro='on_wgs84xyz_ui_btn_wgs842xyz',/align_left)
  btn_x2w=widget_button(base_btn,value='WGS84 <- XYZ',event_pro='on_wgs84xyz_ui_btn_xyz2wgs84',/align_right)
  btn_exit=widget_button(base_btn,value='Quit',event_pro='on_wgs84xyz_ui_btn_exit',/align_center)
  btn_help=widget_button(base_btn,value='About',event_pro='on_wgs84xyz_ui_btn_help',/align_center)
  widget_control,base,/realize
  centerbase,base
  widget_control,base,map=1
  xmanager, 'wgs84xyz_ui', base, /no_block
end
