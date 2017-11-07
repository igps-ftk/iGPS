;+
; Name:
;		CenterBase
;
; Purpose:
;		Position Widget window in the center part of the screen.
;
; Parameters:
;		base	-	Top-Level_Base ID
;
; Returns:
;		None.
;-
;+++
PRO CenterBase, base
	if n_params() lt 1 then begin
		base=widget_base(scr_xsize=200,scr_ysize=100)
		widget_control,base,/realize
	endif
	;
	;Hide the Base Widget
	widget_control, base, map=0
	;
	; Get screen size
	device, get_screen_size=sc_sz
	;
	; Get GUI's size
	ba_sz=widget_info(base,/geometry)
	;
	;Calculate the (Upper, Left) coordinates.
	x_off=(sc_sz(0)-ba_sz.scr_xsize)/2
	y_off=(sc_sz(1)-ba_sz.scr_ysize)/2
	;
	;Set the position and show the widget.
	widget_control, base, xoffset=x_off, yoffset=y_off
	widget_control, base, map=1
	;
END
;///
