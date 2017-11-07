c; $Id: //depot/idl/IDL_70/idldir/lib/julday.pro#1 $
c;
c; Copyright (c) 1988-2007, ITT Visual Information Solutions. All
c;       rights reserved. Unauthorized reproduction is prohibited.

c;+
c; NAME:
c;	JULDAY
c;
c; PURPOSE:
c;	Calculate the Julian Day Number for a given month, day, and year.
c;	This is the inverse of the library function CALDAT.
c;	See also caldat, the inverse of this function.
c;
c; CATEGORY:
c;	Misc.
c;
c; CALLING SEQUENCE:
c;	Result = JULDAY([[[[Month, Day, Year], Hour], Minute], Second])
c;
c; INPUTS:
c;	MONTH:	Number of the desired month (1 = January, ..., 12 = December).
c;
c;	DAY:	Number of day of the month.
c;
c;	YEAR:	Number of the desired year.Year parameters must be valid
c;               values from the civil calendar.  Years B.C.E. are represented
c;               as negative integers.  Years in the common era are represented
c;               as positive integers.  In particular, note that there is no
c;               year 0 in the civil calendar.  1 B.C.E. (-1) is followed by
c;               1 C.E. (1).
c;
c;	HOUR:	Number of the hour of the day.
c;
c;	MINUTE:	Number of the minute of the hour.
c;
c;	SECOND:	Number of the second of the minute.
c;
c;   Note: Month, Day, Year, Hour, Minute, and Second can all be arrays.
c;         The Result will have the same dimensions as the smallest array, or
c;         will be a scalar if all arguments are scalars.
c;
c; OPTIONAL INPUT PARAMETERS:
c;	Hour, Minute, Second = optional time of day.
c;
c; OUTPUTS:
c;	JULDAY returns the Julian Day Number (which begins at noon) of the
c;	specified calendar date.  If Hour, Minute, and Second are not specified,
c;	then the result will be a long integer, otherwise the result is a
c;	double precision floating point number.
c;
c; COMMON BLOCKS:
c;	None.
c;
c; SIDE EFFECTS:
c;	None.
c;
c; RESTRICTIONS:
c;	Accuracy using IEEE double precision numbers is approximately
c;   1/10000th of a second, with higher accuracy for smaller (earlier)
c;   Julian dates.
c;
c; MODIFICATION HISTORY:
c;	Translated from "Numerical Recipies in C", by William H. Press,
c;	Brian P. Flannery, Saul A. Teukolsky, and William T. Vetterling.
c;	Cambridge University Press, 1988 (second printing).
c;
c;	AB, September, 1988
c;	DMS, April, 1995, Added time of day.
c;   CT, April 2000, Now accepts vectors or scalars.
c;-
c;
	function JULDAY (MONTH, DAY, YEAR, Hour, Minute, Second)

	integer*4 greg

cCOMPILE_OPT idl2

cON_ERROR, 2		; Return to caller if errors

c; Gregorian Calander was adopted on Oct. 15, 1582
c; skipping from Oct. 4, 1582 to Oct. 15, 1582
	GREG = 2299171L  
c       ; incorrect Julian day for Oct. 25, 1582
c
c; Process the input, if all are missing, use todays date.
cNP = n_params()
cIF (np EQ 0) THEN RETURN, SYSTIME(/JULIAN)
cIF (np LT 3) THEN MESSAGE, 'Incorrect number of arguments.'

c; Find the dimensions of the Result:
c;  1. Find all of the input arguments that are arrays (ignore scalars)
c;  2. Out of the arrays, find the smallest number of elements
c;  3. Find the dimensions of the smallest array

c; Step 1: find all array arguments
cnDims = [SIZE(month,/N_DIMENSIONS), SIZE(day,/N_DIMENSIONS), $
c	SIZE(year,/N_DIMENSIONS), SIZE(hour,/N_DIMENSIONS), $
c	SIZE(minute,/N_DIMENSIONS), SIZE(second,/N_DIMENSIONS)]
carrays = WHERE(nDims GE 1)

cnJulian = 1L    ; assume everything is a scalar
cIF (arrays[0] GE 0) THEN BEGIN
c	; Step 2: find the smallest number of elements
c	nElement = [N_ELEMENTS(month), N_ELEMENTS(day), $
c		N_ELEMENTS(year), N_ELEMENTS(hour), $
c		N_ELEMENTS(minute), N_ELEMENTS(second)]
c	nJulian = MIN(nElement[arrays], whichVar)
c	; step 3: find dimensions of the smallest array
c	CASE arrays[whichVar] OF
c	0: julianDims = SIZE(month,/DIMENSIONS)
c	1: julianDims = SIZE(day,/DIMENSIONS)
c	2: julianDims = SIZE(year,/DIMENSIONS)
c	3: julianDims = SIZE(hour,/DIMENSIONS)
c	4: julianDims = SIZE(minute,/DIMENSIONS)
c	5: julianDims = SIZE(second,/DIMENSIONS)
c	ENDCASE
cENDIF
c
cd_Second = 0d  ; defaults
cd_Minute = 0d
cd_Hour = 0d
c; convert all Arguments to appropriate array size & type
cSWITCH np OF  ; use switch so we fall thru all arguments...
c6: d_Second = (SIZE(second,/N_DIMENSIONS) GT 0) ? $
c	second[0:nJulian-1] : second
c5: d_Minute = (SIZE(minute,/N_DIMENSIONS) GT 0) ? $
c	minute[0:nJulian-1] : minute
c4: d_Hour = (SIZE(hour,/N_DIMENSIONS) GT 0) ? $
c	hour[0:nJulian-1] : hour
c3: BEGIN ; convert m,d,y to type LONG
c	L_MONTH = (SIZE(month,/N_DIMENSIONS) GT 0) ? $
c		LONG(month[0:nJulian-1]) : LONG(month)
c	L_DAY = (SIZE(day,/N_DIMENSIONS) GT 0) ? $
c		LONG(day[0:nJulian-1]) : LONG(day)
c	L_YEAR = (SIZE(year,/N_DIMENSIONS) GT 0) ? $
c		LONG(year[0:nJulian-1]) : LONG(year)
c	END
cENDSWITCH


c	min_calendar = -4716
c	max_calendar = 5000000
c	minn = MIN(l_year, MAX=maxx)
cIF (minn LT min_calendar) OR (maxx GT max_calendar) THEN MESSAGE, $
c	'Value of Julian date is out of allowed range.'
cif (MAX(L_YEAR eq 0) NE 0) then message, $
c	'There is no year zero in the civil calendar.'


bc = (L_YEAR LT 0)
L_YEAR = TEMPORARY(L_YEAR) + TEMPORARY(bc)
inJanFeb = (L_MONTH LE 2)
JY = L_YEAR - inJanFeb
JM = L_MONTH + (1b + 12b*TEMPORARY(inJanFeb))


JUL = floor(365.25d * JY) + floor(30.6001d*TEMPORARY(JM)) + L_DAY + 1720995L


; Test whether to change to Gregorian Calandar.
IF (MIN(JUL) GE GREG) THEN BEGIN  ; change all dates
	JA = long(0.01d * TEMPORARY(JY))
	JUL = TEMPORARY(JUL) + 2L - JA + long(0.25d * JA)
ENDIF ELSE BEGIN
	gregChange = WHERE(JUL ge GREG, ngreg)
	IF (ngreg GT 0) THEN BEGIN
		JA = long(0.01d * JY[gregChange])
		JUL[gregChange] = JUL[gregChange] + 2L - JA + long(0.25d * JA)
	ENDIF
ENDELSE


; hour,minute,second?
IF (np GT 3) THEN BEGIN ; yes, compute the fractional Julian date
; Add a small offset so we get the hours, minutes, & seconds back correctly
; if we convert the Julian dates back. This offset is proportional to the
; Julian date, so small dates (a long, long time ago) will be "more" accurate.
	eps = (MACHAR(/DOUBLE)).eps
	eps = eps*ABS(jul) > eps
; For Hours, divide by 24, then subtract 0.5, in case we have unsigned ints.
	jul = TEMPORARY(JUL) + ( (TEMPORARY(d_Hour)/24d - 0.5d) + $
		TEMPORARY(d_Minute)/1440d + TEMPORARY(d_Second)/86400d + eps )
ENDIF

; check to see if we need to reform vector to array of correct dimensions
IF (N_ELEMENTS(julianDims) GT 1) THEN $
	JUL = REFORM(TEMPORARY(JUL), julianDims)

RETURN, jul

END


c$$$; $Id: //depot/idl/IDL_70/idldir/lib/julday.pro#1 $
c$$$;
c$$$; Copyright (c) 1988-2007, ITT Visual Information Solutions. All
c$$$;       rights reserved. Unauthorized reproduction is prohibited.
c$$$
c$$$;+
c$$$; NAME:
c$$$;	JULDAY
c$$$;
c$$$; PURPOSE:
c$$$;	Calculate the Julian Day Number for a given month, day, and year.
c$$$;	This is the inverse of the library function CALDAT.
c$$$;	See also caldat, the inverse of this function.
c$$$;
c$$$; CATEGORY:
c$$$;	Misc.
c$$$;
c$$$; CALLING SEQUENCE:
c$$$;	Result = JULDAY([[[[Month, Day, Year], Hour], Minute], Second])
c$$$;
c$$$; INPUTS:
c$$$;	MONTH:	Number of the desired month (1 = January, ..., 12 = December).
c$$$;
c$$$;	DAY:	Number of day of the month.
c$$$;
c$$$;	YEAR:	Number of the desired year.Year parameters must be valid
c$$$;               values from the civil calendar.  Years B.C.E. are represented
c$$$;               as negative integers.  Years in the common era are represented
c$$$;               as positive integers.  In particular, note that there is no
c$$$;               year 0 in the civil calendar.  1 B.C.E. (-1) is followed by
c$$$;               1 C.E. (1).
c$$$;
c$$$;	HOUR:	Number of the hour of the day.
c$$$;
c$$$;	MINUTE:	Number of the minute of the hour.
c$$$;
c$$$;	SECOND:	Number of the second of the minute.
c$$$;
c$$$;   Note: Month, Day, Year, Hour, Minute, and Second can all be arrays.
c$$$;         The Result will have the same dimensions as the smallest array, or
c$$$;         will be a scalar if all arguments are scalars.
c$$$;
c$$$; OPTIONAL INPUT PARAMETERS:
c$$$;	Hour, Minute, Second = optional time of day.
c$$$;
c$$$; OUTPUTS:
c$$$;	JULDAY returns the Julian Day Number (which begins at noon) of the
c$$$;	specified calendar date.  If Hour, Minute, and Second are not specified,
c$$$;	then the result will be a long integer, otherwise the result is a
c$$$;	double precision floating point number.
c$$$;
c$$$; COMMON BLOCKS:
c$$$;	None.
c$$$;
c$$$; SIDE EFFECTS:
c$$$;	None.
c$$$;
c$$$; RESTRICTIONS:
c$$$;	Accuracy using IEEE double precision numbers is approximately
c$$$;   1/10000th of a second, with higher accuracy for smaller (earlier)
c$$$;   Julian dates.
c$$$;
c$$$; MODIFICATION HISTORY:
c$$$;	Translated from "Numerical Recipies in C", by William H. Press,
c$$$;	Brian P. Flannery, Saul A. Teukolsky, and William T. Vetterling.
c$$$;	Cambridge University Press, 1988 (second printing).
c$$$;
c$$$;	AB, September, 1988
c$$$;	DMS, April, 1995, Added time of day.
c$$$;   CT, April 2000, Now accepts vectors or scalars.
c$$$;-
c$$$;
c$$$function JULDAY, MONTH, DAY, YEAR, Hour, Minute, Second
c$$$
c$$$COMPILE_OPT idl2
c$$$
c$$$ON_ERROR, 2		; Return to caller if errors
c$$$
c$$$; Gregorian Calander was adopted on Oct. 15, 1582
c$$$; skipping from Oct. 4, 1582 to Oct. 15, 1582
c$$$GREG = 2299171L  ; incorrect Julian day for Oct. 25, 1582
c$$$
c$$$; Process the input, if all are missing, use todays date.
c$$$NP = n_params()
c$$$IF (np EQ 0) THEN RETURN, SYSTIME(/JULIAN)
c$$$IF (np LT 3) THEN MESSAGE, 'Incorrect number of arguments.'
c$$$
c$$$; Find the dimensions of the Result:
c$$$;  1. Find all of the input arguments that are arrays (ignore scalars)
c$$$;  2. Out of the arrays, find the smallest number of elements
c$$$;  3. Find the dimensions of the smallest array
c$$$
c$$$; Step 1: find all array arguments
c$$$nDims = [SIZE(month,/N_DIMENSIONS), SIZE(day,/N_DIMENSIONS), $
c$$$	SIZE(year,/N_DIMENSIONS), SIZE(hour,/N_DIMENSIONS), $
c$$$	SIZE(minute,/N_DIMENSIONS), SIZE(second,/N_DIMENSIONS)]
c$$$arrays = WHERE(nDims GE 1)
c$$$
c$$$nJulian = 1L    ; assume everything is a scalar
c$$$IF (arrays[0] GE 0) THEN BEGIN
c$$$	; Step 2: find the smallest number of elements
c$$$	nElement = [N_ELEMENTS(month), N_ELEMENTS(day), $
c$$$		N_ELEMENTS(year), N_ELEMENTS(hour), $
c$$$		N_ELEMENTS(minute), N_ELEMENTS(second)]
c$$$	nJulian = MIN(nElement[arrays], whichVar)
c$$$	; step 3: find dimensions of the smallest array
c$$$	CASE arrays[whichVar] OF
c$$$	0: julianDims = SIZE(month,/DIMENSIONS)
c$$$	1: julianDims = SIZE(day,/DIMENSIONS)
c$$$	2: julianDims = SIZE(year,/DIMENSIONS)
c$$$	3: julianDims = SIZE(hour,/DIMENSIONS)
c$$$	4: julianDims = SIZE(minute,/DIMENSIONS)
c$$$	5: julianDims = SIZE(second,/DIMENSIONS)
c$$$	ENDCASE
c$$$ENDIF
c$$$
c$$$d_Second = 0d  ; defaults
c$$$d_Minute = 0d
c$$$d_Hour = 0d
c$$$; convert all Arguments to appropriate array size & type
c$$$SWITCH np OF  ; use switch so we fall thru all arguments...
c$$$6: d_Second = (SIZE(second,/N_DIMENSIONS) GT 0) ? $
c$$$	second[0:nJulian-1] : second
c$$$5: d_Minute = (SIZE(minute,/N_DIMENSIONS) GT 0) ? $
c$$$	minute[0:nJulian-1] : minute
c$$$4: d_Hour = (SIZE(hour,/N_DIMENSIONS) GT 0) ? $
c$$$	hour[0:nJulian-1] : hour
c$$$3: BEGIN ; convert m,d,y to type LONG
c$$$	L_MONTH = (SIZE(month,/N_DIMENSIONS) GT 0) ? $
c$$$		LONG(month[0:nJulian-1]) : LONG(month)
c$$$	L_DAY = (SIZE(day,/N_DIMENSIONS) GT 0) ? $
c$$$		LONG(day[0:nJulian-1]) : LONG(day)
c$$$	L_YEAR = (SIZE(year,/N_DIMENSIONS) GT 0) ? $
c$$$		LONG(year[0:nJulian-1]) : LONG(year)
c$$$	END
c$$$ENDSWITCH
c$$$
c$$$
c$$$min_calendar = -4716
c$$$max_calendar = 5000000
c$$$minn = MIN(l_year, MAX=maxx)
c$$$IF (minn LT min_calendar) OR (maxx GT max_calendar) THEN MESSAGE, $
c$$$	'Value of Julian date is out of allowed range.'
c$$$if (MAX(L_YEAR eq 0) NE 0) then message, $
c$$$	'There is no year zero in the civil calendar.'
c$$$
c$$$
c$$$bc = (L_YEAR LT 0)
c$$$L_YEAR = TEMPORARY(L_YEAR) + TEMPORARY(bc)
c$$$inJanFeb = (L_MONTH LE 2)
c$$$JY = L_YEAR - inJanFeb
c$$$JM = L_MONTH + (1b + 12b*TEMPORARY(inJanFeb))
c$$$
c$$$
c$$$JUL = floor(365.25d * JY) + floor(30.6001d*TEMPORARY(JM)) + L_DAY + 1720995L
c$$$
c$$$
c$$$; Test whether to change to Gregorian Calandar.
c$$$IF (MIN(JUL) GE GREG) THEN BEGIN  ; change all dates
c$$$	JA = long(0.01d * TEMPORARY(JY))
c$$$	JUL = TEMPORARY(JUL) + 2L - JA + long(0.25d * JA)
c$$$ENDIF ELSE BEGIN
c$$$	gregChange = WHERE(JUL ge GREG, ngreg)
c$$$	IF (ngreg GT 0) THEN BEGIN
c$$$		JA = long(0.01d * JY[gregChange])
c$$$		JUL[gregChange] = JUL[gregChange] + 2L - JA + long(0.25d * JA)
c$$$	ENDIF
c$$$ENDELSE
c$$$
c$$$
c$$$; hour,minute,second?
c$$$IF (np GT 3) THEN BEGIN ; yes, compute the fractional Julian date
c$$$; Add a small offset so we get the hours, minutes, & seconds back correctly
c$$$; if we convert the Julian dates back. This offset is proportional to the
c$$$; Julian date, so small dates (a long, long time ago) will be "more" accurate.
c$$$	eps = (MACHAR(/DOUBLE)).eps
c$$$	eps = eps*ABS(jul) > eps
c$$$; For Hours, divide by 24, then subtract 0.5, in case we have unsigned ints.
c$$$	jul = TEMPORARY(JUL) + ( (TEMPORARY(d_Hour)/24d - 0.5d) + $
c$$$		TEMPORARY(d_Minute)/1440d + TEMPORARY(d_Second)/86400d + eps )
c$$$ENDIF
c$$$
c$$$; check to see if we need to reform vector to array of correct dimensions
c$$$IF (N_ELEMENTS(julianDims) GT 1) THEN $
c$$$	JUL = REFORM(TEMPORARY(JUL), julianDims)
c$$$
c$$$RETURN, jul
c$$$
c$$$END
