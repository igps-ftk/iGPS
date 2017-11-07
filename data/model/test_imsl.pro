FUNCTION TEST_IMSL
  CATCH, Error_status 
 
   ;This statement begins the error handler: 
   IF Error_status NE 0 THEN BEGIN 
      ;PRINT, 'Error index: ', Error_status 
      ;PRINT, 'Error message: ', !ERROR_STATE.MSG 
      ; Handle the error by extending A: 
      RETURN,0
      CATCH, /CANCEL 
   ENDIF 

  x = [1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 4.0, 5.0, 5.0]
  
  y = [1.1, 0.1, -1.2, 0.3, 1.4, 2.6, 3.1, 4.2, 9.3, 9.6]
  z = FINDGEN(120)/20
  line = MAKE_ARRAY(120, VALUE = 0.0)
  ; Perform a simple linear regression.
  Coefs = IMSL_MULTIREGRESS(x, y, PREDICT_INFO = predict_info)
  RETURN,1
END
PRO TEST_IMSL
  PRINT,TEST_IMSL()
END