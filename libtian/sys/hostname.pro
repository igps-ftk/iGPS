FUNCTION HOSTNAME
  CASE !VERSION.OS_FAMILY OF
    'Windows': BEGIN
      HOST=GETENV('COMPUTERNAME')+' ['+GETENV('PROCESSOR_IDENTIFIER')+']'
    END
    'unix': BEGIN
      SPAWN, 'hostname', HOST, ERR
      SPAWN, 'uname -a', INFO, ERR
      HOST=HOST+' ['+INFO+']'
    END
    ELSE: BEGIN
      HOST=''
    END
  ENDCASE
  RETURN, HOST
END

PRO HOSTNAME
  PRINT,'[HOSTNAME]You are running on platform ',HOSTNAME()
END