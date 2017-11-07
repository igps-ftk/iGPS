       
      SUBROUTINE sum_ARRAY(argc, argv) 
c!Called by IDL
      INTEGER*4 argc, argv(*) 
c!Argc and Argv are integers
      j = LOC(argc) 
c!Obtains the number of arguments (argc)
c      !Because argc is passed by VALUE.
c      c Call subrotine sum_ARRAY1, converting the IDL parameters
c      c to standard 
      CALL sum_ARRAY1((argv(1)), (argv(2)), (argv(3)))
      RETURN
      END
c      c This subroutine is called by sum_ARRAY and has no
c      c IDL specific code.
c      c
      SUBROUTINE sum_ARRAY1(array, n, sum)
      INTEGER*4 n
      REAL*4 array(n), sum
      sum=0.0
      DO i=1,n
      sum = sum + array(i)
      ENDDO
      RETURN
      END
