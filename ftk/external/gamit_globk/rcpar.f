CTITLE RCPAR
 
      integer*4 function rcpar( iel, arg )

      implicit none

 
*     Routine to emulate RCPAR using the igetarg UNIX subroutine
*     Modified to use getarg
 
*         iel       - Element of runstring to get
*       igetarg     - UNIX routine to read runstring
*       len_arg     - Length of arg.
*       trimlen     - Get length of string
*       offset      - Offset to be applied to the passsed element
*                    (0 is assumed to program name, 1 first argument)
 
      integer*4 iel, len_arg, trimlen, offset
 
*             arg   - Arg of runstring
 
      character*(*) arg
      character*4 test_arg
      
      data offset / -1 /
 
****  Get length of argument and runstring
* MOD TAH 010610: To see where the count starts for getarg
      if( offset.lt.0 ) then
          call getarg(0, test_arg)
	  len_arg = trimlen(test_arg)
	  if( len_arg.eq.0 ) then
	      offset = 1
	  else
	      offset = 0
	  end if
      end if
      
      len_arg = LEN(arg)
      call getarg( iel+offset, arg )
      rcpar = trimlen( arg )
 
***** Thats all
      return
      end
 
