CTITLE 'TRIMLEN'
 
 
      integer*4 function trimlen(string)
 
      implicit none 

* MOD TAH 050129: Adding counting null's as well as blanks at the 
*     end of strings.  Useful for strings returned from C that have
*     nulls at the end.
 
*     Routine to return the length of the used portion of string.
*     Length is with trailing blanks removed.
 
*         len_string    - declared length of string
*         i             - Loop counter
 
      integer*4 len_string, i
 
*       blanks          - Indicates blanks are still being
*                       - found
 
      logical blanks
 
*             string    - the string whose length we want
 
      character*(*) string
 
***** Get full length of string, and work backwards
 
      len_string = LEN(string)
      i = len_string

****  See if null string
      if( ichar(string(1:1)).eq.0 ) then
          trimlen = 0
          RETURN
      endif
 
      blanks = .true.
 
*     Scan string from end to front
      do while ( blanks )
          if( i.gt.0 ) then
* MOD TAH 050129: Add or check on null
              if( string(i:i).eq.' ' .or.
     .            string(i:i).eq.char(0) ) then
                  i = i - 1
              else
                  blanks = .false.
              end if
          else
*                                     ! Force exit at end of string
              blanks = .false.
          end if
      end do
 
***** Save length of string
      trimlen = i
 
***** Thats all
      return
      end
 
 
