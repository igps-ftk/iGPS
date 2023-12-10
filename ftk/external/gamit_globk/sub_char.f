CTITLE SUB_CHAR
 
      subroutine sub_char ( buffer, old, new )
 
      implicit none
 
*     Routine to search string buffer and replace all occurrences
*     of string OLD with string NEW
 
*   end     - End position of moving buffer
*   i,j     - Loop counter
*   len_buffer  - length of buffer (full dimensioned length)
*   len_old - Length of old string
*   len_new - Length of new string
*   len_used    - Used portion of buffer
*   offset  - Difference in length between new and old string
*   pos     - Position of old in string
*   start   - start position for moving buffer
*   trimlen - Length of used portion of string
 
      integer*4 end, i, len_buffer, len_old, len_new, len_used,
     .    offset, pos, start, trimlen
 
*   finished    - Indicates we have run out of string
 
      logical finished
 
*   buffer  - Buffer to be manipulated
*   old     - String to be replaced
*   new     - string to replace old with
 
 
      character*(*) buffer, old, new
 
***** Start by get sizes of strings
* MOD TAH 190621: Changed use of len to trimlen but added back len
*     if trimlen = 0 (case when space " " passed).
      len_old = trimlen( old )
      if( len_old.eq.0 ) len_old = len( old )
      len_new = trimlen( new )
      if( len_new.eq.0 ) len_new = len( new )
      len_buffer = len( buffer )
 
*     Check to make sure old is not a substring of new.  If it is the
*     algorithm used here will not work
 
      pos = index ( new, old )
      if( pos.gt.0 ) RETURN
 
      finished = .false.
 
      do while ( .not. finished )
          if( trimlen(old).gt.0 ) then 
             pos = index( buffer, trim(old) ) 
          else
             pos = index( buffer, old )
          endif 

*                             ! Make substiution
          if( pos.gt.0 ) then
 
*             Now copy string after pos
              len_used = trimlen( buffer )
 
              offset = len_new - len_old  ! change in string length
              start  = pos - offset + 1
              end    = min(len_used,len_buffer+offset)
 
*                                             ! Move string starting at top
              if( len_new.gt.len_old ) then
                  do i = end, start, -1
                      buffer(i+offset:i+offset) = buffer(i:i)
                  end do
              end if
 
*                                             ! Move string starting a bottom
              if( len_new.lt.len_old ) then
                  do i = start, end
                      buffer(i+offset:i+offset) = buffer(i:i)
                  end do
*                                             ! Clear the characters left at
                  buffer(end+offset+1:) = ' '
*                                             ! the end
              end if
 
****          Now put in new string
              if( trimlen(new).gt.0 ) then
                 buffer(pos:pos+len_new-1) = trim(new)
              else
                 buffer(pos:pos+len_new-1) = new
              endif 

 
*                     ! we have finished
          ELSE
 
              finished = .true.
 
          ENDIF
*                     ! Searching through string
      END DO
 
****  Thats all
      return
      end

CTITLE SUBHOME

      subroutine subhome( file )

      implicit none

*     Routine to replace ~ as the first character in a file name with
*     home directory given in the HOME environment variable

      character*(*) file !  Input file, must be a variable

* LOCAL
      character*256 home

****  See if first character is ~, if nor just return
      if( file(1:1).eq.'~' ) then
*         Get home directory
          call getenv('HOME',home)
          call sub_char( file, '~', trim(home))
          
      endif

****  Thats all
      return
      end

 
 
