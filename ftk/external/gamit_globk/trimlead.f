Ctitle TRIMLEAD
 
      subroutine trimlead( line )
 
*     This routine will trim the leading blanks from a string.
 
* line - the line to have its leading characters removed
* la   - length of the used portion of the string
* i,j  - loop counters
* Trimlen - function to return length of string
      integer*4 la, i, j, trimlen
 
 
      character*(*) line
 
***** Get length of string
      la = trimlen( line )
      if( la.gt.0 ) then
          i = 1
          do while (line(i:i).eq.' '.and. i.lt.la )
             i = i + 1
          end do
 
*         if we have leading blanks, remove them now
          if ( i.gt.1 ) then
              do j = i, la
                line(j-i+1:j-i+1) = line(j:j)
              end do
 
*             clear to the end of the line
              line(la-i+2:) = ' '
          end if
      end if
 
***** Thats all
      return
      end
 
