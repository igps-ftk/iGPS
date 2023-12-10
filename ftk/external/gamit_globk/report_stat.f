CTITLE report_stat
     
      subroutine report_stat( typep, modulep, routinep, filep,
     .            messagep, ierr )

      implicit none
 
*     Routine to report on the status on gamit and globk processing.
*     The routine reports status messages which indicate where in the
*     processing we are; warnings which are are not fatal to the processing
*     but could be used as diagnostic of later problems; and fatal errors
*     which cause the program to stop running.
 
*     For all reports, the messsages are appended to files named
*     'module' .status, .warn or .fatal depending on the type of error
*     These files are opened with append status and closed before the
*     routine ends (the later is to ensure that the UNIX file buffers
*     flushed so that the user can see the current status).
 
*     Standard examples of the use of this routine would be:
*     STATUS:
*     call report_stat('STATUS','MODEL','model',' ','Beginning process',0)
*     WARNING:
*     call report_stat('WARNING','MODEL','wrtout',' ',
*    .                  'Site radius too large', 0)
*     call report_stat('WARNING','GLOBK','update_aprioris',line,
*    .                   'IOSTAT error reading string ',ierr)
*     FATAL:
*     call report_stat('FATAL','SOLVE','invert',' ',
*    .                   'Normal equations non-positive definite',0)
*     call report_stat('FATAL','GLOBK','read_glb_mar',glb_mar_file,
*    .                   'IOSTAT error opening file ',ierr)
*     CLEAR:
*     call report_stat('CLEAR','GLOBK',' ',' ',' ',0)

* ROUTINES used from HP1000 library:
* trimlen    - Length of string
* casefold   - Converts string to upper case
* caseunfold - Converts string to lower case
* systime  - Returns system time (available on both HP and Sun)

*
* PASSED Variables
 
*   ierr    - Error number associated with report.  This value is
*           - printed for warnings and fatal errors when non-zero.
 
      integer*4 ierr
 
*   type    - Type of message:  Only three types are allowed, any
*           - other type will cause this routine to print a warning
*           - message.  The types are (only first four characters
*           - checked:
*           - status  -- General reporting of status for progress
*           -       information
*           - warning -- Non-fatal error in the processing, could be
*           -       diagonistic of later fatal errors
*           - fatal   -- Fatal error, If ierr=0 then only the message
*           -       is printed, otherwize the error number is
*           -       printed with the message.
*           - clear   -- Removes the status, warning and fatal files
*                   for a given module name.
 
*   module  - Name of module (main program) (e.g., model, solve)
*   subroutine  - Name of the subroutine from which the report is
*           - made.
*   file    - Name of the file or string being manipulated.  This is
*             printed whenever a non-zero length string is passed.
*             It is included with (Name xxxxx) at the end of the line.
*   message - Message to be printed
                                              
      character*(*) typep, modulep, routinep, filep, messagep
      character*(256) type, module, routine, file, message

* LOCAL VARIABLES
 
*   rep_date(5) - Yr, mon, day, hr, min that the report is made
*   trimlen     - Library routine to returned the non-blank length
*               - of a string
 
*   len_mod     - Length of the module string
*   len_sub     - Length of the subroutine string
*   len_mes     - Length of the message
*   len_fil     - Length of the file name
*   len_typ     - Length of the status type (variable type)
 
*   jerr        - IOSTAT error writing to the status file
*   kerr        - IOSTAT error writing to stdout
*   cerr        - IOSTAT error closing the status file
 
*   rep_unit        - Unit number assigned to the report file.  If we have
*               - trouble with the opening the unit, rep_unit is set
*               - 6 so that output will goto to stdout.

*   i           - Counter used to find / or start of string in the module
*                 name

      integer*4 rep_date(5), trimlen, len_mod, len_sub, len_mes,
     .    len_fil, len_typ, jerr, kerr, cerr, rep_unit, i
 
*   rep_sec     - Seconds that the report is made
 
      real*8 rep_sec
 
*   rep_file        - Name of the report file to which the messages are
*               - written.  Generated from the module name with an an
*               - extent based on the type.  (NOTE: Module is upper
*               - case when name generated).
*   mod_name - Names of the module for creating the file name
*                 (Removes the full information if present)
 
 
      character*256 rep_file
      character*64 mod_name, prog_name
         
***** Make a copy of all passed variables.  Stops us overwritting
*     a fixed string.
      type    = typep
      module  = modulep
      routine = routinep
      file    = filep
      message = messagep

****  Start: First get the system time so that we know when the report
*     was made
 
      call systime( rep_date, rep_sec )

*     Remove the 19-- portiion of the date so that HP and Sun look the
*     same
      rep_date(1) = rep_date(1) - int(rep_date(1)/100)*100
 
*     See what type of report we are to make (this saves
*     unnecessary opening of files).  Casefold the type incase user
*     calls with the wrong case.  Module and subroutine are set case for
*     "nice" looking output.
 
      call casefold( type )
      call casefold( module )
      call caseunfold( routine )
 
*     Get the lengths of the strings.  (1 is minumum length to stop fortran
*     error is string(1:0) is attempted.)

      if( ichar(module(1:1)) .eq. 0 ) module(1:1)  = ' '
      if( ichar(routine(1:1)).eq. 0 ) routine(1:1) = ' '
      if( ichar(message(1:1)).eq. 0 ) message(1:1) = ' '
      if( ichar(file(1:1))   .eq. 0 ) file(1:1)    = ' '
      if( ichar(type(1:1))   .eq. 0 ) type(1:1)    = ' '

*     Make sure all the strings contain ascii characters
      call check_ascii( module )
      call check_ascii( routine )
      call check_ascii( message )
      call check_ascii( file )
      call check_ascii( type )
 
      len_mod = max(1,trimlen(module))
      len_sub = max(1,trimlen(routine))
      len_mes = max(1,trimlen(message))
      len_fil = max(1,trimlen(file))
      len_typ = max(1,trimlen(type))
*
*     Now strip out the module name from the end the module string.
*     (Look for /'s in the name in case full name passed)
      i = len_mod
      do while ( i.gt.1 .and. module(i:i).ne.'/' )
          i = i - 1
      end do
      if( module(i:i).eq.'/' ) i = i + 1
      mod_name = module(i:len_mod) 
*     The following will result in the report files being named 'GAMIT'
*     for the main GAMIT batch sequence, but individual names for the
*     non-batch modules (MAKEXP, MAKEX, FIXDRV, and utilities) and
*     for the /kf routines except for AUTCLN.  Currently, the GAMIT 
*     batch script erases the 'GAMIT' files but all the other modules
*     erase their files upon executation with a 'CLEAR' call of report_stat.
c ** rwk 070130:  We've now changed this so that all programs executed by
c     sh_gamit will have the name 'GAMIT', making it easier for the novice
c     to find the source of a failure in the sh_gamit log file.

      if( mod_name(1:3).eq.'ARC'.or. 
c ** this added rwk 070130        
c ** but then removed rwk 081027
c**     .    mod_name(1:6).eq.'MSTINF'.or.
c ** this added rwk 070912
     .    mod_name(1:6).eq.'ERPTAB'.or.    
C ** this added rwk 110416 and 160812
     .    mod_name(1:6).eq.'NGSTOT'.or.
     .    mod_name(1:6).eq.'ORBFIT'.or.
c ** these added rwk 060628
     .    mod_name(1:6).eq.'MAKEXP'.or.
     .    mod_name(1:6).eq.'MAKEJ '.or.
     .    mod_name(1:6).eq.'MAKEX '.or.
     .    mod_name(1:6).eq.'FIXDRV'.or.
c** --------------
     .    mod_name(1:6).eq.'YAWTAB'.or.
     .    mod_name(1:6).eq.'OCTTAB'.or.  
     .    mod_name(1:6).eq.'GRDTAB'.or.
     .    mod_name(1:5).eq.'MODEL'.or. 
     .    mod_name(1:6).eq.'AUTECL' .or.
     .    mod_name(1:6).eq.'AUTCLN'.or.
     .    mod_name(1:5).eq.'CFMRG'.or.
     .    mod_name(1:5).eq.'SOLVE'.or.
     .    mod_name(1:6).eq.'SCANDD' ) then
           prog_name = 'GAMIT' 
      else
*          MOD TAH 091228: Removed restriction on length of name
*          (only 6 characters used originally)
           prog_name = mod_name
      endif
      
*     Set the error flags for errors in this routine to zero.  At end we
*     check these are still zero.
      jerr = 0
      kerr = 0
      cerr = 0
 
*****  Report the appropriate type of messages.
*     STATUS report
      if( type(1:4).eq.'STAT' ) then
 
*         Generate the name the file to write the status do and get
*         an available unit number to open the file with status append.
          call rep_open( prog_name, '.status', rep_file, rep_unit)
 
*         Now write the status message
          if( len_fil.le.1 ) then
              write(rep_unit, 120, iostat=jerr) rep_date, rep_sec,
     .                module(1:len_mod),
     .                    routine(1:len_sub),
     .            message(1:len_mes)
              write(*, 120, iostat=kerr) rep_date, rep_sec,
     .                module(1:len_mod),
     .                routine(1:len_sub),
     .                message(1:len_mes)
 
 120          format('STATUS :',3(i2.2),':',2(i2.2),':',f4.1,1x,
     .            a,'/',a,': ',a)
           else
*             if file name passed print this as well)           
              write(rep_unit, 140, iostat=jerr) rep_date, rep_sec,
     .                module(1:len_mod),
     .                routine(1:len_sub),
     .                message(1:len_mes),
     .                file(1:len_fil)
              write(*, 140, iostat=kerr) rep_date, rep_sec,
     .                module(1:len_mod),
     .                routine(1:len_sub),
     .                message(1:len_mes),
     .                file(1:len_fil)
 
 140          format('STATUS :',3(i2.2),':',2(i2.2),':',f4.1,1x,
     .            a,'/',a,': ',a,' (Name ',a,')')
           end if
           
****  Warning messages that are not fatal (can be for information or to
*     report problems with file access and/or string reading.
      else if ( type(1:4).eq.'WARN' ) then
 
*         Generate file name and open
          call rep_open( prog_name, '.warning', rep_file, rep_unit)
 
*         If the error flag is zero, assume the file name is not
*         needed
          if( ierr.eq. 0 ) then

*             If the file name is zero or 1 charcater long, just
*             print the message, otherwize output the file name
*             as well.
              if( len_fil.le.1 ) then
                  write(rep_unit, 220, iostat=jerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes)
                  write(*, 220, iostat=kerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes)
 
 220              format('WARNING:',3(i2.2),':',2(i2.2),':',f4.1,1x,
     .                a,'/',a,': ',a)
               else
*                 Print the file name as well.
                  write(rep_unit, 230, iostat=jerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes),
     .                    file(1:len_fil)
                  write(*, 230, iostat=kerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes),
     .                    file(1:len_fil)
 
 230              format('WARNING:',3(i2.2),':',2(i2.2),':',f4.1,1x,
     .                a,'/',a,': ',a,' (Name ',a,')')
              end if
 
          else
*             Report the messsage and the file name and the error number
              write(rep_unit, 240, iostat=jerr) rep_date, rep_sec,
     .                module(1:len_mod),
     .                routine(1:len_sub),
     .                message(1:len_mes),
     .                file(1:len_fil),  ierr
              write(*, 240, iostat=kerr) rep_date, rep_sec,
     .                module(1:len_mod),
     .                routine(1:len_sub),
     .                message(1:len_mes),
     .                file(1:len_fil),  ierr
 240          format('WARNING:',3(i2.2),':',2(i2.2),':',f4.1,1x,
     .            a,'/',a,': ',a,1x,a,' ERROR ',i5)
 
          end if
 
***** FATAL error messages:  These are processed the same as warnings
*     but will cause the program to stop running.
      else if ( type(1:4).eq.'FATA' ) then
 
*         Generate file name and open
          call rep_open( prog_name, '.fatal', rep_file, rep_unit)
 
*         If the error flag is zero, assume the file name is not
*         needed
          if( ierr.eq. 0 ) then

*             Again check to see if the file name has been passed
              if( len_fil.le.1 ) then
                  write(rep_unit, 320, iostat=jerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes)
                  write(*, 320, iostat=kerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes)
 
 320              format('FATAL  :',3(i2.2),':',2(i2.2),':',f4.1,1x,
     .                a,'/',a,': ',a)
              else

*                 Print the file name as well
                  write(rep_unit, 330, iostat=jerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes),
     .                    file(1:len_fil)
                  write(*, 330, iostat=kerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes),
     .                    file(1:len_fil)
 
 330              format('FATAL  :',3(i2.2),':',2(i2.2),':',f4.1,1x,
     .                a,'/',a,': ',a,' (Name ',a,')')
              end if         
          else
*             Report the messsage and the file name and the error number
              write(rep_unit, 340, iostat=jerr) rep_date, rep_sec,
     .                module(1:len_mod),
     .                routine(1:len_sub),
     .                message(1:len_mes),
     .                file(1:len_fil),  ierr
              write(*, 340, iostat=kerr) rep_date, rep_sec,
     .                module(1:len_mod),
     .                routine(1:len_sub),
     .                message(1:len_mes),
     .                file(1:len_fil),  ierr
 340          format('FATAL  :',3(i2.2),':',2(i2.2),':',f4.1,1x,
     .            a,'/',a,': ',a,1x,a,' ERROR ',i5)  
*             if the error is a 101, it's like this is a g77 compiler with a 99-unit-number limit
              if( ierr.eq.101 ) then   
                write(rep_unit,341,iostat=jerr) 
                write(*       ,341,iostat=jerr)
 341            format('FATAL  : Error 101 suggests you are using'
     .             ,' a g77 compiler with Fortran unit numbers limited'
     .                ,' to 99; see GAMIT installation README')
              endif
          end if
 
*         For the fatal errors we close the rep_file and stop the program
*         running
          close( rep_unit, iostat=cerr )
          stop 'FATAL Error: Stop from report_stat'
 
***** See if we clearing the files
      else if( type(1:4).eq.'CLEA' ) then

*         Open each of the types of files and delete on closing
          if( len_mod.gt.1 ) then
              call rep_open( prog_name, '.status', rep_file, rep_unit)
              close(rep_unit, iostat=cerr, status='delete')
              call rep_open( prog_name, '.warning', rep_file, rep_unit)
              close(rep_unit, iostat=cerr, status='delete')
              call rep_open( prog_name, '.fatal', rep_file, rep_unit)
              close(rep_unit, iostat=cerr, status='delete')
          end if
      else
 
*         An unknown type of report has been called.  First warn the user
*         that an unknown type has been attempted, and then print out the
*         message information as if it were a warning.
*         Generate file name and open
          call rep_open( prog_name, '.warning', rep_file, rep_unit)
          write(rep_unit, 420, iostat=jerr) rep_date, rep_sec,
     .                module(1:len_mod), routine(1:len_sub),
     .                type(1:len_typ)
          write(*, 420, iostat=kerr) rep_date, rep_sec,
     .                module(1:len_mod), routine(1:len_sub),
     .                type(1:len_typ)
 420      format('WARNING:',3(i2.2),':',2(i2.2),':',f4.1,1x,
     .            a,'/',a,': Unknown type of status. Error in call to ',
     .           'report_stat -- ',a)
 
*         Now print the standard warning format.
          if( ierr.eq. 0 ) then

*             See if the file name has been passed.
              if( len_fil.le.1 ) then
                  write(rep_unit, 220, iostat=jerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes)
                  write(*, 220, iostat=kerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes)
               else
*                 Print the file name as well.
                  write(rep_unit, 230, iostat=jerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes),
     .                    file(1:len_fil)
                  write(*, 230, iostat=kerr) rep_date, rep_sec,
     .                    module(1:len_mod),
     .                    routine(1:len_sub),
     .                    message(1:len_mes),
     .                    file(1:len_fil)
              end if
          else
*             Report the messsage and the file name and the error number
              write(rep_unit, 240, iostat=jerr) rep_date, rep_sec,
     .                module(1:len_mod),
     .                routine(1:len_sub),
     .                message(1:len_mes),
     .                file(1:len_fil),  ierr
              write(*, 240, iostat=kerr) rep_date, rep_sec,
     .                module(1:len_mod),
     .                routine(1:len_sub),
     .                message(1:len_mes),
     .                file(1:len_fil),  ierr
          end if
 
      end if
 
****  Now clean up before exiting. Close the report file so that it will
*     be updated and check if any errors occurred during the writing of
*     the messages
 
      close( rep_unit, iostat=cerr )
      if( jerr.ne.0 ) then
          write(*,520) jerr, rep_file(1:max(1,trimlen(rep_file)))
 520      format('WARNING: IOSTAT error ',i4,' occurred in ',
     .        'report_stat writing the status file ', a)
      end if
      if( kerr.ne.0 ) then
          write(*,540) kerr
 540      format('WARNING: IOSTAT error ',i4,' occurred in ',
     .            'report_stat writing the status report to stdout')
      end if
 
      if( cerr.ne.0 ) then
          write(*,560) cerr, rep_file(1:max(1,trimlen(rep_file)))
 560      format('WARNING: IOSTAT error ',i4,' occurred in ',
     .            'report_stat closing the status file ', a)
      end if
 
****  Thats all
      return
      end
 
CTITLE REP_OPEN
 
      subroutine rep_open( module, extent, rep_file, rep_unit )
 
*     This routine will open the status/warning/fatal file with access
*     append.  The unit number is searched to find an available unit
*     and if no unit can be found the unit is returned as 6 so that
*     stdout will be used.
 
* PASSED VARIABLES
 
*   rep_unit    - Unit number for the file.  Returned by this
*               - routine
 
      integer*4 rep_unit
 
*   module      - Name of the module calling report_stat
*   extent      - extent to be given to file name (status, warning
*               - or fatal).
*   rep_file        - Name given to output file (returned).
 
      character*(*) module, extent, rep_file
 
* LOCAL VARIABLES
 
*   ierr        - IOSTAT error when file opened.
*   trimlen     - Length of non-blank portion of string
 
      integer*4 ierr, trimlen
 
*   unit_open   - Set true by inquire if unit is already in use.
 
      logical unit_open

      character*8 access

 
****  First generate the file name
 
      rep_file = module(1:max(1,trimlen(module))) // extent
 
*     Now find an available unit.  Units 100-499 are searched
      unit_open = .true.
      rep_unit  =    100
      do while ( unit_open .and. rep_unit.lt.500 )
          inquire( unit=rep_unit, opened=unit_open)
 
*         If the unit is in use, try the next one
          if( unit_open ) rep_unit = rep_unit + 1
      end do
 
***   If the unit is still open, then we have a major problem.   We can
*     find a unit number for output so warning user and set the unit
*     to 6
      if( unit_open ) then
          write(*,120) extent(2:)
 120      format('WARNING: Cannot find available unit number for ',a,
     .            ' reports.  Output to stdout')
          rep_unit = 6
      else
 
*         Found unit number, so open file
          access = 'append'
*          open(rep_unit, file=rep_file, status='unknown', 
*     .         access=access, iostat=ierr)
* for IBM AIX
          open(rep_unit, file=rep_file, status='unknown',
     .         position=access, iostat=ierr)
          if( ierr.ne.0 ) then
 
*             Error opening the output file.  Tell user of problem
              write(*,140) ierr, extent(2:),
     .                rep_file(1:max(1,trimlen(rep_file))),
     .                rep_unit
 140          format('WARNING: IOSTAT error ',i4,' occurred opening ',
     .               a,' file ',a,' on unit ',i3)
              rep_unit = 6
          end if
      end if
 
***** Thats all
      return
      end
 
