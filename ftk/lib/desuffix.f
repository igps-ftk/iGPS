c+
c Name:
c     desuffix
c Purpose:
c     to get the filename from full pathname
c Type:
c     subroutine
c Algorightm:
c     Return the last part after the last path separator (/).
c Platform:
c     Unix/Linux
c-
      subroutine desuffix(fullname,ofullname)


      implicit none
      character*(*) fullname,ofullname
      character period,pathsep
      integer*4 i,possep,posp


      period='.'
      pathsep='/'

      possep=0

c     get the last occurence of //pathsep//
c     maybe there is no path separator
      do i=len(fullname),1,-1
         if (fullname(i:i).eq.pathsep) then
            possep=i
c            write(*,*) 'the last / is at:',possep, fullname(1:possep)
            goto 600
         endif
      enddo

c     continue the next step
 600  continue
      do i=len(fullname),1,-1
c        if there is no period ('.') in filename
         if (i.le.possep) then
            posp=len(fullname)
            goto 601
         endif

         if (fullname(i:i).eq.period) then
            posp=i-1
            go to 601
         endif
c         print*,i
      enddo
 601  continue
      ofullname=fullname(1:posp)
c      write(*,*) posp,i,possep,fullname,ofullname

      
      return
      end


      
