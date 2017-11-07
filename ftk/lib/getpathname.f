c+
c Name:
c     getpathname
c Purpose:
c     to get the filename from full pathname
c Type:
c     subroutine
c Algorightm:
c     Return the last part after the last path separator (/).
c Platform:
c     Unix/Linux
c-
      subroutine getpathname(fullname,pathname)


      implicit none
      character*(*) fullname,pathname
      character pathsep
      integer i


      pathsep='/'
      do i=len(fullname),1,-1
         if (fullname(i:i).eq.pathsep) then
            go to 600
         endif
      enddo
 600  continue
      pathname=fullname(1:i-1)

      
      return
      end


      
