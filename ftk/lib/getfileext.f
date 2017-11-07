c+
c Name:
c     getfileext
c Purpose:
c     to get the filename from full pathname
c Type:
c     subroutine
c Algorightm:
c     Return the last part after the last path separator (/).
c Platform:
c     Unix/Linux
c-
      subroutine getfileext(fullname,ext)


      implicit none
      character*(*) fullname,ext
      character period,pathsep
      integer*4 i,possep,posp,tmplen

      do i=1,len(ext)
         ext(i:i)=' '
      enddo

      period='.'
      pathsep='/'

      possep=0

      do i=len(fullname),1,-1
         if (fullname(i:i).ne.' ') then
            tmplen=i
            goto 605            
         endif
      enddo
 605  continue

c     get the last occurence of //pathsep//
c     maybe there is no path separator
      do i=tmplen,1,-1
         if (fullname(i:i).eq.pathsep) then
            possep=i
c            write(*,*) 'the last / is at:',possep, fullname(1:possep)
            goto 600
         endif
      enddo

c     continue the next step
 600  continue

      do i=tmplen,1,-1
c        if there is no period ('.') in filename
         if (i.le.possep) then
c            posp=len(fullname)
            ext=''
            goto 601
         endif

         if (fullname(i:i).eq.period) then
            posp=i
            go to 601
         endif
c         print*,i
      enddo
 601  continue
      if (posp.eq.tmplen) then
         ext=''
      else
         ext=fullname(posp+1:tmplen)
      endif
c     ext=fullname(posp:len(fullname))



c      ofullname=fullname(1:posp)
c      write(*,*) posp,i,possep,tmplen,ext

      
      return
      end


      
