CC NAME:
CC   STRREP
CC PURPOSE:
cC   String replacement.
      subroutine strrep(str,sea,rep,stro)

C     Inputs:
c     string to search
c     string to replace
c     string used to replace
      character*(*) str
      character*(*) sea
      character*(*) rep

C     Outputs:
      character*(*) stro
     

      
C     inner variables
      integer*4 pos, lenofstr,isnew

      character*512 tmpstr
      integer*4 j,i
      integer nblen
      
     
      stro=str

 700  pos=index(stro,sea)
      if (pos.ge.1) then
c         stro=stro(1:pos-1)//rep(1:nblen(rep))//
c     &        stro(pos+nblen(sea):nblen(stro))
         stro=stro(1:pos-1)//rep//
     &        stro(pos+nblen(sea):nblen(stro))
c         write(*,*) pos,len(str)
c         write(*,*) 'str:1-pos=',str(1:pos-1)
c         write(*,*) pos+nblen(sea),'-',len(str)
c         write(*,*) 'str: - =',str(pos+len(sea):len(str))
c         write(*,*) 'len(sea):',len(sea)
c         write(*,*) 'sub2:',str(pos+nblen(sea):nblen(str))
c         write(*,*) 'output:',stro
         goto 700
      endif
      
      return
      end
