CC NAME:
CC   STRSPLIT
CC PURPOSE:
CC   like IDL's strsplit function
cc   split string into string array by specific separator (default blank)

      subroutine strsplit2(str,sep,n,strs)
c      program strsplit

c      integer*4 NMAX
c      parameter(NMAX=100)

C     Inputs:
c     str
c     sep

C     Outputs:
c     n
c     strs(n)
     
      integer*4 n
      character*(*) str
      character*(*) strs(*)
      character*(*) sep
      
C     inner variables
      integer*4 pos, lenofstr,isnew,len_sep

      character*1023 tstr
      integer*4 j,i
      integer nblen
      
C     for debug purpose:
c      str=' a bc defg hi j k'
c      sep=' '
      
      n=0
      isnew=0


c     ??error
      tstr=str//' '
cc      lenofstr=nblen(str)-1
c     and why?

c     why not?
c     Mon Nov 17 10:13:56 CST 2008
      lenofstr=nblen(tstr)
      len_sep=len(sep)
      
c     begin of the part
      ib=1
c     end of the part
      ie=lenofstr

c      write(*,*) 'Input separator:', sep//'.'
c      write(*,*) 'Input str:', str(1:lenofstr)//'.'
c      i=1
      do while (nblen(tstr).gt.0)
c         write(*,*) 'tstr:',tstr(1:nblen(tstr))
          pos=index(tstr,sep)

c     if not found
         if (pos.eq.0) then
c            write(*,*) 'n:',n
            n=n+1
            strs(n)=tstr
            goto 999
         endif
         
         n=n+1
         ie=pos-1
         strs(n)=tstr(1:pos-len_sep)
         ib=pos

c         write(*,*) pos, n, '|'//tstr(1:pos-len_sep)//'|'
         tstr=tstr(pos+1:nblen(tstr))
c         write(*,*) 'tstr(remain):',tstr(1:nblen(tstr))
c         stop

c$$$         write(*,*) str(i:i+len_sep-1)
c$$$cccc  this is a good    point!!
c$$$c     Skip the leading separators.
c$$$         if (str(i:i+len_sep-1).eq.sep.and.
c$$$     +        isnew.eq.0) then
c$$$c            write(*,*), 'skip-', str(i:i),'-', sep, isnew,i
c$$$            goto 20
c$$$         endif
c$$$         
c$$$         if (str(i:i+len_sep-1).ne.sep.and.
c$$$     +        isnew.eq.0) then
c$$$            isnew=1
c$$$            j=len_sep
c$$$         endif
c$$$         
c$$$         if (str(i:i+len_sep-1).eq.sep.and.
c$$$     +        isnew.eq.1) then
c$$$            isnew=0
c$$$            n=n+1
c$$$            strs(n)=tmpstr(1:j-1)
c$$$c            write(*,'(a4,i3,1x,a,i5)') 'new:',n, 'hi',j-1
c$$$            tmpstr=' '
c$$$            goto 20
c$$$         endif
c$$$
c$$$         if (isnew.eq.1) then
c$$$            tmpstr(j-len_sep+1:j) = str(i:i+len_sep-1)
c$$$            j=j+len_sep
c$$$         endif

 20      continue
c         i=pos+len_sep
c$$$c         write(*,*) i,j-1,n,isnew, '|',str(i:i),'-',isnew 
c$$$c     &        ,tmpstr(1:nblen(tmpstr))
      enddo
c$$$c      write(*,*) 'last n1:',n
c$$$c     NO blank right-side spaces exist:
c$$$      if (isnew.eq.1) then
c$$$         n=n+1
c$$$	 tmpstr(j:j) = str(i:i)
c$$$         strs(n)=tmpstr(1:j+1)
c$$$c         write(*,'(a4,i3,a30,i5)') 'new:',n, strs(n),j
c$$$      endif
c$$$c     ??? JUN-10-2007 Tian
c$$$

 999  continue

c$$$      do i=1,n
c$$$         tstr=strs(i)
c$$$         write(*,*) i,tstr(1:nblen(tstr))
c$$$      enddo
c      stop
c     ab c defg hi j kl  mhin
c      write(*,*) '#parts:',n
      ni=0
      do i=1, n
         if (nblen(strs(i)).ne.0) then
            ni=ni+1
            goto 997
         endif
         do j=i+1,n
            if (nblen(strs(j)).ne.0) then
               strs(i)=strs(j)
               strs(j)=''
               ni=ni+1
               goto 997
            endif
         enddo
 997     continue
      enddo
      n=ni
c$$$      do i=1,n
c$$$         tstr=strs(i)
c$$$         write(*,*) i,tstr(1:nblen(tstr))
c$$$      enddo
c$$$      stop

c 998  continue
c      n=j
c      write(*,*) 'stop'
c      stop
      return
      end
