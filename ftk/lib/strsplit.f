CC NAME:
CC   STRSPLIT
CC PURPOSE:
CC   like IDL's strsplit function
cc   split string into string array by specific separator (default blank)

      subroutine strsplit(str,sep,n,strs)
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
      character*1 sep
      
C     inner variables
      integer*4 pos, lenofstr,isnew

      character*512 tmpstr
      integer*4 j,i
      integer nblen
      
C     for debug purpose:
c      str=' a bc defg hi j k'
c      sep=' '
      
      n=0
      isnew=0

c     ??error
      str=str//' '
      lenofstr=nblen(str)-1
c     and why?

c     why not?
c     Mon Nov 17 10:13:56 CST 2008
      lenofstr=nblen(str)
      

c      write(*,*) 'Input separator:', sep(1:nblen(sep))
c      write(*,*) 'Input str:', str(1:lenofstr)

      do i=1, lenofstr
         if (str(i:i).eq.sep.and.isnew.eq.0) then
c            write(*,*), 'skip-', str(i:i),'-', sep, isnew,i
            goto 20
         endif
         
         if (str(i:i).ne.sep.and.isnew.eq.0) then
            isnew=1
            j=1
         endif
         
         if (str(i:i).eq.sep.and.isnew.eq.1) then
            isnew=0
            n=n+1
            strs(n)=tmpstr(1:j-1)
c            write(*,'(a4,i3,1x,a,i5)') 'new:',n, 'hi',j-1
            tmpstr=' '
            goto 20
         endif

         if (isnew.eq.1) then
            tmpstr(j:j) = str(i:i)
            j=j+1
         endif


 20      continue
c         write(*,*) i,j-1,n,isnew, '|',str(i:i),'-',isnew 
c     &        ,tmpstr(1:nblen(tmpstr))
      enddo
c      write(*,*) 'last n1:',n
c     NO blank right-side spaces exist:
      if (isnew.eq.1) then
         n=n+1
	 tmpstr(j:j) = str(i:i)
         strs(n)=tmpstr(1:j+1)
c         write(*,'(a4,i3,a30,i5)') 'new:',n, strs(n),j
      endif
c     ??? JUN-10-2007 Tian

c      write(*,*) '#parts:',n
c      do i=1, n
c         tmpstr=strs(i)
c         write(*,'(a,i9)') tmpstr(1:nblen(tmpstr)),i
c      enddo
      
      return
      end
