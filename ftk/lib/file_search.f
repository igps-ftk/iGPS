c     find files in directory.

      subroutine file_search(path,files,filter,n,incsub,foldcase) 

c     input parameters
      character*(*) path
      character*(*)  files(*)
      character*(*) filter
      integer*4 n, incsub,foldcase

c     temp file name
      character*512 tfname
      character*512 bufline

      integer*4 fidt,fid,ioerr
      character*512 bufcmd,optname


c      write(*,*) 'filter:',filter

      tfname='/tmp/tfile.tmp'  
      tfname='.tfile'

      if(foldcase.eq.1) then
        optname='-iname'
      else
        optname='-name'
      endif

c     list files to temp file
      if (incsub.eq.1) then
      bufcmd='find '//path(1:index(path,' '))
c     &     //' -name "'//filter(1:index(filter,' ')-1)
c     &     //' -name "'//filter(1:nblen(filter))
     &     //' '//optname(1:nblen(optname))//' "'
     &     //filter(1:nblen(filter))

     &     //'" |sort > '//tfname(1:index(tfname,' '))
      else
      bufcmd='find '//path(1:index(path,' '))
     &     //' -maxdepth 1 '
c     &     //' -name "'//filter(1:index(filter,' ')-1)
     &     //' '//optname(1:nblen(optname))//' "'
     &     //filter(1:index(filter,' ')-1)

     &     //'"  |sort> '//tfname(1:index(tfname,' '))
      endif

      write(*,'(A)') 'Output to temp file: ', bufcmd
      call system(bufcmd)

c     read temp file
c      bufcmd='cat '//tfname(1:index(tfname,' '))
c      call system(bufcmd)

      fid=11
      open(unit=fid,file=tfname,status='old',iostat=ioerr)
      if (ioerr.ne.0) then
         write(*,*) 'open tmp file error.'
         stop
      endif
      read(fid,'(A)',iostat=ioerr) bufline
      n=0
      do while(ioerr.eq.0)
c         write(*,*) bufline(1:index(bufline,' ')-1)
c         write(*,*) n, len(bufline),len(files(n))
         n=n+1
         files(n)=bufline
         read(fid,'(A)',iostat=ioerr) bufline
      end do
      close(fid)
      

c     delete temp file
      bufcmd='rm -rf '//tfname(1:index(tfname,' '))
      call system(bufcmd)
      
      end


      
