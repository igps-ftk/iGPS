CTITLE
      PROGRAM sar_intf_tab_2_psxy

      IMPLICIT NONE
C      INCLUDE '../../../inc/ftk.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
c     intf.tab, baseline_tab.dat
      character*1023 ifile,bfile
      integer*4 d1,d2,y1,y2
      character*1023 y

c     --OUTPUT--
      character*1023 ofile

c     --EXTERNAL--
      integer*4 nblen,wheres

c     --Local Parameters--
      integer*4 ny_max,nf_max,ns_max,nmax
      parameter(ny_max=20)
      parameter(nf_max=20000)
      parameter(ns_max=500)
      parameter(nmax=500)
      character*1023 tmpstr,tmpstrs(ns_max)
      character*1023 strs(ny_max)
      integer*4 nstr,nf
      integer*4 i,j,k,pos
      integer*4 np,n
      integer*4 year1,doyr1,year2,doyr2
      real*8 dyr1,dyr2,sec1,sec2,sec,jd1,jd2
      integer*4 secs(nmax)
      character*1023 file_intf,file_scene
      character*1023 lines_intf(nf_max),lines_scene(nf_max),line
      integer*4 nl_intf,nl_scene
      integer*4 fido,ioerr
      character*15 id
      character*7 id1,id2,ids(nmax)
      real*8 blens(nmax),blen
      character*1 sep
     

c     <<VAR_DEC

      sep=' '
      
      ifile='intf.tab'
      bfile='baseline_table.dat'
      ofile='intf_tab.psxy'
      

      do i=1,iargc()
         call getarg(i,tmpstr)
c         write(*,*) tmpstr(1:nblen(tmpstr))
         pos=index(tmpstr,'=')
c         write(*,*) 'pos:',pos
         if (pos.le.0) then
            write(*,*) '[]ERROR:invalid parameter(',
     +        tmpstr(1:nblen(tmpstr)),')!!'
            stop
         endif

         if (tmpstr(1:pos).eq.'--i=') then
            ifile=tmpstr(pos+1:)
         else if (tmpstr(1:pos).eq.'--b=') then
            read(tmpstr(pos+1:),*) bfile
         else if (tmpstr(1:pos).eq.'--o=') then
            read(tmpstr(pos+1:),*) ofile
         else
            write(*,*) '[]ERROR: invlaid parameter(',
     +           tmpstr(1:nblen(tmpstr)),')!!'
            stop
         endif
      enddo

      write(*,*) 'ifile: ',ifile(1:nblen(ifile))
      write(*,*) 'bfile: ',bfile(1:nblen(bfile))
      write(*,*) 'ofile: ',ofile(1:nblen(bfile))


c     read intf.tab
c     ./ia/2016217_2016265/unwrap_mask.grd ./ia/2016217_2016265/corr_cut.grd 2016217 2016265 -76.4447
c$$$      file_intf='/g7e/gsar/172-a-m4-0082_0087_0092_0097-jiali5'//
c$$$     +     '_chayu3/f123/intf.tab1'
c      file_intf='intf.tab1'
      call read_txt(ifile,lines_intf,nl_intf)
c      do i=1,nl_intf
c         line=lines_intf(i)
c         write(*,*) line(1:nblen(line))
c      enddo

c     read baseline_table.dat
c     S1_20150119_ALL_F1 2015018.0348288198 383 27.374668002346 83.122753390508
      call read_txt(bfile,lines_scene,nl_scene)
      do i=1,nl_scene
         line=lines_scene(i)
         call strsplit(line,sep,nstr,tmpstrs)
         tmpstr=tmpstrs(2)
         ids(i)=tmpstr
         read(tmpstr(8:),*) sec
         secs(i)=sec*24d0*60*60d0
         read(tmpstrs(5),*) blen
         blens(i)=blen
c         do j=1,i
c            write(*,*) ids(j),secs(j),blens(j)
c         enddo
c         write(*,*) ids(i),secs(i),blens(i)
c        write(*,*) line(1:nblen(line))
      enddo

c      do i=1,nl_scene
c         write(*,*) ids(i),secs(i),blens(i)
c      enddo

c      stop


      call getlun(fido)
      open(unit=fido,file=ofile,iostat=ioerr)
      if (ioerr.ne.0) then
        write(*,'(5a)') '[]ERROR:cannot open output file (',
     +    ofile(1:nblen(ofile)),')!!'
        stop
      endif
      

c      do i=1,nl_scene
c         write(*,*) ids(i),secs(i),blens(i)
c      enddo
c      stop
 
      do i=1,nl_intf
         line=lines_intf(i)
c         write(*,*) line(1:nblen(line))
         call strsplit(line,sep,nstr,tmpstrs)
         id1=tmpstrs(3)
         id2=tmpstrs(4)
c         write(*,*) nstr
c         write(*,*) 'id1:',id1,' id2:',id2
         j=wheres(ids,nmax,nl_scene,id1)         
c         write(*,*) j,ids(j),blens(j)
         k=wheres(ids,nmax,nl_scene,id2)
c         write(*,*) k,ids(k),blens(k)
         sec1=secs(j)
         sec2=secs(k)
         
         read(id1(1:4),*) year1
         read(id1(5:),*) doyr1
         read(id2(1:4),*) year2
         read(id2(5:),*) doyr2
         
         call yds_to_jd(year1,doyr1,sec1,jd1)
         call yds_to_jd(year2,doyr2,sec2,jd2)
         call jd_to_decyrs(jd1,dyr1)
         call jd_to_decyrs(jd2,dyr2)
c         write(*,*) year1,doyr1,sec1,year2,doyr2,sec2,jd1,jd2,dyr1,dyr2
         
         write(fido,'(4a)') '> -L',id1,'_',id2
         write(fido,*) dyr1, blens(j)
         write(fido,*) dyr2, blens(k)
         
      enddo

      close(fido)

      write(*,'(1a)') '[]INFO: normal end.'
      STOP
      END
