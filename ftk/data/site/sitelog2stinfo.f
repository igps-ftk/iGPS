CTITLE
      PROGRAM sitelog2stinfo

      IMPLICIT NONE
C      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--
c     create station.info from site log files

c     --ALGORITHM--

c     --EXAMPLE--
c     sitelog2stinfo /tianyf/trnj_missing.sit /tianyf/pbo/logs/ /tianyf/station.info.missing

c     --MODIFICATIONS--
c     Create by tyf on Tue Jan 15 07:29:31 CST 2013

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1023 sfile,path,ofile

c     --OUTPUT--

c     --EXTERNAL--
      integer*4 iargc,nblen


c     --Local Parameters--
      integer*4 nmaxsite,nsite,i,j,k,nf,fid,fido,err
      parameter(nmaxsite=10000)
      character*4 sites(nmaxsite),site,siteU
      character*1023 files(nmaxsite),file,filter

c     antenna & receiver information
      integer*4 nrcv,nant,nmax_ra,ri,ai,njd
      character*3 typRcvAnt
      parameter(nmax_ra=300)
      character*30 rcvs(nmax_ra),rcvts(nmax_ra),rcv,ant,sitename
      character*30 ants(nmax_ra),antts(nmax_ra),arps(nmax_ra)
      character*30 rcvSNs(nmax_ra),Vers(nmax_ra),antSNs(nmax_ra)
      character*30 domes(nmax_ra)
      character*30 htcod,Ver,RcvSN,dome,antSN
      real*8 SwVer,tmp,SwVers(nmax_ra),antNs(nmax_ra),antEs(nmax_ra)
      real*8 antN,antE
c     hts - antenna height
      integer*4 nmax_jd
      parameter(nmax_jd=600)
      real*8 hts(nmax_ra),ht,jds(nmax_jd),jd,jds_sort(nmax_jd),sec
      real*8 jds_rcv(nmax_ra),jds_ant(nmax_ra)
      integer*4 year,mon,day,hr,min,date(5),isec,date1(5),date2(5)
      integer*4 isec1,isec2,yr1,yr2,doy1,doy2
      integer*4 indx(nmax_jd),nindx

      character*1023 lines(10000),line,bufstr,tmpstr
      integer*4 nline
      

c     <<VAR_DEC
      if (iargc().lt.3) then
c         write(*,'(a)') 'ERROR: no enough input parameter!!'
         write(*,'(2a)') 'Usage: sitelog2stinfo sit_file(*.sit)',
     +        ' site_log_path ofile'
         write(*,'(a)') '    sit_file(*.sit) - iGPS site list file'
c         write(*,*) iargc()
         stop
      endif
c     decode command-line parameters
      call getarg(1,sfile)
      call getarg(2,path)
      call getarg(3,ofile)
      write(*,'(2a,1x,a)') '|<input: ', sfile(1:nblen(sfile)), 
     +     path(1:nblen(path))

c     read site list
      call rdsit_(sfile,sites,nmaxsite,nsite)
      write(*,'(a,i)') '__#site:',nsite
      write(*,'(16(1x,a4))'), (sites(i),i=1,nsite)

c     open output file and write header lines
      write(*,'(2a)') '|>output: ', ofile(1:nblen(ofile))
      call getlun(fido)
      open(unit=fido,file=ofile)
      tmpstr='# written by sitelog2stinfo program '
      write(fido,'(a)') tmpstr(1:nblen(tmpstr))
      tmpstr='* Reference file: none'
      write(fido,'(a)') tmpstr(1:nblen(tmpstr))
      tmpstr='* Site Log File : '
      write(fido,'(2a)') tmpstr(1:nblen(tmpstr)),path(1:nblen(path))
      tmpstr='*'
      write(fido,'(a)') tmpstr(1:nblen(tmpstr))
      tmpstr='*'
      write(fido,'(a)') tmpstr(1:nblen(tmpstr))
      tmpstr='*SITE  Station Name      Session Start      Session'//
     +     ' Stop       Ant Ht   HtCod  Ant N    Ant E    Receiver'//
     +     ' Type         Vers                  SwVer  Receiver SN'//
     +     '           Antenna Type     Dome   Antenna SN'
      write(fido,'(a)') tmpstr(1:nblen(tmpstr))

c     loop for site
      do i=1,nsite
         site=sites(i)
         write(*,'(2a)') '__*getting information for ',site

c     find site.log file for current site; & read
         filter=site//'*'
         call ffind(path,files,filter,nf,1) 
         write(*,'(a,i)') '__#file:',nf
c         write(*,*) files(1:nf)
         if (nf.le.0) then
            write(*,'(2a)') '__WARNING:no file found for ',site
            goto 700
         endif
         if (nf.gt.1) then
            write(*,'(3a)') '__WARNING:more than one file found for ',
     +           site,' 1st used!'
            nf=1
         endif
         file=files(1)
         write(*,'("__",a,1x,a)') site,file(1:nblen(file))
         call read_txt(file,lines,nline)
         write(*,'(a,i)') '__#line',nline

c     loop for each line to read receiver & antenna information
         nrcv=0
         nant=0
         typRcvAnt=''
         do j=1,nline
            bufstr=lines(j)
            call strtrim(bufstr,line)
c     output trimmed line for debug
c$$$            write(*,'(a)') line(1:nblen(line))

c     not dip further after section 5
            if (line(1:2).eq.'5.')  goto 702

c     skip comment lines
            if (line(1:1).eq.':') goto 701

c     site information
            if (index(line,'Site Name').ne.0) then
               call strtrim(line(index(line,':')+1:nblen(line)),
     +              tmpstr)
               sitename=tmpstr
            endif

c     receiver section
            if (line(1:2).eq.'3.') then
               typRcvAnt='rcv'
               if (line(3:3).eq.' ') goto 701
               if (line(3:3).eq.'x'.or.line(3:3).eq.'X') then
                  typRcvAnt=''
                  goto 701
               endif
               read(line(3:4),*,err=900) ri
               nrcv=nrcv+1
               tmpstr=line(index(line,':')+1:nblen(line))
               rcvs(ri)=tmpstr
            endif
            if (typRcvAnt.eq.'rcv') then
               if (index(line,'Date Installed').ne.0) then
c                  tmpstr=line(index(line,':')+1:nblen(line))
                  call strtrim(line(index(line,':')+1:nblen(line)),
     +                 tmpstr)
                  rcvts(ri)=tmpstr
               endif
               if (index(line,'Serial Number').ne.0) then
c                  tmpstr=line(index(line,':')+1:nblen(line))
                  call strtrim(line(index(line,':')+1:nblen(line)),
     +                 tmpstr)
                  rcvSNs(ri)=tmpstr
               endif
               if (index(line,'Firmware Version').ne.0) then
c                  tmpstr=line(index(line,':')+1:nblen(line))
                  call strtrim(line(index(line,':')+1:nblen(line)),
     +                 tmpstr)
                  Vers(ri)=tmpstr
                  read(tmpstr,*,iostat=err) SwVer
                  if (err.eq.0) then
                     SwVers(ri)=SwVer
                  else
                     write(*,'(2a)') '__invalid SwVer:',
     +                    tmpstr(1:nblen(tmpstr))
                  endif
               endif
            endif


c     antenna section
            if (line(1:2).eq.'4.') then
               typRcvAnt='ant'
               if (line(3:3).eq.' ') goto 701
               if (line(3:3).eq.'x'.or.line(3:3).eq.'X') then
                  typRcvAnt=''
                  goto 701
               endif
               read(line(3:4),*) ai
               nant=nant+1
               tmpstr=line(index(line,':')+1:nblen(line))
               ants(ai)=tmpstr
            endif            
            if (typRcvAnt.eq.'ant') then
               if (index(line,'Date Installed').ne.0) then
                  call strtrim(line(index(line,':')+1:nblen(line)),
     +                 tmpstr)
                  antts(ai)=tmpstr
c                  write(*,*) ai,tmpstr
               endif
               if (index(line,'Serial Number            :').ne.0) then
c                  tmpstr=line(index(line,':')+1:nblen(line))
                  call strtrim(line(index(line,':')+1:nblen(line)),
     +                 tmpstr)
                  antSNs(ai)=tmpstr
               endif
               if (index(line,'Antenna Radome Type').ne.0) then
c                  tmpstr=line(index(line,':')+1:nblen(line))
                  call strtrim(line(index(line,':')+1:nblen(line)),
     +                 tmpstr)
                  domes(ai)=tmpstr
               endif
               if (index(line,'Antenna Reference Point').ne.0) then
                  call strtrim(line(index(line,':')+1:nblen(line)),
     +                 tmpstr)
                  arps(ai)=tmpstr
c                  write(*,*) ai,tmpstr
               endif
               if (index(line,'ARP North Ecc').ne.0) then
                  call strtrim(line(index(line,':')+1:nblen(line)),
     +                 tmpstr)
c                  write(*,'("|",a,"|")') tmpstr(1:nblen(tmpstr))
c     for the case of Marker->ARP North Ecc(m): (F8.4)
                  if (index(tmpstr,'(').ne.0) then
                     do k=index(tmpstr,'('),index(tmpstr,')')
                        tmpstr(k:k)=' '
                     enddo
                  endif
c     remove unit in value side
                  k=index(tmpstr,'m')
                  if (k.ne.0) tmpstr(k:k)=' '
c                  write(*,'("|",a,"|",i)') tmpstr(1:nblen(tmpstr)),
c     +                 nblen(tmpstr)
                  if (nblen(tmpstr).eq.0) tmpstr='0'
c     sometimes, the value of tmpstr is strange (e.g. mfp0)!
c                  tmpstr='-'
c                  write(*,*)nblen(tmpstr),"-|",
c     +                 tmpstr(1:nblen(tmpstr)),
c     +                 '|-',
c     +                 nblen(tmpstr)  
                  read(tmpstr,*) tmp
                  antNs(ai)=tmp
c                  write(*,*) ai,tmpstr
               endif
               if (index(line,'ARP East Ecc').ne.0) then
                  call strtrim(line(index(line,':')+1:nblen(line)),
     +                 tmpstr)
                  if (index(tmpstr,'(').ne.0) then
                     do k=index(tmpstr,'('),index(tmpstr,')')
                        tmpstr(k:k)=' '
                     enddo
                  endif
                  k=index(tmpstr,'m')
                  if (k.ne.0) tmpstr(k:k)=' '
                  if (nblen(tmpstr).eq.0) tmpstr='0'
                  read(tmpstr,*) tmp
                  antEs(ai)=tmp
c                  write(*,*) ai,tmpstr
               endif
               if (index(line,'ARP Up Ecc').ne.0) then
                  call strtrim(line(index(line,':')+1:nblen(line)),
     +                 tmpstr)
                  if (index(tmpstr,'(').ne.0) then
                     do k=index(tmpstr,'('),index(tmpstr,')')
                        tmpstr(k:k)=' '
                     enddo
                  endif
                  k=index(tmpstr,'m')
                  if (k.ne.0) tmpstr(k:k)=' '
                  if (nblen(tmpstr).eq.0) tmpstr='0'
c                  write(*,*) ai,'|',tmpstr(1:nblen(tmpstr)),'|',
c     +                 nblen(tmpstr)
                  read(tmpstr,*) tmp
                  hts(ai)=tmp
c                  write(*,*) ai,tmp
               endif
            endif
          
c     next line
 701        continue
         enddo

 702     continue

c$$$         write(*,*) '#rcv:',nrcv
c$$$         do j=1,nrcv
c$$$            write(*,'(2a)') rcvs(j), rcvts(j)
c$$$         enddo
c$$$         write(*,*) '#ant:',nant
c$$$         do j=1,nant
c$$$            write(*,'(2a)') ants(j), antts(j)
c$$$         enddo
        
c     find unique dates (sort then uniq)
         njd=nrcv+nant
         do j=1,nrcv
            tmpstr=rcvts(j)
c$$$            write(*,'("|",a,"|")') tmpstr(1:nblen(tmpstr))
c            read(tmpstr,'(i4,1x,i2,1x,i2,1x,i2,1x,i2)') year,mon,day,
c     +           hr,min
            read(tmpstr,'(i4,1x,i2,1x,i2,1x,i2,1x,i2)',err=900) year,
     +           mon,day,hr,min
c            write(*,*) year,mon,day,hr,min
            date(1)=year
            date(2)=mon
            date(3)=day
            date(4)=hr
            date(5)=min
            call ymdhms_to_jd(date,0.d0,jd)
            jds(j)=jd
            jds_rcv(j)=jd
         enddo
         do j=1,nant
            tmpstr=antts(j)
c$$$            write(*,'("|",a,"|")') tmpstr(1:nblen(tmpstr))
            read(tmpstr,'(i4,1x,i2,1x,i2,1x,i2,1x,i2)',err=900) year,
     +           mon,day,hr,min
c            write(*,*) year,mon,day,hr,min
            date(1)=year
            date(2)=mon
            date(3)=day
            date(4)=hr
            date(5)=min
            call ymdhms_to_jd(date,0.d0,jd)
            jds(j+nrcv)=jd
            jds_ant(j)=jd
         enddo
c         write(*,*) (jds(j),j=1,njd)
         call sort_r8(jds,nmax_jd,njd,jds_sort)
c         write(*,*) (jds_sort(j),j=1,njd)

         call uniq(jds_sort,nmax_jd,njd,indx,nindx)
c         write(*,*) (jds_sort(indx(j)),j=1,nindx)
         

c     for each unique date, write receiver & antenna information
         do j=1,nindx
c     receiver info for current date
            do k=1,nrcv
               if (jds_sort(indx(j)).eq.jds_rcv(k)) then
                  call strtrim(rcvs(k),rcv)
                  rcvSN=rcvSNs(k)
                  SwVer=SwVers(k)
                  Ver=Vers(k)
               endif
            enddo
c     antenna info for current date
            do k=1,nant
               if (jds_sort(indx(j)).eq.jds_ant(k)) then
                  call strtrim(ants(k),ant)
                  call strtrim(arps(k),htcod)
                  if (htcod.eq.'BPA'.or.htcod.eq.'TOP'.or.
     +                 htcod.eq.'BAM') then
                     htcod='DHPAB'
                  elseif (htcod.eq.'ARP'.or.htcod.eq.'BCR'.or.
     +                    htcod.eq.'TCR'.or.htcod.eq.'BGP') then
                     htcod='DH'//htcod(1:3)
                  else
                     htcod='DHPAB'
                  endif
                  ht=hts(k)
                  antN=antNs(k)
                  antE=antEs(k)
                  call strtrim(antSNs(k),antSN)
                  dome=domes(k)
               endif
            enddo

c     convert JD to DOY
            jd=jds_sort(indx(j))
            call jd_to_ymdhms(jd,date1,sec)
            call jd_to_yds (jd, yr1, doy1, isec1 )
            if (j.lt.nindx) then
               jd=jds_sort(indx(j+1))
               call jd_to_ymdhms(jd,date2,sec)
               call jd_to_yds (jd, yr2, doy2, isec2 )
            else
               date2(1)=9999
               doy2=999
               date2(4)=0
               date2(4)=0
               isec2=0
            endif
            isec=int(sec)

c            htcod='zzzzz'
c            Vers='a'
c            SwVer=5.2
c            RcvSN='c'
c            dome='NONE'
c            antSN='-'
            siteu=site
            call uppers(siteU)

c     output station.info record for current date
            write(fido,600) siteU,sitename,
     +           date1(1),doy1,date1(4),date1(5),0,
c     +           date1(1),doy1,date1(4),date1(5),isec1
     +           date2(1),doy2,date2(4),date2(5),0,
     +           ht,htcod,antN,antE,rcv,Ver,SwVer,RcvSN,ant,dome,antSN

 600        format(1x,a4,2x,a16,2x,i4,1x,i03,1x,i02,1x,i02,1x,i02,2x,
     +           i4,1x,i03,1x,i02,1x,i02,1x,i02,
     +           2x,f7.4,2x,a5,2x,f7.4,2x,f7.4,2x,a20,2x,a20,2x,f5.2,2x,
     +           a20,2x,a15,2x,a5,2x,a20)
c            write(*,'("|",a21,"|",a21)') rcv,'a'
         enddo

c     if any errors occur in the above steps, skip current site
c     (not finished yet)
         if (0.eq.1) then
 900        write(*,'(2a)') '__ERROR:',
     +           line(1:nblen(line))
         endif
c         stop
 700     continue
      enddo
      close(fido)


      write(*,'(2a)') '|>results written to: ', ofile(1:nblen(ofile))
      
      STOP
      END
