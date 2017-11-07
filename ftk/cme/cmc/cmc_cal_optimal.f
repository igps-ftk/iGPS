CTITLE
      PROGRAM cmc_cal_optimal

      IMPLICIT NONE
      INCLUDE '../../inc/cgps.h'

c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --INPUT--
c     --Command-line Parameters--
      character*1023 path,netFile

c     --OUTPUT--
      character*1023 opath

c     --EXTERNAL--
      integer*4 iargc,nblen,status,system,intlen

c     --Local Parameters--
      character*1023 prog
      character*10230 cmdstr

      character*1023 snxfile,corrFile,tpath
      character*30 orgext,netext


      character*4 sites_corr(nmax_sites)
      integer*4 nsit_corr,i,j,k
      real*8 corr(nmax_sites,nmax_sites,3),llh(3,nmax_sites)
      real*8 blen_deg(nmax_sites,nmax_sites)
      real*8 blen_km(nmax_sites,nmax_sites)

c     minimum distance
      real*8 dmin
c     minumum number of CMC sites
      integer*4 nmin
c     grid-searching parameters
      integer*4 nTau,nW
      parameter(nTau=11)
      parameter(nW=11)
      real*8 taus(nTau),Ws(nW)

c     files searching
      character*1023 filter,file,files(nmax_sites),fmtstr
      integer*4 nf,fi,perc,nsit,neui
      integer*4 nrows(nmax_sites),ncols(nmax_sites)
      integer*4 nrow, ncol, nhead
      character*10240 headers(nmax_head),tmpstr
      real*8 data_all(nmax_row,nmax_col,nmax_proc)
      real*8 data(nmax_row,nmax_col)
      character*4 sites(nmax_sites),site
      real*8 jdmins(nmax_sites),jdmaxs(nmax_sites),jdmin,jdmax
      real*8 jd,jds(nmax_row),dyrs(nmax_row),dyr
      integer*4 yrs(nmax_row),doyrs(nmax_row)
      integer*4 yr,doyr,sec,date(5),ndays

c     CMC calculation
      integer*4 ind_s
      real*8 llh_cur(3)
      real*8 sblen(nmax_row,nmax_col),scorr(nmax_row,nmax_col,3)
      character*4 ssites(nmax_sites)
      integer*4 nsit_cmc
      real*8 sllh(3,nmax_sites)

c     derive CMC weights
      real*8 flt_rmss(nTau,nW),tau,w
      real*8 w_ij(nmax_sites),w_ij_s(nmax_sites)
      integer*4 inds(nmax_sites)
      real*8 sum_w_ij
      integer*4 n

      character*4 csites(nmax_sites)
      real*8 cblen(nmax_sites)
      real*8 ccorr(nmax_sites)
      real*8 c_w_ij(nmax_sites)
      integer*4 ncmc
      real*8 cllh(3,nmax_sites)

      integer*4 taui,wi
      real*8 llh_ij(nmax_sites,3),d_ij(nmax_sites),c_ij(nmax_sites,3)
      character*4 sites_ij(nmax_sites)

      real*8 ts(nmax_row,nmax_sites),sigmas(nmax_row,nmax_sites)

      real*8 cmc(nmax_row)
      integer*4 dayNsits(nmax_row)

c     filtered residual
      real*8 rmsi,flt_rms_min
      integer*4 nepoch

c     CMC
c      real*8 cmc_all(nmax_row,3,nmax_sites)
c      integer*4 ndays_all(nmax_row,3,nmax_sites)
      real*8 cmcs(nmax_row,3),flts(nmax_row,3)

c     output file
      integer*4 fid
      character*1023 ofile,opath_cmc,opath_flt

      integer*4 sitid
      integer*4 pos(nmax_sites)

      character*1 neustr(3)

c     <<VAR_DEC

      prog='CMC_CAL_OPTIMAL'

      if (iargc().lt.3) then
         write(*,'(3a)') 'Usage: ',prog(1:nblen(prog)), 
     +        ' path netFile opath'
c         write(*,'(a)') '         [--netfile=NET_FILE]'
         write(*,'(2x,a)') 'Array limits of this program are'
         write(*,'(4x,a,i9)') 'Max number of sites: ',nmax_sites
         write(*,'(4x,a,i9)') 'Max number of processing:',nmax_proc
         write(*,'(4x,a,i9)') 'Max number of rows:',nmax_row
         write(*,'(4x,a,i9)') 'Max number of columns:',nmax_col
         write(*,'(4x,a,i9)') 'Max number of headers:',nmax_head
         write(*,'(2x,a)') 'Please edit those variables accordingly.'
         write(*,'(2a)') '(c)Yunfeng Tian      tianyf@gmail.com',
     .        '      \\/ http://gps.xinbaibaihuo.com'
         stop
      endif

      write(*,'(4a)') '[',prog(1:nblen(prog)), ']Program started ...'

c     initial values
c     include all stations in the derivation of CMC
      dmin=0d0
c     Only use distances greater than 3.5 degrees.
c$$$      dmin=3.5d0
c     
      nmin=3
c     
      do i=1,nTau
         taus(i)=1+(i-1)*5
      enddo
      do i=1,Nw
         Ws(i)=1+(i-1)*5
      enddo
c$$$      write(*,*) 'taus:', (taus(i),i=1,nTau)
c$$$      write(*,*) 'ws:',(ws(i),i=1,nw)
c$$$      stop

c     SIO/NEU file type
      filter='*.neu'

      neustr(1)='N'
      neustr(2)='E'
      neustr(3)='U'
      netext='NEU'
      orgext='CMC'

c     Set data_all to -9999
      do i=1,nmax_row
         data_all(i,1,1)=-9999
      enddo

c     command-line inputs
      call getarg(1,path)
      write(*,'(4a)') '[',prog(1:nblen(prog)),']Input: ',
     +     path(1:nblen(path))
      call getarg(2,netFile)
      write(*,'(4a)') '[',prog(1:nblen(prog)),']A priori coordinate: ',
     +     netFile(1:nblen(netFile))
      call getarg(3,opath)
      write(*,'(4a)') '[',prog(1:nblen(prog)),']Output to: ',
     +     opath(1:nblen(opath))

c     **Get extra command-line parameters.
      do i=4,iargc()
         call getarg(i,tmpstr)
         if (tmpstr(1:7).eq.'--dmin=') then
            read(tmpstr(8:nblen(tmpstr)),*) dmin
            write(*,'(3a,1x,f9.3)') '[',prog(1:nblen(prog)),
     +           ']User input dmin:',dmin
         else if (tmpstr(1:9).eq.'--filter=') then
            filter=tmpstr(10:nblen(tmpstr))
         else if (tmpstr(1:9).eq.'--netext=') then
            netext=tmpstr(10:nblen(tmpstr))
         else if (tmpstr(1:9).eq.'--orgext=') then
            orgext=tmpstr(10:nblen(tmpstr))  
         else
            write(*,'(5a)') '[',prog(1:nblen(prog)),
     +           ']ERROR: invalid parameter -',
     &           tmpstr(1:nblen(tmpstr)),'-.'
            stop
         endif
      enddo

c      stop
c     First, calling "ts_corr" to calculate correlation coefficients.
      tpath=opath(1:nblen(opath))//pathsep//'corr'
      cmdstr='mkdir -p '//tpath(1:nblen(tpath))
      status=system(cmdstr)
c      write(*,*) cmdstr(1:nblen(cmdstr))

c      snxfile='FOR_CMC.snx'
      write(cmdstr,701) 'ts_corr',path(1:nblen(path)),
     +     tpath(1:nblen(tpath)),netfile(1:nblen(netfile)),
     +     orgext(1:nblen(orgext)),netext(1:nblen(netext))
 701  format(a,2(1x,a),1x,"--coordsfile=",a,1x,"--orgext=",a,
     +     1x,"--netext=",a)
c      write(*,*) cmdstr(1:nblen(cmdstr))
      status=system(cmdstr)

      write(*,'(3a)') '[',prog(1:nblen(prog)),']reading corr matrix ...'
      snxfile=tpath(1:nblen(tpath))//pathsep//
     +     netext(1:nblen(netext))//'_'//
     +     orgext(1:nblen(orgext))
     +     //'_neu.snx'
c      write(*,*) 'snxfile:',snxfile(1:nblen(snxfile))
      call read_corr_snx(snxfile,sites_corr,nsit_corr,corr,blen_deg,
     +     blen_km,llh)
c      write(*,*) '#corr sites:',nsit_corr
c$$$      write(*,*) 'Corr North:'
c$$$      do i=1,nsit
c$$$         write(*,'(1000(1x,f5.2))') (corr(i,j,1),j=1,nsit)
c$$$      enddo
c$$$      write(*,*) 'Corr East:'
c$$$      do i=1,nsit
c$$$         write(*,'(1000(1x,f5.2))') (corr(i,j,2),j=1,nsit)
c$$$      enddo
c$$$      write(*,*) 'Corr Up:'
c$$$      do i=1,nsit
c$$$         write(*,'(1000(1x,f5.2))') (corr(i,j,3),j=1,nsit)
c$$$      enddo
c$$$      write(*,*) 'Blen Deg:'
c$$$      do i=1,nsit
c$$$         write(*,'(1000(1x,f5.2))') (blen_deg(i,j),j=1,nsit)
c$$$      enddo
c$$$      write(*,*) 'Blen Km:'
c$$$      do i=1,nsit
c$$$         write(*,'(1000(1x,f8.2))') (blen_km(i,j),j=1,nsit)
c$$$      enddo

c     Read all data files
      call ffind (path,files,filter,nf,1) 
      if (nf.le.0) then
         write(*,'(5a)') '[',prog(1:nblen(prog)),
     +        ']WARNING: no time series files found in "',
     +        path(1:nblen(path)),'"!'
         stop
      endif

      write(*,'(3a)') '[',prog(1:nblen(prog)),']reading all files ...'
      write(*,'(3a,i6)') '[',prog(1:nblen(prog)),']#files:',nf
      write(fmtstr,'(3a,i1,a)')  '("[',prog(1:nblen(prog)),
     +     ']Reading ",a,"(",i',
     &     intlen(nf),
     &     ',"/",i3,"%)..")' 
      jdmin=-1
      jdmax=-1
      do fi=1,nf
         file=files(fi)
         call getfilename(file,tmpstr)
         perc=int(fi*1.d0/nf*100)
         write(*,fmtstr) 
     &        tmpstr(1:nblen(tmpstr)),
     &        fi,
     &        perc
         call read_sio(file,data,nrow,ncol,nhead,headers)
         nrows(fi)=nrow
         ncols(fi)=ncol
c         write(*,*) '#rows:',nrow,'    #cols:',ncol
         do i=1,nrow
            do j=1,ncol
               data_all(i,j,fi)=data(i,j)
            enddo
c            write(*,'(9f11.5)') (data(i,j),j=1,ncol)
         enddo
         call getfilename(file, site)
         sites(fi)=site(1:4)
c$$$         write(*,'(a)') site(1:4),sites(fi)
c$$$         do i=nrow-5,nrow
c$$$            write(*,'(9f11.5)') (data(i,j),j=1,ncol)
c$$$         enddo
c$$$         stop

c     convert date to MJD
         yr=data(1,2)
         doyr=data(1,3)
         sec=43200
         call yds_to_jd(yr,doyr,sec,jd)
         jdmins(fi)=jd
         if (jdmin.eq.-1.or.jdmin.gt.jd) jdmin=jd
c         write(*,*) 'debug:',yr,doyr,sec,jd
         yr=data(nrow,2)
         doyr=data(nrow,3)
         call yds_to_jd(yr,doyr,sec,jd)
         jdmaxs(fi)=jd
         if (jdmax.eq.-1.or.jdmax.lt.jd) jdmax=jd
c         write(*,*) 'debug:',yr,doyr,sec,jd
c         write(*,*) 'jds:',jdmins(fi),jdmaxs(fi),jdmin,jdmax

      enddo
      ndays=jdmax-jdmin+1
      write(*,'(3a,i6)') '[',prog(1:nblen(prog)),']#days:', ndays
      do i=1,ndays
         jds(i)=jdmin+i-1
         call jd_to_decyrs(jds(i),dyr)
         dyrs(i)=dyr
         yrs(i)=int(dyr)
         call jd_to_yds(jds(i),yr,doyr,sec)
         doyrs(i)=doyr
      enddo

c     Reformat the data_all array
      do fi=1,nf
c     initial data3 time column
         do i=1,nmax_row
            do j=1,ncols(fi)
               data(i,j)=-9999
            enddo
         enddo
c     fit data3 with each data line in data_all for individual file
c         write(*,*),fi,nrows(fi),ncols(fi)
         do i=1,nrows(fi)
            yr=data_all(i,2,fi)
            doyr=data_all(i,3,fi)
            call yds_to_jd(yr,doyr,sec,jd)
c            write(*,*) i,yr,doyr,jd
            do j=1,ndays
               if (jd.eq.jds(j)) goto 704
            enddo
 704        continue
            do k=1,ncols(fi)
               data(j,k)=data_all(i,k,fi)
            enddo
         enddo
         
c     copy back data to data_all file section
         do i=1,ndays
            do j=1,ncols(fi)
               data_all(i,j,fi)=data(i,j)
            enddo
         enddo

c$$$c      test output to temporary file
c$$$         open(unit=100,file='/home/tianyf/tmp/11.txt')
c$$$         do i=1,ndays
c$$$            write(100,'(1000(f11.5,1x))') (data_all(i,j,1),j=1,
c$$$     +           ncols(1))
c$$$         enddo
c$$$         close(100)
c         write(*,*) files(10)
c         stop
      enddo

c     Create output directories
      opath_cmc=opath(1:nblen(opath))//pathsep//'CMC'
      cmdstr='mkdir -p '//opath_cmc(1:nblen(opath_cmc))
      status=system(cmdstr)
      opath_flt=opath(1:nblen(opath))//pathsep//'Flt'
      cmdstr='mkdir -p '//opath_flt(1:nblen(opath_flt))
      status=system(cmdstr)
 
c     Calculate CMC time series for each site
      write(*,'(3a)') '[',prog(1:nblen(prog)),']Calcualte CMC ...'
c      do fi=2,2
      sitid=39
      sitid=1
      sitid=107
c      do fi=sitid,sitid
c      do fi=1,nf
      do fi=1,10
         site=sites(fi)
         nhead=0

         ind_s=-1
         do i=1,nsit_corr
            if (site.eq.sites_corr(i)) then
               ind_s=i
               goto 706
            endif
         enddo
 706     continue
         if (ind_s.eq.-1) then
            write(*,801) '[',prog(1:nblen(prog)),']No correlations for',
     +           site,';skipped!'
 801        format(3a,1x,a,1x,a)
            goto 705
         endif

         llh_cur(1)=llh(1,ind_s)
         llh_cur(2)=llh(2,ind_s)
         llh_cur(3)=llh(3,ind_s)
c         write(*,*) site,llh_cur,ind_s

c     
         nsit_cmc=0
         do i=1,nsit_corr
            if (blen_deg(ind_s,i).gt.dmin) then
               nsit_cmc=nsit_cmc+1
               pos(nsit_cmc)=i
               d_ij(nsit_cmc)=blen_deg(ind_s,i)
               c_ij(nsit_cmc,1)=corr(ind_s,i,1)
               c_ij(nsit_cmc,2)=corr(ind_s,i,2)
               c_ij(nsit_cmc,3)=corr(ind_s,i,3)
               sites_ij(nsit_cmc)=sites_corr(i)
               llh_ij(nsit_cmc,1)=llh(1,i)
               llh_ij(nsit_cmc,2)=llh(2,i)
               llh_ij(nsit_cmc,3)=llh(3,i)
c$$$               write(*,*) sites_ij(nsit_cmc),llh_ij(nsit_cmc,1),
c$$$     +              llh_ij(nsit_cmc,2),llh_ij(nsit_cmc,3) 
c     +              ,len_deg(ind_s,i),ind_s
            endif
         enddo
c         write(*,*) 'blen:',(d_ij(i),i=1,nsit_cmc)
c         write(*,*) 'corr n:',(c_ij(i,1),i=1,nsit_cmc)
c         write(*,*) 'corr e:',(c_ij(i,2),i=1,nsit_cmc)
c         write(*,*) 'corr u:',(c_ij(i,3),i=1,nsit_cmc)

c         stop
         do neui=1,3
            write(*,*) 'neui:',neui
            do taui=1,nTau
               tau=Taus(taui)
c               write(*,*) 'tau:',taui,tau
               do i=1,nsit_cmc
                  w_ij(i)=c_ij(i,neui)*exp(-1d0*d_ij(i)**2/
     +                 tau**2)
c$$$                     write(*,'(i,1x,e,4(1x,f9.2))') i,w_ij(i),w,tau,
c$$$     +                    c_ij(i,neui),
c$$$     +                    d_ij(i)
               enddo
c                  write(*,*) 'w_ij:',(w_ij(i),i=1,nsit_cmc)
               call sort_r8(w_ij,nmax_sites,nsit_cmc,w_ij_s,inds)
c                  write(*,*) 'w_ij_s:',(w_ij_s(i),i=1,nsit_cmc)
c                  stop

               do wi=1,nW
                  w=Ws(wi)
                  flt_rmss(taui,wi)=-9999

                  sum_w_ij=0d0
                  do n=nsit_cmc,nmin,-1
                     sum_w_ij=sum_w_ij+w_ij_s(n)
c                     write(*,*) 'n:',n,sum_w_ij
                     if (sum_w_ij.ge.w) then
                        goto 707 
                     endif
c     Next W
                     if (n.lt.nmin) then
c                        write(*,*) 'out:',n,nmin,sum_w_ij,w,tau
                        goto 708
                     endif
                  enddo
 707              continue
c                  write(*,*) 'ok:',n,nmin,sum_w_ij,w,tau
                  
                  do i=n,nsit_cmc
c                     csites(i-n+1)=sites_ij(inds(i))
c                     write(*,*) i,inds(i),csites(i)
c                     cblen(i-n+1)=d_ij(inds(i))
                     c_w_ij(i-n+1)=w_ij_s(i)
c$$$                     cllh(i-n+1,1)=llh_ij(inds(i),1)
c$$$                     cllh(i-n+1,2)=llh_ij(inds(i),2)
c$$$                     cllh(i-n+1,3)=llh_ij(inds(i),3)
                     do j=1,ndays
                        ts(j,i-n+1)=data_all(j,neui+3,pos(inds(i)))
                        sigmas(j,i-n+1)=data_all(j,neui+6,pos(inds(i)))
                     enddo
                  enddo
                  

                  call cmc_form(nsit_cmc-n+1,ndays,
     +                 ts,sigmas,c_w_ij,cmc,
     +                 dayNsits)

c$$$                  do i=1,ndays
c$$$                     write(*,802) dyrs(i),yrs(i),doyrs(i),cmc(i),cmc(i),
c$$$     +                    cmc(i)
c$$$c     +                    ,ts(i,1),sigmas(i,1),
c$$$c     +                    dayNsits(i)
c$$$ 802                 format(1x,f10.5,1x,i04,1x,i03,3(1x,f9.5))
c$$$                  enddo
c$$$                  stop
                  
                  nepoch=0
                  rmsi=0d0
                  do i=1,ndays
                     if (data_all(i,neui+3,fi).ne.-9999) then
                        nepoch=nepoch+1
                        rmsi=rmsi+(data_all(i,neui+3,fi)-cmc(i))**2
c                        write(*,*) rmsi,data_all(i,neui+3,fi),cmc(i)
                     endif
                  enddo
c                  stop
                  if (nepoch.gt.0) then
                     rmsi=dsqrt(rmsi/nepoch)
                  endif
c                  write(*,*) 'rms:', rmsi,w,tau,n,nepoch
                  flt_rmss(taui,wi)=rmsi
c                  stop
 708              continue
c     stop
c     END-of-W-loop
               enddo
c     END-of-Tau-loop
            enddo

            flt_rms_min=9d30
            do taui=1,ntau
               do wi=1,nw
                  if (flt_rmss(taui,wi).ne.-9999) then
                     if (flt_rmss(taui,wi).lt.flt_rms_min) then
                        flt_rms_min=flt_rmss(taui,wi)
                        tau=taus(taui)
                        w=ws(wi)
                     endif
                  endif
               enddo
            enddo
c            write(*,*) 'minumum rms:',flt_rms_min,tau,w
c            stop

            do i=1,nsit_cmc
               w_ij(i)=c_ij(i,neui)*exp(-1d0*d_ij(i)**2/
     +              tau**2)
c                     write(*,*) i,w_ij(i),w,tau,c_ij(i,neui),
c     +                    d_ij(i)
            enddo
c                  write(*,*) 'w_ij:',(w_ij(i),i=1,nsit_cmc)
            call sort_r8(w_ij,nmax_sites,nsit_cmc,w_ij_s,inds)
c                  write(*,*) 'w_ij_s:',(w_ij_s(i),i=1,nsit_cmc)
c                  stop
            
            sum_w_ij=0d0
            do n=nsit_cmc,nmin,-1
               sum_w_ij=sum_w_ij+w_ij_s(n)
c                     write(*,*) 'n:',n,sum_w_ij
               if (sum_w_ij.ge.w) then
                  goto 710
               endif
            enddo
 710        continue
            do i=nsit_cmc,n,-1
               csites(i-n+1)=sites_ij(inds(i))
c               write(*,*) neui,i-n+1,inds(i),csites(i-n+1),
c     +              pos(inds(i))
               cblen(i-n+1)=d_ij(inds(i))
               ccorr(i-n+1)=c_ij(inds(i),neui)
               c_w_ij(i-n+1)=w_ij_s(i)
               cllh(i-n+1,1)=llh_ij(inds(i),1)
               cllh(i-n+1,2)=llh_ij(inds(i),2)
               cllh(i-n+1,3)=llh_ij(inds(i),3)
               do j=1,ndays
                  ts(j,i-n+1)=data_all(j,neui+3,pos(inds(i)))
                  sigmas(j,i-n+1)=data_all(j,neui+6,pos(inds(i)))
               enddo
            enddo
                  
c$$$            write(*,*) '#cmc sites:',nsit_cmc-n+1
c$$$            write(*,*) (ts(1,i),i=1,nsit_cmc-n+1)
c$$$            write(*,*) (sigmas(1,i),i=1,nsit_cmc-n+1)
c$$$            write(*,*) (c_w_ij(i),i=1,nsit_cmc-n+1)
            nhead=nhead+1
            write(tmpstr,804) neustr(neui),nsit_cmc-n+1,
     +           (csites(i),i=1,nsit_cmc-n+1)
 804        format('#',a1,'[site-',i5,']:',2000(1x,a4))
            headers(nhead)=tmpstr(1:nblen(tmpstr))
            nhead=nhead+1
            write(tmpstr,805) neustr(neui),nsit_cmc-n+1,
     +           (ccorr(i),i=1,nsit_cmc-n+1)
 805        format('#',a1,'[corr-',i5,']:',2000(1x,f4.1))
            headers(nhead)=tmpstr(1:nblen(tmpstr))
            nhead=nhead+1
            write(tmpstr,806) neustr(neui),nsit_cmc-n+1,
     +           (cblen(i),i=1,nsit_cmc-n+1)
 806        format('#',a1,'[dist-',i5,']:',2000(1x,f4.1))
            headers(nhead)=tmpstr(1:nblen(tmpstr))
            nhead=nhead+1
            write(tmpstr,807) neustr(neui),nsit_cmc-n+1,
     +           (c_w_ij(i),i=1,nsit_cmc-n+1)
 807        format('#',a1,'[wegt-',i5,']:',2000(1x,f4.1))
            headers(nhead)=tmpstr(1:nblen(tmpstr))
            
            call cmc_form(nsit_cmc-n+1,ndays,
     +           ts,sigmas,c_w_ij,cmc,
     +           dayNsits)                  
c            write(*,*) 'cmc1:',cmc(1)
c            stop

            do i=1,ndays
               cmcs(i,neui)=cmc(i)
c               cmc_all(i,neui,fi)=cmc(i)
c               ndays_all(i,neui,fi)=dayNsits(i)
               flts(i,neui)=-9999
               if (data_all(i,neui+3,fi).ne.-9999) then
                  flts(i,neui)=data_all(i,neui+3,fi)-cmc(i)
               endif
            enddo
c            stop
c     END-of-Component (N/E/U) loop
         enddo
         call getlun(fid)
         ofile=opath_cmc(1:nblen(opath_cmc))//pathsep//site//'Cmc.neu'
         write(*,'(4a)') '[',prog(1:nblen(prog)),']output CMC:', 
     +        ofile(1:nblen(ofile))
c         stop
         open(unit=fid,file=ofile)
c     header
c         do i=1,nhead
c            write(fid,'(a)') headers(i)
c         enddo
         do i=1,ndays
            write(fid,803) dyrs(i),yrs(i),doyrs(i),
     +           (cmcs(i,j),j=1,3)
 803        format(1x,f10.5,1x,i04,1x,i03,3(1x,f9.5))
         enddo
         close(fid)
         ofile=opath_flt(1:nblen(opath_flt))//pathsep//site//'Flt.neu'
         write(*,'(4a)') '[',prog(1:nblen(prog)),']output filtered:', 
     +        ofile(1:nblen(ofile))
c         stop
         open(unit=fid,file=ofile)
         do i=1,ndays
            if (flts(i,1).ne.-9999) then
               write(fid,803) dyrs(i),yrs(i),doyrs(i),
     +              (flts(i,j),j=1,3)
c 803           format(1x,f10.5,1x,i04,1x,i03,3(1x,f9.5))
            endif
         enddo
         close(fid)

c     Next file
 705     continue
c     End-of-CMC-claculation-loop
      enddo

      write(*,'(3a)') '[',prog(1:nblen(prog)),']Normal end.'

      STOP
      END

      subroutine cmc_form(nsit,nday,ts,sigmas,weights,cmc,dayNsits)
c     --PURPOSE--

c     --ALGORITHM--

c     --EXAMPLE--

c     --MODIFICATIONS--

      IMPLICIT NONE
      INCLUDE '../../inc/cgps.h'

c     >>VAR_DEC
c     --INPUT--
c     #CMC sites
      integer*4 nsit,nday
c     time series array
      real*8 ts(nmax_row,nmax_sites),sigmas(nmax_row,nmax_sites)
c     weight array
      real*8 weights(nmax_sites)

c     --OUTPUT--
      real*8 cmc(nmax_row)
      integer*4 dayNsits(nmax_row)

c     --EXTERNAL--

c     --Local Parameters--
      integer*4 di,inddi(nmax_sites),nsiti,i,j,k
      real*8 tsi(nmax_sites),sigmai(nmax_sites)
      real*8 cmci,cmcia,cmcib

c     <<VAR_DEC

      do di=1,nday
         nsiti=0
         do i=1,nsit
            tsi(i)=ts(di,i)
            sigmai(i)=sigmas(di,i)
            if (tsi(i).ne.-9999.and.sigmai(i).ne.-9999) then
               nsiti=nsiti+1
               inddi(nsiti)=i
            endif
         enddo
c
c         write(*,*) 'nsiti:',di,nsiti
         if (nsiti.lt.3) then
            cmc(di)=0d0
            dayNsits(di)=0
         else
            cmcia=0d0
            cmcib=0d0
            do i=1,nsiti
               cmcia=cmcia+tsi(inddi(i))*weights(inddi(i))/
     +              sigmai(inddi(i))**2
               cmcib=cmcib+weights(inddi(i))/
     +              sigmai(inddi(i))**2

            enddo
            cmci=cmcia/cmcib
            cmc(di)=cmci
            dayNsits(di)=nsiti
c            write(*,*) 'cmci:',cmci,nsiti,nsit
c            stop
         endif
 
      enddo

      RETURN
   
      end
