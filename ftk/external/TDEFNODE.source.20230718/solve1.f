c*******************************************************************************
      subroutine solve1(inchar, itnum, itertype)

c--- run inversion

      implicit real*8 (a-h,o-z)
      include "tdefcom1.h"
      include "tdefcom2.h"
      
      character ptype(50)*15, p2codes(50)*2
      common /ptype1/ ptype, p2codes

c      COMMON /rnd1/ idum

      parameter (MAX_steps = 200)

c for amebsa
      dimension pa(MAX_parms,MAX_parms),
     .   yp(MAX_parms), pb(MAX_parms)

c parms() is a global (common) array with the parameter values
c p() is temporary array holding values to be tested

      dimension p(MAX_parms), pc(MAX_parms)
c      dimension p3tmp(nparms,3), p3(nparms)
      dimension padjust(MAX_parms), bestfit(3)
     
      dimension chi_trials(MAX_steps,4), pgtmp(3) 
      dimension grscale(50), dnew(50), grmin(50), grmax(50)
      dimension islope(MAX_steps)

      character*1 star, inchar 
      character*12 dat
c      character*15 c15
c      character*80 fnout

      logical neg0, no, yes, gridparm(nparms), gradient, ls_inv
      logical dodp(3), flip, krandom, kstop 

c* ranges of simplex depending on parameter type
      data dnew /
     .   10.0,    10.0,   100.0,     1.0,    19.0, 
     .  190.0,   190.0,    90.0,    90.0,   5.0d4,
     
     .   99.0,    99.0,    99.0,    50.0,     1.0,
     .   20.0,     0.4,     1.0,    90.0,   250.0,
     
     .    2.0,     2.0,    20.0,   2.0d2,   1.0d4,
     .  1.0d2,     2.0,   1.0d3,   1.0d2,   1.0d2,
     
     .   50.0,    50.0,   1.0d2,    1.0d3,   1.0d3,
     .  1.0d5,     1.0,   100.0,    50.0,     1.0,
     
     .   50.0,    90.0,   250.0,   250.0,   250.0,
     .  500.0,    50.0,   250.0,    50.0,    50.0  /

c* scaling for grid steps depending on parameter type
      data grscale / 
     .  0.1d0,   0.1d0,   0.1d0,   0.1d0,   1.0d0, 
     .  2.0d0,   2.0d0,   2.0d0,   1.0d0,   1.0d4,
     
     .  1.0d1,   1.0d1,   1.0d1,   1.0d2,   1.0d0,
     .  1.0d2,   1.0d1,   1.0d2,   1.0d0,   1.0d0,
     
     .  1.0d-1,   1.0d-1,   1.0d0,   1.0d1,   1.0d1,
     .  1.0d1,   1.0d-2,  1.0d0,   1.0d1,   1.0d1,
     
     .  1.1d1,   1.1d1,   1.0d1,   1.0d2,   1.0d2,
     .  1.0d2,   1.0d1,   1.0d1,   1.0d0,   1.0d0,
     
     .  1.0d0,   1.0d0,   1.0d0,   1.0d0,   1.0d0,
     .  1.0d0,   1.0d0,   1.0d0,   1.0d0,   1.0d0 /

c* minimum grid step depending on parameter type
      data grmin / 
     .  1.0d-5,  1.0d-5,  1.0d-5,  1.0d-4,  1.0d-5, 
     .  1.0d-3,  1.0d-3,  1.0d-5,  1.0d-5,  1.0d-1, 
     
     .  1.0d-3,  1.0d-5,  1.0d-5,  1.0d-5,  1.0d-5, 
     .  1.0d-5,  1.0d-5,  1.0d-5,  1.0d-5,  1.0d-5, 
     
     .  1.0d-5,  1.0d-5,  1.0d-3,  1.0d-4,  1.0d-5, 
     .  1.0d-4,  1.0d-7,  1.0d-5,  1.0d-5,  1.0d-5, 
     
     .  1.0d-5,  1.0d-5,  1.0d-5,  1.0d-3,  1.0d-3, 
     .  1.0d-5,  1.0d-5,  1.0d-3,  1.0d-3,  1.0d-5, 
     
     .  1.0d-5,  1.0d-5,  1.0d-5,  1.0d-5,  1.0d-5, 
     .  1.0d-5,  1.0d-5,  1.0d-5,  1.0d-5,  1.0d-5 /

c* maximum grid step depending on parameter type
      data grmax / 
     .  1.0d0,  1.0d0,  1.0d0,  1.0d0,  1.0d5, 
     .  1.0d3,  1.0d3,  1.0d5,  1.0d5,  1.0d5, 
     
     .  1.0d2,  1.0d5,  1.0d5,  1.0d5,  1.0d5, 
     .  1.0d5,  1.0d5,  1.0d5,  1.0d5,  1.0d5, 
     
     .  3.0d1,  3.0d1,  1.0d3,  1.0d3,  1.0d5, 
     .  1.0d3,  1.0d2,  1.0d5,  1.0d5,  1.0d5, 
     
     .  1.0d5,  1.0d5,  1.0d5,  1.0d3,  1.0d3, 
     .  1.0d5,  1.0d5,  1.0d3,  1.0d3,  1.0d5, 
     
     .  1.0d5,  1.0d5,  1.0d5,  1.0d5,  1.0d5, 
     .  1.0d5,  1.0d5,  1.0d5,  1.0d5,  1.0d5 /

c p2codes / 'vp', 'bp', 'sr', 'ph', 'wg', 'z1', 'z2', 'mi', 'ms', 'rx', '  ', '  ', '  ', '  ', '  ', '  ', '  ', '  ', '  ', '  ',
c           'ln', 'lt', 'zh', 'ww', 'am', 'xw', 'to', 'tc', 'xr', 'wr', 'st', 'dp', 'rk', 'az', 'rd', 'ta', 'ga', 'gm', 'gs', '__', 'mo'   

c set up p() array, held globally in parm()
      do i=1,nparms
        gridparm(i) = .true.
        p(i)=parm(i)
      enddo
c        write(*, '(30f10.4)' ) (p(i),i=1,nparms)

    
      call getdof(kdof,nd)
      real_dof = real(kdof)
c      print *, 'Ndata ', nd

      neg0 = .false.
      no   = .false.
      yes  = .true.
      kstop = .false.
    
      zero = 0.0d0
      ngr = 0
      dpp = zero

c- bounds of node values
      phi_min=psmin(4)
      phi_max=psmax(4)
      dnew(4) = phi_max-phi_min
      chi_old = 1.0e25
      chi_last_simplex = 1.0e25

c forward model only
      if (itertype.eq.0) then
        last_iter = .true.
        call solve2(p, x, dchi2, pensum, mp)
        bestfit(1) = x
        bestfit(2) = dchi2
        bestfit(3) = pensum
        goto 999
      endif

c*********************************************************
c***  do least squares inversion
c*********************************************************
      if ( itertype.eq.3 ) then
        ls_inv = .true.
         do i=1,nparms
           p(i)=parm(i)
         enddo
        call solve2(p, x, dchi2, pensum, mp)
        call sa_screen (0, itnum, x, dchi2, pensum, parm)
        call sa_screen (1, itnum, x, dchi2, pensum, parm)
        call getdof( kdof, ndat)
        print *, 'LS Inverting ndat ', ndat
        call findiffders (ls_inv, padjust, ndat)
        do i=1,nparms
          parm(i) = parm(i) + padjust(i)
          p(i)=parm(i)
        enddo
        call solve2(p, x, dchi2, pensum, mp)
        bestfit(1) = x
        bestfit(2) = dchi2
        bestfit(3) = pensum
c        call sa_screen (0, itnum, x, dchi2, pensum, parm)
        call sa_screen (1, itnum, x, dchi2, pensum, parm)
         goto 999
      endif

c*********************************************************
c***  do grid search if requested
c*********************************************************
      nt = (int(gs_controls(1)))

      if ( itertype.eq.2 .and. nt.gt.0 ) then

c how many steps for each parameter
      nt = min(nt, MAX_steps-1)
      maxsteps = nt
      if (maxsteps.eq.0) maxsteps = MAX_steps-1
      realnt = real(nt)

c grid search type
      kgstype = int(gs_controls(4))

      gradient = .false.
      if (kgstype.eq.2 .or. kgstype.eq.3) gradient = .true.

c initial step size
      dpole = gs_controls(2)
      if ( dpole.eq.0.0d0 ) dpole = 0.01d0

c how many times to run through parameters
      ngs  = max( int(gs_controls(3)), 1 )

c jump 
      njump = int(gs_controls(6))
      if (njump.eq.0) njump = 20 
      
c grid step size decrease
      pstep = gs_controls(5)
      if (pstep .le. 1.0d0)  pstep = 3.0d0
 
c start with current solution
c      call solve2(p, x4, dchi2, pensum, mp)
c        bestfit(1) = x4
c        bestfit(2) = dchi2
c        bestfit(3) = pensum

      
c********************************************************************    
c-- randomly perturb parameters   
c********************************************************************    
      if ( kgstype.eq.4 ) then
       dp = dpole 
       xmin =  bestfit(1)
       dchi2 = bestfit(2) 
       pensum = bestfit(3)
       print *, 'Starting random search '
       kk=0
       maxp = 0
       k=1
      call grid_screen(izero, kk, xmin, dchi2, pensum, pmin2, ptmp,
     .  star, islope, nt, kgstype, ngr, maxp, dpole)
       
      call grid_screen(ione, kk, xmin, dchi2, pensum,
     .   p(kk), ptmp0, star, islope, ntrials, kgstype, k, maxp, dp)

      do kk = 1,ngs
       if ( kk.gt.1) dp = dp/pstep
      
      do k=1, nt
      
c* check for stop
       call defstop(expname, kstop, inchar)
        if (kstop) then
         do i=1,nparms
          p(i)=parm(i)
         enddo
         call solve2(p, yb, dchi2, pensum, mp)
         call grid_screen(ione, kk, yb, dchi2, pensum,
     .   p(kk), ptmp0, star, islope, ntrials, kgstype, k, mp, dp)
         call rwparms(4)
         goto 999
        endif
        
c*** randomly perturb parameter array
       do i=1,nparms
        kpt = kparm_type(i)
        p(i) = parm(i)+(0.5d0-ran1(idum))*dnew(kpt)*dp*grscale(kpt)
        pc(i) = parm(i)
       enddo
       
c solve
       call solve2(p, x, dchi2, pensum,mp)
       
       if (x.lt.xmin) then
        do i=1,nparms
         parm(i)=p(i)
        enddo
        xmin = x
        bestfit(1) = xmin
        bestfit(2) = dchi2
        bestfit(3) = pensum
        maxp = mp
        call grid_screen(ione, kk, xmin, dchi2, pensum,
     .   p(kk), ptmp0, star, islope, ntrials, kgstype, k, maxp, dp)

       else
        do i=1,nparms
         parm(i) = pc(i)
        enddo
       endif
       
      enddo
    
      enddo
      call rwparms(4)


      goto 999
      endif    
c********************************************************************  
c** END OF RANDOM SEARCH  
c********************************************************************    
      
c********************************************************************    
c*** reset p array
      do i=1,nparms
       p(i)=parm(i)
      enddo

      print *, 'Starting grid search ... '

  83  format(a4, 1x, "N_steps=", i4, " Step=", f9.5,  
     .       " Iter=", i2, 
     .       " Run=",  i2," of", i2, 
     .       " Type=", i2,
     .       " Range=", f9.5)

      ntrials = nt
      flip = .true.
      krandom = ( kgstype.eq.1 .or. kgstype.eq.3 ) 

      call solve2(p, xmin, dchi2, pensum, maxp)
        bestfit(1) = xmin
        bestfit(2) = dchi2
        bestfit(3) = pensum

      do 66 nruns = 1, ngs
       xntr = real(ntrials - 1)
       
c** decrease step for each time through parameters      
       if ( nruns.gt.1) dpole = dpole / pstep
       
       range = dpole * xntr / 2.0d0

      write( *,83)  expname, maxsteps, dpole, itnum, 
     .    nruns, ngs, kgstype, dpole*maxsteps

      call grid_screen(izero, kk, xmin, dchi2, pensum, 
     .  pmin2, ptmp,
     .  star, islope, ntrials, kgstype, ngr, maxp, dpole)

      xpp = real(nparms+1)
      flip = .not. flip

      ngos = nparms
      if ( krandom ) ngos = 2*ngos
      npar = 0
      
c-- loop through parameters 
      do 57 kkk = 1, ngos

c* check for character from keyboard
       call defstop(expname, kstop, inchar)

c-- to next grid search
        if(inchar.eq.'n') goto 66

c-- quitting or next IC: item
        if (kstop .or. inchar.eq.'s' ) then
         call solve2(p, yb, dchi2, pensum,maxp)
         call rwparms(4)
         goto 999
        endif

        npar = npar + 1

c-- jump ahead njump parms
       if ( .not. krandom ) then
        if(inchar.eq.'j') npar = npar + njump
       else
c* random order of parameter search
        npar = int ( ran1(idum) * xpp )
       endif

c* reverse order of parameters every other run
c        if ( flip .and. .not. krandom) kk = nparms - kkk +1

        npar = max(1,npar)
        if (npar.gt.nparms ) goto 58

c kk is the parameter being processed
        kk = npar

        ptmp = parm(kk)
        kptype = kparm_type(kk)
        kpt = kparm_type(kk)
        
      if ( gridparm(kk) ) then
        pmin = parm(kk)
        pmin2 = parm(kk)

        call cleareal(chi_trials, 3*MAX_steps)
        xmax = zero

c********************************************************************
c* do the gradient grid search
c********************************************************************
      if ( gradient ) then

       call clearint(islope, MAX_steps)
       call cleareal(chi_trials, 4*MAX_steps)

c ptmp0 is original value of parm
       ptmp0    = p(kk)
       pgtmp(2) = p(kk)
       ngr=0

c-- dp is step size for this parameter       
       dp =  grscale(kpt) * dpole
       
c-- skip parm if dp too small or dp too big        
c       if ( dp.lt.grmin(kpt) .or. dp.gt.grmax(kpt)) goto 57

c second trial is the incoming solution
        chi_trials(2,1)=xmin
        chi_trials(2,2)=dchi2
        chi_trials(2,3)=pensum
        chi_trials(2,4)=real(maxp)
        pgtmp(2) = p(kk)

c flag trials       
        dodp(1)=.true.
        dodp(2)=.true.
        dodp(3)=.true.

c start next look
  88   ptmp = pgtmp(2)
       ngr=ngr+1
       
c** reached maximum steps, print a '+'
        if (ngr.gt.maxsteps) then
          star='+'
c         p(kk) = ptmp
c         call solve2( p, xmin, dchi2, pensum, maxp )
          p(kk) = pgtmp(2)
          do i=1,nparms
           parm(i) = p(i)
          enddo
          xmin = chi_trials(2,1)
          dchi2 = chi_trials(2,2)
          pensum = chi_trials(2,3)
          maxp = int(chi_trials(2,4))
          bestfit(1) = xmin
          bestfit(2) = dchi2
          bestfit(3) = pensum
         call grid_screen(ione, kk, xmin, dchi2, pensum,
     .     p(kk), ptmp0, star, islope, ntrials, kgstype, ngr, maxp, dp)
          goto 57
        endif

c-- check 3 solutions, first dp less than current parm value, then at parm value, then one dp more, 
c-- the second trial should always hold the current best fit value of the parameter
       do jj=1,3

        if ( dodp(jj)) then

         if (jj.eq.1) dpp = -dp
         if (jj.eq.2) dpp =  0.0d0
         if (jj.eq.3) dpp =  dp

         p(kk) = ptmp + dpp
         pgtmp(jj) = ptmp + dpp
c         print *, p(kk), parm(kk) 
         call solve2( p, xerr0, dchi0, pensum0, mp0 )
c         print *, p(kk), parm(kk), xerr0

         chi_trials(jj,1)=xerr0
         chi_trials(jj,2)=dchi0
         chi_trials(jj,3)=pensum0
         chi_trials(jj,4)=real(mp0)

        endif

       enddo

c*  chi**2
        x1   = chi_trials(1,1)
        x2   = chi_trials(2,1)
        x3   = chi_trials(3,1)

c soln2 is best
c* this parameter is at minimum chi**2, save the second trial values, and go to next parameter
c print a '*' if the chi2 is flat (all 3 are equal)
c x4 is global min
        if( x2 .le. x1 .and. x2 .le. x3 ) then
          pmin2 = pgtmp(2)
          p(kk) = pgtmp(2)
          xmin = chi_trials(2,1)
          x4 = chi_trials(2,1)
          dchi2 = chi_trials(2,2)
          pensum = chi_trials(2,3)
          maxp = int(chi_trials(2,4))
          dodp(2)=.false.
          do i=1,nparms
           parm(i) = p(i)
          enddo

          star = ' '
          if( x2.eq.x1 .and. x2.eq.x3 ) star = '*'

        call grid_screen(ione, kk, xmin, dchi2, pensum,
     .    pmin2, ptmp0, star, islope, ntrials, kgstype, ngr, maxp, dp)
         bestfit(1) = xmin
         bestfit(2) = dchi2
         bestfit(3) = pensum

          goto 57
        endif

c* soln1 is best, put soln1 in soln2, put soln2 in soln3, redo soln1
        if( x1 .le. x2 .and. x1 .le. x3 ) then
          dodp(1)=.true.
          dodp(2)=.false.
          dodp(3)=.false.
          pgtmp(3)=pgtmp(2)
          pgtmp(2)=pgtmp(1)
           do k=1,4
            chi_trials(3,k)=chi_trials(2,k)
            chi_trials(2,k)=chi_trials(1,k)
           enddo
          goto 88
        endif

c  sol3 is best, put sol3 in soln2, put soln2 in soln1, redo soln3
        if( x3 .le. x1 .and. x3 .le. x2 ) then
          dodp(3)=.true.
          dodp(1)=.false.
          dodp(2)=.false.
          pgtmp(1)=pgtmp(2)
          pgtmp(2)=pgtmp(3)
          do k=1,4
            chi_trials(1,k)=chi_trials(2,k)
            chi_trials(2,k)=chi_trials(3,k)
          enddo
          goto 88
        endif
        
          print *, 'Stuck on ', kk, x1, x2, x3
          goto 57

      endif
c********************************************************************
c* end of gradient grid search
c********************************************************************

c********************************************************************
c* do the regular grid search type 0, look at all values in range
c********************************************************************
       call clearint(islope, MAX_steps)
       call cleareal(chi_trials, 3*MAX_steps)
       
       ptmp = p(kk)
       pmin = ptmp
       pmin2= ptmp
       xmax = 0.0d0
       xmin =  bestfit(1)
       dchimin = bestfit(2) 
       penmin = bestfit(3)

       nt0 = int(min(MAX_steps-1,nt)/2)+1
       nt0 = min(nt0, 89)
       jt = 0
       jtmin = 0

       do 39 jj = -nt0, nt0
        realjj = real(jj)
        jt = jt+1
        dp = grscale(kpt) * realjj * dpole
        p(kk) = ptmp + dp

        call solve2(p, x, dchi2, pensum, mp)
        chi_trials(jt,1)=x
        chi_trials(jt,2)=dchi2
        chi_trials(jt,3)=pensum

        xmax = max(xmax,x)

        if ( x.lt.xmin ) then
          pmin2=p(kk)
          xmin=x
          yb = xmin
          penmin=pensum
          dchimin=dchi2
          maxp = mp
          jtmin = jt
          jjmin = jj
          bestfit(1) = xmin
          bestfit(2) = dchi2
          bestfit(3) = pensum
        endif

        if ( jt.gt.1) then
         dc = chi_trials(jt,1) - chi_trials(jt-1,1)
         islope(jt) = 0
         if ( dc.lt.0.0d0 ) islope(jt) = -1
         if ( dc.gt.0.0d0 ) islope(jt) = 1
        endif

  39  continue
  

        p(kk) = pmin2
        parm(kk) = pmin2
  
        dx = min(xmax-xmin, 99999.999d0)
        star=' '
        if ( dx.lt.1.0d-4 ) then
c          gridparm(kk) = .false.
          star='*'
        endif

       ngr = abs(jjmin)
        
       call grid_screen(ione, kk, xmin, dchimin, penmin,
     .    pmin2, ptmp,
     .    star, islope, ntrials, kgstype, ngr, maxp, dp)

        endif

c  56  continue
  

  57  continue

  58  call rwparms(4)

  66  continue

      call rwparms(4)

      endif
c*****************************************************
c* End of grid search
c*****************************************************

c*****************************************************
c* Set up simlated annealing
c*****************************************************
      n = int(sa_controls(2))
      if ( itertype.eq.1 .and. n.gt.0) then

      print *, 'Starting simulated annealing ', expname

      temptr0           = sa_controls(1)
      max_iterate       = max(1, int(sa_controls(2)))
      num_sa_iterations = int(sa_controls(3))

      if(num_sa_iterations.eq.0) num_sa_iterations = 50*max_iterate

      ftol = 1.0d-10
      if (sa_controls(4).gt.0.0d0) ftol = sa_controls(4)

      num_simplex=10
      neds=max_iterate
      ncalls=0
      nsame_max=10
      nsame_quit=25
      temptr=temptr0
      nsim_calls=0

      write (*,12) ftol, temptr0, max_iterate, num_sa_iterations
  12  format('Tolerance: ',e12.5,' Temp0: ',e12.5, 
     . ' SA Iterations:', i5, ' SA steps:', i5)

c      print *, 'Ndata ', nd
c---  p() holds the best solution now
      do k=1, nparms
        p(k) = parm(k)
c        parm(k) = p(k)
        pa(1,k) = p(k)
        parm_best(k) = p(k)
        pb(k) = p(k)
      enddo

c* get fit to starting model 
      call solve2(p, xchi2, dchi2, pensum, mp)

      yp(1)=xchi2
      yb=xchi2
      ybold=yb
      bestfit(1) = xchi2
      bestfit(2) = dchi2
      bestfit(3) = pensum

      call sa_screen (0, ncalls, xchi2, dchi2, pensum, parm)
      call sa_screen (1, ncalls, xchi2, dchi2, pensum, parm)

c*****************************************************
      if (max_iterate .le. 1) goto 999
c*****************************************************

      nsameq = 0
 912  nsame=0

c***** -- make simplex
      neg0 = ( .not. neg0 )

      num_simplex = num_simplex+1
      if ( yb.eq.chi_last_simplex) then
        if (num_simplex.eq.nsame_max) goto 999
      else
        chi_last_simplex = yb
        num_simplex = 0
      endif

      r = 1.0d0

      ymin = yb
      kkmin=1

c* make models for simplex
c 100  continue
      do 20 kk = 2, nparms+1
       do 21 jj=1, nparms

        pa(kk,jj)=parm_best(jj)
        da0new = dnew(kparm_type(jj))

c random change in starting simplex
        r = 1.0d0 - 2.0d0*(ran1(idum)-0.5d0)/2.0d0

c no random change
c        r = 1.0d0

        dpar = r*da0new
        if (jj.eq.(kk-1)) pa(kk,jj) = pa(kk,jj) + dpar
        p(jj) = pa(kk,jj)

 21    continue

        call solve2(p, x, dchi2, pensum, mp)
        yp(kk)=x

        if ( x.lt.ymin ) then
          kkmin=kk
          ymin=x
        endif

 20   continue

      if  ( kkmin.gt.1) then
        call swap (yp(1), yp(kkmin))
        yb = yp(1)
        do i=1,nparms
         pb(i) = pa(kkmin,i)
         call swap(pa(1,i), pa(kkmin,i))
        enddo
      endif
      
c*** reset temperature and run a few at end with T=0
  100  continue

c   randomly perturb the simplex
      ymin = yp(1)
      do 30 kk = 1, nparms+1
       do 31 jj=1, nparms
c        pa(kk,jj)=parm_best(jj)
        da0new = dnew(kparm_type(jj))
        r = (1.0d0 - 2.0d0*(ran1(idum)-0.5d0)) / 50.0d0
c        r=1.0d0
        if ( kk.eq.1) r=0.0d0
        dpar = r*da0new
        if (jj.eq.(kk-1)) pa(kk,jj) = pa(kk,jj) + dpar
        p(jj) = pa(kk,jj)
 31    continue
        call solve2(p, x, dchi2, pensum, mp)
        yp(kk)=x
 30   continue

c modify temp at each iteration
      temptr = temptr0*
     .   (1.0d0 -real(ncalls)/real(neds-num_simplex) )**alpha 
      if (ncalls .ge. neds-num_simplex) temptr=0
      if (nsim_calls.gt.0) temptr=0
      tt=-temptr

c* set number of iterations in amebsa
      iter=num_sa_iterations

c**** if it gets stuck, generate a new simplex and keep iterating
      db = abs(chi_old - yb)/chi_old
      dbmin = 1.0e-5
      if (db.lt.dbmin) then 
        nsame=nsame+1
        nsameq=nsameq+1
        if (nsame.eq.nsame_max) goto 912
c        if (nsameq.eq.nsame_quit) goto 999
      else
        nsame = 0
        nsameq = 0
      endif

      chi_old = yb

      call amebsa(pa, yp, MAX_parms, MAX_parms, nparms, pb, yb, 
     .  ftol, iter, temptr, dchi2, pens)
     
      do 50 i=1, nparms
        parm(i)=pb(i)
        parm_best(i)=pb(i)
  50  continue

      ncalls=ncalls+1
      call solve2(pb, yb, dchi2, pensum,mp)
c      call solve2(pb, dx, dd, ps, mp)
      call sa_screen (0, ncalls, yb, dchi2, pensum, parm)
      call sa_screen (1, ncalls, yb, dchi2, pensum, parm)
c      xmin = yb
      bestfit(1) = yb
      bestfit(2) = dchi2
      bestfit(3) = pensum


c 333  format (1(e16.4), 300f8.3)
c 334  format (i5, 1(e11.4), 300f8.3)

      if (iter.gt.0) goto 999

c* check for stop
       call defstop(expname, kstop, inchar)
        if (kstop .or. inchar.eq.'s' .or. inchar.eq.'n') then
         call solve2(pb, yb, dchi2, pensum,mp)
         call rwparms(4)
         goto 999
        endif

      if (ncalls.lt.max_iterate ) goto 100
      
      endif
      
 999  continue
c*****************************************************
c********* end of simulated annealing
c*****************************************************

  
c* write out new parameter file
      if (iparmread.eq.2 .or. iparmread.eq.3) 
     .   call rwparms(itwo)

c run final time to get stats
      call getdof( kdof, ndat)
      do i=1, nparms
        pb(i)=parm(i)
      enddo
      call solve2(pb, yb, dchi2, pensum, mp)
      xchi2_bf = yb

c*** get derivatives for error analysis
      if ( last_iter .or. kstop ) then

      if ( getcovariance ) then
        call cleareal(parm_err, nparms)
c        print *, 'Getting derivatives'
        ls_inv = .false.
        call findiffders (ls_inv, padjust, ndat)

c -- write final parameters to file and screen
        call fopen (kkprm, 1, '_prm.out ')
        write(*, *) 'No.  Type   Parameter     Error        Type '
        write(kkprm, *) 'No.  Type   Parameter     Error        Type '
        do i=1,nparms
         write (*, 13) i, kparm_type(i), 
     .     parm(i), parm_err(i), ptype(kparm_type(i))
         write (kkprm, 13) i, kparm_type(i), 
     .     parm(i), parm_err(i), ptype(kparm_type(i))
        enddo
      ik = kfclose (kkprm)
 13   format(i4,i5,1x,2(2x,1pe12.5),2x,a15)
      endif
      
c write summary line
      dof = real(kdof)
      chi2 = xchi2_bf * dof
      pfit = gammq(dof,chi2)*1.0d2

       call dater(dat)
       write (SUMline, 335) expname, dat, xchi2_bf, kdof, ndat, nparms, 
     .     pfit, chi2, dchi2, pensum
 335  format('SUM:',1x,a4,1x,a12,f9.3, 3i7, f8.3, 3(1pe12.4) )

      endif
      
      return 
      end
      
