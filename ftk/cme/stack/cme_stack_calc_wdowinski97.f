CTITLE cme_stack_calc_wdowinski97
      SUBROUTINE cme_stack_calc_wdowinski97(path,sitefile,cme,n)
c     data,nrow,ncol,si,cme)
c     --PURPOSE--

c     --INPUT--
c*     data - raw time series (no-detrended,no-cleaned,no-demeanded)
c     path - residual time series
c     sitefile - contains sites used to calcualte CME
c      character*(*) path,sitefile

c     --OUTPUT--
c     cme - CME for each component (mjd,cme_n,cme_e,cme_u)
c      real*8 cmes(nmax_

c     --ALGORITHM--
c     Calculation of CME requires residual time series with standard error.
c

c     --EXAMPLE--

c     --MODIFICATIONS--

c     >>VAR_DEC
c     --Global Variables--
      IMPLICIT NONE
      INCLUDE '../../inc/cgps.h'

      character*(*) path,sitefile
      real*8 cmes(nmax_row,4)
      integer*4 n

c     --Local Parameters--
      

c     <<VAR_DEC



      END
