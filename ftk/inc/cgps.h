C$$$c for NEU format
C$$$c 1996.0014 1996   1        0.14819        0.19599       -0.02419   0.00177   0.00148   0.00697
C$$$c 1996.0041 1996   2        0.14340        0.19175       -0.02486   0.00259   0.00220   0.00902
C$$$c 1996.0068 1996   3        0.14934        0.19744       -0.01892   0.00281   0.00233   0.00969 r_ne r_nu r_eu
      integer*4 nmax_row,nmax_col,nmax_head
C  number of data lines (usually < 7000 as of 2015 for daily position time series).
c     parameter(nmax_row=6000)
c      parameter(nmax_row=10300)
      parameter(nmax_row=8750)
C  number of columns (SIO/NEU format only has less than 12 columns as exampled above)
      parameter(nmax_col=12)
C  number of header lines (lines start with #): supposed to be less then 1000
      parameter(nmax_head=1000)

C For other time series format, the columns in raw files may be more than 12 (e.g. 30 for PBO pos format)
      integer*4 nmax_col_else
      parameter(nmax_col_else=30)


      character*1 pathsep
      parameter(pathsep='/')
C     defined for Unix/Linux

C     for reading pole tide records
      integer*4 nmax_pt
      parameter(nmax_pt=20000)

C     maximum number of sites
      integer*4 nmax_site
      parameter(nmax_site=30000)
c      parameter(nmax_site=3000)

c     debug
      logical debug
c      data debug /.false./
      data debug /.true./

      integer*4 nmax_row_large
      parameter(nmax_row_large=100000)

c
c     only affect ts_corr program  
      integer*4 nmax_sites,nmax_proc
c      parameter(nmax_sites=1300)
c      parameter(nmax_proc=1125)
      parameter(nmax_sites=2490)
      parameter(nmax_proc=2490)


      integer*4 nmax
      parameter(nmax=1000000)