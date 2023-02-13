* Command file for AUTCLN version 3.125 to be used for global and regional data
* Default values are listed with comment flag (non-blank first character)
* Last edited by tah/rwk/scm  061204  
                     
* Don't use any GAMIT edits
  use_gamit  no

* Remove more bias flags by allowing a base satellite if multiple slips 
   allow_one_bg  yes  
               
* If needed to increase the number of channels (compiled default is now 15)
*  max_chan 15 
x set by tianyf on March 23 2012 for solution 2012/014~2012/019 of TRNC
  max_chan 18


* Allow up to three missing epochs before flagging a data point; this helps with
* hourly telemetry gaps and seems to do no harm otherwise
   gap_size all 3

* Set minimum elevation for editing and output: 15 10 better for older receivers
*  also sets minimum SNR 
  site_param all 10 10 0 0
         
* Set the ionospheric tolerances so you don't throw out too much data.
* These are the current defaults and will work under both low and high
* ionospheric conditions with well-behaved receivers.  For poorly tracking 
* receivers and low ionosphere, you can improve the editing using 
* 240 4 0.3 0.8.
*  ion_jump all   30  6 2 5
  
* Criteria for detecting slips (initial bias flags).  Defaults shown. 
* First three are for WL, irrelevant for codeless L2 receivers
* Second three (LC) might be set tighter (e.g. 4 0.2 0.5) to catch 
* partial-cycle jumps with poorly performing receivers.
* With poor prefit coordinates, set the last two numbers to 2 5 (or 5 10)
* but use the defaults for POST or, with noisy data, skip the postfit
* edit until a second pass with good coordinates allows tight detection of jumps.
*  dd_fit_tol 5 2 10   3 0.35 0.8
    
* The following three commands control the repair of cycle slips and subsequent
* removal of bias flags. The default values are conservative in the sense
* that they retain the most data.  They are optimal for global networks but
* will work ok also for regional networks.  However, for better ambiguity
* resolution in regional networks, different values are optimal. 
*
* Set the tolerances used in trimming the one-way data to remove small
* segments between bias flags.  The following are defaults:
  trim_oneway   120  8  0.1  24
* For regional networks use
*   trim_oneway   1000  10  0.2  50
* The first two parameters are the minimum times in seconds and minimum
* epochs for attempting to remove a bias flag; the last two are the minimum
* fraction of total span and minimum number of epochs allowed after last bias
* flag.  To strengthen ambiguity resolution for regional data, increase the
* last two parameters.  For fewer bias flags in 24-hr data increase the first
* two parameters.
* 
* Number of data used to repair cycle slips.  Defaults are ok for all data but
* all values could be reduced for data sampled less often than 30s.
*  dd_return_size  100  50  10  10
*
* DD criteria for removing bias flags: chi-sq ratio  chi-sq min  max gap  gap scale 
* For global networks use
  remove_bias 10  3  1800  5
* For regional networks use
*   remove_bias 10  3  3600  5
* For fewer flags but more risk over small gaps, decrease the first value (see
* autcln.out).  For fewer flags and more risk over large gaps, increase the 
* third and decrease the fourth,
 
* Maximum number of bias flags per SV before deleting all the data.  
* Default infinite (not checked).
*  max_scan_edit  30

* To enhance numerical stability in SOLVE (but be careful in interpreting 
* one-way residuals)   
  apply_phs_clk 1 
  
* Set the summary filename to agree with the command file produced by FIXDRV
  summary autcln.prefit.sum

* Exclude L1-only (or bad RINEX files) to avoid problems: comment out if you want to process L1 data
*  noL1only 

* Commands to be used if post-fit editing invoked in the sestbl.
POST  summary  autcln.post.sum
POST  apply_phs_clk 30
POST  use_postfit
POST  postfit_edit 10 4.0 
* Remove biases in one-ways after postfit edit
POST pf_remove_bf
* Possibly allow patching over larger gaps 
* POST  remove_bias 10  3  3600 2   
* Output phase residuals for sky plots
#POST  phs_res_root DPH 
* Resolve widelane ambiguities in autcln
POST lc_autcln

* Explicit edits added by sh_autedit or the analyst 
x edit_site_sv algo 0 1 2800
x edit_site_sv all 23 1 400
x edit_site_sv trom 15 451 460

