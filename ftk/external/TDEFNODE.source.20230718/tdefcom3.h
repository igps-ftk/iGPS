c include commons for tdefnode  2013.03.01

c********************************************************************
c  DON'T EDIT ANYTHING IN THIS FILE
c********************************************************************

      character*3   gfdir
      character*4   expname, runid 
      character*5   gf_version
c      character*8   model8
      character*10  defnode_version
      character*12  time_string
      character*23  longname
      character*80  infile, parmfile 
      character*100 SUMline
      character*256 aline, aline2

      parameter ( 
     .   alpha=3.0d0, 

c max types of penalties, leave at 6000
     .   MAX_pen = 10000 )

      integer gf_code, fault_fit_type

      logical kcalculate, vel_errors, getcovariance, insarfac2,
     .  triangles, write_der, write_cov, all_fault_0, all_fault_1, 
     .  node_decrease, wsimplex, read_votw, write_info, long_pos,
     .  time_dependence, redo_layer_GF, readgflag, insar_gmt, xe3 
     
      logical mapinfo, gres, all_fault_fix, add_rand, gps_cov, do_dgt, 
     .  write_hdr, chk_data, rand_seed, intseismic, nofaults, htdp,
     .  insar, insardata, veldata, tsdata, dispdata, 
     .  model_v, make_blocks, data_span, do_trans, restart, funcgfs,
     .  add_rand0, last_iter, ela_by_fault, use_node_delay
     
      logical  write_pens, parm_cov, wtable, get_stress, read_errors,
     .  keep_bad, use_gps, data_flag, get_phi_err, new_frame,
     .  use_crust2, use_prem, quakes, apply_penalty, write_all_fault
     
      logical gamma5, fault_atr, donew, okada, do_all_gf,
     .  get_gps_parm, get_blk_parm, get_flt_parm, get_str_parm, invert,
     .  parmpio, pioflag, verbose, w_syn, sphstrain, sort_ts,
     .  check_v_equates, check_name_equates,  nodes_on_profile

      logical no_srs, no_svs, no_ups, no_hcs, no_tilts, no_rot, scec,
     .  no_wt_srs, no_wt_svs, no_wt_gps, no_wt_mts, damp_strain, l1norm,
     .  adjust_hw, adjust_fw, interactive, use_blocks,
     .  makekml, wf1km, wfsegs, wair, check_boundary, use_sr_az

c special regions flags
      logical wusflag, sumflag, pnwflag, wkbob, myflag
      
      logical parmsread, check_psig, invmt
     
      common /flag1/ kcalculate, vel_errors, getcovariance, insarfac2,
     .  triangles, write_der, write_cov, all_fault_0, all_fault_1, 
     .  node_decrease, wsimplex, read_votw, write_info, long_pos,
     .  mapinfo, gres, all_fault_fix, add_rand, gps_cov, do_dgt, htdp,
     .  add_rand0, last_iter, ela_by_fault, use_sr_az, redo_layer_GF
     
      common /flag2/ write_pens, parm_cov, wtable, rand_seed,
     .  get_stress, keep_bad, use_gps, data_flag(6), get_phi_err, 
     .  new_frame, read_errors, write_hdr, chk_data, restart,
     .  use_crust2, use_prem, quakes, apply_penalty, write_all_fault,
     .  l1norm, adjust_hw, adjust_fw, interactive, sort_ts,xe3
     
      common /flag3/ wusflag, sumflag, pnwflag, wkbob, myflag, scec,
     .  no_wt_srs, no_wt_svs, no_wt_gps, no_wt_mts, sphstrain, wair,
     .  check_boundary, use_node_delay, time_dependence, readgflag
      
      common /flag4/ no_srs, no_svs, no_ups, no_hcs, no_tilts, no_rot,
     .  intseismic, nofaults, insar, insardata, veldata, tsdata, 
     .  dispdata, model_v, make_blocks, data_span,  nodes_on_profile,
     .  use_blocks, funcgfs, makekml, wf1km, wfsegs, insar_gmt 

      common /flag5/ gamma5, fault_atr, donew, okada, do_all_gf,
     .  get_gps_parm, get_blk_parm, get_flt_parm, get_str_parm, invert,
     .  pioflag, do_trans, verbose, w_syn,
     .  check_v_equates, check_name_equates

      common /flag6/ parmsread, check_psig, invmt, damp_strain
      common /flag7/ parmpio(20)
      
c constants      
      common /co1/ zero, one, two, three, four, five, six, seven, eight
      common /co1a/ today
      common /co2/ expname, runid 
      common /co4/ gfdir, infile, SUMline, defnode_version, gf_version
      common /co5/ izero, ione, itwo

c random seed
      COMMON /rnd1/ idum
      
c edits      
      integer edit_flag, edit_type
      common /ed1/ nedits, edit_flag(MAX_edits), edit_type(MAX_edits)
      common /ed2/ ptedit(MAX_edits,6)

c i/o
      common /io1/ k10, iparmread, ksum, ksumg, inline
      common /io2/ aline, aline2
      common /io3/ parmfile, longname, time_string

c inversion controls
      logical ksim, grid_search, makeGF, makeGF_all, ksim_done, 
     .        adjust_pole
      common /sa1/ sa_controls(4), gs_controls(6), sa_min, sa_max 
      common /sa2/ ksim, grid_search, makeGF, makeGF_all, ksim_done, 
     .             adjust_pole
      common /sa3/ psmin(50), psmax(50), penalty_factor(50), 
     .             penalty_factor2(6)
      common /sa4/ icontrol(30), nitmax
c      common /sa5/ bestfit(3)
   

c SA stuff
      common /ambsa/ tt
      
c merging time series
      character*4 merge_sites
      common /ms1/ nsmerge
      common /ms2/ merge_sites(10,7)      

c blocks=MAX_block, numcorners
      character*4 pole_out, b_name, b_rename
      logical block_flag
      common /bl0/ block_flag(MAX_block)
      common /bl2/ block_centroid(MAX_block,2), block_area(MAX_block),
     .             v_block(MAX_block,2), spin(MAX_block),
     .             blockxy(MAX_corner,MAX_block,2),
     .             res_strain(MAX_block,20), bl_strain(MAX_block,6),
     .             tot_strain(MAX_block,3),
     .             block_res(MAX_block,3)
      common /bl3/ b_name(MAX_block), pole_out(MAX_block), 
     .             b_rename(20,25)
      common /bl4/ npole_block(MAX_block), nc_block(MAX_block), 
     .             nblocks, nstrain_block(MAX_block),
     .             n_b_renames

c moment tensors for strain
      character*10 eq_name 
      common /mt1/ qmo(MAX_mts,11), qms(MAX_block, 3), 
     .             fmtfac(MAX_block), qmtwt
      common /mt2/ eq_name(MAX_mts)
      common /mt3/ nqmt(MAX_block)

c poles max=MAX_poles
      logical pi_flag
      common /po0/ pi_flag(MAX_poles)
      common /po1/ poles(MAX_poles,9), fpole(9)
      common /po2/ npole_invert(MAX_poles), nblock_ref, num_pole_invert,
     .             num_poles

c strain tensors
      logical si_flag
      common /str0/ si_flag(MAX_strain)
      common /str1/ strain(MAX_strain,9), strain2(MAX_strain,4)
      common /str2/ nstrain_invert(MAX_strain), num_strain_invert,
     .              num_strain

c GPS data  MAX_gps
      character*4 gps_fname, ref_site 
      character*8 gps_name
      character*80 gps_filename
      logical gps_invert, gps_keep, redo_site, subt_av
      integer gps_type, gps_file_type, gps_index
      common /gps1/ num_gps, num_gps_file, num_gps_poles, num_gps_used, 
     .              num_gf_gps, num_gf_gps2, num_gf_run, 
     .              gps_type(MAX_gps),
     .              nblock_gps(MAX_gps), 
     .              gps_index(MAX_gps),
     .              loc_gps(MAX_gps),  
     .              ngps_index(MAX_gps_files),
     .              gps_file_type(MAX_gps_files),
     .              nGF_rlx(MAX_gps, MAX_mrlx_files)
      common /gps2/ gps_obs(MAX_gps,7),   gps_ela(MAX_gps,3), 
     .              gps_rot(MAX_gps,5),   gps_net(MAX_gps,5),
     .              gps_str(MAX_gps,2),   gps_pos(MAX_gps,3),
     .              ggf_pos(MAX_gps,3),   ggf_pos2(MAX_gps,3),
     .              gps_calc(MAX_gps,2,3), gps_rlx(MAX_gps,3)
      common /gps3/ gps_name(MAX_gps),  gps_filename(MAX_gps_files),
     .              gps_fname(MAX_gps_files), ref_site(MAX_gps_files)
      common /gps4/ gps_invert(MAX_gps_files), gps_keep(MAX_gps), 
     .              redo_site(MAX_gps), subt_av(MAX_gps_files,3)
      common /gps5/ gps_pole(MAX_gps_files,9), gps_wt(MAX_gps_files,4),
     .              ref_vel(MAX_gps_files,3),  
     .              gps_info(MAX_gps_files,16) 
      common /gps6/ gps_near, gps_too_far, dx_node_new

c INSAR data   MAX_insar_pts
      real*8 insar_pos, insar_obs, insar_calc, insar_info, insar_los,
     .       insar_tmp, insar_unit, igf_pos, igf_pos2, 
     .       ins_rot, ins_str, ins_ela
      character*4  insar_fname, insar_sname
      character*80 insar_filename
      logical insar_flag, insar_off, insar_tropo, insar_offset, 
     .              insar_slope 
      common /ins1/ num_insar, num_insar_file, num_gf_ins, num_gf_ins2,
     .              insar_file(MAX_insar_pts)

      common /ins2/ insar_obs(MAX_insar_pts,2), 
     .              insar_unit(MAX_insar_pts,3),
     .              insar_pos(MAX_insar_pts,3),
     .              igf_pos(MAX_insar_pts,3), 
     .              igf_pos2(MAX_insar_pts,3)

      common /ins3/ insar_info(MAX_insar_files,12),
     .              insar_tmp(MAX_insar_files,2),
     .              insar_calc(MAX_insar_pts,2,3),
     .              insar_los(MAX_insar_pts),
     .              ins_rot(MAX_insar_pts,3),
     .              ins_str(MAX_insar_pts,3),
     .              ins_ela(MAX_insar_pts,3)

      common /ins4/ insar_fname(MAX_insar_files), 
     .              insar_filename(MAX_insar_files),
     .              insar_sname(MAX_insar_pts)

      common /ins5/ insar_flag(MAX_insar_files), 
     .              insar_off(MAX_insar_files), insar_tropo, 
     .              insar_offset, insar_slope

      common /ins6/ nblock_insar(MAX_insar_pts),
     .              insar_corr(MAX_insar_files),
     .              loc_ins(MAX_insar_pts)


c profiles 
      character*10 prof_title
      integer prof_n, prof_num
      logical make_profile
      common /pr1/ u_lines(MAX_pr_pts,3,3), prof_dx(MAX_pr_lines), 
     .             prof_az(MAX_pr_lines), prof_width(MAX_pr_lines), 
     .             prof_start(MAX_pr_lines,2), prof_pos(MAX_pr_pts,2)
      common /pr2/ prof_n(MAX_pr_lines), kfirst_point(MAX_pr_lines), 
     .             nlines, prof_num(MAX_pr_lines)
      common /pr3/ make_profile
      common /pr4/ prof_title(MAX_pr_lines)


c quake files
c      character*80 volcfile, quakefile
c      common /pr5/ volcfile, quakefile(MAX_qfiles)
      
c misc pts
      character*4  name_pt
      common /pt1/ ndef_pts
      common /pt2/ pdef_pt(1000,2), u_pt(1000,3)
      common /pt3/ name_pt(1000)

c grids
      logical make_grid, on_fault
      integer grid1
      common /gr1/ u_grid(15, MAX_gridpts), 
     .             pgrid(MAX_grids,8)
      common /gr3/ jblock_grid(MAX_gridpts), grid1(MAX_grids)
      common /gr4/ make_grid, on_fault(MAX_gridpts)

c slip vectors = MAX_sv
      character sv_label*30, sv_file*80
      common /sv1/ num_sv, num_svfile, kblk_sv(MAX_sv,2), 
     .             ksv_file(MAX_sv)
      common /sv2/ sv_obs(MAX_sv), sv_sig(MAX_sv),sv_pos(MAX_sv,2),
     .             sv_calc(MAX_sv)
      common /sv3/ sv_label(MAX_sv), sv_file(MAX_sv_files)

c num slip rates = MAX_sr
      character sr_label*30, sr_file*80
      common /sr1/ num_sr, num_sr_file, kblk_sr(MAX_sr,2), 
     .             ksr_file(MAX_sr), ksr_type(MAX_sr)
      common /sr2/ sr_obs(MAX_sr,2), sr_sig(MAX_sr),
     .             sr_pos(MAX_sr,2), sr_calc(MAX_sr), sr_az(MAX_sr)
      common /sr3/ sr_label(MAX_sr), sr_file(MAX_sr_files)

c line length changes = MAX_ll
      character*4 name_ll
      common /ll1/ num_ll, num_ll_sites, num_site_ll(MAX_ll,2),
     .             ll_block(MAX_ll)
      common /ll2/ name_ll(MAX_ll)
      common /ll3/ pos_ll(MAX_ll,2), obs_ll(MAX_ll), sig_ll(MAX_ll),
     .             calc_ll(MAX_ll), vel_ll(MAX_ll,2), daz_ll(MAX_ll,2)

c faults=MAX_f, x-nodes=MAX_x, z-nodes=MAX_z, MAX_nodes = MAX_x*MAX_z
      logical phi_free, useGF, fflag, fprm_fixed, from_pio
      logical node_hwblock, nodes_sigma, nn_read, nv_read

      character fault_name*10, dd_line*240, fwname*4

      common /fa1/ nxf(MAX_f), nzf(MAX_f), 
     .             khw_blk(MAX_x,MAX_z,MAX_f),
     .             kfw_blk(MAX_x,MAX_z,MAX_f), 
     .             nfault, ksliptype(MAX_f),
     .             nfcnct(100,2), nfcncts, 
     .             nfmerge(100,2), nfmerges, 
     .             fault_fit_type(MAX_f), 
     .             nfault_poly(MAX_f), 
     .             node_prof(MAX_f, MAX_x),
     .             gf_code(MAX_f,MAX_x,MAX_z,6),
     .             NN_in(MAX_f, MAX_nodes),
     .             kfc(MAX_fc, MAX_fc),
     .             MAX_nix, MAX_niy, MAX_xin, MAX_zin

      common /fa2/ slip_n(MAX_x,MAX_z,MAX_f,5), 
     .             smooth(MAX_f,6),
     .             xynode(2,MAX_x,MAX_z,MAX_f), 
     .             xwnode(2,MAX_x,MAX_z,MAX_f), 
     .             znode(MAX_z,MAX_f), 
     .             phi(MAX_x,MAX_z,MAX_f), 
     .             phi_err(MAX_x,MAX_z,MAX_f),
     .             fault_sums(MAX_f,7), 
     .             cff(MAX_x,MAX_z,MAX_f)

      common /fa9/ depth_max(MAX_f), 
     .             tmom_node(MAX_x,MAX_z,MAX_f),
     .             fault_dip(MAX_x,MAX_z,MAX_f), 
     .             fault_strike(MAX_x,MAX_z,MAX_f), 
     .             av_fault_strike(MAX_f), 
     .             Gskew(MAX_f), 
     .             fault_poly(2*(MAX_x+MAX_z),2,MAX_f), 
     .             f_parm(MAX_f, MAX_x, 3), 
     .             f_parm_err(MAX_f, MAX_x, 3),
     .             fault_dip_az(MAX_f, MAX_x), 
     .             VN_in(MAX_f, MAX_nodes)
     
      common /fa3/ X_interp, W_interp, GFx_interp, GFw_interp,
     .             sd_umin, tr_umin, snap
      common /fa4/ kmode, 
     .             nphi(MAX_x,MAX_z,MAX_f), node_fix(MAX_f, MAX_nodes)
      common /fa5/ node_hwblock, nodes_sigma
      common /fa6/ nn_read(MAX_f), nv_read(MAX_f), 
     .             phi_free(MAX_x,MAX_z,MAX_f), useGF(MAX_f),
     .             fflag(MAX_f,5), fprm_fixed(MAX_f,3), from_pio(MAX_f)
      common /fa7/ fault_name(MAX_f), dd_line(MAX_f, MAX_z-1), 
     .             fwname(MAX_f,2)

c  transient sources
      logical sflag, mmparm, srce_inv, tsliceflag
      integer tnode_prof
      character*10 source_name

      common /ts1/ transient(MAX_srce, 20),
     .             errtransient(MAX_srce, 20), 
     .             tminmax(MAX_srce, 31, 2), 
     .             tr_def(MAX_srce,MAX_gps,3),
     .             tr_ins(MAX_srce,MAX_insar_pts,3),
     .             tslice(MAX_t_slice,2)

      common /ts2/ Tarrive(MAX_srce,MAX_gps),
     .             tphi (MAX_x, MAX_z, MAX_srce), 
     .             dtnode(MAX_x, MAX_z, MAX_srce), 
     .             danode(MAX_x, MAX_z, MAX_srce, 3),
     .             tphi_err(MAX_x,MAX_z,MAX_srce),
     .             atau(MAX_srce,  MAX_tau),
     .             tau_smooth(MAX_srce),
     .             errtau(MAX_srce,MAX_tau)

      common /ts3/ dtau(MAX_srce),
     .             tdur(MAX_srce), tdur2(MAX_srce),
     .             trsmooth(MAX_srce,6),
     .             stf_area(MAX_srce),
     .             slip_t(MAX_x, MAX_z, MAX_srce,2),
     .             tf_parm(MAX_srce, MAX_x, 3), 
     .             tf_parm_err(MAX_srce, MAX_x, 3)

      common /ts4/ win(MAX_f, MAX_x, 3),
     .             twin(MAX_srce, MAX_x, 3),
     .             damp_poly(MAX_srce),
     .             rpoly(MAX_srce, 20),
     .             rpoly_err(MAX_srce, 20),
     .             VN_tr(MAX_srce, MAX_nodes),     
     .             trans_sums(MAX_srce, 7)
     
      common /ts5/ nsource_parm(MAX_srce, 20), 
     .             info_source(MAX_srce, 8),
     .             ntransient(MAX_srce, 30), 
     .             ntau_parm(MAX_srce, MAX_tau),
     .             nrad_parm(MAX_srce, 20),
     .             kfslice(MAX_t_slice),
     .             kdamp2(10,2), isrce_order(MAX_srce)

      common /ts6/ ntphi(MAX_x,MAX_z,MAX_srce),
     .             tnode_prof(MAX_srce, MAX_x),
     .             kev_grid(MAX_srce, 4),
     .             NN_tr(MAX_srce, MAX_nodes),
     .             nodefree(MAX_x,MAX_z,MAX_srce),
     .             npars_eq, npar_eq(30,6), npar_eq_type(30)

      common /ts7/ sflag(MAX_srce),
     .             srce_inv(MAX_srce),
     .             mmparm(MAX_srce,30),
     .             tsliceflag(MAX_t_slice, MAX_srce)

      common /ts8/ source_name(MAX_srce)

c mantle relaxation signals, read in GFs from visco1d, estimate amplitude
      character rlx_file*80
      logical rlx_inv_flag
      common /mr1/ num_mrlx, mrlx_pts(MAX_mrlx_files), 
     .             kprlx(MAX_mrlx_files)
      common /mr2/ rlxGF (MAX_mrlx_files, MAX_mrlx_pts, 5),
     .             rlxParms(MAX_mrlx_files, 7)
      common /mr3/ rlx_file(MAX_mrlx_files)
      common /mr4/ rlx_inv_flag(MAX_mrlx_files)

c for the subsurface strain calculations at nodes
c**c      common /fa11/ s_tensor(MAX_x,MAX_z,MAX_f,6)

c max tilts=MAX_tilt
      character*8 tilt_name
      common /tlt1/ num_tilts
      common /tlt2/ tilt_name(MAX_tilt)
      common /tlt3/ tilt_obs(MAX_tilt), tilt_sig(MAX_tilt), 
     .              tilt_pos(MAX_tilt,2,2), tilt_length(MAX_tilt), 
     .              tilt_calc(MAX_tilt) 

c max shear strain rates =MAX_ss
      character*10  ss_name
      integer ss_type, ss_block
      common /ss1/ ss_name(MAX_ss)
      common /ss2/ ss_pos(MAX_ss,4,2), ss_obs(MAX_ss,3),
     .             ss_sig(MAX_ss,3), ss_calc(MAX_ss,3), 
     .             ss_vel(MAX_ss,9,2) 
      common /ss3/ num_ss, ss_type(MAX_ss), ss_block(MAX_ss,9)


c fault slip files
      integer fs_block
      common /fsp1/ num_fs, fs_block(MAX_num_fs,2)
      common /fsp2/ fs_xy(MAX_num_fs,3)


c time series displacement data
      integer XVflag
      character*4 tfilt_name, site_segs 
      character*80 rm_filename, off_filename
      common /dis1/ num_disp, num_disp_used, num_ts, num_tfilt, 
     .              num_segs, num_f_rm, num_f_off
      common /dis2/ t_disp(MAX_disp),  x_disp(MAX_disp,3), 
     .              x_sig(MAX_disp,3), x_calc(MAX_disp,3),
     .              GVo(MAX_gps,3,7), timespan(MAX_gps,2),
     .              GXo(MAX_gps,3,MAX_tsegs), off_sign(10),
     .              tfilt(MAX_gps_files,6), dtsyn0,
     .              t_segs(MAX_tsegs,10,2), o_segs(MAX_gps,3,10,2)  

      common /dis3/ ndx(MAX_gps,2), 
     .              XVflag(MAX_gps,3,2), 
     .              i_segs(MAX_tsegs,3),
     .              krm_filename_type(10)

      common /dis4/ tfilt_name(MAX_gps_files), site_segs(MAX_tsegs,2),
     .              rm_filename(10), off_filename(10)

c rotations = MAX_rot
      character*40 rot_name
      common /rot1/ num_rot, num_rotfile, kblk_rot(MAX_rot), 
     .              krot_file(MAX_rot)
      common /rot2/ rot_obs(MAX_rot),   rot_sig(MAX_rot),
     .              rot_pos(MAX_rot,2), rot_calc(MAX_rot)
      common /rot3/ rot_name(MAX_rot)

c max hard constraints = MAX_hc
      logical hc_flag
      integer hc, hc_block
      common /hc1/ hc(MAX_hc), hc_block(MAX_hc,2), num_hc
      common /hc2/ hc_pos(MAX_hc,2), hc_val(MAX_hc,3), hc_pen(MAX_hc)
      common /hc3/ hc_flag(MAX_hc)

c parameter arrays   
      integer first_strain_parm, first_node_parm 
      common /sa6/ parm(MAX_parms), parm_best(MAX_parms)
      common /sa6a/ parm_err(MAX_parms), pentype(MAX_pen)
c , gf_new(MAX_gps,3) 

      common /sa7/ nodes_free, first_node_parm, nparms, nparmd
      common /sa7a/ npp(MAX_x, MAX_z, MAX_f), first_strain_parm
      common /sa7b/ nparm_types, nppt(MAX_x, MAX_z, MAX_srce)

      common /sa8/ ndat_gf(6), num_gf_new

      common /sa9/ kparm_type(MAX_parms), kparm_index(MAX_parms),
     .   node_index(MAX_parms) 

c END of include
