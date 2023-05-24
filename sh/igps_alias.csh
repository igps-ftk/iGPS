alias  liw  'ls intf_all | wc -l'
alias  lpw  'ls intf_all_png | wc -l'
alias  lxw  'ls intf_all_x2 | wc -l'
alias  dss  'du -hs sbas*'
alias  sq   'squeue -o "%A %R %.58j %T"'
alias  lm   'ls | more'
alias  rs   'sh_grd2kml && sh_grd2kml vel_mask_ll && sh_esa_s1_sbas_tar'
alias  dF   'du -hs F?/sbas*'


#alias    add_look.csh
#alias    align_batch_ENVI_SLC.csh
#alias    align_ENVI_SLC.csh
#alias    arlib
#alias    cron_ac6_backup_gmtsar
#alias    cron_asf_by_track
#alias    cron_asf_china_east
#alias    cron_asf_tibet
#alias    cron_backup_igps
#alias    cron_dc_get_orbit_rapid
#alias    cron_dc_get_rnx
#alias    cron_esa_s1_arp
#alias    cron_esa_s1_asf_get_csv
#alias    cron_esa_s1_get_aux_orb_gnss
#alias    cron_esa_s1_get_orb
#alias    cron_esa_s1_get_resorb
#alias    cron_esa_s1_zip_check
#alias    cron_esa_s1_zip_check8
#alias    cron_gnss_check_rinex_ok
#alias    cron_gps_ninh_check_holding
#alias    cron.node1
#alias    cron_node_remove_old_s1_zip
#alias    cron_node_safe2kml
#alias    cron_node_sync_to_weiyun
#alias    cron_node_upload_s1_zip
#alias    cron_sar_check_slc_md5_g16h
alias  sag  sh_sar_auto_grid_batch #cron_sar_node_auto_grid_batch
#alias    cron_sar_node_auto_grid_batch_update_align
#alias    cron_sar_node_auto_grid_check_bad_align
#alias    cron_sar_node_auto_grid_check_master
#alias    cron_sar_node_auto_grid_create
#alias    cron_sar_node_check_in_progress
#alias    cron_sar_node_check_md5
#alias    cron_sar_node_extract_manifest_safe_by_track_frame
#alias    cron_sar_node_get_metalink
#alias    cron_sar_node_link_s1_files
#alias    cron_sar_node_link_s1_files2
#alias    cron_sar_node_link_s1_files2_lys
#alias    cron_sar_node_link_s1_files2_nas50t
#alias    cron_sar_node_link_s1_files_ac
#alias    cron_sar_node_link_s1_files_ac2
#alias    cron_sar_node_link_s1_files_nas50t
#alias    cron_sar_node_mv_ok_s1
#alias    cron_sync_ac11_up
#alias    cron_sync_ac4_down
#alias    cron_sync_ac4_down_arp
#alias    cron_sync_ac8_up
#alias    cron_sync_livemail
#alias    eml2txt.py
#alias    igps_alias.csh
#alias    intf_tops_fls.csh
#alias    intf_tops_parallel_fls.csh
#alias    ndays
#alias    plot_s1_footprint
#alias    preproc_batch_tops_esd_update.csh
#alias    proj_ll2ra_full.csh
#alias    README
#alias    readme.txt
#alias    run_gamit_qsub
#alias    sar_incidence.pdf
#alias    sar_incidence.pptx
#alias    sh_achfile
#alias    sh_acmnr
#alias    sh_acofile
#alias    sh_acqfile
#alias    sh_acsp3
#alias    sh_afile
#alias    sh_alos_intf_in_get_remain
#alias    sh_alos_link_raw
#alias    sh_alos_prep_align
#alias    sh_alos_prep_align_all
#alias    sh_alos_prep_intf_batch
#alias    sh_alos_prep_preproc
#alias    sh_alos_prep_preproc_batch
#alias    sh_alos_prep_sbas
#alias    sh_alos_unzip
#alias    sh_alos_unzip_workreport
#alias    sh_arch
#alias    sh_asf_csv_all_to_ones
#alias    sh_casefolds
#alias    sh_check_los_ll_png
#alias    sh_chk_rinex
#alias    sh_clean_expt
#alias    sh_clean_tables
#alias    sh_count_num_lys
#alias    sh_create_input_lst
#alias    sh_envisat_link_raw
#alias    sh_envisat_prep_intf_batch
#alias    sh_envisat_preprocess
#alias    sh_envisat_prep_sbas
#alias    sh_envislc_intf_in_get_remain
#alias    sh_ers2_link_raw
#alias    sh_esa_s1_align_node_caller
#alias    sh_esa_s1_align_node_group
#alias    sh_esa_s1_all_link_here
#alias    sh_esa_s1_arp
#alias    sh_esa_s1_asf_list_by_track
#alias    sh_esa_s1_assemble_snap
alias  csb  sh_sar_call_sbas
#alias    sh_esa_s1_check_md5
#alias    sh_esa_s1_chmod_node
#alias    sh_esa_s1_corr_mask
#alias    sh_esa_s1_del_zip
#alias    sh_esa_s1_disp_extract_time_series
alias es   sh_s1_expt_safe
#alias    sh_esa_s1_get_csv
#alias    sh_esa_s1_get_data_list_asf
#alias    sh_esa_s1_get_data_meta4_asf
#alias    sh_esa_s1_get_data_py_asf
#alias    sh_esa_s1_get_in_polygon_tibetan_plateau
#alias    sh_asf_s1_get_metalink
#alias    sh_esa_s1_get_orb
#alias    sh_esa_s1_get_resorb
#alias    sh_esa_s1_gsar_expt_safe
#alias    sh_esa_s1_intf_all_mv_bad
#alias    sh_esa_s1_intf_all_relink_LED_PRM
#alias    sh_esa_s1_intf_grd_2_classic
#alias    sh_esa_s1_intf_grd_2_classic_intf_tab
#alias    sh_esa_s1_intf_grd_2_classic_intf_tab_cut
#alias    sh_esa_s1_intf_grd_cut_roi
alias  irsp  sh_sar_intf_all_resample
#alias    sh_esa_s1_intf_in_del_bad
#alias    sh_esa_s1_intf_in_del_bad_ymd
#alias    sh_esa_s1_intf_in_from_intf_all.all
alias  irem  sh_s1_intf_in_unfinished
#alias    sh_esa_s1_intf_in_sort_by_los
#alias    sh_esa_s1_kml2ra
#alias    sh_esa_s1_landslide_batch
#alias    sh_esa_s1_landslide_clean_intf_all
#alias    sh_esa_s1_link_g4d
#alias    sh_esa_s1_link_g4d_2
#alias    sh_esa_s1_link_g4d_nas50t
#alias    sh_esa_s1_link_h2
#alias    sh_esa_s1_link_h2_2
#alias    sh_esa_s1_link_node_alll
#alias    sh_esa_s1_link_orb
#alias    sh_esa_s1_link_orb2
#alias    sh_esa_s1_link_orb3
#alias    sh_esa_s1_link_orb4
#alias    sh_esa_s1_link_raworig
#alias    sh_esa_s1_link_remove_broken
#alias    sh_esa_s1_link_slc
#alias    sh_esa_s1_link_slc2
#alias    sh_esa_s1_metalink_to_md5
#alias    sh_esa_s1_mv_aria2_tmp
#alias    sh_esa_s1_mv_by_track
#alias    sh_esa_s1_mv_by_track2
#alias    sh_esa_s1_mv_correct_track
#alias    sh_esa_s1_mv_zip_inside
#alias    sh_esa_s1_mv_zip_outside
#alias    sh_esa_s1_orb_res_archive
alias  f3  sh_s1_prep_f123
alias  f3i  sh_s1_prep_f123_in
#alias    sh_esa_s1_prep_merge_batch
#alias    sh_esa_s1_prep_merge_batch_from_intf_in
#alias    sh_esa_s1_prep_merge_batch_from_intf_in_F
#alias    sh_esa_s1_prep_merge_batch_from_intf_in_swath12
#alias    sh_esa_s1_prep_prep
#alias    sh_esa_s1_prep_proc
#alias    sh_esa_s1_prep_proc_baseline
#alias    sh_esa_s1_prep_proc_baseline_sort
#alias    sh_esa_s1_prep_proc_full
#alias    sh_esa_s1_prep_proc_super_master
#alias    sh_esa_s1_prep_proc_time_sort
#alias    sh_esa_s1_prep_sbas
#alias    sh_esa_s1_query_asf
#alias    sh_esa_s1_rsync_esa_data
#alias    sh_esa_s1_rsync_zip
#alias    sh_esa_s1_rsync_zip_inside
#alias    sh_esa_s1_rsync_zip_inside_node
#alias    sh_esa_s1_run_aria2_batch
alias  tsa  sh_s1_run_tsa
alias  tsau  sh_s1_run_tsa_update
alias  esb  sh_sar_sbas
alias  esbt  sh_sar_sbas_tar
#alias    sh_esa_s1_slc_test
#alias    sh_esa_s1_stitch_snap
#alias    sh_esa_s1_track_ad
alias  eu  sh_s1_unzip
alias  euc  sh_s1_unzip_clean
#alias    sh_esa_s1_unzip_manifest
#alias    sh_esa_s1_unzip_manifest2
#alias    sh_esa_s1_unzip_manifest_ac
#alias    sh_esa_s1_unzip_manifest_track
alias  eun  sh_s1_unzip_node
#alias    sh_esa_s1_unzip_node_caller
#alias    sh_esa_s1_unzip_node_group
#alias    sh_esa_s1_unzip_node_parallel
#alias    sh_esa_s1_unzip_remote
#alias    sh_esa_s1_unzip_test
#alias    sh_esa_s1_unzip_test_doit
#alias    sh_esa_s1_wget_data_list_asf
#alias    sh_esa_s1_xml_to_md5
#alias    sh_esa_s1_zip_del_vh
#alias    sh_gamit_igps
#alias    sh_get_in_triangle
#alias    sh_gmtsar_sbas_grd_mask
#alias    sh_gps_sinfo_not_in
alias  gj  sh_grd2jpg
alias  gk  sh_grd2kml
#alias    sh_grd2nc
#alias    sh_grd2nc_noproj
#alias    sh_grd2xyz
#alias    sh_grd_detrend_ref
#alias    sh_grd_dims
#alias    sh_grd_ra2ll_nc
#alias    sh_grd_resample
#alias    sh_grd_resample_mask
#alias    sh_igps_backup
#alias    sh_igpsftk_clean_make
#alias    sh_intf_all_detrend_resample
#alias    sh_intf_remove_trend
#alias    sh_link_slc_iw_tracks
#alias    sh_lsattr_esa_data
#alias    sh_mail
#alias    sh_mount_ac
#alias    sh_mv_hfiles
#alias    sh_mv_rnx
#alias    sh_nc2jpg
#alias    sh_new
#alias    sh_org_sig
#alias    sh_org_xyz
#alias    sh_plot_scalebar
alias  p2j  sh_png2jpg
#alias    sh_ps2jpg
#alias    sh_ps2pdf
#alias    sh_qsub_run
#alias    sh_recompile_gmt5sar
#alias    sh_reDown_bad_rnx
#alias    sh_rep_cart_info
#alias    sh_resample_intf_all
#alias    sh_run_gamit
#alias    sh_run_globk
alias  agr  sh_sar_auto_grid_restore_to_init
#alias    sh_sar_baseline_2_csv
#alias    sh_sar_cal_sat_incidence
#alias    sh_sar_clean_intf
#alias    sh_sar_clean_intf_all
#alias    sh_sar_clean_intf_all_all
#alias    sh_sar_clean_intf_all_merge
#alias    sh_sar_clean_intf_more
#alias    sh_sar_clean_merge
#alias    sh_sar_copy_F
alias  clp  sh_sar_cp_intf_png
#alias    sh_sar_cp_snaphued
#alias    sh_sar_create_link_F123
#alias    sh_sar_expt_rename1
#alias    sh_sar_gacos_apply_intf
#alias    sh_sar_gacos_apply_intf_add
#alias    sh_sar_gacos_intf_all
#alias    sh_sar_gacos_link_intf
#alias    sh_sar_gacos_ll2ra
#alias    sh_sar_gacos_tgz_unzip
#alias    sh_sar_gacos_time
#alias    sh_sar_gacos_to_gmtsar
#alias    sh_sar_gacos_ztd2ll
#alias    sh_sar_grd2envi
#alias    sh_sar_gsar_list
#alias    sh_sar_incidence_grd
#alias    sh_sar_intf_in_get_remain
#alias    sh_sar_intf_in_sort_by_coherence
#alias    sh_sar_name_expt
#alias    sh_sar_node_auto_grid_create_input_list
#alias    sh_sar_node_clean_f123_intf_all
#alias    sh_sar_node_clean_f123_SLC
#alias    sh_sar_node_clean_f123_SLC2
#alias    sh_sar_node_clean_Fx_intf_all
#alias    sh_sar_node_del_merged_tiff
#alias    sh_sar_node_link_s1_files_by_path_frame
#alias    sh_sar_node_link_s1_files_by_path_frame2
#alias    sh_sar_node_wc_l_f123_intf_all
#alias    sh_sar_plot_intf_in
alias  pit  sh_sar_plot_intf_tab
alias  pv  sh_sar_plot_vel_profile
#alias    sh_sar_prep_align_in
alias  sps  sh_sar_prep_sbases
#alias    sh_sar_run_sbas
#alias    sh_sar_sbas_grd_detrend
#alias    sh_sar_sbas_grd_ref
#alias    sh_sar_sbas_look
#alias    sh_sar_sbas_ref_ll2rc
#alias    sh_sar_sbas_ref_ll2rc.ll2rc
#alias    sh_sar_sbas_reproj
alias  sbp  sh_sar_sbas_tab_by_png
alias  sbpf  sar_sbas_tab_from_png
#alias    sh_sar_unwrap_mask_ll_png
#alias    sh_sar_unwrap_mask_png
#alias    sh_sar_xcorr_stat
#alias    sh_sdm_mdl_plot
#alias    sh_sdm_plot_fault_slip
#alias    sh_sitminus
#alias    sh_sitsit
#alias    sh_slc_cut
alias  sit  sh_slurm_intf_tops
#alias    sh_slurm_intf_tops_fls
alias  smf  sh_slurm_merge_batch_F
#alias    sh_slurm_merge_batch_Fold
#alias    sh_stat_hfile
#alias    sh_touch_zips
#alias    sh_tsplot_sio
#alias    sh_txtfile_minus
#alias    sh_vel2sdm
#alias    sh_wget_highrate
#alias    sh_wget_ion
#alias    sh_wget_nav
#alias    sh_wget_orb
#alias    sh_wget_rnx
#alias    sh_xyz2nc
#alias    snaphu_fls.csh
#alias    use_ifc
#alias    
#alias    admin:
#alias    ac
#alias    cron
#alias    etc.hosts
#alias    home.root
#alias    home.tianyf
#alias    httpd
#alias    ids
#alias    ids_12
#alias    ids2
#alias    mount
#alias    nis
#alias    sh_user_tianyf
#alias    slurm
#alias    slurm.8
#alias    smb
#alias    ssh.root
#alias    ssh.tianyf
#alias    sync
#alias    timezone
#alias    vnc
#alias    vsftpd
#alias    yum
#alias    
#alias    old:
#alias    sh_esa_s1_assemble
#alias    sh_esa_s1_assemble1_pins
#alias    sh_esa_s1_assemble2
#alias    sh_esa_s1_assemble2_pins
#alias    sh_esa_s1_assemble2_snap
#alias    sh_esa_s1_assemble3
#alias    sh_esa_s1_assemble3a
#alias    sh_esa_s1_assemble3b
#alias    sh_esa_s1_assemble3_pins
#alias    sh_esa_s1_assemble3_snap
#alias    sh_esa_s1_assemble4
#alias    sh_esa_s1_assemble_pins
