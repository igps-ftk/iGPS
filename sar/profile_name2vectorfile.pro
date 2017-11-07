PRO PROFILE_NAME2VECTORFILE,   $
    fa,   $ ;input, fault name
    ffile=ffile,  $ ;output, fault file
    pfile=pfile ;output, profile file
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  ;program test
  IF N_PARAMS() LT 1 THEN BEGIN
    fa='dd'
    ;fa='jiali'
  ENDIF
  
  ffile=''
  pfile=''
  
  IF N_ELEMENTS(fa) NE 1 THEN BEGIN
    PRINT,'['+prog+']ERROR:wrong input parameter!'
    HELP, fa
    RETURN
  ENDIF
  
  ;fault and profile files
  CASE fa OF
    'anduo_sewa': BEGIN
      ffile='D:\data\vector\profile\fault_anduo_sewa.psxy'
    ;pfile='D:\data\vector\profile\profile_u1.psxy'
    END
    'altyntagh': BEGIN
      ffile='D:\data\vector\profile\fault_altyntagh.psxy'
      pfile='D:\data\vector\profile\profile_altyntagh.psxy'
    END
    'altyntagh3': BEGIN
      ffile='D:\data\vector\profile\fa_altyntagh3.psxy'
      pfile='D:\data\vector\profile\pf_altyntagh3.psxy'
    END
    'bengco': BEGIN
      ffile='D:\data\vector\profile\fault_beng_co_east.psxy'
      ffile='D:\data\vector\profile\fault_beng_co_west.psxy'
      ;ffile='D:\data\vector\profile\fault_west_to_bengco.psxy'
      ffile='D:\data\vector\profile\fa_bengco_all.psxy'
      pfile='D:\data\vector\profile\profile_bengco2.psxy'
    ;    pfile='D:\data\vector\profile\ebc1.psxy'
    END
    'coe_west': BEGIN
      ffile='D:\data\vector\profile\fa_coe_west.psxy'
    END
    'coe_west_v2': BEGIN
      ffile='D:\data\vector\profile\fa_coe_west_v2.psxy'
    END
    'coe_west_perp': BEGIN
      ffile='D:\data\vector\profile\fa_coe_west_perp.psxy'
    END
    'cona_east': BEGIN
      ffile='D:\data\vector\profile\fault_cona_east.psxy'
      pfile='D:\data\vector\profile\profile_cona_east.psxy'
    END
    'dongqiao2': BEGIN
      ffile='D:\data\vector\profile\fault_dongqiao2.psxy'
      pfile='D:\data\vector\profile\profile_dongqiao.psxy'
    END
    'duoma_nima': BEGIN
      ffile='D:\data\vector\profile\fault_duoma_nima.psxy'
      pfile='D:\data\vector\profile\profile_duoma_nima.psxy'
    END
    'east_lhasa_block_fault': BEGIN
      ffile='D:\data\vector\profile\east_lhasa_block_fault.psxy'
    END
    'eq_gongbujiangda1': BEGIN
      ffile='D:\data\vector\profile\eq_gongbujiangda1.psxy'
    END
    'eq_gongbujiangda1_perp': BEGIN
      ffile='D:\data\vector\profile\eq_gongbujiangda1_perp.psxy'
    END
    'eq_yutian08': BEGIN
      ffile='D:\data\vector\profile\eq_yutian08.psxy'
    END
    'fake_cuoe': BEGIN
      ffile='D:\data\vector\profile\fa_fake_cuoe.psxy'
    END
    'fake_cuoe2': BEGIN
      ffile='D:\data\vector\profile\fa_fake_cuoe2.psxy'
    END
    'fake_lasa': BEGIN
      ffile='D:\data\vector\profile\fa_fake_lasa.psxy'
    END
    'fake_mila': BEGIN
      ffile='D:\data\vector\profile\fa_fake_mila.psxy'
    END
    'fake_sangri': BEGIN
      ffile='D:\data\vector\profile\fault_fake_sangri.psxy'
    END
    'fake_shuanghu_north': BEGIN
      ffile='D:\data\vector\profile\fa_fake_ew_shuanghu_north.psxy'
    END
    'fake_yangbajing_electricity_plant': BEGIN
      ffile='D:\data\vector\profile\fa_fake_yangbajing_electricity_plant.psxy'
    END
    'gongbujiangda': BEGIN
      ffile='D:\data\vector\profile\fa_gongbujiangda.psxy'
    END
    'gyaringco': BEGIN
      ffile='D:\data\vector\profile\fault_gyaringco.psxy'
      pfile='D:\data\vector\profile\profile_gyaringco.psxy'
    END
    'gyaringco2': begin
      ffile='D:\data\vector\profile\fa_gyaringco.psxy'
    end
    'gulu': BEGIN
      ffile='D:\data\vector\profile\fault_gulu2.psxy'
      pfile='D:\data\vector\profile\profile_gulu.psxy'
    ;pfile='D:\data\vector\profile\profile_gulu.psxy'
    ;ffile='D:\data\vector\profile\fault_ydgl.psxy'
    ;    ffile='D:\data\vector\profile\fault_ydgl2.psxy'
    ;    ffile='D:\data\vector\profile\fault_gulu2.psxy'
    ;    ffile='D:\data\vector\profile\fault_ydgl.psxy'
    ;    pfile='D:\data\vector\profile\profile_gulu.psxy'
    ;;    ffile='D:\data\vector\profile\fault_gulu2.psxy'
    ;;    pfile='D:\data\vector\profile\profile_gulu.psxy'
    END
    'honghe1': BEGIN
      ffile='D:\data\vector\profile\fa_honghe1.psxy'
    END
    'jiali': BEGIN
      ffile='D:\data\vector\profile\fault_jiali.psxy'
      pfile='D:\data\vector\profile\profile_jiali.psxy'
    END
    'kunlun': BEGIN
      ffile='D:\data\vector\profile\fault_kunlun.psxy'
      pfile='D:\data\vector\profile\profile_kunlun.psxy'
    END
    'kunlun1': BEGIN
      ffile='D:\data\vector\profile\fault_kunlun1.psxy'
      pfile='D:\data\vector\profile\profile_kunlun1.psxy'
    END
    'lenglongling': BEGIN
      ffile='D:\data\vector\profile\fault_lenglongling.psxy'
      pfile='D:\data\vector\profile\profile_lenglongling.psxy'
    END
    'longmenshan': BEGIN
      pfile='D:\data\vector\profile\pf_longmenshan.psxy'
      ffile='D:\data\vector\profile\fa_longmenshan.psxy'
    END
    'mani97': BEGIN
      ffile='D:\data\vector\profile\fa_mani97.psxy'
    END
    'mft': BEGIN
      pfile='D:\data\vector\profile\pf_mft.psxy'
      ffile='D:\data\vector\profile\fa_mft.psxy'
    END
    'mila': BEGIN
      ffile='D:\data\vector\profile\fa_mila.psxy'
    END
    'fake_mila': BEGIN
      ffile='D:\data\vector\profile\fa_fake_mila.psxy'
    END
    'mila_tunnel': BEGIN
      ffile='D:\data\vector\profile\fa_mila_tunnel.psxy'
    END
    'mila_tunnel_ext': BEGIN
      ffile='D:\data\vector\profile\fa_mila_tunnel_ext.psxy'
    END
    'naqu_north': BEGIN
      ffile='D:\data\vector\profile\fault_naqu_north.psxy
    END
    'perp_bengco': BEGIN
      ffile='D:\data\vector\profile\fault_perp_bengco.psxy'
      pfile='D:\data\vector\profile\profile_along_bengco.psxy'
    END
    'riganpei': BEGIN
      ffile='D:\data\vector\profile\fault_riganpeico.psxy'
      pfile='D:\data\vector\profile\profile_riganpeico.psxy'
    END
    'sangri_cuona': BEGIN
      ffile='D:\data\vector\profile\fault_sangri_cuona.psxy'
    END
    'u1': BEGIN
      ffile='D:\data\vector\profile\fault_u1.psxy'
      pfile='D:\data\vector\profile\profile_u1.psxy'
    END
    'wulanwula': BEGIN
      ffile='D:\data\vector\profile\fa_wulanwula.psxy'
    END
    'wulanwula_gangqiqu': BEGIN
      ffile='D:\data\vector\profile\fa_wulanwula_gangqiqu.psxy'
    END
    'xianshuihe': BEGIN
      ffile='D:\data\vector\profile\fault_xianshuihe.psxy'
      pfile='D:\data\vector\profile\profile_xianshuihe.psxy'
    END
    'ydgl': BEGIN ;yadong-gulu rift
      ffile='D:\data\vector\profile\fault_ydgl.psxy'
      pfile='D:\data\vector\profile\profile_gulu.psxy'
    ENDCASE
    'ydgl2': BEGIN ;yadong-gulu rift
      ffile='D:\data\vector\profile\fault_ydgl2.psxy'
      pfile='D:\data\vector\profile\profile_gulu.psxy'
    END
    'yzs': BEGIN
      ffile='D:\data\vector\profile\fault_yzs.psxy'
    END
    'zhajiazangbu': BEGIN
      ffile='D:\data\vector\profile\fa_zhajiazangbu.psxy'
    END
    'zhajiazangbu_fake': BEGIN
      ffile='D:\data\vector\profile\fa_fake_zhajiazangbu.psxy'
    END
    ELSE: BEGIN
      PRINT,'['+prog+']ERROR:unknown fault name ['+fa+']!'
      RETURN
    END
  ENDCASE
  
  IF N_PARAMS() LT 1 THEN BEGIN
    PRINT,'fault name:',fa,format='(a13,1x,a)'
    PRINT,'fault file:',ffile,format='(a13,1x,a)'
    PRINT,'profile file:', pfile,format='(a13,1x,a)'
  ENDIF
END
