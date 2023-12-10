c  tdefcom2 2011.12.16
c  commons for simulated annealing routines
c  dimensions set in tdefcom1.h


      real*8 i_gf
      character g_gf_date*12, i_gf_date*12

c Green's function arrays
      common /gf2/  index_gf(MAX_x, MAX_z, MAX_f)
      common /gf3/  g_gf_date(MAX_nodes), i_gf_date(MAX_nodes)
      common /gf4/  g_gf(MAX_nodes, MAX_gps,   6), 
     .              t_gf(MAX_nodes, MAX_tilt,  4),
     .              s_gf(MAX_nodes, MAX_ss,    9, 2, 2)
      common /gf5/
     .              x_gf(MAX_nodes, MAX_ll,        4),
     .              i_gf(MAX_nodes, MAX_insar_pts, 6)

c END
   

