      REAL*8
     &      x                    ! interpolation variables
     &     ,x1
     &     ,x2
     &     ,y1
     &     ,y2
     &     ,temp_all_elev(max_elevations)
                                ! all elevations in current channel incl duplicates
     &     ,temparea(max_assg_sec,max_elevations)
                                ! all areas in current section in current channel
     &     ,tempwet_p(max_assg_sec,max_elevations)
                                ! all wet_p in current section in current channel
     &     ,tempwidth(max_assg_sec,max_elevations)
                                ! all widths in current section in current channel
     &     ,tempz_centroid(max_assg_sec,max_elevations)
                                ! all z cent. values in current sec in current chan
      common /interp_var/x,x1,x2,y1,y2

      common /com_temp_all_elev/
     &     temp_all_elev
      common /com_temp_data/
     &     temparea
     &     ,tempwet_p
     &     ,tempwidth
      common /com_temp_z_centroid/
     &     tempz_centroid
