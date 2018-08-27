 module sed_bed_utils
    
    
 
    
    contains

    subroutine print_last_stage_sed(cdtdate,        &
                                intdate,        &
                                out_conc_resv,  &
                                ncell,          &
                                nresv,          &
                                nvar)                                
        use common_variables, only : constituents, resv_geom    
        use gtm_precision
        use sed_type_defs
        implicit none
        character(len=14), intent(in) :: cdtdate
        integer, intent(in) :: intdate
        integer, intent(in) :: ncell
        integer, intent(in) :: nresv
        integer, intent(in) :: nvar
        real(gtm_real) :: out_conc_resv(nresv,nvar)
        integer :: ncol
        integer :: a(nvar)
        character*16 :: c(nvar)
        integer :: i, j , k        
        ncol = 8    !<nosolids
               
        open(801,file="init_sed.txt")
        write(801,*) cdtdate, "/time"
        write(801,*) intdate, "/julmin"
        write(801,*) ncol, "/n_column"
        write(801,*) ncell, "/n_cell"
        write(801,*) n_zones, "/n_zone"
        write(801,'(a7,a,a7,a, <ncol>(a18,a))') "cell_no",achar(9),"zone",achar(9),"thickness_1",achar(9),"inorg_L1",achar(9),"org_L1",achar(9),"sand_L1",achar(9),"thickness_2",achar(9),"inorg_L2",achar(9),"org_L2",achar(9),"sand_L2"
        do i = 1, ncell
            do j = 1, n_zones
                !write(801,'(i32,i32,<ncol>f32.16)') i, j, bed(i,j,1).thickness, (bed(i,j,1).mass_frac(k),k=1,isolids), bed(i,j,2).thickness, (bed(i,j,2).mass_frac(k),k=1,isolids)
                write(801,'(i5,a,i5,a,<ncol>(f18.15, a))') i, achar(9), j, achar(9),bed(i,j,1).thickness, achar(9), (bed(i,j,1).mass_frac(k),achar(9),k=1,isolids), bed(i,j,2).thickness,achar(9), (bed(i,j,2).mass_frac(k),achar(9),k=1,isolids)
            end do
        end do
        close(801)
        return
        write(801,*) nresv, "/n_resv"   !todo: add reservoirs
        write(801,'(a32,<ncol>a32)') "reservoir_name", (c(j),j=1,ncol)
        do i = 1, nresv
            write(801,'(a32,<ncol>f32.16)') resv_geom(i)%name, (out_conc_resv(i,a(j)),j=1,ncol) 
        end do
        close(801)
        return
end subroutine
                                
                                
                                
end  module