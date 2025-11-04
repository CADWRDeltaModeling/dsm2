module common_vars
    !! Module defining common variables and procedures used throughout DSM2.
    use constants
    implicit none
    ! Common variables used throughout DSM2

    integer :: memory_buffer = 20                  !< time buffer use to store hdf5 time series
    integer :: n_comp = LARGEINT                   !< number of computational points
    integer :: n_chan = LARGEINT                   !< number of channels
    integer :: n_segm = LARGEINT                   !< number of segments
    integer :: n_conn = LARGEINT                   !< number of connected cells
    integer :: n_boun = LARGEINT                   !< number of boundaries
    integer :: n_junc = LARGEINT                   !< number of junctions
    integer :: n_node = LARGEINT                   !< number of DSM2 nodes
    integer :: n_non_sequential = LARGEINT         !< number of non-sequential nodes
    integer :: n_xsect = LARGEINT                  !< number of entries in virt xsect table
    integer :: n_resv = LARGEINT                   !< number of reservoirs
    integer :: n_resv_conn = LARGEINT              !< number of reservoir connects
    integer :: n_resv_flow = LARGEINT              !< number of reservoir flow connects
    integer :: n_qext = LARGEINT                   !< number of external flows
    integer :: n_tran = LARGEINT                   !< number of transfer flows
    integer :: n_gate = LARGEINT                   !< number of gates
    integer :: n_flwbnd = LARGEINT                 !< number of boundary flow
    integer :: n_stgbnd = LARGEINT                 !< number of boundary stage
    integer :: n_sflow = LARGEINT                  !< number of source flow
    integer :: n_bfbs = LARGEINT                   !< number of boundary flows and stage
    integer :: n_cell = LARGEINT                   !< number of cells in the entire network
    integer :: n_var = LARGEINT                    !< number of variables
    integer :: nquadpts = 3                 !< Number of quadrature points

    real(kind=dp) :: theta = 0.6D0
        !! Weighting factor for implicit scheme
    real(kind=dp) :: quadwt(MaxQuadPts)
    real(kind=dp) :: quadpt(MaxQuadPts)

contains

end module common_vars
