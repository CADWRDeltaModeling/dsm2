module constants_qual
    !! Module defining constant index numbers for water quality
    implicit none

    ! module variables
    ! non-conservative constituents index numbers
    integer, parameter :: ncc_do = 1
    integer, parameter :: ncc_organic_n = 2
    integer, parameter :: ncc_nh3 = 3
    integer, parameter :: ncc_no2 = 4
    integer, parameter :: ncc_no3 = 5
    integer, parameter :: ncc_organic_p = 6
    integer, parameter :: ncc_po4 = 7
    integer, parameter :: ncc_algae = 8
    integer, parameter :: ncc_bod = 9
    integer, parameter :: ncc_temp = 10
    integer, parameter :: ncc_ssc = 11
    integer, parameter :: ncc_turbidity = 12
    integer, parameter :: ncc_hgii = 13
    integer, parameter :: ncc_mehg = 14
    integer, parameter :: ncc_hg0 = 15
    integer, parameter :: ncc_hgii_s1 = 16
    integer, parameter :: ncc_hgii_s2 = 17
    integer, parameter :: ncc_hgii_s3 = 18

    !!coefficient type index numbers
    ! NOTE: These are from GTM. QUAL does nat have input and the list starts from decay with 1.
    integer, parameter :: input = 1
    integer, parameter :: decay = 2
    integer, parameter :: settle = 3
    integer, parameter :: benthic = 4
    integer, parameter :: alg_grow = 5
    integer, parameter :: alg_resp = 6
    integer, parameter :: alg_die = 7

contains
end module constants_qual
