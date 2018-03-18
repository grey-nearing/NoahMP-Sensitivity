module type_decs

 type forcing_data
  real :: q2, prcprate, lwrad, swrad, sfcprs, sfctmp, sfcspd
  real :: shdfac
 end type forcing_data

 type state_data
  real :: albold, sneqvo, tah, eah, fwet
  real, allocatable, dimension(:) :: stc, zsnso ! nsnow+nsoil
  real, allocatable, dimension(:) :: sh2o, smc
  real :: canliq, canice, tv, tg, qsnow, snowh, sneqv
  integer :: isnow
  real :: zwt, wa, wt, wslake, lfmass,rtmass, stmass, wood, stblcp
  real :: fastcp, lai, sai
  real :: cm, ch, tauss
  real :: smcwtd, deeprech, rech
  real, allocatable, dimension(:) :: tsno, snice, snliq
 end type

 type setup_data
  integer :: ntimes, vegtyp
  real :: zlvl, latitude, longitude, dt
  integer :: nsoil, nsnow
  character(len=12) :: startdate
  integer :: dveg, opt_crs, opt_btr, opt_run, opt_sfc, opt_frz, &
   opt_inf, opt_rad, opt_alb, opt_snf, opt_tbot, opt_stc
  real, allocatable, dimension(:) :: sldpth
  real :: tbot ! this should be a parameter
  real, dimension(12) :: shdfac_monthly
 end type setup_data

 type output_data
  real :: Qe, Qh, NEE
 endtype

end module type_decs
