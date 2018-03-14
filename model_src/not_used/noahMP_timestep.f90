program driver
  use module_io
  use module_sf_noahmplsm, only : dveg, opt_crs, opt_btr, opt_run, opt_sfc, opt_frz, opt_inf, opt_rad, &
       &                          opt_alb, opt_snf, opt_tbot, opt_stc, noahmp_options, redprm, noahmp_sflx, &
       &                          read_mp_veg_parameters
  use kwm_date_utilities

  implicit none

  real, external :: month_d

  character(len=12)   :: startdate        ! Starting date of the data ( YYYYMMDDHHmm ) 
  character(len=12)   :: enddate          ! Ending date of the data ( YYYYMMDDHHmm )
  integer             :: forcing_timestep_second ! The time interval ( seconds ) of the data in the forcing file
  integer             :: noahlsm_timestep_second ! The timestep ( seconds ) to use when integrating the Noah LSM
  real, dimension(12) :: albedo_monthly   ! Monthly values of background (i.e., snow-free) albedo ( Fraction [0.0-1.0] )
  real, dimension(12) :: shdfac_monthly   ! Monthly values for green vegetation fraction ( Fraction [0.0-1.0] )
  real, dimension(12) :: z0brd_monthly    ! Monthly values for background (i.e., snow-free) roughness length ( m )
  real, dimension(12) :: lai_monthly      ! Monthly values for Leaf Area Index ( dimensionless )
  real :: cmc
  integer :: ktime
  integer :: n

  character(len=256) :: executable_name   ! The name of the executable, as found by Fortran library 
  !                                       ! function GETARG

  character(len=256) :: namelist_filename ! The name of the initial conditions file, as found
  !                                       ! by Fortran library function GETARG
  character(len=256) :: forcing_filename    ! The name of the forcing conditions file, as found
  !                                       ! by Fortran library function GETARG
  character(len=4096) :: infotext         ! Character string returned by subroutine OPEN_FORCING_FILE, 
                                          ! giving some possibly useful information for the user.

  integer :: ierr
  integer, parameter :: iunit = 10
  integer, parameter :: ounit = 30
  integer, parameter :: funit = 20
  real :: latitude, longitude
  integer :: isurban
  real :: longwave
  logical :: rdlai2d
  character(len=12) :: nowdate
  real :: sfcspd, sfcu, sfcv
  integer :: slopetype

  real, pointer, dimension(:) :: SLDPTH ! Thicknesses of each soil level
  real, pointer, dimension(:) :: STC_PTR ! Thicknesses of each soil level
  real, pointer, dimension(:) :: SMC_PTR ! Thicknesses of each soil level
  real, pointer, dimension(:) :: SH2O_PTR ! Thicknesses of each soil level
  real :: snoalb
  integer :: soiltype
  real :: t1v, t1, th2v, zlvl_wind
  logical :: use_urban_module, usemonalb


  character(len=256) :: LLANDUSE  ! Land-use dataset.  Valid values are :
  !                               ! "USGS" (USGS 24/27 category dataset) and
  !                               ! "MODIFIED_IGBP_MODIS_NOAH" (MODIS 20-category dataset)
  character(len=256) :: LSOIL = "STAS" ! Soil-category dateset.  Only "STAS" (STATSGO dataset) supported.

  character(len=256) :: teststr
  character(len=256) :: cmd
  integer :: reloop_count = 0
  integer :: spin_up_while

  integer :: ice
  integer :: ist
  integer :: vegtype
  integer :: isc
  integer :: nsnow
  integer :: nsoil
  real, allocatable, dimension(:) :: zsoil
  real    :: dt
  real    :: q2
  real    :: sfctmp
  real    :: uu
  real    :: vv
  real    :: soldn
  real    :: lwdn
  real    :: prcp
  real    :: zlvl
  real    :: co2air
  real    :: o2air
  real    :: cosz
  real    :: tbot
  real    :: foln
  real    :: sfcprs
  integer :: yearlen
  real    :: julian
  real    :: shdfac
  real    :: shdmax
  real    :: lat
  real    :: z0
  integer :: ix
  integer :: iy
  integer :: ipoint
  real    :: eah
  real    :: tah
  real    :: fwet
  real, allocatable, dimension(:) :: ficeold
  real    :: qsnow
  real    :: sneqvo
  integer :: isnow
  real, allocatable, dimension(:) :: zsnso
  real    :: canliq
  real    :: canice
  real    :: snowh
  real    :: sneqv
  real, allocatable, dimension(:) :: snice
  real, allocatable, dimension(:) :: snliq
  real    :: tv
  real    :: tg
  real, allocatable, dimension(:) :: stc
  real, allocatable, dimension(:) :: sh2o
  real, allocatable, dimension(:) :: smc
  real, allocatable, dimension(:) :: tsno
  real    :: zwt
  real    :: wa
  real    :: wt
  real    :: wslake
  real    :: lfmass
  real    :: rtmass
  real    :: stmass
  real    :: wood
  real    :: stblcp
  real    :: fastcp
  real    :: lai
  real    :: sai
  real    :: albold
  real    :: cm
  real    :: ch
  real    :: UST
  real    :: FSA
  real    :: FSR
  real    :: FIRA
  real    :: FSH
  real    :: SSOIL
  real    :: FCEV
  real    :: FGEV
  real    :: FCTR
  real    :: TRAD
  real    :: ECAN
  real    :: ETRAN
  real    :: EDIR
  real    :: RUNSRF
  real    :: RUNSUB
  real    :: APAR
  real    :: PSN
  real    :: SAV
  real    :: SAG
  real    :: FSNO
  real    :: NEE
  real    :: GPP
  real    :: NPP
  real    :: TS
  real    :: FVEG
  real    :: ALBEDO

  real    :: qfx
  real    :: flxsum
  real    :: ir_sh_ev_gh
  real    :: fsa_fsr
  real    :: dz8w
  real    :: bgap
  real    :: wgap
  real    :: tgv
  real    :: tgb
!  real    :: tstar
  real    :: t2mb
  real    :: t2mv
  real    :: rssun
  real    :: rssha
  real    :: qsfc
!  real    :: q1
  real    :: q2v
  real    :: q2b
!  real    :: qmelt
  real    :: qc
  real    :: ponding
  real    :: ponding1
  real    :: ponding2
  real    :: psfc
  real    :: pblh
!  real    :: chstar
  real    :: tauss
!  real    :: chstar2
  real    :: dx
!  real    :: errwat
!  real    :: gap
  real    :: cqstar2
  integer :: iz0tlnd
  real    :: chv
  real    :: chb
  real    :: emissi

  real, allocatable, dimension(:) :: smceq
  real    :: smcwtd
  real    :: deeprech
  real    :: rech
  real    :: qsnbot
  real    :: shg
  real    :: shc
  real    :: shb
  real    :: evg
  real    :: evb
  real    :: ghv
  real    :: ghb
  real    :: irg
  real    :: irc
  real    :: irb
  real    :: tr
  real    :: evc
  real    :: chleaf
  real    :: chuc
  real    :: chv2
  real    :: chb2
  real    :: fpice
  logical :: FirstTime
  character(len=1024) :: output_dir

  logical :: restart

  call getarg(0, executable_name)
  call getarg(1, namelist_filename)

  if (namelist_filename == "") then
     write(*,'(/," ***** Problem:  Program expects a command-line argument *****")')
     write(*,'(" ***** Please specify the namelist filename on the command-line.")')
     write(*,'(" ***** E.g.:  ''",A,1x,A,"''",/)') trim(executable_name), "namelist"
     stop ":  ERROR EXIT"
  endif

  call open_namelist_file(iunit, output_dir, namelist_filename,forcing_filename,infotext, nsoil, startdate, enddate, spin_up_while, & 
       latitude, longitude, &
       forcing_timestep_second, noahlsm_timestep_second, ice, t1, stc_ptr, smc_ptr, sh2o_ptr, sldpth, cmc, snowh, sneqv, tbot,        &
       dveg, opt_crs, opt_btr, opt_run, opt_sfc, &
       opt_frz, opt_inf, opt_rad, opt_alb, opt_snf, opt_tbot, opt_stc, &
       vegtype, soiltype, slopetype, snoalb, zlvl, zlvl_wind, albedo_monthly, shdfac_monthly,                  &
       z0brd_monthly, lai_monthly, use_urban_module, isurban, usemonalb, rdlai2d, llanduse)
 if (trim(forcing_filename) == "") then
     write(*,'(/," ***** Problem:  Program expects a command-line argument *****")')
     write(*,'(" ***** Please specify the forcing filename in the namelist")')
     stop ":  ERROR EXIT"
  endif
 
 open(funit, file=trim(forcing_filename), form='formatted', action='read', iostat=ierr)
  if (ierr /= 0) then
       write(*,'("Problem opening file ''", A, "''")') trim(forcing_filename)
       stop ":  ERROR EXIT"
  endif
  if (opt_sfc == 3 .or. opt_sfc ==4) then
     stop "(OPT_SFC == 3) and (OPT_SFC == 4) are not for offline use"
  endif

  if (opt_run == 5) then
     stop " OPT_RUN==5 (groundwater scheme)are not for single point driver"
  endif

  call soil_veg_gen_parm( LLANDUSE, LSOIL )

  call read_mp_veg_parameters ( LLANDUSE )

  ist     = 1  ! Surface type:  IST=1 => soil;  IST=2 => lake
  isc     = 4  ! Soil color type
  ice     = 0  ! Surface type:  ICE=0 => soil;  ICE=1 => sea-ice

#ifdef _OUT_
  albold = albtbl(vegtype)
  z0     = z0tbl (vegtype)
#else
  albold = 0.19
#endif

  ! Results are not sensitive to the LAI initialization, because 
  ! LAI gets updated in SFLX depending on RTMASS.  Does this depend 
  ! on the Noah-MP options chosen?
#ifdef _OUT_
  lai    = laitbl(vegtype)
#endif
  lai    = 0.5 ! Try this instead

  allocate(zsoil(nsoil))
  allocate(smceq(nsoil))

  ! ZSOIL is negative.
  zsoil(1) = -sldpth(1)
  do n = 2, nsoil
     zsoil(n) = zsoil(n-1) - sldpth(n)
  enddo

  dt = noahlsm_timestep_second

  nsnow   = 3
  ix      = 1  ! Unused, but probably handy for output and debugging.
  iy      = 1  ! Unused, but probably handy for output and debugging.
  ipoint  = 1  ! Unused, but probably handy for output and debugging.

  allocate(ficeold(-nsnow+1:0))
  allocate(zsnso(-nsnow+1:nsoil))
  allocate(snice(-nsnow+1:0))
  allocate(snliq(-nsnow+1:0))
  allocate(stc(-nsnow+1:nsoil))
  allocate(sh2o(1:nsoil))
  allocate(smc(1:nsoil))
  allocate(tsno(-nsnow+1:0))

  tauss   = 0.0    ! Initialize with new snow.
  qsnow   = 0.0    ! Initialization value from NOAH-MP-WRF
  sneqvo  = 0.0    ! Initialization value from NOAH-MP-WRF
  fwet    = 0.00   ! Initialization value from NOAH-MP-WRF
  foln    = 1.0    ! Initialization value from NOAH-MP-WRF
  wa      = 4900.0 ! Initialization value from NOAH-MP-WRF
  wt      = wa     ! Initialization value from NOAH-MP-WRF
  zwt     = (25.0 + 2.0) - wa/1000.0/0.2 ! Initialization value from NOAH-MP-WRF
  wslake  = 0.0    ! Initialization value from NOAH-MP-WRF
  rtmass  = 500.0  ! Initialization value from NOAH-MP-WRF
  wood    = 500.0  ! Initialization value from NOAH-MP-WRF
  stblcp  = 1000.0 ! Initialization value from NOAH-MP-WRF
  fastcp  = 1000.0 ! Initialization value from NOAH-MP-WRF
  sai     = 0.1    ! Initialization value from NOAH-MP-WRF

  cm      = 0.00 ! Initialization value from NOAH-MP-WRF
  ch      = 0.00 ! Initialization value from NOAH-MP-WRF
  UST     = 0.1  ! Initialization value from NOAH-MP-WRF
  ! LFMASS = 9.0 seems to be a good initial value for the Bondville 1998 case.
  ! it makes LAI match the annual cycle pretty well.
  lfmass = 9.0 
  ! STMASS = 3.33 seems to be a good initial value for the Bondville 1998 case.
  ! it makes STMASS match the annual cycle pretty well.
  stmass = 3.33 

      open(50,file='restart.txt')
      read(50,*) restart
      close(50)

      if (restart) then
       open(122,file='state_save.txt')
       read(122,*)  nowdate
       read(122,*)  ktime
       read(122,*) ALBOLD  , SNEQVO  ,                                         & ! IN/OUT : 
                   STC     , SH2O    , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
                   CANLIQ  , CANICE  , TV      , TG      , QSFC    , QSNOW   , & ! IN/OUT : 
                   ISNOW   , ZSNSO   , SNOWH   , SNEQV   , SNICE   , SNLIQ   , & ! IN/OUT : 
                   ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
                   STMASS  , WOOD    , STBLCP  , FASTCP  , LAI     , SAI     , & ! IN/OUT : 
                   CM      , CH      , TAUSS   ,                               & ! IN/OUT : 
                   SMCWTD  , DEEPRECH, RECH,        &    
                   NSNOW, NSOIL, ZSOIL, SNEQV, TG, SNOWH
!       open(122,file='state_save.txt')
!       read(122,*) qfx
!       read(122,*) fveg
!       read(122,*) nowdate
!       read(122,*) ktime
!       call geth_newdate(nowdate,nowdate,noahlsm_timestep_second/60)
!       read(122,*) SMC(1)
!       read(122,*) SMC(2)
!       read(122,*) SMC(3)
!       read(122,*) SMC(4)
!       read(122,*) LAI
!       read(122,*) SAI
!       read(122,*) LFMASS
!       read(122,*) RTMASS
!       read(122,*) STMASS
!       read(122,*) WOOD
!       read(122,*)    ALBOLD, SNEQVO , STC     , SH2O , TAH   , EAH   , FWET  , &
!      CANLIQ, CANICE, TV    , TG     , QSFC    , QSNOW, ISNOW , ZSNSO , SNOWH , &
!      SNEQV , SNICE , SNLIQ , ZWT    , WA      , WT   , WSLAKE, STBLCP, FASTCP, &
!      CM    , CH    , TAUSS , SMCWTD , DEEPRECH, RECH , TSNO  , EMISSI,  &
!      CMC   , T1    , ALBEDO
       close(122)
      else
       nowdate = startdate
       ktime = 0
       ! Initial values:
       stc(1:4) = stc_ptr
       smc  = smc_ptr
       sh2o = smc
       tv      = t1  ! Initialized with skin temperature as in NOAH-MP-WRF
       tg      = t1  ! Initialized with skin temperature as in NOAH-MP-WRF
       canliq  = cmc ! Initialized with the canopy water content as in NOAH-MP-WRF
       canice  = 0.0 ! Initialization value from NOAH-MP-WRF
      endif

  lat     = latitude * (3.1415926535/180.0)

  ! intent (in)  :: NSNOW, NSOIL, ZSOIL, SNEQV, TG, SNOWH
  ! intent (out) :: ZSNSO, TSNO, SNICE, SNLIQ, ISNOW

  CALL snow_init ( 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 3 , &
            &           NSOIL , zsoil , sneqv , tg , snowh ,     &
            &           zsnso , tsno , snice , snliq , isnow )
 FirstTime=.TRUE.

!  TIMELOOP : do

     call geth_newdate(nowdate, startdate, ktime*(noahlsm_timestep_second/60))
     ktime = ktime + 1

     call read_forcing_text(funit, nowdate, forcing_timestep_second, &
          sfcspd, sfcu, sfcv, sfctmp, q2, sfcprs, soldn, longwave, prcp, ierr,FirstTime)
     if (ierr /= 0) then
        stop ":  FORCING DATA READ PROBLEM"
     endif

     if (ktime == 1 .and. reloop_count == 0) then
        tah     = sfctmp              ! Temperature of the canopy; NOAH-MP-WRF initializes this with 287.  ????
        eah = (sfcprs*q2)/(0.622+q2)  ! Water Vapor Pressure of the canopy; NOAH-MP-WRF initializes this with 2000. ????
     endif

     call calc_declin(nowdate(1:4)//"-"//nowdate(5:6)//"-"//nowdate(7:8)//"_"//nowdate(9:10)//":"//nowdate(11:12)//":00", &
          latitude, longitude, cosz, yearlen, julian)

     ! CALL SFCDIF_off (ZLVL_WIND,Z0,T1V,TH2V,SFCSPD,CZIL,CM,CH) ! Out:  CM, CH

     ! All arguments to REDPRM are intent(in).  REDPRM sets many module variables:
     !    NROOT, RSMIN, RGL, RSMAX, QUARTZ, SMCWLT, DWSAT, DKSAT, PSISAT, SMCREF,
     !    SMCMAX, F1, SMCDRY, BEXP, REFKDT, REFDK, CSOIL, ZBOT, CZIL, FRZK, FRZX,
     !    TOPT, KDT, HS
     call REDPRM(VEGTYPE, SOILTYPE, SLOPETYPE, ZSOIL, NSOIL, ISURBAN)

     uu = sfcu
     vv = sfcv

     lwdn = longwave ! The SFLX routine takes into account emissivity

     co2air  = 355.E-6 * SFCPRS ! Partial pressure of CO2 (Pa) ! From NOAH-MP-WRF
     o2air   = 0.209   * SFCPRS ! Partial pressure of O2 (Pa)  ! From NOAH-MP-WRF
     ficeold(isnow+1:0) = snice(isnow+1:0) / ( snice(isnow+1:0)+snliq(isnow+1:0) ) ! Ice fraction at the last timestep

     stc(isnow+1:0) = tsno(isnow+1:0) ! fill snow levels of STC array with TSNO

     if ( DVEG == 1 ) then
        ! With DVEG==1, SHDFAC is fed directly to FVEG
        shdfac = month_d(shdfac_monthly, nowdate)
     else
        ! With DVEG==2, FVEG is computed from LAI and SAI, and SHDFAC is unused
        shdfac = -1.E38
     endif
     shdmax = maxval(shdfac_monthly)

     DZ8W = -1.E36 ! Not used
     QSFC = Q2 !?
     PSFC = SFCPRS !?
    
   ! opt_run=5 if for Miguez-Macho&Fan groundwater scheme it needs data from surround points.
   ! It is not used in here. 
     SMCEQ(       1:NSOIL) = 0.
     SMCWTD                = 0.
     RECH                  = 0.
     DEEPRECH              = 0.

    call noahmp_sflx (&
                   IX      , IY      , LAT     , YEARLEN , JULIAN  , COSZ    , & ! IN : Time/Space-related
                   DT      , DX      , DZ8W    , NSOIL   , ZSOIL   , NSNOW   , & ! IN : Model configuration 
                   SHDFAC  , SHDMAX  , VEGTYPE  , ISURBAN , ICE    , IST     , & ! IN : Vegetation/Soil characteristics
                   ISC     , SMCEQ   ,                                         & ! IN : Vegetation/Soil characteristics
                   IZ0TLND ,                                                   & ! IN : User options
                   SFCTMP  , SFCPRS  , PSFC    , UU      , VV      , Q2      , & ! IN : Forcing
                   QC      , SOLDN   , LWDN    , PRCP    , TBOT    , CO2AIR  , & ! IN : Forcing
                   O2AIR   , FOLN    , FICEOLD , PBLH    , ZLVL    ,           & ! IN : Forcing
                   ALBOLD  , SNEQVO  ,                                         & ! IN/OUT : 
                   STC     , SH2O    , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
                   CANLIQ  , CANICE  , TV      , TG      , QSFC    , QSNOW   , & ! IN/OUT : 
                   ISNOW   , ZSNSO   , SNOWH   , SNEQV   , SNICE   , SNLIQ   , & ! IN/OUT : 
                   ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
                   STMASS  , WOOD    , STBLCP  , FASTCP  , LAI     , SAI     , & ! IN/OUT : 
                   CM      , CH      , TAUSS   ,                               & ! IN/OUT : 
                   SMCWTD  ,DEEPRECH , RECH    ,                               & ! IN/OUT :
                   FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & ! OUT : 
                   FGEV    , FCTR    , ECAN    , ETRAN   , EDIR    , TRAD    , & ! OUT :
                   TGB     , TGV     , T2MV    , T2MB    , Q2V     , Q2B     , & ! OUT :
                   RUNSRF  , RUNSUB  , APAR    , PSN     , SAV     , SAG     , & ! OUT :
                   FSNO    , NEE     , GPP     , NPP     , FVEG    , ALBEDO  , & ! OUT :
                   QSNBOT  , PONDING , PONDING1, PONDING2, RSSUN   , RSSHA   , & ! OUT :
                   BGAP    , WGAP    , CHV     , CHB     , EMISSI  ,           & ! OUT :
                   SHG     , SHC     , SHB     , EVG     , EVB     , GHV     , & ! OUT :
                   GHB     , IRG     , IRC     , IRB     , TR      , EVC     , & ! OUT :
                   CHLEAF  , CHUC    , CHV2    , CHB2    , FPICE   )            

     tsno(isnow+1:0)  = stc(isnow+1:0) ! fill TSNO with snow levels of STC array.

     qfx = fgev + fcev + fctr

     ir_sh_ev_gh = FIRA + FSH + QFX + SSOIL

     flxsum = (-FSA) + FIRA + FSH + QFX + SSOIL

     fsa_fsr = FSA + FSR

     !
     ! Write the output data for this timestep.
     !

     if (ktime.eq.1) then
       open(ounit,name='OUTPUT.txt',status='replace')
     else
       open(ounit,name='OUTPUT.txt',access='append')
     endif
     write(ounit,*) nowdate, smc 
!     write(ounit,*) zsnso, stc, snice, snliq, q2, sfctmp, uu, vv, &
!      soldn, lwdn, prcp, co2air, o2air, foln, sfcprs, eah, tah,   &
!      fwet, qsnow, cosz, canliq, canice, snowh, sneqv, tv, tg,    &
!      zwt, wa, wt, wslake, lfmass, rtmass, wood, lai, sai, cm, ch,&
!      fsa, fsr, fira, fsh, ssoil, qfx, fcev, fgev, fctr, trad,    &
!      ecan, etran, edir, runsrf, runsub, apar, psn, sav, sag,     &
!      fsno, gpp, npp, fveg, albedo, flxsum, tauss, shg, shc, shb, &
!      evg, evc, evb, ghv, ghb, irg, irc, irb, tr, fpice

     open(122,file='state_save.txt',status='replace')
     write(122,*)  nowdate
     write(122,*)  ktime
     write(122,*)  ALBOLD  , SNEQVO  ,                                         & ! IN/OUT : 
                   STC     , SH2O    , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
                   CANLIQ  , CANICE  , TV      , TG      , QSFC    , QSNOW   , & ! IN/OUT : 
                   ISNOW   , ZSNSO   , SNOWH   , SNEQV   , SNICE   , SNLIQ   , & ! IN/OUT : 
                   ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
                   STMASS  , WOOD    , STBLCP  , FASTCP  , LAI     , SAI     , & ! IN/OUT : 
                   CM      , CH      , TAUSS   ,                               & ! IN/OUT : 
                   SMCWTD  , DEEPRECH, RECH,        &    
                   NSNOW, NSOIL, ZSOIL, SNEQV, TG, SNOWH
!     write(122,*) qfx
!     write(122,*) fveg
!     write(122,*) nowdate
!     write(122,*) ktime
!     write(122,*) SMC(1)
!     write(122,*) SMC(2)
!     write(122,*) SMC(3)
!     write(122,*) SMC(4)
!     write(122,*) LAI
!     write(122,*) SAI
!     write(122,*) LFMASS
!     write(122,*) RTMASS
!     write(122,*) STMASS
!     write(122,*) WOOD
!     write(122,*)     ALBOLD, SNEQVO , STC     , SH2O , TAH   , EAH   , FWET  , &
!      CANLIQ, CANICE, TV    , TG     , QSFC    , QSNOW, ISNOW , ZSNSO , SNOWH , & 
!      SNEQV , SNICE , SNLIQ , ZWT    , WA      , WT   , WSLAKE, STBLCP, FASTCP, & 
!      CM    , CH    , TAUSS , SMCWTD , DEEPRECH, RECH , TSNO  , EMISSI, & 
!      CMC   , T1    , ALBEDO
     close(122)

        print*, nowdate, smc

!  enddo TIMELOOP

  close(iunit)
  close(funit)
  close(ounit)
end program driver

!-----------------------------------------------------------------
SUBROUTINE SOIL_VEG_GEN_PARM( MMINLU, MMINSL)
!-----------------------------------------------------------------
  use module_sf_noahlsm, only : shdtbl, nrotbl, rstbl, rgltbl, &
       &                        hstbl, snuptbl, maxalb, laimintbl, &
       &                        bb, drysmc, f11, maxsmc, laimaxtbl, &
       &                        emissmintbl, emissmaxtbl, albedomintbl, &
       &                        albedomaxtbl, wltsmc, qtz, refsmc, &
       &                        z0mintbl, z0maxtbl, &
       &                        satpsi, satdk, satdw, &
       &                        fxexp_data, lvcoef_data, &
       &                        lutype, maxalb, &
       &                        slope_data, frzk_data, bare, cmcmax_data, &
       &                        cfactr_data, csoil_data, czil_data, &
       &                        refkdt_data, natural, refdk_data, &
       &                        rsmax_data, salp_data, sbeta_data, &
       &                        zbot_data, smhigh_data, smlow_data, &
       &                        lucats, topt_data, slcats, slpcats, sltype

  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN) :: MMINLU, MMINSL
  integer :: LUMATCH, IINDEX, LC, NUM_SLOPE
  integer :: ierr
  INTEGER , PARAMETER :: OPEN_OK = 0

  character*128 :: mess , message

!-----SPECIFY VEGETATION RELATED CHARACTERISTICS :
!             ALBBCK: SFC albedo (in percentage)
!                 Z0: Roughness length (m)
!             SHDFAC: Green vegetation fraction (in percentage)
!  Note: The ALBEDO, Z0, and SHDFAC values read from the following table
!          ALBEDO, amd Z0 are specified in LAND-USE TABLE; and SHDFAC is
!          the monthly green vegetation data
!             CMXTBL: MAX CNPY Capacity (m)
!             NROTBL: Rooting depth (layer)
!              RSMIN: Mimimum stomatal resistance (s m-1)
!              RSMAX: Max. stomatal resistance (s m-1)
!                RGL: Parameters used in radiation stress function
!                 HS: Parameter used in vapor pressure deficit functio
!               TOPT: Optimum transpiration air temperature. (K)
!             CMCMAX: Maximum canopy water capacity
!             CFACTR: Parameter used in the canopy inteception calculati
!               SNUP: Threshold snow depth (in water equivalent m) that
!                     implies 100% snow cover
!                LAI: Leaf area index (dimensionless)
!             MAXALB: Upper bound on maximum albedo over deep snow
!
!-----READ IN VEGETAION PROPERTIES FROM VEGPARM.TBL
!

  OPEN(19, FILE='parm_tables/VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
     WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening VEGPARM.TBL'
     CALL wrf_error_fatal ( message )
  END IF


  LUMATCH=0

  FIND_LUTYPE : DO WHILE (LUMATCH == 0)
     READ (19,*,END=2002)
     READ (19,*,END=2002)LUTYPE
     READ (19,*)LUCATS,IINDEX

     IF(LUTYPE.EQ.MMINLU)THEN
        WRITE( mess , * ) 'LANDUSE TYPE = ' // TRIM ( LUTYPE ) // ' FOUND', LUCATS,' CATEGORIES'
        ! CALL wrf_message( mess )
        LUMATCH=1
     ELSE
        call wrf_message ( "Skipping over LUTYPE = " // TRIM ( LUTYPE ) )
        DO LC = 1, LUCATS+12
           read(19,*)
        ENDDO
     ENDIF
  ENDDO FIND_LUTYPE
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(SHDTBL)       < LUCATS .OR. &
       SIZE(NROTBL)       < LUCATS .OR. &
       SIZE(RSTBL)        < LUCATS .OR. &
       SIZE(RGLTBL)       < LUCATS .OR. &
       SIZE(HSTBL)        < LUCATS .OR. &
       SIZE(SNUPTBL)      < LUCATS .OR. &
       SIZE(MAXALB)       < LUCATS .OR. &
       SIZE(LAIMINTBL)    < LUCATS .OR. &
       SIZE(LAIMAXTBL)    < LUCATS .OR. &
       SIZE(Z0MINTBL)     < LUCATS .OR. &
       SIZE(Z0MAXTBL)     < LUCATS .OR. &
       SIZE(ALBEDOMINTBL) < LUCATS .OR. &
       SIZE(ALBEDOMAXTBL) < LUCATS .OR. &
       SIZE(EMISSMINTBL ) < LUCATS .OR. &
       SIZE(EMISSMAXTBL ) < LUCATS ) THEN
     CALL wrf_error_fatal('Table sizes too small for value of LUCATS in module_sf_noahdrv.F')
  ENDIF

  IF(LUTYPE.EQ.MMINLU)THEN
     DO LC=1,LUCATS
        READ (19,*)IINDEX,SHDTBL(LC),                        &
             NROTBL(LC),RSTBL(LC),RGLTBL(LC),HSTBL(LC), &
             SNUPTBL(LC),MAXALB(LC), LAIMINTBL(LC),     &
             LAIMAXTBL(LC),EMISSMINTBL(LC),             &
             EMISSMAXTBL(LC), ALBEDOMINTBL(LC),         &
             ALBEDOMAXTBL(LC), Z0MINTBL(LC), Z0MAXTBL(LC)
     ENDDO
!
     READ (19,*)
     READ (19,*)TOPT_DATA
     READ (19,*)
     READ (19,*)CMCMAX_DATA
     READ (19,*)
     READ (19,*)CFACTR_DATA
     READ (19,*)
     READ (19,*)RSMAX_DATA
     READ (19,*)
     READ (19,*)BARE
     READ (19,*)
     READ (19,*)NATURAL
  ENDIF
!
2002 CONTINUE

  CLOSE (19)
  IF (LUMATCH == 0) then
     CALL wrf_error_fatal ("Land Use Dataset '"//MMINLU//"' not found in VEGPARM.TBL.")
  ENDIF

!
!-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
!
  OPEN(19, FILE='parm_tables/SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
     WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening SOILPARM.TBL'
     CALL wrf_error_fatal ( message )
  END IF

  WRITE(mess,*) 'INPUT SOIL TEXTURE CLASSIFICATION = ', TRIM ( MMINSL )
  ! CALL wrf_message( mess )

  LUMATCH=0

  READ (19,*)
  READ (19,2000,END=2003)SLTYPE
2000 FORMAT (A4)
  READ (19,*)SLCATS,IINDEX
  IF(SLTYPE.EQ.MMINSL)THEN
     WRITE( mess , * ) 'SOIL TEXTURE CLASSIFICATION = ', TRIM ( SLTYPE ) , ' FOUND', &
          SLCATS,' CATEGORIES'
     ! CALL wrf_message ( mess )
     LUMATCH=1
  ENDIF
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(BB    ) < SLCATS .OR. &
       SIZE(DRYSMC) < SLCATS .OR. &
       SIZE(F11   ) < SLCATS .OR. &
       SIZE(MAXSMC) < SLCATS .OR. &
       SIZE(REFSMC) < SLCATS .OR. &
       SIZE(SATPSI) < SLCATS .OR. &
       SIZE(SATDK ) < SLCATS .OR. &
       SIZE(SATDW ) < SLCATS .OR. &
       SIZE(WLTSMC) < SLCATS .OR. &
       SIZE(QTZ   ) < SLCATS  ) THEN
     CALL wrf_error_fatal('Table sizes too small for value of SLCATS in module_sf_noahdrv.F')
  ENDIF
  IF(SLTYPE.EQ.MMINSL)THEN
     DO LC=1,SLCATS
        READ (19,*) IINDEX,BB(LC),DRYSMC(LC),F11(LC),MAXSMC(LC),&
             REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
             WLTSMC(LC), QTZ(LC)
     ENDDO
  ENDIF

2003 CONTINUE

  CLOSE (19)

  IF(LUMATCH.EQ.0)THEN
     CALL wrf_message( 'SOIl TEXTURE IN INPUT FILE DOES NOT ' )
     CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
     CALL wrf_error_fatal ( 'INCONSISTENT OR MISSING SOILPARM FILE' )
  ENDIF

!
!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
!
  OPEN(19, FILE='parm_tables/GENPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
     WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening GENPARM.TBL'
     CALL wrf_error_fatal ( message )
  END IF

  READ (19,*)
  READ (19,*)
  READ (19,*) NUM_SLOPE

  SLPCATS=NUM_SLOPE
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(slope_data) < NUM_SLOPE ) THEN
     CALL wrf_error_fatal('NUM_SLOPE too large for slope_data array in module_sf_noahdrv')
  ENDIF

  DO LC=1,SLPCATS
     READ (19,*)SLOPE_DATA(LC)
  ENDDO

  READ (19,*)
  READ (19,*)SBETA_DATA
  READ (19,*)
  READ (19,*)FXEXP_DATA
  READ (19,*)
  READ (19,*)CSOIL_DATA
  READ (19,*)
  READ (19,*)SALP_DATA
  READ (19,*)
  READ (19,*)REFDK_DATA
  READ (19,*)
  READ (19,*)REFKDT_DATA
  READ (19,*)
  READ (19,*)FRZK_DATA
  READ (19,*)
  READ (19,*)ZBOT_DATA
  READ (19,*)
  READ (19,*)CZIL_DATA
  READ (19,*)
  READ (19,*)SMLOW_DATA
  READ (19,*)
  READ (19,*)SMHIGH_DATA
  READ (19,*)
  READ (19,*)LVCOEF_DATA
  CLOSE (19)

!-----------------------------------------------------------------
END SUBROUTINE SOIL_VEG_GEN_PARM
!-----------------------------------------------------------------
SUBROUTINE calc_declin ( nowdate, latitude, longitude, cosz, yearlen, julian)
  use kwm_date_utilities
!---------------------------------------------------------------------
  IMPLICIT NONE
!---------------------------------------------------------------------

  REAL, PARAMETER :: DEGRAD = 3.14159265/180.
  REAL, PARAMETER :: DPD    = 360./365.
! !ARGUMENTS:
  character(len=19), intent(in)  :: nowdate    ! YYYY-MM-DD_HH:mm:ss
  real,              intent(in)  :: latitude
  real,              intent(in)  :: longitude
  real,              intent(out) :: cosz
  integer,           intent(out) :: yearlen
  real,              intent(out) :: JULIAN

  REAL                           :: hrang
  real                           :: DECLIN
  real                           :: tloctim
  REAL                           :: OBECL
  REAL                           :: SINOB
  REAL                           :: SXLONG
  REAL                           :: ARG
  integer                        :: iyear
  integer                        :: iday
  integer                        :: ihour
  integer                        :: iminute
  integer                        :: isecond

  !
  ! Determine the number of days in the year
  !

  read(nowdate(1:4), '(I4)') iyear
  yearlen = 365
  if (mod(iyear,4) == 0) then
     yearlen = 366
     if (mod(iyear,100) == 0) then
        yearlen = 365
        if (mod(iyear,400) == 0) then
           yearlen = 366
           if (mod(iyear,3600) == 0) then
              yearlen = 365
           endif
        endif
     endif
  endif

  !
  ! Determine the Julian time (floating-point day of year).
  !

  call geth_idts(nowdate(1:10), nowdate(1:4)//"-01-01", iday)
  read(nowdate(12:13), *) ihour
  read(nowdate(15:16), *) iminute
  read(nowdate(18:19), *) isecond
  julian = real(iday) + real(ihour)/24.

!
! for short wave radiation

  DECLIN=0.

!-----OBECL : OBLIQUITY = 23.5 DEGREE.

  OBECL=23.5*DEGRAD
  SINOB=SIN(OBECL)

!-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:

  IF(JULIAN.GE.80.)SXLONG=DPD*(JULIAN-80.)*DEGRAD
  IF(JULIAN.LT.80.)SXLONG=DPD*(JULIAN+285.)*DEGRAD
  ARG=SINOB*SIN(SXLONG)
  DECLIN=ASIN(ARG)

  TLOCTIM = REAL(IHOUR) + REAL(IMINUTE)/60.0 + REAL(ISECOND)/3600.0 + LONGITUDE/15.0 ! Local time in hours
  tloctim = AMOD(tloctim+24.0, 24.0)
  HRANG=15.*(TLOCTIM-12.)*DEGRAD
  COSZ=SIN(LATITUDE*DEGRAD)*SIN(DECLIN)+COS(LATITUDE*DEGRAD)*COS(DECLIN)*COS(HRANG)

!KWM   write(wrf_err_message,10)DECDEG/DEGRAD
!KWM10 FORMAT(1X,'*** SOLAR DECLINATION ANGLE = ',F6.2,' DEGREES.',' ***')
!KWM   CALL wrf_debug (50, wrf_err_message)

END SUBROUTINE calc_declin

! Subroutine SNOW_INIT grabbed from NOAH-MP-WRF
SUBROUTINE SNOW_INIT ( jts, jtf, its, itf, ims, ime, jms, jme, NSNOW, NSOIL, ZSOIL,  &
     SWE, tgxy, SNODEP, ZSNSOXY, TSNOXY, SNICEXY, SNLIQXY, ISNOWXY)

! ------------------------------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------------------------------------------------------------------------
  INTEGER, INTENT(IN) :: jts,jtf,its,itf,ims,ime, jms,jme,NSNOW,NSOIL
  REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SWE 
  REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SNODEP
  REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: tgxy
  REAL,    INTENT(IN), DIMENSION(1:NSOIL) :: ZSOIL

  INTEGER, INTENT(OUT), DIMENSION(ims:ime, jms:jme) :: ISNOWXY
  REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:NSOIL,jms:jme) :: ZSNSOXY
  REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: TSNOXY
  REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNICEXY
  REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNLIQXY

!local
  INTEGER :: I,J,IZ
  REAL,                 DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: DZSNOXY
  REAL,                 DIMENSION(ims:ime, -NSNOW+1:NSOIL,jms:jme) :: DZSNSOXY
! ------------------------------------------------------------------------------------------


  DO J = jts,jtf
     DO I = its,itf
        IF (SNODEP(I,J) < 0.025) THEN
           ISNOWXY(I,J) = 0
           DZSNOXY(I,-NSNOW+1:0,J) = 0.
        ELSE
           IF ((SNODEP(I,J) >= 0.025) .AND. (SNODEP(I,J) <= 0.05)) THEN
              ISNOWXY(I,J)    = -1
              DZSNOXY(I,0,J)  = SNODEP(I,J)
           ELSE IF ((SNODEP(I,J) > 0.05) .AND. (SNODEP(I,J) <= 0.10)) THEN
              ISNOWXY(I,J)    = -2
              DZSNOXY(I,-1,J) = SNODEP(I,J)/2.
              DZSNOXY(I, 0,J) = SNODEP(I,J)/2.
           ELSE IF ((SNODEP(I,J) > 0.10) .AND. (SNODEP(I,J) <= 0.25)) THEN
              ISNOWXY(I,J)    = -2
              DZSNOXY(I,-1,J) = 0.05
              DZSNOXY(I, 0,J) = SNODEP(I,J) - DZSNOXY(I,-1,J)
           ELSE IF ((SNODEP(I,J) > 0.25) .AND. (SNODEP(I,J) <= 0.35)) THEN
              ISNOWXY(I,J)    = -3
              DZSNOXY(I,-2,J) = 0.05
              DZSNOXY(I,-1,J) = 0.5*(SNODEP(I,J)-DZSNOXY(I,-2,J))
              DZSNOXY(I, 0,J) = 0.5*(SNODEP(I,J)-DZSNOXY(I,-2,J))
           ELSE IF (SNODEP(I,J) > 0.35) THEN
              ISNOWXY(I,J)     = -3
              DZSNOXY(I,-2,J) = 0.05
              DZSNOXY(I,-1,J) = 0.10
              DZSNOXY(I, 0,J) = SNODEP(I,J) - DZSNOXY(I,-1,J) - DZSNOXY(I,-2,J)
           END IF
        END IF
     ENDDO
  ENDDO

  DO J = jts,jtf
     DO I = its,itf
        TSNOXY( I,-NSNOW+1:0,J) = 0.
        SNICEXY(I,-NSNOW+1:0,J) = 0.
        SNLIQXY(I,-NSNOW+1:0,J) = 0.
        DO IZ = ISNOWXY(I,J)+1, 0
           TSNOXY(I,IZ,J)  = tgxy(I,J)  ! [k]
           SNLIQXY(I,IZ,J) = 0.00
           SNICEXY(I,IZ,J) = 1.00 * DZSNOXY(I,IZ,J) * (SWE(I,J)/SNODEP(I,J))  ! [kg/m3]
        END DO

        DO IZ = ISNOWXY(I,J)+1, 0
           DZSNSOXY(I,IZ,J) = -DZSNOXY(I,IZ,J)
        END DO

        DZSNSOXY(I,1,J) = ZSOIL(1)
        DO IZ = 2,NSOIL
           DZSNSOXY(I,IZ,J) = (ZSOIL(IZ) - ZSOIL(IZ-1))
        END DO

        ZSNSOXY(I,ISNOWXY(I,J)+1,J) = DZSNSOXY(I,ISNOWXY(I,J)+1,J)
        DO IZ = ISNOWXY(I,J)+2 ,NSOIL
           ZSNSOXY(I,IZ,J) = ZSNSOXY(I,IZ-1,J) + DZSNSOXY(I,IZ,J)
        ENDDO

     END DO
  END DO

END SUBROUTINE SNOW_INIT

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

real function month_d(a12, nowdate) result (nowval)
  !
  ! Given a set of 12 values, taken to be valid on the fifteenth of each month (Jan through Dec)
  ! and a date in the form <YYYYMMDD[HHmmss]> ....
  ! 
  ! Return a value valid for the day given in <nowdate>, as an interpolation from the 12
  ! monthly values.
  !
  use kwm_date_utilities
  implicit none
  real, dimension(12), intent(in) :: a12 ! 12 monthly values, taken to be valid on the 15th of
  !                                      ! the month
  character(len=12), intent(in) :: nowdate ! Date, in the form <YYYYMMDD[HHmmss]>
  integer :: nowy, nowm, nowd
  integer :: prevm, postm
  real    :: factor
  integer, dimension(12) :: ndays = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !
  ! Handle leap year by setting the number of days in February for the year in question.
  !
  read(nowdate(1:8),'(I4,I2,I2)') nowy, nowm, nowd
  ndays(2) = nfeb(nowy)

  !
  ! Do interpolation between the fifteenth of two successive months.
  !
  if (nowd == 15) then
     nowval = a12(nowm)
     return
  else if (nowd < 15) then
     postm = nowm
     prevm = nowm - 1
     if (prevm == 0) prevm = 12
     factor = real(ndays(prevm)-15+nowd)/real(ndays(prevm))
  else if (nowd > 15) then
     prevm = nowm
     postm = nowm + 1
     if (postm == 13) postm = 1
     factor = real(nowd-15)/real(ndays(prevm))
  endif

  nowval = a12(prevm)*(1.0-factor) + a12(postm)*factor

end function month_d

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------
