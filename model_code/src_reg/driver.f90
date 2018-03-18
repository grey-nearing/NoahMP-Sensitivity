subroutine driver(ktime,setup,forcing,state,output)
 use module_sf_noahmplsm, only : dveg, opt_crs, opt_btr,     &
        opt_run, opt_sfc, opt_frz, opt_inf, opt_rad,         &
        opt_alb, opt_snf, opt_tbot, opt_stc, noahmp_options, & 
        noahmp_sflx
 use kwm_date_utilities
 use type_decs

! --- Var Declarations --------------------------------------
 implicit none

! funcitons 
 real, external :: EsFuncT
 real, external :: month_d

! timing stuff
 character(len=12) :: startdate  ! Starting date ( YYYYMMDDHHmm ) 
 character(len=12) :: nowdate
 integer :: ktime, yearlen
 real :: julian
 
! parameters
 real, dimension(12) :: shdfac_monthly   ! Monthly green veg frac
 real    :: tbot, shdfac, shdmax
 real    :: foln

! runtime config parameters
 real :: latitude, longitude
 real, pointer, dimension(:) :: sldpth ! Thicknesses of each soil level
 real, allocatable, dimension(:) :: zsoil
 integer :: isurban, ice, ist, isc, vegtype
 integer :: nsnow, nsoil
 real    :: dt, zlvl, cosz, lat

! state
 real, allocatable, dimension(:) :: ficeold, zsnso, snice, snliq, stc, &
    sh2o, smc, tsno
 real    :: qsnow, sneqvo, canliq, canice, snowh, snoeqv, tg, tv,       &
    eah, tah, fwet, zwt, wa, wt, wslake, lfmass, stmass, wood, stblcp,  &
    fastcp, lai, sai, albold, cm, ch, tauss, sneqv, rtmass 
 integer :: isnow

! forcing
 real :: sfcspd, sfcu, sfcv, q2, sfctmp, uu, vv, soldn, lwdn, prcp, sfcprs
 real :: co2air, o2air
 real ::   eps, svp, E, QS

! internal indexes
 integer :: n

! otput
  real    :: fsa, fsr, fira, fsh, ssoil, fcev, fgev, fctr, trad, ecan, &
     etran, edir, runsrf, runsub, apar, sav, sag, fsno, nee, gpp, npp, &
     ts, fveg, albedo, bgap, wgap, tgv, tgb, t2mb, t2mv, rssun, rssha, &
     qsfc, q2v, q2b, qc, ponding, ponding1, ponding2, chv, chb, emissi,&
     qsnbot, shg, shc, shb, evg, evb, ghv, ghb, irg, irc, irb, tr, evc,&
     chleaf, chuc, chv2, chb2, fpice, psn
  real    :: qfx, flxsum, ir_sh_ev_gh, fsa_fsr

! not used
  real    :: dz8w, smcwtd, deeprech, rech, dx, psfc, pblh
  integer ix, iy
  integer :: iz0tlnd
  real, allocatable, dimension(:) :: smceq

! passing types
  type(forcing_data) :: forcing 
  type(state_data)   :: state
  type(setup_data)   :: setup
  type(output_data)  :: output

! ---- run-time setup ---------------------------------------------------
! allocate arrays that depend on number of soil/snow layers
  vegtype = setup%vegtyp
  nsoil = setup%nsoil
  nsnow = setup%nsnow
  allocate(ficeold(-nsnow+1:0))
  allocate(zsnso(-nsnow+1:nsoil))
  allocate(snice(-nsnow+1:0))
  allocate(snliq(-nsnow+1:0))
  allocate(stc(-nsnow+1:nsoil))
  allocate(sh2o(1:nsoil))
  allocate(smc(1:nsoil))
  allocate(tsno(-nsnow+1:0))
  allocate(sldpth(1:nsoil))
  allocate(zsoil(nsoil))
  allocate(smceq(nsoil))

! pull parameters (not to be calibrated)
  startdate = setup%startdate
  latitude  = setup%latitude
  longitude = setup%longitude
  dt        = setup%dt
  zlvl      = setup%zlvl
  lat       = latitude * (3.1415926535/180.0)
  sldpth    = setup%sldpth 
  shdfac_monthly = setup%shdfac_monthly

! re-work soil depths  
  zsoil(1) = -sldpth(1)
  do n = 2, nsoil
     zsoil(n) = zsoil(n-1) - sldpth(n)
  enddo

! augment date 
  call geth_newdate(nowdate,startdate,int((ktime-1)*(dt/60)))

! MP configuration 
  dveg      = setup%dveg 
  opt_crs   = setup%opt_crs
  opt_btr   = setup%opt_btr
  opt_run   = setup%opt_run
  opt_sfc   = setup%opt_sfc
  opt_frz   = setup%opt_frz
  opt_inf   = setup%opt_inf
  opt_rad   = setup%opt_rad
  opt_alb   = setup%opt_alb
  opt_snf   = setup%opt_snf
  opt_tbot  = setup%opt_tbot    
  opt_stc   = setup%opt_stc 
  if (opt_sfc == 3 .or. opt_sfc ==4) then
   stop "(OPT_SFC == 3) and (OPT_SFC == 4) are not for offline use"
  endif
  if (opt_run == 5) then
   stop " OPT_RUN==5 (groundwater scheme) are not for single point driver"
  endif

! parameters that make no difference, but are require so that we don't have to change the passing arguments
  dx             = 1
  ix             = 1
  iy             = 1
  ist            = 1  ! Surface type:  IST=1 => soil;  IST=2 => lake
  isc            = 4  ! Soil color type
  qc             = 0 
  pblh           = 0 
  dz8w           = -1.E36 
  smcwtd         = 0.
  rech           = 0.
  DEEPRECH       = 0.
  smceq(1:nsoil) = 0.

! this standalone version does not do ice  
  ice           = 0 
! this version cannot do urban
  isurban               = 1

! --- Forcing -----------------------------------------------------------
! pull forcing
  sfcspd   = forcing%sfcspd
  uu       = sfcspd
  vv       = 0
  sfctmp   = forcing%sfctmp
  sfcprs   = forcing%sfcprs * 1.e2
  soldn    = forcing%swrad
  lwdn     = forcing%lwrad
  prcp     = forcing%prcprate
  q2       = forcing%q2 !* 1.e-2
 
! tranform relative humidity
!  eps      = 0.622
!  svp      = EsFuncT(sfctmp)
!  QS       = eps * svp / (sfcprs - (1.-eps) * svp)
!  E        = (sfcprs*svp*q2)/(sfcprs - svp*(1. - q2))
!  q2       = (eps*e)/(sfcprs-(1.0-eps)*E)
!  if(q2.lt.0.1E-5) q2 = 0.1E-5
!  if(q2.ge.QS) q2 = QS*0.99

! partial pressures
  co2air  = 355.E-6*sfcprs ! Partial pressure of CO2 (Pa) 
  o2air   = 0.209  *sfcprs ! Partial pressure of O2 (Pa) 

! solar zenith angle based on current date and time
  call calc_declin(nowdate(1:4)//"-"//nowdate(5:6)//"-"//nowdate(7:8)// & 
                   "_"//nowdate(9:10)//":"//nowdate(11:12)//":00", &
                   latitude, longitude, cosz, yearlen, julian)
! ????
 qsfc    = q2 
 psfc    = sfcprs
    
! --- Parameters  -------------------------------------------------------
! With DVEG==1, SHDFAC is fed directly to FVEG
! With DVEG==2, FVEG is computed from LAI and SAI, and SHDFAC is unused
  if (dveg.eq.1) then
    if (forcing%shdfac.ge.0) then
      shdfac = forcing%shdfac
    else
      shdfac = month_d(shdfac_monthly,nowdate)
    endif
  else
    shdfac = -1.E38
  endif
  shdmax = maxval(shdfac_monthly)

! fix this!
  tbot = setup%tbot
  foln    = 1.0     
  
! --- State -------------------------------------------------------------
! pull from last timestep
  albold  = state%albold
  sneqvo  = state%sneqvo
  stc     = state%stc     ! y
  sh2o    = state%sh2o    ! y
  smc     = state%smc     ! y
  tah     = state%tah
  eah     = state%eah
  fwet    = state%fwet
  canliq  = state%canliq
  canice  = state%canice
  tv      = state%tv
  tg      = state%tg
  qsnow   = state%qsnow
  isnow   = state%isnow
  zsnso   = state%zsnso
  snowh   = state%snowh
  sneqv   = state%sneqv
  snice   = state%snice
  snliq   = state%snliq
  zwt     = state%zwt
  wa      = state%wa
  wt      = state%wt
  wslake  = state%wslake
  lfmass  = state%lfmass  ! y
  rtmass  = state%rtmass  ! y
  stmass  = state%stmass  ! y
  wood    = state%wood    ! y
  stblcp  = state%stblcp  ! ?
  fastcp  = state%fastcp  ! ?
  lai     = state%lai
  sai     = state%sai
  cm      = state%cm
  ch      = state%ch
  tauss   = state%tauss
  smcwtd  = state%smcwtd
  tsno    = state%tsno
   
! initial state
  if (ktime.eq.1) then
! these can probably be kept here as defaults? All of these defaults are from WRF
   tauss   = 0.0    ! Initialize with new snow.
   qsnow   = 0.0   
   sneqvo  = 0.0    
   fwet    = 0.00   
   wa      = 4900.0 
   wt      = wa     
   zwt     = (25.0+2.0)- wa/1000.0/0.2 
   wslake  = 0.0    
   stblcp  = 1000.0 
   fastcp  = 1000.0 
   sai     = 0.1  
   cm      = 0.00 
   ch      = 0.00 
   canice  = 0.0 ! Initialization value from NOAH-MP-WRF
   call snow_init(1,1,1,1,1,1,1,1,3,               &
          NSOIL , zsoil , sneqv , tg , snowh ,     &
          zsnso , tsno , snice , snliq , isnow )

! these two values can stay here, but they are not initialized the same an in WRF
   tah     = sfctmp ! Temperature of the canopy; 
   eah = (sfcprs*q2)/(0.622+q2)  ! Water Vapor Pressure of the canopy;
 
! fill snow levels of STC array with TSNO
   stc(isnow+1:0) = tsno(isnow+1:0) 
  endif

! ice fraction at the last timestep
  ficeold = 0.
  ficeold(isnow+1:0) = snice(isnow+1:0)/(snice(isnow+1:0)+snliq(isnow+1:0))

! for debugging
!  open(156,name='state.save',status='replace')
!   write(156,*) '1', ktime,ZSOIL,SHDFAC,SHDMAX,VEGTYPE,ISURBAN,ICE,IST
!   write(156,*) '2', ISC,SMCEQ,SFCTMP,SFCPRS,PSFC,UU,VV,Q2
!   write(156,*) '3', QC,SOLDN,LWDN,PRCP,TBOT,CO2AIR
!   write(156,*) '4', O2AIR,FOLN,FICEOLD,PBLH,ZLVL,ALBOLD,SNEQVO
!   write(156,*) '5', STC,SH2O,SMC,TAH,EAH,FWET 
!   write(156,*) '6', CANLIQ,CANICE,TV,TG,QSFC,QSNOW
!   write(156,*) '7', ISNOW,ZSNSO,SNOWH,SNEQV,SNICE,SNLIQ
!   write(156,*) '8', ZWT,WA,WT,WSLAKE,LFMASS,RTMASS
!   write(156,*) '9', STMASS,WOOD,STBLCP,FASTCP,LAI,SAI
!   write(156,*) '10', CM,CH,TAUSS,isnow 
!   close(156) 
!if (ktime.eq.3) stop

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
! --- Output -------------------------------------------------------------
! fill TSNO with snow levels of STC array.
  tsno(isnow+1:0)  = stc(isnow+1:0) 

! calculate output fluxes
  qfx = fgev + fcev + fctr
  ir_sh_ev_gh = FIRA + FSH + QFX + SSOIL
  flxsum = (-FSA) + FIRA + FSH + QFX + SSOIL
  fsa_fsr = FSA + FSR

! pack state
  state%albold  = albold
  state%sneqvo  = sneqvo
  state%stc     = stc
  state%sh2o    = sh2o
  state%smc     = smc
  state%tah     = tah
  state%eah     = eah
  state%fwet    = fwet
  state%canliq  = canliq
  state%canice  = canice
  state%tv      = tv
  state%tg      = tg
  state%qsnow   = qsnow
  state%isnow   = isnow
  state%zsnso   = zsnso
  state%snowh   = snowh
  state%sneqv   = sneqv
  state%snice   = snice
  state%snliq   = snliq
  state%zwt     = zwt
  state%wa      = wa
  state%wt      = wt
  state%wslake  = wslake
  state%lfmass  = lfmass
  state%rtmass  = rtmass
  state%stmass  = stmass
  state%wood    = wood
  state%stblcp  = stblcp
  state%fastcp  = fastcp
  state%lai     = lai
  state%sai     = sai
  state%cm      = cm
  state%ch      = ch
  state%tauss   = tauss
  state%smcwtd  = smcwtd
  state%tsno    = tsno

! pack output
!  print*, ktime,nowdate,fgev,fcev,fctr,fsh,soldn,lwdn
  output%cosz  = cosz
  output%Qe    = fgev+fcev+fctr
  output%Qh    = fsh
  output%NEE   = nee


end subroutine driver

!-----------------------------------------------------------------
subroutine calc_declin(nowdate,latitude,longitude,cosz,yearlen,julian)
  use kwm_date_utilities
  implicit none

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
  IMPLICIT NONE
  
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

  REAL FUNCTION EsFuncT (T) result (E)

    IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C  PURPOSE:  TO CALCULATE VALUES OF SAT. VAPOR PRESSURE E [ Pa ]
!C            FORMULAS AND CONSTANTS FROM ROGERS AND YAU, 1989.
!C
!C                         ADDED BY PABLO J. GRUNMANN, 7/9/97.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
    real, intent(in) :: T  ! Temperature [ K ]

    REAL, parameter  :: TO    = 273.15
    REAL, parameter  :: CPV   = 1870.0  ! Specific heat of water vapor  [ J kg{-1} K{-1} ]
    REAL, parameter  :: RV    = 461.5   ! Water vapor gas constant      [ J kg{-1} K{-1} ]
    REAL, parameter  :: CW    = 4187.0  ! Specific heat of liquid water [ J kg{-1} K{-1} ]
    REAL, parameter  :: ESO   = 611.2   ! Sat. vapor pres. at T = T0    [ Pa ]
    REAL, parameter  :: LVH2O = 2.501E6 ! Latent Heat of Vaporization   [ J kg{-1} ]

    REAL :: LW
!
!     CLAUSIUS-CLAPEYRON: DES/DT = L*ES/(RV*T^2)
!
      LW = LVH2O - ( CW - CPV ) * ( T - TO )
      E = ESO*EXP (LW*(1.0/TO - 1.0/T)/RV)

    END FUNCTION ESFUNCT


