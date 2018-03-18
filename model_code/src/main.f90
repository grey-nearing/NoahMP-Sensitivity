program run_timestep
 use noahmp_veg_parameters
 use noahmp_globals
 use type_decs

! -- Variable Declarations ----------------------------------------------
 implicit none

 ! simulation parameters
 integer :: Ne, Nt
 logical :: perturb, data_assim

 ! file I/O
 integer, parameter :: fid = 15
 character(1000)    :: fname
 character(1)       :: es1
 character(2)       :: es2
 character(3)       :: es3

 ! internal indexes
 integer :: t, e, ee, l, d
 real    :: dummy
 integer :: vegtyp
 logical :: fexists
 
 ! model data
 type(forcing_data), allocatable, dimension(:,:) :: forcing
 type(state_data),   allocatable, dimension(:,:) :: state, background
 type(state_data) :: state_tmp
 type(setup_data) :: setup
 type(setup_data),   allocatable, dimension(:)   :: setup_tmp
 type(output_data),  allocatable, dimension(:,:) :: output

 ! obs data
 integer, parameter                  :: Nlag = 1
 integer, parameter                  :: Dz = 1
 real, allocatable, dimension(:,:)   :: obs,sig
 real, dimension(Dz)                 :: zcov
 real, allocatable, dimension(:,:,:) :: X
 real, allocatable, dimension(:,:)   :: Y
 real, allocatable, dimension(:)     :: Z, R
 real :: random_normal, eta

 ! mean output data
 real :: mean_smc1,mean_smc2,mean_smc3,mean_smc4
 real :: std_smc1,std_smc2,std_smc3,std_smc4

! --- Set Up Run --------------------------------------------------------
! setup simulation 
 call sim_init(setup,state_tmp,perturb,data_assim,Nt,Ne)

 allocate(state(Nt,Ne))
 if (data_assim) allocate(background(Nt,Ne))
 allocate(setup_tmp(Ne))
 allocate(output(Nt,Ne))
 do e = 1,Ne
  call sim_init(setup_tmp(e),state(1,e),perturb,data_assim,Nt,Ne)
  do t = 2,Nt
   allocate(state(t,e)%stc(-setup%nsnow+1:setup%nsoil))
   allocate(state(t,e)%zsnso(-setup%nsnow+1:setup%nsoil))
   allocate(state(t,e)%tsno(setup%nsnow))
   allocate(state(t,e)%snice(setup%nsnow))
   allocate(state(t,e)%snliq(setup%nsnow))
   allocate(state(t,e)%sh2o(setup%nsoil))
   allocate(state(t,e)%smc(setup%nsoil))
   if (data_assim) then
    allocate(background(t,e)%stc(-setup%nsnow+1:setup%nsoil))
    allocate(background(t,e)%zsnso(-setup%nsnow+1:setup%nsoil))
    allocate(background(t,e)%tsno(setup%nsnow))
    allocate(background(t,e)%snice(setup%nsnow))
    allocate(background(t,e)%snliq(setup%nsnow))
    allocate(background(t,e)%sh2o(setup%nsoil))
    allocate(background(t,e)%smc(setup%nsoil))
   endif
  enddo
 enddo

 ! forcing from file
 allocate(forcing(Nt,Ne))
 do e = 1,Ne

  ! don't want to use more than 50 forcing inputs
  ee = mod(e,50)
  if (ee == 0) ee = 1 

  ! forcing file name
  fname = 'forcing_'
  if (ee.lt.10) then
   write(es1,'(i1)') ee
   fname = trim(fname)//es1
  elseif (ee.lt.100) then
   write(es2,'(i2)') ee
   fname = trim(fname)//es2
  elseif (ee.lt.1000) then
   write(es3,'(i3)') ee
   fname = trim(fname)//es3
  endif
  fname = trim(fname)//'.txt'
  open(fid,file=trim(fname))

  do t = 1,Nt
   ! the humidity here is kg/kg, not % and not relative humidity.
   read(fid,*) dummy,dummy,dummy,forcing(t,e)%sfcspd,dummy,   &
               forcing(t,e)%sfctmp,forcing(t,e)%q2,           &
               forcing(t,e)%sfcprs,forcing(t,e)%swrad,        &
               forcing(t,e)%lwrad,forcing(t,e)%prcprate
  enddo ! times
  close(fid)
 enddo ! ensembles

! prescribed shade fraction
 forcing%shdfac = -9999.
! inquire(file='shdfac.txt',exist=fexists)
! if ((setup%dveg.eq.1).and.(fexists)) then
!   do e = 1,Ne
!    fname = 'shdfac.txt'
!    open(fid,file=trim(fname))
!      do t = 1,Nt
!        read(fid,*) dummy,dummy,dummy,forcing(t,e)%shdfac
!      enddo ! times
!    close(fid)
!   enddo ! ensembles
!  setup%shdfac_monthly = 0.
!  setup%shdfac_monthly(1) = maxval(forcing%shdfac)
! endif
 
! parameters
! This standalone version does not use the parameter tables. The one place where this may cause a problem is on line 8866 of module_sf_noahmplsm.f90 where carbon partitioning to the leaf is different for elbforest than for other vegetation types. We have set a constant vegetation type so that isurban, iswater, issnow, and isbaren are not triggered.

 do e = 1,Ne
  call redprm(setup%nsoil,vegtyp)
  setup%vegtyp = vegtyp ! this should !not! be a parameter 
  setup%tbot = 285. ! this should really be a parameter
 enddo

! --- Load Observations -------------------------------------------------
 if (data_assim) then
  allocate(obs(Nt,Dz))
  allocate(sig(Nt,Dz))
 
  fname = 'obs_cov.txt'
  open(fid,file=trim(fname))
   read(fid,*) zcov
  close(fid)

  fname = 'obs.txt'
  open(fid,file=trim(fname))
   do t = 1,Nt
    read(fid,*) dummy,dummy,dummy,dummy,obs(t,:),sig(t,:) 
   enddo
  close(fid)

  allocate(Y(1,Ne))
  allocate(Z(1))
  allocate(R(1))
  allocate(X(Nlag,4,Ne))
 endif

! --- Run the Model -----------------------------------------------------
! initial timestep
 t = 1
 do e = 1,Ne
  call driver(t,setup,forcing(t,e),state(t,e),output(t,e))

  ! save background state
  if (data_assim) then
   background(t,e) = state(t,e)
  endif 
 enddo

! time loop
 do t = 2,Nt
!print*,t
  ! run each ensembel member at time step
  do e = 1,Ne
   state(t,e) = state(t-1,e)

   ! perturb the state if diong data assimilation or an open loop
   if (perturb) then
    call perturb_state(setup,state(t,e))
   endif

   ! run the model at this timestep
   call driver(t,setup,forcing(t,e),state(t,e),output(t,e))

   ! save background state
   if (data_assim) then
    background(t,e) = state(t,e)
   endif 

   ! check for bad state values
   if (isnan(state(t,e)%stmass).or.isnan(state(t,e)%lfmass)) then
    stop 'Error in Noah-MP veg state'
   endif
  enddo ! ensemble

  ! call data assimilation
  if (data_assim) then
   if (t.gt.Nlag) then

    ! plant state updating
    if ((obs(t,1).gt.-9000)) then
     do l = 1,Nlag
      do e = 1,Ne
       X(l,1:4,e) = state(t-l+1,e)%smc
      enddo
     enddo

     do e = 1,Ne
      Y(1,e) = state(t,e)%smc(1)
     enddo
     Z = obs(t,1)
     R = max(0.04,sig(t,1)*zcov(1))
     call enks(X,Y,Z,R,Nlag,Ne,4,1) 

     do l = 1,Nlag
      do e = 1,Ne
       state(t-l+1,e)%smc    = X(l,1:4,e)
       do d = 1,4
        state(t-l+1,e)%sh2o(d) = state(t-l+1,e)%sh2o(d)+X(l,d,e)-state(t-l+1,e)%smc(d)
        state(t-l+1,e)%smc(d)  = X(l,d,e)
        if (state(t-l+1,e)%smc(d).gt.smcmax)    state(t-l+1,e)%smc(d) = smcmax
        if (state(t-l+1,e)%smc(d).lt.0.02)      state(t-l+1,e)%smc(d) = 0.02
        if (state(t-l+1,e)%sh2o(d).gt.smcmax)   state(t-l+1,e)%sh2o(d) = smcmax
        if (state(t-l+1,e)%sh2o(d).lt.0.02)     state(t-l+1,e)%sh2o(d) = 0.02
       enddo
      enddo ! ensemble
     enddo ! lag
    endif ! obs present 
   endif ! time > 10
  endif ! data assim

  do e = 1,Ne 
   do d = 1,setup%nsoil
    if (state(t,e)%smc(d).gt.smcmax)  state(t,e)%smc(d) = smcmax
    if (state(t,e)%smc(d).lt.0.02)    state(t,e)%smc(d) = 0.02
    if (state(t,e)%sh2o(d).gt.smcmax) state(t,e)%sh2o(d) = smcmax
    if (state(t,e)%sh2o(d).lt.0.02)   state(t,e)%sh2o(d) = 0.02
   enddo ! soil dimension
  enddo ! ensemble for bounds checking

 enddo ! time loop
 
! ------- Write Output ------------------------------------------------

 if (perturb) then
  if (data_assim) then
   fname = 'enks_mean.out'
  else
   fname = 'open_mean.out'
  endif
  open(fid,file=trim(fname),status='replace')
  
  do t = 1,Nt 
   mean_smc1     = 0
   mean_smc2     = 0
   mean_smc3     = 0
   mean_smc4     = 0
   do e = 1,Ne
    mean_smc1     = mean_smc1 + state(t,e)%smc(1)
    mean_smc2     = mean_smc1 + state(t,e)%smc(2)
    mean_smc3     = mean_smc1 + state(t,e)%smc(3)
    mean_smc4     = mean_smc1 + state(t,e)%smc(4)
   enddo
   mean_smc1     = mean_smc1/Ne
   mean_smc2     = mean_smc2/Ne
   mean_smc3     = mean_smc3/Ne
   mean_smc4     = mean_smc4/Ne

   std_smc1     = 0
   std_smc2     = 0
   std_smc3     = 0
   std_smc4     = 0
   do e = 1,Ne
    std_smc1     = std_smc1 + (state(t,e)%smc(1)-mean_smc1)**2
    std_smc2     = std_smc1 + (state(t,e)%smc(2)-mean_smc2)**2
    std_smc3     = std_smc1 + (state(t,e)%smc(3)-mean_smc3)**2
    std_smc4     = std_smc1 + (state(t,e)%smc(4)-mean_smc4)**2
   enddo
   std_smc1     = sqrt(std_smc1/(Ne-1))
   std_smc2     = sqrt(std_smc2/(Ne-1))
   std_smc3     = sqrt(std_smc3/(Ne-1))
   std_smc4     = sqrt(std_smc4/(Ne-1))

   write(fid,'(f17.6, f17.6, f17.6, f17.6,       &
               f17.6, f17.6, f17.6, f17.6)')     & 
    mean_smc1, mean_smc2, mean_smc3, mean_smc4,  &
    std_smc1 , std_smc2 , std_smc3 , std_smc4  
  enddo
  close(fid)
 endif

 if (data_assim) then
  fname = 'back_mean.out'
  open(fid,file=trim(fname),status='replace')
  
  do t = 1,Nt 
   mean_smc1     = 0
   mean_smc2     = 0
   mean_smc3     = 0
   mean_smc4     = 0
   do e = 1,Ne
    mean_smc1     = mean_smc1 + background(t,e)%smc(1)
    mean_smc2     = mean_smc1 + background(t,e)%smc(2)
    mean_smc3     = mean_smc1 + background(t,e)%smc(3)
    mean_smc4     = mean_smc1 + background(t,e)%smc(4)
   enddo
   mean_smc1     = mean_smc1/Ne
   mean_smc2     = mean_smc2/Ne
   mean_smc3     = mean_smc3/Ne
   mean_smc4     = mean_smc4/Ne
   
   std_smc1     = 0
   std_smc2     = 0
   std_smc3     = 0
   std_smc4     = 0
   do e = 1,Ne
    std_smc1     = std_smc1 + (background(t,e)%smc(1)-mean_smc1)**2
    std_smc2     = std_smc1 + (background(t,e)%smc(2)-mean_smc2)**2
    std_smc3     = std_smc1 + (background(t,e)%smc(3)-mean_smc3)**2
    std_smc4     = std_smc1 + (background(t,e)%smc(4)-mean_smc4)**2
   enddo
   std_smc1     = sqrt(std_smc1/(Ne-1))
   std_smc2     = sqrt(std_smc2/(Ne-1))
   std_smc3     = sqrt(std_smc3/(Ne-1))
   std_smc4     = sqrt(std_smc4/(Ne-1))

   write(fid,'(f17.6, f17.6, f17.6, f17.6,       &
               f17.6, f17.6, f17.6, f17.6)')     & 
    mean_smc1, mean_smc2, mean_smc3, mean_smc4,  &
    std_smc1, std_smc2, std_smc3, std_smc4 
  enddo ! time
  close(fid) ! mean file
 endif

! ------------------------------------------------------------------

 if (.not.data_assim) then
  do e = 1,Ne
   fname = 'output.out'
!   if (e.lt.10) then
!    write(es1,'(i1)') e
!    fname = trim(fname)//es1
!   elseif (e.lt.100) then
!    write(es2,'(i2)') e
!    fname = trim(fname)//es2
!   elseif (e.lt.1000) then
!    write(es3,'(i3)') e
!    fname = trim(fname)//es3
!   endif
!   fname = trim(fname)//'.out'
   open(fid,file=trim(fname),status='replace')

   do t = 1,Nt
    write(fid,'(f17.6, f17.6, f17.6, f17.6, f17.6)')  & 
     output(t,e)%Qe, output(t,e)%Qh, state(t,e)%smc(1), state(t,e)%smc(2), output(t,e)%NEE 
   enddo ! time

   close(fid) ! ensemble file
  enddo ! ensemble
 endif ! not data_assim

! -----------------------------------------------------------------------
end program

! ----------------------------------------------------------------
! ----------------------------------------------------------------
subroutine redprm(nsoil,vegtyp)
 use noahmp_globals
 use noahmp_veg_parameters
    
! ----------------------------------------------------------------------
 implicit none

 ! inputs 
 integer, intent(in)    :: nsoil

 ! Locals
 integer, parameter :: fid = 134
 real    :: refdk
 real    :: refkdt
 real    :: frzk
 real    :: frzfact
 character(1000) :: fname
 character(1) :: es1
 character(2) :: es2
 integer :: vegtyp

! ----Read in Paramter File----------------------------------------------
 fname = 'parms.txt'
 open(fid,file=trim(fname),action='read')

 ! veg parms
  read(fid,*) CH2OP
  read(fid,*) DLEAF
  read(fid,*) Z0MVT
  read(fid,*) HVT
  read(fid,*) HVB
  read(fid,*) RC
  read(fid,*) RHOL(1)
  read(fid,*) RHOL(2)
  read(fid,*) RHOS(1)
  read(fid,*) RHOS(2)
  read(fid,*) TAUL(1)
  read(fid,*) TAUL(2)
  read(fid,*) TAUS(1)
  read(fid,*) TAUS(2)
  read(fid,*) XL
  read(fid,*) CWPVT
  read(fid,*) C3PSN
  read(fid,*) KC25
  read(fid,*) AKC
  read(fid,*) KO25
  read(fid,*) AKO
  read(fid,*) AVCMX
  read(fid,*) LTOVRC
  read(fid,*) DILEFC
  read(fid,*) DILEFW
  read(fid,*) RMF25
  read(fid,*) SLA
  read(fid,*) FRAGR
  read(fid,*) TMIN
  read(fid,*) VCMX25
  read(fid,*) TDLEF
  read(fid,*) BP
  read(fid,*) MP
  read(fid,*) QE25
  read(fid,*) RMS25
  read(fid,*) RMR25
  read(fid,*) ARM
  read(fid,*) FOLNMX
  read(fid,*) WDPOOL
  read(fid,*) WRRAT
  read(fid,*) MRP
!  SAIM = 0.
!  LAIM = 0.
!  read(fid,*) SAIM
!  read(fid,*) LAIM
  read(fid,*) SLAREA
!  read(fid,*) EPS
  read(fid,*) VEGTYP

 ! gen parms
  read(fid,*) csoil
  read(fid,*) bexp
  read(fid,*) dksat
  read(fid,*) dwsat
  read(fid,*) f1
  read(fid,*) psisat
  read(fid,*) quartz
  read(fid,*) smcdry
  read(fid,*) smcmax
  read(fid,*) smcref
  read(fid,*) smcwlt
  read(fid,*) zbot
  read(fid,*) czil
  read(fid,*) frzk
  read(fid,*) refdk
  read(fid,*) refkdt
  read(fid,*) slope
  read(fid,*) topt
  read(fid,*) rgl
  read(fid,*) rsmax
  read(fid,*) rsmin
  read(fid,*) hs
  read(fid,*) nroot
 close(fid)

 fname = 'time_parms.txt'
 open(fid,file=trim(fname),action='read')
  read(fid,*) SAIM
  read(fid,*) LAIM
!  read(fid,*) EPS
 close(fid)

 ! some basic manipulations
 kdt = refkdt * dksat / refdk
 frzfact = (smcmax / smcref) * (0.412 / 0.468)
 frzx = frzk * frzfact

 ! error check on rooting layers
 if (nroot.gt.nsoil) nroot = nsoil

end subroutine redprm


subroutine sim_init(setup,state,perturb,data_assim,Ntimes,Nens)
 use type_decs

 integer, parameter :: fid = 14
 type(state_data)   :: state
 type(setup_data)   :: setup
 logical, intent(out) :: perturb,data_assim
 integer, intent(out) :: Nens
 integer, intent(out) :: Ntimes
 integer :: da_flag
 logical :: fexists

! simulation setup
 open(fid,file='init.txt',action='read')
  read(fid,*) setup%nsoil     
  read(fid,*) setup%nsnow     
 
! allocate dimensions
  allocate(state%stc(-setup%nsnow+1:setup%nsoil))
  allocate(state%zsnso(-setup%nsnow+1:setup%nsoil))
  allocate(state%tsno(setup%nsnow))
  allocate(state%snice(setup%nsnow))
  allocate(state%snliq(setup%nsnow))
  allocate(state%sh2o(setup%nsoil))
  allocate(state%smc(setup%nsoil))
  allocate(setup%sldpth(setup%nsoil))

! setup parameters
  read(fid,*) setup%zlvl      
  !read(fid,*) setup%startdate ! SY: see at end of this subroutine
  read(fid,*) setup%dt        
  read(fid,*) setup%opt_crs 
  read(fid,*) setup%opt_btr 
  read(fid,*) setup%opt_run 
  read(fid,*) setup%opt_sfc 
  read(fid,*) setup%opt_frz 
  read(fid,*) setup%opt_inf 
  read(fid,*) setup%opt_rad 
  read(fid,*) setup%opt_alb 
  read(fid,*) setup%opt_snf 
  read(fid,*) setup%opt_tbot 
  read(fid,*) setup%opt_stc 
  read(fid,*) setup%dveg     
  read(fid,*) setup%sldpth    

! initial state
  read(fid,*) state%stc(1:setup%nsoil) 
  read(fid,*) state%snowh   
  read(fid,*) state%sneqv   
  read(fid,*) state%canliq  
  read(fid,*) state%rtmass  
  read(fid,*) state%albold  
  read(fid,*) state%lai     
  read(fid,*) state%tv      
  read(fid,*) state%tg      
 close(fid)

 open(fid,file='da_flag.txt')
  read(fid,*) da_flag
 close(fid)
 if (da_flag.gt.0) then
  perturb = .true.
  data_assim = .true.
  nens = da_flag
 elseif (da_flag.lt.-1) then
  perturb = .true.
  data_assim = .false.
  nens = -da_flag
 elseif ((da_flag.eq.0).or.(da_flag.eq.-1)) then
  perturb = .false.
  data_assim = .false.
  nens = 1
 else
  stop 9813
 endif

 inquire(file='state_pert.txt',exist=fexists)
 if (fexists) then
  open(fid,file='state_pert.txt')
   read(fid,*) setup%smc_state_pert
  close(fid)
 else
  setup%smc_state_pert = 0
 endif
 !print*,setup%smc_state_pert

 open(fid,file='num_times.txt')
  read(fid,*) ntimes
 close(fid)

 open(fid,file='lat_lon.txt')
  read(fid,*) setup%latitude
  read(fid,*) setup%longitude
 close(fid)

 open(fid,file='plant_init.txt')
  read(fid,*) state%rtmass
  read(fid,*) state%wood
  read(fid,*) state%lfmass
  read(fid,*) state%stmass
 close(fid)

 open(fid,file='soil_init.txt')
  read(fid,*) state%smc(1)
  read(fid,*) state%smc(2)
  read(fid,*) state%smc(3)
  read(fid,*) state%smc(4)
  state%sh2o = state%smc
 close(fid)

 inquire(file='shdfac.txt',exist=fexists)
 if (fexists) then
   open(fid,file='shdfac.txt')
     read(fid,*) setup%shdfac_monthly
   close(fid)
 else
  setup%shdfac_monthly = (/0.98,0.98,0.98,0.98,0.98,0.98,0.98,0.98,0.98,0.98,0.98,0.98/)
 endif

 ! SY: Begin code instead of the line from near beginning
 open(fid,file='startdate.txt')
  read(fid,*) setup%startdate ! SY: see at end of this subroutine
 close(fid)
 ! SY: End code instead of the line from near beginning

end subroutine sim_init

subroutine dealoc(setup,state)
 use type_decs

 type(state_data)   :: state
 type(setup_data)   :: setup

 deallocate(state%stc)
 deallocate(state%zsnso)
 deallocate(state%tsno)
 deallocate(state%snice)
 deallocate(state%snliq)
 deallocate(state%sh2o)
 deallocate(state%smc)
 deallocate(setup%sldpth)

end subroutine dealoc

subroutine soil_veg_gen_parm(stype,slptype,vtype)
  use module_sf_noahlsm, only : shdtbl, nrotbl, rstbl, rgltbl, &
       &                        hstbl, snuptbl, maxalb, laimintbl, &
       &                        bb, drysmc, f11, maxsmc, laimaxtbl, &
       &                        emissmintbl, emissmaxtbl, albedomintbl, &
       &                        albedomaxtbl, wltsmc, qtz, refsmc, &
       &                        z0mintbl, z0maxtbl, &
       &                        satpsi, satdk, satdw, &
       &                        fxexp_data, lvcoef_data, &
       &                        maxalb, &
       &                        slope_data, frzk_data, bare, cmcmax_data, &
       &                        cfactr_data, csoil_data, czil_data, &
       &                        refkdt_data, natural, refdk_data, &
       &                        rsmax_data, salp_data, sbeta_data, &
       &                        zbot_data, smhigh_data, smlow_data, &
       &                        topt_data

  implicit none
  integer :: lucats, slcats, slpcats, lc
  integer :: ierr, iindex
  integer, intent(in) :: stype, slptype, vtype

! noah veg parameters
  open(19,file='parm_tables/VEGPARM.TBL',form='formatted',status='old',iostat=ierr)
   if(ierr.ne.0) then
    stop 'soil_veg_gen_parm: failure opening VEGPARM.TBL'
   endif
   read(19,*)
   read(19,*) 
   read(19,*) lucats
   do lc=1,lucats
    read(19,*)IINDEX,SHDTBL(LC),NROTBL(LC),RSTBL(LC),RGLTBL(LC),HSTBL(LC)
   enddo
   read(19,*)
   read(19,*) TOPT_DATA
  close(19)

! soil properties
  OPEN(19,file='parm_tables/SOILPARM.TBL',form='formatted',status='old',iostat=ierr)
  if (ierr.ne.0) then
    stop 'soil_veg_gen_parm: failure opening SOILPARM.TBL'
  endif
  read(19,*)
  read(19,*) 
  read(19,*) slcats
  do lc = 1,slcats
   read(19,*) IINDEX,BB(LC),DRYSMC(LC),F11(LC),MAXSMC(LC),&
          REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
          WLTSMC(LC), QTZ(LC)
  enddo
  close(19)

! general parameters
  open(19,file='parm_tables/GENPARM.TBL',form='formatted',status='old',iostat=ierr)
  if (ierr.ne.0) then
    stop 'soil_veg_gen_parm: failure opening GENPARM.TBL'
  endif 
  read(19,*)
  read(19,*)
  read(19,*) slpcats
  do lc = 1,slpcats
   read(19,*) SLOPE_DATA(LC)
  enddo
  read(19,*)
  read(19,*)SBETA_DATA
  read(19,*)
  read(19,*)FXEXP_DATA
  read(19,*)
  read(19,*)CSOIL_DATA
  read(19,*)
  read(19,*)SALP_DATA
  read(19,*)
  read(19,*)REFDK_DATA
  read(19,*)
  read(19,*)REFKDT_DATA
  read(19,*)
  read(19,*)FRZK_DATA
  read(19,*)
  read(19,*)ZBOT_DATA
  read(19,*)
  read(19,*)CZIL_DATA
  read(19,*)
  read(19,*)SMLOW_DATA
  read(19,*)
  read(19,*)SMHIGH_DATA
  read(19,*)
  read(19,*)LVCOEF_DATA
  close(19)

  open(20,file='parms.txt',status='replace')
 ! gen parms
   write(20,*) csoil_data   !
   write(20,*) bb(stype)    !
   write(20,*) satdk(stype) !
   write(20,*) satdw(stype) ! 
   write(20,*) f11(stype)   !
   write(20,*) satpsi(stype)!
   write(20,*) qtz(stype)   !
   write(20,*) drysmc(stype)!
   write(20,*) maxsmc(stype)!
   write(20,*) refsmc(stype)!
   write(20,*) wltsmc(stype)!
   write(20,*) zbot_data    !
   write(20,*) czil_data    !
   write(20,*) frzk_data    !
   write(20,*) refdk_data   !
   write(20,*) refkdt_data  !
   write(20,*) slope_data(slptype)!
   write(20,*) topt_data    !
   write(20,*) rgltbl(vtype)!
   write(20,*) rsmax_data   !
   write(20,*) rstbl(vtype) !
   write(20,*) hstbl(vtype) !
   write(20,*) nrotbl(vtype)!
  close(20)

END SUBROUTINE SOIL_VEG_GEN_PARM

subroutine perturb_state(setup,state)
 use type_decs
 use noahmp_veg_parameters
 use noahmp_globals
 implicit none

 type(state_data), intent(inout) :: state
 type(setup_data), intent(in)    :: setup
! real, parameter :: sig_sm = 0.005
! real, parameter :: sig_veg = 0.01
 real, dimension(4,4) :: R 
 integer, parameter :: N = 1
 real, dimension(4) :: eta
 real :: random_normal
 integer :: d

 R(1,1) = 1.0000
 R(1,2) = 0.6000
 R(1,3) = 0.4000
 R(1,4) = 0.2000

 R(2,1) = 0.0
 R(2,2) = 0.8000
 R(2,3) = 0.4500
 R(2,4) = 0.3500

 R(3,1) = 0.0
 R(3,2) = 0.0
 R(3,3) = 0.7984
 R(3,4) = 0.4540

 R(4,1) = 0.0
 R(4,2) = 0.0
 R(4,3) = 0.0
 R(4,4) = 0.7946

 eta(1) = random_normal()
 eta(2) = random_normal()
 eta(3) = random_normal()
 eta(4) = random_normal()
 eta = matmul(eta,R)

 ! soil moisture
 state%smc(1)  = state%smc(1) +eta(1)*setup%smc_state_pert(1)!0.006
 state%sh2o(1) = state%sh2o(1)+eta(1)*setup%smc_state_pert(1)!0.006

 state%smc(2)  = state%smc(2) +eta(2)*setup%smc_state_pert(2)!0.00011
 state%sh2o(2) = state%sh2o(2)+eta(2)*setup%smc_state_pert(2)!0.00011

 state%smc(3)  = state%smc(3) +eta(3)*setup%smc_state_pert(3)!0.0006
 state%sh2o(3) = state%sh2o(3)+eta(3)*setup%smc_state_pert(3)!0.0006

 state%smc(4)  = state%smc(4) +eta(4)*setup%smc_state_pert(4)!0.0004
 state%sh2o(4) = state%sh2o(4)+eta(4)*setup%smc_state_pert(4)!0.0004

! ! plant stores
! eta = random_normal()
! state%lfmass = state%lfmass+eta*(sig_veg*(state%lfmass-50/SLA))
! if (state%lfmass.le.50/SLA) state%lfmass = 50/SLA+0.01
! if (state%lfmass.ge.5000/SLA) state%lfmass = 5000/SLA

! eta = random_normal()
! state%stmass = state%stmass+eta*(sig_veg*(state%stmass-1))!0.05/0.003))
!! if (state%stmass.le.0.05/0.003) state%stmass = 0.05/0.003+0.01 
! if (state%stmass.le.1) state%stmass = 1!0.05/0.003+0.01 

! eta = random_normal()
! state%rtmass = state%rtmass+eta*(sig_veg*state%rtmass)
! if (state%rtmass.lt.5) state%rtmass = 5.01
!
! state%lai = max(state%lfmass*SLA/1000,0.05)
! state%sai = max(state%stmass*3*0.001,0.05)

! SY: Begin addition per Grey email 12/05/15
 do d = 1,setup%nsoil
  if (state%smc(d).gt.smcmax)    state%smc(d) = smcmax
  if (state%smc(d).lt.0.02)      state%smc(d) = 0.02
  if (state%sh2o(d).gt.smcmax)   state%sh2o(d) = smcmax
  if (state%sh2o(d).lt.0.02)     state%sh2o(d) = 0.02
 enddo
! SY: End addition per Grey email 12/05/15

 return
end subroutine perturb_state


FUNCTION random_normal() RESULT(fn_val)
 IMPLICIT NONE
 REAL     :: half = 0.5
 REAL :: fn_val
 REAL     :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472,    &
             r1 = 0.27597, r2 = 0.27846, u, v, x, y, q
 INTEGER :: i, n, clock
 INTEGER, DIMENSION(:), ALLOCATABLE :: seed

!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

!  CALL RANDOM_SEED(size = n)
!  ALLOCATE(seed(n))
!  CALL SYSTEM_CLOCK(COUNT=clock)
!  seed = clock + 37 * (/ (i - 1, i = 1, n) /)
!  CALL RANDOM_SEED(PUT = seed)
!  DEALLOCATE(seed)

  DO
   CALL RANDOM_NUMBER(u)
   CALL RANDOM_NUMBER(v)
   v = 1.7156 * (v - half)

!     Evaluate the quadratic form
   x = u - s
   y = ABS(v) - t
   q = x**2 + y*(a*y - b*x)

!     Accept P if inside inner ellipse
   IF (q < r1) EXIT
!     Reject P if outside outer ellipse
   IF (q > r2) CYCLE
!     Reject P if outside acceptance region
   IF (v**2 < -4.0*LOG(u)*u**2) EXIT
  END DO

!     Return ratio of P's coordinates as the normal deviate
  fn_val = v/u
 RETURN
END FUNCTION random_normal
