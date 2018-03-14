module module_ascii_io

  implicit none

  real,    parameter :: badval = -1.E36
  integer, parameter :: ibadval = -999999

  integer, parameter :: output_unit = 41

  character(len=2048) :: varstring
  character(len=2048) :: unitstring
  character(len=2048) :: fmtstring
  integer             :: lenstring

contains

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

  subroutine open_namelist_file(iunit, output_dir, namelist_filename, forcing_filename, infotext, num_soil_levels, startdate, enddate, spin_up_while, &
       latitude, longitude,        &
       forcing_timestep_second, noahlsm_timestep_second, ice, skin_temperature, stc, smc, sh2o, sldpth, canopy_water,  &
       snow_depth, snow_equivalent, deep_soil_temperature,                                               &
       opt_dveg, opt_crs, opt_btr, opt_run, opt_sfc, &
       opt_frz, opt_inf, opt_rad, opt_alb, opt_snf, opt_tbot, opt_stc, &
       vegetation_type_index, soil_type_index,                                                           &
       slope_type_index, max_snow_albedo, air_temperature_level, wind_level,                             &
       albedo_monthly, shdfac_monthly, z0brd_monthly, lai_monthly, use_urban_module, urban_veg_category, &
       usemonalb, rdlai2d, landuse_dataset)
    !
    ! Purpose:  
    !
    !   Open an initial/forcing conditions file for input.
    !   Read some metadata regarding the site and dataset.
    !   Read the initial conditions for the site.
    !   Return namelist settings to the calling routine.
    !
    ! Note: 
    !
    !   The arrays STC, SMC, SH2O, and SLDPTH are pointer arrays, which should be unassociated
    !   on input.  On output, these pointers are associated with the appropriate data arrays
    !   read from the namelist.  Because we're passing around these pointers, this subroutine
    !   needs to be contained by the calling routine, or, alternately, could be put into a 
    !   module that's used by the calling program.
    !
    ! Input:
    !
    !   IUNIT   :  The Fortran unit number to attach to the opened file.
    !   FILENAME:  The pathname to the existing initial/forcing conditions to be opened.
    !
    ! Output:
    !
    !   INFOTEXT             :  Character string returning some information about the namelist
    !                           selections which may be of interest to the user
    !
    !   NUM_SOIL_LEVELS      :  The number of layers for which the user specifies data for 
    !                           the soil fields
    !
    !   STARTDATE            :  The date representing the starting date/time of the forcing 
    !                           data in the file ( YYYYMMDDHHmm ).
    !
    !   ENDDATE              :  The date representing the ending date/time of the forcing 
    !                           data in the file ( YYYYMMDDHHmm ).
    !
    !   LATITUDE             :  The latitude of the point represented in the file ( Degrees N ).
    !
    !   LONGITUDE            :  The longitude of the point represented in the file ( Degrees E ).
    !
    !   FORCING_TIMESTEP_SECONDS:  The time interval between data records in the file ( Seconds ).
    !
    !   NOAHLSM_TIMESTEP_SECONDS:  The timestep to use when integrating the Noah LSM ( Seconds ).
    !
    !   ICE                  :  Flag representing whether the point in the file is:
    !                                a sea-ice point            ( ICE == 1 ),
    !                                an ice-free land point     ( ICE == 0 ), or
    !                                a glacial land (i.e., permanent ice cover) ice point ( ICE == -1)
    !
    !   SKIN_TEMPERATURE     :  The initial conditions skin temperature ( K ).
    !
    !   STC                  :  The initial soil temperature in the soil layers ( K ).
    !
    !   SMC                  :  The initial volumetric soil moisture in the 
    !                           soil layers ( m{3} m{-3} ).
    !
    !   SH2O                 :  The initial volumetric liquid soil moisture content in the 
    !                           soil layers ( m{3} m{-3} ).
    !
    !   SLDPTH               :  The thicknesses of the soil layers ( m ).
    !
    !   CANOPY_WATER         :  The initial conditions canopy water content ( m ).
    !
    !   SNOW_DEPTH           :  The initial conditions snow depth, not water equivalent, 
    !                           but real snow depth ( m ).
    !
    !   SNOW_EQUIVALENT      :  The initial conditison water equivalent snow depth (m).
    !
    !   DEEP_SOIL_TEMPERATURE:  The constant-value deep soil temperature ( K ).
    !
    !   OPT_DVEG             :  Use Dynamic Vegetation (2) or not (1).
    !
    !   VEGETATION_TYPE_INDEX:  The vegetation type index ( USGS 27-category dataset ).
    !
    !   SOIL_TYPE_INDEX      :  The soil type index ( STATSGO 19-category dataset ).
    !
    !   SLOPE_TYPE_INDEX     :  The slope type index ( Dateset ??? ).
    !
    !   MAX_SNOW_ALBEDO      :  The maximum albedo over deep snow ( Fraction 0.0 to 1.0 ).
    !
    !   AIR_TEMPERATURE_LEVEL:  The level at which air temperature and humidity forcing variables
    !                           are taken to be valid ( m AGL ).
    !
    !   WIND_LEVEL           :  The level at which the wind-speed forcing variable is taken to
    !                           be valid ( m AGL ).
    !
    !   ALBEDO_MONTHLY       :  List of 12 background albedo values, climatological values 
    !                           appropriate for each month of the year.
    !
    !   SHDFAC_MONTHLY       :  List of 12 Vegetation Fraction values, climatological values 
    !                           appropriate foreach month of the year.
    !
    !   Z0BRD_MONTHLY        :  List of 12 background roughness length  values, climatological 
    !                           values appropriate foreach month of the year.
    !
    !
    !   LAI_MONTHLY          :  List of 12 Leaf Area Index  values, climatological 
    !                           values appropriate foreach month of the year.
    !
    !   USE_URBAN_MODULE     :  Logical -- whether or not to use the urban module.
    !
    !   URBAN_VEG_CATEGORY   :  The vegetation index that corresponds to the urban category
    !                           in the selected landuse dataset
    !
    !   USEMONALB            :  User option to use the monthly albedo values as set in ALBEDO_MONTHLY,
    !                           rather than compute the albedo from the green vegetation fraction and
    !                           the minimum and maximum albedos set in VEGPARM.TBL.
    !
    !   RDLAI2D              :  User option to use the monthly LAI values as set in LAI_MONTHLY,
    !                           rather than compute the LAI from the green vegetation fraction and
    !                           the minimum and maximum LAI set in VEGPARM.TBL
    !
    !   LANDUSE_DATASET      :  Character string identifying the landuse dataset used for vegetation 
    !                           category indices.  Recognized values "USGS" and "MODIFIED_IGBP_MODIS_NOAH"

    implicit none
    !
    ! Input
    !

    integer,           intent(in)   :: iunit
    character(len=*),  intent(in)   :: namelist_filename

    !
    ! Output
    !
    character(len=256),  intent(out)  :: forcing_filename
    character(len=4096), intent(out)  :: infotext

    integer,             intent(out)  :: num_soil_levels
    character(len=12),   intent(out)  :: startdate
    character(len=12),   intent(out)  :: enddate
    integer,             intent(out)  :: spin_up_while
    real,                intent(out)  :: latitude
    real,                intent(out)  :: longitude
    integer,             intent(out)  :: forcing_timestep_second
    integer,             intent(out)  :: noahlsm_timestep_second
    integer,             intent(out)  :: ice
    real,                intent(out)  :: skin_temperature
    real, pointer,       dimension(:) :: stc
    real, pointer,       dimension(:) :: smc
    real, pointer,       dimension(:) :: sh2o
    real,                intent(out)  :: canopy_water
    real,                intent(out)  :: snow_depth
    real,                intent(out)  :: snow_equivalent
    real,                intent(out)  :: deep_soil_temperature
    integer,             intent(out)  :: opt_dveg
    integer,             intent(out)  :: opt_crs
    integer,             intent(out)  :: opt_btr
    integer,             intent(out)  :: opt_run
    integer,             intent(out)  :: opt_sfc
    integer,             intent(out)  :: opt_frz
    integer,             intent(out)  :: opt_inf
    integer,             intent(out)  :: opt_rad
    integer,             intent(out)  :: opt_alb
    integer,             intent(out)  :: opt_snf
    integer,             intent(out)  :: opt_tbot
    integer,             intent(out)  :: opt_stc
    integer,             intent(out)  :: vegetation_type_index
    integer,             intent(out)  :: soil_type_index
    integer,             intent(out)  :: slope_type_index
    real,                intent(out)  :: max_snow_albedo
    real,                intent(out)  :: air_temperature_level
    real,                intent(out)  :: wind_level
    real, pointer,       dimension(:) :: sldpth
    real, dimension(12), intent(out)  :: albedo_monthly
    real, dimension(12), intent(out)  :: shdfac_monthly
    real, dimension(12), intent(out)  :: z0brd_monthly
    real, dimension(12), intent(out)  :: lai_monthly
    logical,             intent(out)  :: use_urban_module
    integer,             intent(out)  :: urban_veg_category
    logical,             intent(out)  :: usemonalb
    logical,             intent(out)  :: rdlai2d
    character(len=256),  intent(out)  :: landuse_dataset

    !
    ! Local
    ! 

    integer :: ierr
    logical :: sea_ice_point
    integer :: glacial_veg_category

    real, target, dimension(100) :: soil_layer_thickness = -1.E36
    real, target, dimension(100) :: soil_temperature     = -1.E36
    real, target, dimension(100) :: soil_moisture        = -1.E36
    real, target, dimension(100) :: soil_liquid          = -1.E36
    integer :: i
    integer :: nlayers
    integer :: nlayers_temperature
    integer :: nlayers_moisture
    integer :: nlayers_liquid
    logical :: lexist
    character(len=1024) :: output_dir

    namelist /METADATA_NAMELIST/ startdate, enddate, forcing_filename,spin_up_while,latitude, longitude, forcing_timestep_second,     &
         noahlsm_timestep_second, sea_ice_point, skin_temperature, soil_layer_thickness, soil_temperature, &
         soil_moisture, soil_liquid, num_soil_levels, canopy_water, snow_depth, snow_equivalent,                 &
         deep_soil_temperature, &
         opt_dveg, opt_crs, opt_btr, opt_run, opt_sfc, &
         opt_frz, opt_inf, opt_rad, opt_alb, opt_snf, opt_tbot, opt_stc, &
         soil_type_index, vegetation_type_index, slope_type_index,                              &
         max_snow_albedo, air_temperature_level, wind_level, albedo_monthly,                    &
         shdfac_monthly, z0brd_monthly, lai_monthly, use_urban_module, urban_veg_category,      &
         usemonalb, rdlai2d, landuse_dataset,       &
         glacial_veg_category, output_dir

    !
    !  Check if the specified file exists.
    !

    inquire(file=trim(namelist_filename), exist=lexist)
    if (.not. lexist) then
       write(*,'(/," ***** Problem *****")')
       write(*,'(" ***** File ''", A, "'' does not exist.")') trim(namelist_filename)
       write(*,'(" ***** Check the forcing file specified as a command-line argument",/)')
       stop ":  ERROR EXIT"
    endif

    !
    !  Dummy values, which we can later test on to make sure a namelist with this information 
    !  is being used.
    !

    urban_veg_category = ibadval

    glacial_veg_category = ibadval

    skin_temperature = badval

    landuse_dataset = ""

    output_dir = "."

    !
    !  Default values
    !

    use_urban_module = .FALSE.
    usemonalb        = .FALSE.
    rdlai2d          = .FALSE.

    opt_dveg         = 2
    opt_crs          = 1
    opt_btr          = 1
    opt_run          = 1
    opt_sfc          = 1
    opt_frz          = 1
    opt_inf          = 1
    opt_rad          = 1
    opt_alb          = 2
    opt_snf          = 1
    opt_tbot         = 2
    opt_stc          = 1

    spin_up_while = 0

    !
    !  Open the file, stopping the program if there's a problem.
    !

    open(iunit, file=trim(namelist_filename), form='formatted', action='read', iostat=ierr)
    if (ierr /= 0) then
       write(*,'("Problem opening file ''", A, "''")') trim(namelist_filename)
       stop ":  ERROR EXIT"
    endif

    !
    !  Read the metadata and initial conditions namelist.
    !

    read(iunit, METADATA_NAMELIST, iostat=ierr)
    if (ierr /= 0) then
       write(*,'("Problem reading namelist file ''", A, "''")') trim(namelist_filename)
       stop ":  ERROR EXIT"
    endif

    !
    !  Set the integer ICE from the logical namelist option SEA_ICE_POINT and the GLACIAL_VEG_CATEGORY
    !
    if (sea_ice_point) then
       ice = 1
    else
       if ( vegetation_type_index == glacial_veg_category ) then
          ice = -1
       else
          ice = 0
       endif
    endif

    !
    ! Check the timestep settings.
    !

    if ( mod ( noahlsm_timestep_second, 60 ) /= 0 ) then
       write(*,'(/," ***** Problem *****")')
       write(*,'(" ***** Currently, the dates in the simple driver are accurate to the minute.")')
       write(*,'(" ***** Therefore, the NOAHLSM_TIMESTEP (in seconds) must be an integral ")')
       write(*,'(" ***** number of minutes.")')
       write(*,'(" ***** You have set NOAHLSM_TIMESTEP to ", I12)') noahlsm_timestep_second
       write(*,'(" ***** ")')
       write(*,'(" ***** Check the namelist entries in the file ''",A,"''"/)') trim(namelist_filename)
       stop ":  ERROR EXIT"
       stop
    endif

    !
    ! Check that certain variables are set:
    !

    if (urban_veg_category == ibadval) then
       write(*,'(/," ***** Problem *****")')
       write(*,'(" ***** ''URBAN_VEG_CATEGORY'' must be set in the namelist.")')
       write(*,'(" ***** Check the namelist entries in the file ''",A,"''"/)') trim(namelist_filename)
       stop ":  ERROR EXIT"
    endif

    if (glacial_veg_category == ibadval) then
       write(*,'(/," ***** Problem *****")')
       write(*,'(" ***** ''GLACIAL_VEG_CATEGORY'' must be set in the namelist.")')
       write(*,'(" ***** Check the namelist entries in the file ''",A,"''"/)') trim(namelist_filename)
       stop ":  ERROR EXIT"
    endif

    if (skin_temperature < 150) then
       write(*,'(/," ***** Problem *****")')
       write(*,'(" ***** ''SKIN_TEMPERATURE'' must be set in the namelist.")')
       write(*,'(" ***** The unit for skin_temperature is ''K''")')
       write(*,'(" ***** Check the namelist entries in the file ''",A,"''"/)') trim(namelist_filename)
       stop ":  ERROR EXIT"
    endif

    !
    !  Determine the number of layers in the soil, based on the number
    !  of layers the user has specified in the namelist.  Make sure that 
    !  the user has specified the same number of layers for all of the
    !  layer variables.
    !

    nlayers = 0
    do i=1, size(soil_layer_thickness)
       if (soil_layer_thickness(i) < -1.E25) exit
       nlayers = i
    enddo

    nlayers_temperature = 0
    do i=1, size(soil_temperature)
       if (soil_temperature(i) < -1.E25) exit
       nlayers_temperature = i
    enddo

    nlayers_moisture = 0
    do i=1, size(soil_moisture)
       if (soil_moisture(i) < -1.E25) exit
       nlayers_moisture = i
    enddo

    nlayers_liquid = 0
    do i=1, size(soil_liquid)
       if (soil_liquid(i) < -1.E25) exit
       nlayers_liquid = i
    enddo

    !
    !  If the number of layers that the user specifies for each soil variable does not match,
    !  alert the user and stop the program.
    !
    if ( nlayers     /= num_soil_levels )  then

       write(*,'(/," ***** Problem *****")')
       write(*,'(" ***** In initial/forcing conditions file ''", A, "''")') trim(namelist_filename)
       write(*,'(" ***** The number of layers does not match with the soil_layer_thickness.")')
       write(*,'(" ***** Soil_layer_thickness specified with ", I10, " layers.")') nlayers
       write(*,'(" ***** Num_soil_levels      specified with ", I10, " layers.")') num_soil_levels
       write(*,'(" ***** Check the namelist entries in the file ''",A,"''"/)') trim(namelist_filename)
       stop ":  ERROR EXIT"

    endif
    if ( ( nlayers_temperature /= nlayers ) .or. &
         ( nlayers_moisture    /= nlayers ) .or. &
         ( nlayers_liquid      /= nlayers ) ) then

       write(*,'(/," ***** Problem *****")')
       write(*,'(" ***** In initial/forcing conditions file ''", A, "''")') trim(namelist_filename)
       write(*,'(" ***** The number of layers specified in each of the soil fields does not match.")')
       write(*,'(" ***** Soil_layer_thickness specified with ", I10, " layers.")') nlayers
       write(*,'(" ***** Soil_temperature     specified with ", I10, " layers.")') nlayers_temperature
       write(*,'(" ***** Soil_moisture        specified with ", I10, " layers.")') nlayers_moisture
       write(*,'(" ***** Soil_liquid          specified with ", I10, " layers.")') nlayers_liquid
       write(*,'(" ***** Check the namelist entries in the file ''",A,"''"/)') trim(namelist_filename)
       stop ":  ERROR EXIT"

    endif

    !
    !  Nullify our pointer arrays (just in case) before we associate them with the appropriate arrays.
    !

    nullify (stc)
    nullify (smc)
    nullify (sh2o)
    nullify (sldpth)

    !
    !  Associate our pointer arrays with the appropriate data arrays from the namelist.
    !

    stc    => soil_temperature(1:nlayers)
    smc    => soil_moisture(1:nlayers)
    sh2o   => soil_liquid(1:nlayers)
    sldpth => soil_layer_thickness(1:nlayers)

    !
    !  Print some useful information based on the user's namelist selections.
    !

    infotext = '(80("*"),/,'
    if ( usemonalb ) then
       infotext = trim(infotext) // '"USEMONALB is set to .TRUE.  The ALBEDO_MONTHLY data as set in the namelist are ",/,'
       infotext = trim(infotext) // '"used to set the background, snow-free albedo at each time step.",/'
    else
       infotext = trim(infotext) // '"USEMONALB is set to .FALSE.  The ALBEDO_MONTHLY data as set in the namelist are",/,'
       infotext = trim(infotext) // '"used only in initialization before the first time step.  Background albedo ALB ",/,'
       infotext = trim(infotext) // '"is computed internally from the Green Vegetation Fraction and VEGPARM.TBL      ",/,'
       infotext = trim(infotext) // '"values for ALBEDOMIN and ALBEDOMAX.",/,'
    endif

    infotext = trim(infotext) // '80("*"),/'

    if ( rdlai2d ) then
       infotext = trim(infotext) // ',"RDLAI2D is set to .TRUE.  The LAI_MONTHLY data as set in the namelist are used ",/'
       infotext = trim(infotext) // ',"to set the XLAI value at each time step.",/'
    else
       infotext = trim(infotext) // ',"RDLAI2D is set to .FALSE.  The LAI_MONTHLY data as set in the namelist are     ",/'
       infotext = trim(infotext) // ',"ignored.  LAI is computed internally from the Green Vegetation Fraction and the",/'
       infotext = trim(infotext) // ',"VEGPARM.TBL values for LAIMIN and LAIMAX.",/,'
    endif

    infotext = trim(infotext) // '80("*"),/'

    if (.TRUE.) then
       infotext = trim(infotext) // ',"The Z0BRD_MONTHLY data as set in the namelist are used only in initialization   ",/'
       infotext = trim(infotext) // ',"before the first time step, not for Z0BRD values at each time step.  Z0BRD is   ",/'
       infotext = trim(infotext) // ',"computed internally from the Green Vegetation Fraction and the VEGPARM.TBL      ",/'
       infotext = trim(infotext) // ',"values for ZOMIN and ZOMAX.",/,'
    endif

    infotext = trim(infotext) // '80("*"),/'
    
    infotext = trim(infotext) // ')'
    !KWM write(*, FMT=trim(infotext))

  end subroutine open_namelist_file

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------
  
  subroutine read_urban_namelist(iunit, max_urban_layers, num_layers, utype, zlvl_urban, lsolar_urb, &
       initial_roof_temperature, initial_wall_temperature, initial_road_temperature)
    !
    ! MAX_URBAN_LAYERS is a parameter for dimensioning the layer arrays.  
    !
    ! NUM_LAYERS, the number of layers to run for the model, must be less than or equal to MAX_URBAN_LAYERS
    !
    implicit none

    !
    ! MAX_U_LAYERS is a work-around for a peculiarity of Fortran namelists.
    ! MAX_U_LAYERS must be the same as MAX_URBAN_LAYERS
    !
    integer, parameter :: MAX_U_LAYERS = 100
    integer,                           intent(in)  :: iunit
    integer,                           intent(in)  :: max_urban_layers
    integer,                           intent(out) :: num_layers
    integer,                           intent(out) :: utype
    real,                              intent(out) :: zlvl_urban
    logical,                           intent(out) :: lsolar_urb
    real, dimension(MAX_U_LAYERS),     intent(out) :: initial_roof_temperature
    real, dimension(MAX_U_LAYERS),     intent(out) :: initial_wall_temperature
    real, dimension(MAX_U_LAYERS),     intent(out) :: initial_road_temperature

    namelist/urban_namelist/ num_layers, utype, zlvl_urban, lsolar_urb, &
         initial_roof_temperature, initial_wall_temperature, initial_road_temperature

    !
    ! A peculiarity of Fortran namelists won't let me dimension INITIAL_ROOF_TEMPERATURE, etc.
    ! by an argument passed in from higher up (in this case, MAX_URBAN_LAYERS).  This may be 
    ! compiler dependent, but I try to make things work for a variety of compilers.  The use
    ! of MAX_U_LAYERS is a work-around, but MAX_U_LAYERS must equal MAX_URBAN_LAYERS.
    !

    if (MAX_U_LAYERS /= MAX_URBAN_LAYERS) then
       write(*,'("MAX_U_LAYERS (in read_urban_namelist) must be the same as MAX_URBAN_LAYERS (in simple_driver_urban.F")')
       write(*,'("MAX_U_LAYERS     = ", I10)') MAX_U_LAYERS
       write(*,'("MAX_URBAN_LAYERS = ", I10)') MAX_URBAN_LAYERS
       write(*,'("Change one or the other, and recompile.")')
       stop
    endif

    !
    ! Default values, to be overwritten when we actually read the namelist.
    !

    num_layers       = -1
    utype            = -1
    lsolar_urb       = .FALSE.
    zlvl_urban       = -1.E36
    initial_roof_temperature = -1.E36
    initial_wall_temperature = -1.E36
    initial_road_temperature = -1.E36

    !
    ! Read the namelist
    !

    read(iunit,urban_namelist)

    !
    ! Make sure that the namelist has sane values.
    !

    if (num_layers < 1) then
       write(*,'("Urban namelist must define NUM_LAYERS.")')
       STOP "READ_URBAN_NAMELIST"
    endif
    if (num_layers > max_urban_layers) then
       write(*,'("NUM_LAYERS as defined in the urban namelist exceeds hard-coded dimension")')
       write(*,'("for MAX_URBAN_LAYERS (=", I6, ")")') max_urban_layers
       write(*,*)
       write(*,'("Either choose fewer layers, or boost the MAX_URBAN_LAYERS parameter and recompile.")')
       write(*,*)
       STOP "READ_URBAN_NAMELIST"
    endif
    if (utype < 0) then
       write(*,'("Urban namelist must define the urban category, UTYPE.")')
       STOP  "READ_URBAN_NAMELIST"
    endif
    if (zlvl_urban < -1.E35) then
       write(*,'("Urban namelist must define ZLVL_URBAN.")')
       STOP "READ_URBAN_NAMELIST"
    endif
    if (initial_roof_temperature(1) < -1.E35) then
       write(*,'("Urban namelist must define INITIAL_ROOF_TEMPERATURE.")')
       STOP  "READ_URBAN_NAMELIST"
    endif
    if (initial_wall_temperature(1) < -1.E35) then
       write(*,'("Urban namelist must define INITIAL_WALL_TEMPERATURE.")')
       STOP  "READ_URBAN_NAMELIST"
    endif
    if (initial_road_temperature(1) < -1.E35) then
       write(*,'("Urban namelist must define INITIAL_ROAD_TEMPERATURE.")')
       STOP  "READ_URBAN_NAMELIST"
    endif

  end subroutine read_urban_namelist

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

  subroutine read_forcing_text(iunit, nowdate, forcing_timestep_second, wspd, u, v, &
       sfctmp, spechumd, sfcprs, swrad, lwrad, pcprate, ierr,FirstTime)
    !
    ! Purpose:
    !
    !      Read the forcing data for a single time step from an open ASCII text file attached to 
    !      Fortran unit <iunit>.
    !
    !      This suddenly got a lot more complicated, now that we need to allow for temporal
    !      interpolation to a shorter timestep than is available in the forcing dataset.
    !
    ! Input:
    !
    !      IUNIT    :  The Fortran unit number from which we read
    !      NOWDATE  :  The current date ("YYYYMMDDHHmm"), for which we want to retrieve data
    !      DT       :  The LSM time step, which should be the same as the time interval of the data.
    !
    ! Output:
    !
    !      WSPD     :  Wind speed near the surface ( m s{-1} )
    !      SFCTMP   :  Air temperature near the surface ( K ) 
    !      SPECHUMD :  Specific humidity near the surface  ( kg kg{-1} )
    !      SFCPRS   :  Surface pressure ( Pa )
    !      SWRAD    :  Incoming SW radiation at the surface ( W m{-2} )
    !      LWRAD    :  Downwelling LW radiation at the surface ( W m{-2} )
    !      PCPRATE  :  Precipitation rate ( kg m{-2} s{-1} )
    !      IERR     :  Error flag:  0 == No error on read
    !                               1 == Hit end-of-file attempting to read data
    !                               2 == Other read error attempting to read data
    !                               3 == We could not find data matching <nowdate>
    !

    use kwm_date_utilities

    implicit none

    !
    ! Input
    !
    integer,           intent(in)  :: iunit
    character(len=12), intent(in)  :: nowdate
    integer,           intent(in)  :: forcing_timestep_second

    !
    ! Output
    !
    real,              intent(out) :: wspd
    real,              intent(out) :: sfctmp
    real,              intent(out) :: spechumd
    real,              intent(out) :: sfcprs
    real,              intent(out) :: swrad
    real,              intent(out) :: lwrad
    real,              intent(out) :: pcprate
    integer,           intent(out) :: ierr
    real,              intent(out) :: u
    real,              intent(out) :: v
    logical,           intent(inout) :: FirstTime 
    !
    ! Local
    !
    integer           :: year
    integer           :: month
    integer           :: day
    integer           :: hour
    integer           :: minute
    character(len=12) :: readdate
    real              :: read_windspeed
    real              :: read_winddir
    real              :: read_temperature
    real              :: read_pressure
    real              :: read_humidity
    real              :: read_swrad
    real              :: read_lwrad
    real              :: read_rain
    real              :: wdir

    type fdata
       character(len=12) :: readdate
       real              :: windspeed
       real              :: winddir
       real              :: temperature
       real              :: humidity
       real              :: pressure
       real              :: swrad
       real              :: lwrad
       real              :: rain
    end type fdata

    type(fdata) :: before = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
    type(fdata) :: after  = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 

    integer :: idts
    integer :: idts2
    real    :: fraction


    real    :: svp ! Saturation Vapor pressure, computed herein as a function of Temperature
    real    :: e   ! Water Vapor Pressure, computed herein as a function of Temperature, Pressure, and Relative Humidity
    real    :: rhf ! Relative humidity expressed as a fraction [ 0.0 to 1.0 ]
    real    :: qs  ! Saturation specific humidity [ kg kg{-1} ]

    ! Parameters used to compute Saturation Vapor Pressure as a function of Temperature
    real, parameter :: svp1  = 611.2
    real, parameter :: svp2  = 17.67
    real, parameter :: svp3  = 29.65
    real, parameter :: svpt0 = 273.15

    ! Parameter used to compute Specific Humidity from Pressure and Saturation Vapor Pressure.
    real, parameter :: eps   = 0.622

    character(len=1024) :: string

    ! Flag to tell us whether this is the first time this subroutine is called, in which case
    ! we need to seek forward to the data.


    ! The format string for reading the forcing data:
    character(len=64), parameter :: read_format = "(I4.4, 4(1x,I2.2),8(F17.10))"

    real, parameter :: pi = 3.14159265

    !
    ! First time in, skip forward, positioning ourself at the beginning of the data.
    !  
    
    if ( FirstTime ) then
       FirstTime = .FALSE.
       do
          read(iunit, '(A1024)') string
          string = upcase(adjustl(string))
          if (string(1:9) == "<FORCING>") exit
       enddo
          before = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
          after  = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
    endif
    ! Wind Speed in this file is m s{-1}
    ! Wind direction in this file is degrees from north.
    ! Temperature in this file is in Degrees C.
    ! Humidity in this file is Relative Humidity, in % (i.e., between 0 and 100+).
    ! Pressure in this file is in mb.
    ! Incoming Short-wave Radiation in this file is in W m{-2}
    ! Incoming Long-wave Radiation in this file is in W m{-2}
    ! Precipitation rate in this file is in Inches per forcing timestep

    READLOOP : do

       !
       ! If our dates in storage are already bracketing NOWDATE, we don't have to
       ! read anything; we can just exit.
       !
       if (before%readdate <= nowdate .and. nowdate <= after%readdate) exit READLOOP

       !
       ! But if we do have to read data, let's read some data!
       ! 
      ! read(UNIT=iunit, FMT=read_format, IOSTAT=ierr) &
       read(iunit,*, IOSTAT=ierr) &
            year, month, day, hour, minute, &
            read_windspeed,   &
            read_winddir,     &
            read_temperature, &
            read_humidity,    &
            read_pressure,    &
            read_swrad,       &
            read_lwrad,       &
            read_rain
       if (ierr < 0) then
          !KWM write(*,'("Hit the end of file.")')
          ierr = 1

          before = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
          after  = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
          FirstTime = .TRUE.

          return
       endif
       if (ierr /= 0) then
          write(*,'("Error reading from data file.")')
          ierr = 2
          return
       endif
       write(readdate,'(I4.4,4I2.2)') year, month, day, hour, minute
       if ( readdate > nowdate ) then
          ! After becomes before, and then we have a new before
          if (after%readdate > "000000000000" ) before = after
          after = fdata ( readdate, read_windspeed, read_winddir, read_temperature, read_humidity, read_pressure, read_swrad, read_lwrad, read_rain )
          exit READLOOP
       else if (readdate == nowdate) then
          before = fdata ( readdate, read_windspeed, read_winddir, read_temperature, read_humidity, read_pressure, read_swrad, read_lwrad, read_rain )
          exit READLOOP
       else if (readdate < nowdate) then
          before = fdata ( readdate, read_windspeed, read_winddir, read_temperature, read_humidity, read_pressure, read_swrad, read_lwrad, read_rain )
          cycle READLOOP
       else
          stop "Logic problem"
       endif
    enddo READLOOP

    if (before%readdate == nowdate) then

       pcprate = before%rain                              ! No conversion necessary
       sfctmp  = before%temperature                       ! No conversion necessary
       sfcprs  = before%pressure*1.E2                     ! Convert pressure from mb to Pa
       wspd    = before%windspeed                         ! No conversion necessary
       wdir    = before%winddir                           ! No conversion necessary
       swrad   = before%swrad                             ! No conversion necessary
       lwrad   = before%lwrad                             ! No conversion necessary
       rhf     = before%humidity * 1.E-2                  ! Convert Relative Humidity from percent to fraction

    else if (after%readdate == nowdate) then

       pcprate = after%rain                              ! No conversion necessary
       sfctmp  = after%temperature                       ! No conversion necessary
       sfcprs  = after%pressure*1.E2                     ! Convert pressure from mb to Pa
       wspd    = after%windspeed                         ! No conversion necessary
       wdir    = after%winddir                           ! No conversion necessary
       swrad   = after%swrad                             ! No conversion necessary
       lwrad   = after%lwrad                             ! No conversion necessary
       rhf     = after%humidity * 1.E-2                  ! Convert Relative Humidity from percent to fraction
       
       before = after
       after  = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 )        

    else if (before%readdate < nowdate .and. nowdate < after%readdate) then

       call geth_idts(nowdate, before%readdate, idts)
       call geth_idts(after%readdate, before%readdate, idts2)
       if (idts2*60 /= forcing_timestep_second) then
          print*, 'forcing_timestep_second = ', forcing_timestep_second
          print*,' nowdate = ', nowdate
          print*, 'before%readdate = ', before%readdate
          print*, 'idts = ', idts
          print*,' after%readdate = ', after%readdate
          print*, 'idts2 = ', idts2
          stop "IDTS PROBLEM"
       endif
       fraction = real(idts2-idts)/real(idts2)

       pcprate = before%rain  ! Precip rate is not interpolated, but carried forward.

       sfctmp = ( before%temperature * fraction )  + ( after%temperature * ( 1.0 - fraction ) )

       sfcprs = ( before%pressure * fraction ) + ( after%pressure * ( 1.0 - fraction ) )
       sfcprs = sfcprs * 1.E2

       wspd = ( before%windspeed * fraction ) + ( after%windspeed * ( 1.0 - fraction ) )

       wdir = ( before%winddir * fraction ) + ( after%winddir * ( 1.0 - fraction ) )

       swrad = ( before%swrad * fraction ) + ( after%swrad * ( 1.0 - fraction ) )

       lwrad = ( before%lwrad * fraction ) + ( after%lwrad * ( 1.0 - fraction ) )

       rhf = ( before%humidity * fraction ) + ( after%humidity * ( 1.0 - fraction ) )
       rhf = rhf * 1.E-2

    else
       print*, 'nowdate = "'//nowdate//'"'
       stop "Problem in the logic of read_forcing_text."
    endif


#define _NCEP_CODE_SPECHUMD_FROM_RH_

#ifdef _NCEP_CODE_SPECHUMD_FROM_RH_

    ! Convert RH [ % ] to Specific Humidity [ kg kg{-1} ] 
    ! This computation from NCEP's Noah v2.7.1 driver.

    svp = EsFuncT(sfctmp)
    QS = eps * svp / (sfcprs - (1.-eps) * svp)
    E = (sfcprs*svp*rhf)/(sfcprs - svp*(1. - rhf))
    spechumd = (eps*e)/(sfcprs-(1.0-eps)*E)
    IF (spechumd .LT. 0.1E-5) spechumd = 0.1E-5
    IF (spechumd .GE.  QS) spechumd = QS*0.99

! write(6,'(f15.12, f16.12, f20.12, f20.12, f20.12)'), eps,e,sfcprs,spechumd


#else

    ! Convert RH [ % ] to Specific Humidity [ kg kg{-1} ]
    ! This computation from MM5/WRF heritage.

    svp      = svp1*exp(svp2*(sfctmp-svpt0)/(sfctmp-svp3))
    e        = rhf * svp
    spechumd = (eps*e)/(sfcprs-(1.0-eps)*e)

#endif

    ! Compute u and v components from wind speed and wind direction
    !u = - wspd * sin (wdir * pi/180.)
    !v = - wspd * cos (wdir * pi/180.)

    u =  wspd 
    v = 0
  end subroutine read_forcing_text


#ifdef _NCEP_CODE_SPECHUMD_FROM_RH_

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
#endif

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

  subroutine initialize_ascii_output(filename)

    ! Open file <filename> for ASCII output.
    !    Variables VARSTRING, UNITSTRING, FMTSTRING, and LENSTRING are module variables, which 
    !    may be set and used by routines that use module_ascii_io.

    implicit none

    character(len=*) :: filename ! The file name to open for output.

    open(output_unit, file=filename, form="formatted", action="write")
    varstring = ""
    unitstring = ""
    fmtstring = ""
    lenstring = 0
  end subroutine initialize_ascii_output

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

  subroutine output_ascii_time(ktime, nowdate,   name, description, units  )
    implicit none
    integer, intent(in) :: ktime
    character(len=12), intent(in) :: nowdate
    character(len=*),  intent(in) :: name
    character(len=*),  intent(in) :: description
    character(len=*),  intent(in) :: units
    
    character(len=15) :: n15
    if (ktime == 1) then
       n15 = trim(name)
       write(output_unit, '(A15)', advance="no") n15

       write(n15, '(A15)') units
       n15 = adjustl(n15)
       unitstring = unitstring(1:lenstring) // n15

       write(n15, '(A15)') "I8,1X,2(I2,1X)"   !"A15"
       n15 = adjustl(n15)
       fmtstring = fmtstring(1:lenstring) // n15

       write(n15, '(A15)') nowdate(1:8)//" "//nowdate(9:10)//":"//nowdate(11:12)//" "
       varstring = varstring(1:lenstring) // n15
       lenstring = lenstring + 15
    else
       write(output_unit,'(A15)', advance="no") nowdate(1:8)//" "//nowdate(9:10)//":"//nowdate(11:12)//" "
    endif
    
  end subroutine output_ascii_time
    
!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

  subroutine output_ascii_var(ktime, value, name, description, units)
    implicit none
    integer, intent(in) :: ktime
    real, intent(in) :: value
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: description
    character(len=*), intent(in) :: units

    character(len=15) :: n15
    if (ktime == 1) then
       n15 = trim(name)
       write(output_unit, '(A15)', advance="no") n15

       write(n15, '(A15)') units
       n15 = adjustl(n15)
       unitstring = unitstring(1:lenstring) // n15

       write(n15, '(A15)') "G15.6"
       n15 = adjustl(n15)
       fmtstring = fmtstring(1:lenstring) // n15

       write(n15, '(G15.6)') value
       varstring = varstring(1:lenstring) // n15
       lenstring = lenstring + 15
    else
       write(output_unit,'(G15.6)', advance="no") value
    endif
  end subroutine output_ascii_var

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

  subroutine output_ascii_levels(ktime, nsoil, value, name, description, units)
    implicit none
    integer, intent(in) :: ktime
    integer, intent(in) :: nsoil
    real, dimension(nsoil), intent(in) :: value
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: description
    character(len=*), intent(in) :: units

    integer :: n
    character(len=15) :: n15
    do n = 1, nsoil
       if (ktime == 1) then
          !n15 = trim(name)
          write(n15,'(A,"(",I2,")")') name, n
          write(output_unit, '(A15)', advance="no") n15

          write(n15, '(A15)') units
          n15 = adjustl(n15)
          unitstring = unitstring(1:lenstring) // n15

          write(n15, '(A15)') "G15.6"
          n15 = adjustl(n15)
          fmtstring = fmtstring(1:lenstring) // n15

          write(n15, '(G15.6)') value(n)
          varstring = varstring(1:lenstring) // n15
          lenstring = lenstring + 15
       else
          write(output_unit,'(G15.6)', advance="no") value(n)
       endif
    enddo
  end subroutine output_ascii_levels

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

  subroutine end_ascii_record(ktime)
    implicit none
    integer, intent(in) :: ktime
    write(output_unit,*)
    if (ktime==1) then
       write(output_unit,'(A)') unitstring(1:lenstring)
       write(output_unit,'(A)') fmtstring(1:lenstring)
       write(output_unit,'(A)') varstring(1:lenstring)
       varstring = ""
       unitstring = ""
       fmtstring = ""
       lenstring = 0
    endif
  end subroutine end_ascii_record
  
!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

  subroutine output_ascii_close()
    close(output_unit)
  end subroutine output_ascii_close

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

  character(len=256) function upcase(h) result(return_string)
    implicit none
    character(len=*), intent(in) :: h
    integer :: i
    
    return_string = ""

    do i = 1, len_trim(h)

       if ((ichar(h(i:i)).ge.96) .and. (ichar(h(i:i)).le.123)) then
          return_string(i:i) = char(ichar(h(i:i))-32)
       else
          return_string(i:i) = h(i:i)
       endif
    enddo

  end function upcase

!------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------

end module module_ascii_io
