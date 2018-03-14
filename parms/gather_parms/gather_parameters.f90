program gather_parameters
 use netcdf
 implicit none

! nldas grid  
 integer, parameter :: d = 5
 real, parameter :: minlat = -59.875
 real, parameter :: minlon = -179.875
 real, parameter :: maxlat = 89.875
 real, parameter :: maxlon = 179.875
 integer, parameter :: nsites = 20
 real, parameter :: dlat = 0.25
 real, parameter :: dlon = 0.25
 integer, parameter :: nltype = 20
 integer, parameter :: nstype = 16

! indexes
 integer :: i,j

! site info
 real, dimension(nsites) :: lat, lon
 integer, dimension(nsites) :: grid_id
 integer, dimension(nsites) :: lon_id, lat_id
 integer :: fid
 character*1000 :: fname_site 
 character(1) :: fn_site1
 character(2) :: fn_site2
 character(3) :: fn_site3
 real :: dummy
 logical, dimension(nsites) :: in_domain
 real :: sa, si, cl
 integer*4 :: stype, lctype, slptyp, lmask
 real :: llat, llon
 real :: elev, tbot

! param file info
 character*1000 :: fname
 integer :: ftn, ios, varid
 real, allocatable, dimension(:,:,:) :: landcover, soiltype
 real, allocatable, dimension(:,:) :: var_slp, var_tbot, var_elev, var_msk, var_lat, var_lon
 integer :: nlat, nlon

! read scan site lists
 in_domain = .false.
 open(12,name='Sites.txt')
 do i = 1,nsites
   read(12,*) lat(i),lon(i)
   if ((lat(i).gt.minlat).and.(lon(i).gt.minlon).and.(lat(i).lt.maxlat).and.(lon(i).lt.maxlon)) then
     in_domain(i) = .true. 
     lat_id(i) = ceiling((lat(i)-minlat)/dlat)
     lon_id(i) = ceiling((lon(i)-minlon)/dlon)
   endif
 enddo
 close(12)

! number of grid cells
 nlat = (maxlat-minlat)/dlat+1
 nlon = (maxlon-minlon)/dlon+1

 allocate(landcover(nlon,nlat,nltype))
 allocate(soiltype(nlon,nlat,nstype))
 allocate(var_slp(nlon,nlat))
 allocate(var_tbot(nlon,nlat))
 allocate(var_elev(nlon,nlat))
 allocate(var_msk(nlon,nlat))
 allocate(var_lat(nlon,nlat))
 allocate(var_lon(nlon,nlat))

 ! input filename
 fname = './LDT/lis_input.nc'

 ! open file   
 ios = nf90_open(path=trim(fname),mode=NF90_NOWRITE,ncid=ftn)
 if (ios.ne.0) stop 1
 write(6,*) 'Opened LDT File'

 ! read landmask
 ios = nf90_inq_varid(ftn,'LANDMASK',varid)
 if (ios.ne.0) stop 11
 ios = nf90_get_var(ftn,varid,var_msk)
 if (ios.ne.0) stop 12
 write(6,*) 'Read Land Mask'

 ! read landcover
 ios = nf90_inq_varid(ftn,'LANDCOVER',varid)
 if (ios.ne.0) stop 21
 ios = nf90_get_var(ftn,varid,landcover)
 if (ios.ne.0) stop 22
 write(6,*) 'Read Landcover'

 ! read soil type
 ios = nf90_inq_varid(ftn,'TEXTURE',varid)
 if (ios.ne.0) stop 31
 ios = nf90_get_var(ftn,varid,soiltype)
 if (ios.ne.0) stop 32
 write(6,*) 'Read Texture'

 ! read slope type
 ios = nf90_inq_varid(ftn,'SLOPETYPE',varid)
 if (ios.ne.0) stop 71
 ios = nf90_get_var(ftn,varid,var_slp)
 if (ios.ne.0) stop 72
 write(6,*) 'Read SlopeType'

 ! read bottom temp
 ios = nf90_inq_varid(ftn,'TBOT',varid)
 if (ios.ne.0) stop 111
 ios = nf90_get_var(ftn,varid,var_tbot)
 if (ios.ne.0) stop 112
 write(6,*) 'Read Tbot'
  
 ! read elevation
 ios = nf90_inq_varid(ftn,'ELEVATION',varid)
 if (ios.ne.0) stop 111
 ios = nf90_get_var(ftn,varid,var_elev)
 if (ios.ne.0) stop 112
 write(6,*) 'Read Elevation'

 ! read lat
 ios = nf90_inq_varid(ftn,'lat',varid)
 if (ios.ne.0) stop 111
 ios = nf90_get_var(ftn,varid,var_lat)
 if (ios.ne.0) stop 112
 write(6,*) 'Read Latitude'

 ! read lat
 ios = nf90_inq_varid(ftn,'lon',varid)
 if (ios.ne.0) stop 111
 ios = nf90_get_var(ftn,varid,var_lon)
 if (ios.ne.0) stop 112
 write(6,*) 'Read Longitude'

 ios = nf90_close(ftn)
 
 ! loop through sites
 do i = 1,nsites
   if (in_domain(i)) then
     
     ! open output files
     if (i.lt.10) then
       write(fn_site1,'(i1)') i
       fname_site = './site_data/parms_'//fn_site1
       fname_site = trim(fname_site)//'.txt'
     elseif (i.lt.100) then
       write(fn_site2,'(i2)') i
       fname_site = './site_data/parms_'//fn_site2
       fname_site = trim(fname_site)//'.txt'
     else
       write(fn_site3,'(i3)') i
       fname_site = './site_data/parms_'//fn_site3
       fname_site = trim(fname_site)//'.txt'
     endif
     fid = i + 12
     open(fid,name=fname_site,status='replace')

     write(6,*) 'Opened Output File @ Site:',i

     ! check that this is not a water point
     lmask = var_msk(lon_id(i),lat_id(i))
     if (lmask.eq.1) then
      write(6,*) 'Read Land Mask @ Site:',i,lmask
     else
      write(6,*) 'Read Land Mask @ Site:',i,lmask
      stop 'not a land point'
     endif

     ! check that lat/lon is correct
     llat = var_lat(lon_id(i),lat_id(i))
     llon = var_lon(lon_id(i),lat_id(i))
     print*, 'Latitude @ Site',i,llat,lat(i)
     print*, 'Longitude @ Site',i,llon,lon(i)

     ! read landcover
     do j = 1,nltype
       if (landcover(lon_id(i),lat_id(i),j).eq.maxval(landcover(lon_id(i),lat_id(i),:))) then
         lctype = j
       endif
     enddo 
     write(6,*) 'Read Landcover @ Site:',i,lctype

     ! read soil type
     do j = 1,nstype
       if (soiltype(lon_id(i),lat_id(i),j).eq.maxval(soiltype(lon_id(i),lat_id(i),:))) then
         stype = j
       endif
     enddo 
     write(6,*) 'Read Texture @ Site:',i,stype

     slptyp = var_slp(lon_id(i),lat_id(i))
     write(6,*) 'Read SlopeType @ Site:',i,slptyp

     tbot = var_tbot(lon_id(i),lat_id(i))
     write(6,*) 'Read Tbot @ Site:',i,tbot
  
     elev = var_elev(lon_id(i),lat_id(i))
     write(6,*) 'Read Elevation @ Site:',i,elev
 
     ! write to output file
     write(fid,*) lctype, stype, slptyp, tbot, elev
     close (fid)

     ! screen report
     write(6,*) 'Finished site:',i
  
   endif ! in domain
 enddo ! sites

end program gather_parameters

