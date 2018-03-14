#!/bin/bash

INC_NETCDF = /discover/nobackup/projects/lis/libs/netcdf/4.1.3_intel_13_0_1_117/include/
LIB_NETCDF = /discover/nobackup/projects/lis/libs/netcdf/4.1.3_intel_13_0_1_117/lib/
INC_HDF5 = /discover/nobackup/projects/lis/libs/hdf5/1.8.8_intel_13_0_1_117/include/
LIB_HDF5 = /discover/nobackup/projects/lis/libs/hdf5/1.8.8_intel_13_0_1_117/lib/


LDFLAGS = -L$(LIB_NETCDF) -L$(LIB_HDF5) 
FFLAGS = -I$(INC_NETCDF) -I$(INC_HDF5) #-I$(INC_HDF4)
FC = mpif90

EXEFILE = gather_parameters.exe
OBJS = gather_parameters.o
SRCS = gather_parameters.f90

all: $(EXEFILE) partclean

$(EXEFILE): $(OBJS) 
	$(FC) -o $(EXEFILE) $(OBJS) -L$(LIB_NETCDF) -lnetcdff -lnetcdf -L$(LIB_HDF5) -lhdf5_fortran -lhdf5_hl -lhdf5  
$(OBJS):  
	$(FC) -c -g $(FFLAGS) $(SRCS) 

partclean:
	/bin/rm $(OBJS)

clean: 
	/bin/rm $(OBJS) $(EXEFILE)  


# DO NOT DELETE THIS LINE -- make depend depends on it.
