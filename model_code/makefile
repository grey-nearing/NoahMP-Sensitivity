#!/bin/bash

FC  = ifort
LIBS = -L${MKLROOT}/lib -lmkl_lapack95_lp64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core  -liomp5 -lpthread
LDFLAGS = -O3 -xHost -xAVX -axCORE-AVX2 -ipo -fpp
#LDFLAGS = -fpp 
#FFLAGS =  -g -O0 -traceback 

EXEFILE = main.exe
#EXEFILE = main.exe
MODOBJS = type_decs.o module_model_constants.o module_sf_noahlsm.o kwm_date_utilities.o module_sf_noahutl.o module_sf_noahmplsm.o 

MODSRCS = ./src/type_decs.f90 ./src/module_model_constants.f90 ./src/module_sf_noahlsm.f90 ./src/kwm_date_utilities.f90 ./src/module_sf_noahutl.f90 ./src/module_sf_noahmplsm.f90 

MODS = type_decs.mod module_model_constants.mod module_sf_noahlsm.mod kwm_date_utilities.mod module_sf_noahutl.mod module_sf_noahmplsm.mod  

OBJS =  enks.o driver.o main.o

all: $(EXEFILE) cln 

$(EXEFILE): $(MODOBJS) $(OBJS) 
	$(FC) -o $(EXEFILE) $(OBJS) $(MODOBJS) $(LIBS)
$(MODOBJS): 
	$(FC) $(LDFLAGS) $(FFLAGS) -c $(MODSRCS)
$(OBJS): %.o: ./src/%.f90
	$(FC) $(LDFLAGS) $(FFLAGS) -c $<

cln:
	mv *.o objs
	mv *.mod objs
clean: 
	/bin/rm ./objs/*.mod ./objs/*.o $(EXEFILE) 

module_ascii_io.mod:	kwm_date_utilities.o



# DO NOT DELETE THIS LINE -- make depend depends on it.
