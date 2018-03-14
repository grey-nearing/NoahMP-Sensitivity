.SUFFIXES:
.SUFFIXES: .o .F

include user_build_options

OBJS =	module_sf_noahmplsm.o \
	module_model_constants.o \
	module_sf_myjsfc.o \
	module_sf_sfclay.o \
	module_sf_noahlsm.o \
	module_ascii_io.o \
	module_io.o \
	kwm_date_utilities.o \
	module_sf_noahutl.o \
	driver.o

CMD = driver.exe

FFLAGS =	$(FREESOURCE) $(F90FLAGS)

all:	$(CMD)

driver.exe:	$(OBJS)
	$(COMPILERF90) -o $(@) $(OBJS) #$(NETCDFLIB)

.F.o:
	$(RM) $(*).f90
	$(CPP) $(CPPMACROS) $(*).F > $(*).f90 # $(NETCDFINC) $(*).F > $(*).f90
	$(COMPILERF90) -c -g  $(FFLAGS) $(*).f90 # $(NETCDFINC) $(*).f90
	$(RM) $(*).f90

clean:
	$(RM) $(OBJS) $(CMD) *.mod *~

driver.exe:		driver.o
driver.exe:		module_sf_noahmplsm.o
driver.exe:		module_sf_noahutl.o
driver.exe:		module_io.o
module_io.o:		module_ascii_io.o
module_ascii_io.o:	kwm_date_utilities.o
module_sf_noahmplsm.o:	module_sf_myjsfc.o module_sf_sfclay.o module_sf_noahlsm.o
module_sf_myjsfc.o:	module_model_constants.o
