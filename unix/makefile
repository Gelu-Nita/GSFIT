FC = gfortran # fortran compiler

TARGET_LIB = fit_Spectrum_Kl.so # target lib
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	FFLAGS = -ffixed-line-length-0 -march=native -O3 -fPIC # F flags
endif
ifeq ($(UNAME_S),Darwin)
	FFLAGS = -ffixed-line-length-0 -march=native -O3 -fPIC -static-libgfortran # F flags
endif

objects1 = Calc_GS_Spec_hom.o angular.o spidrsub.o FitFun.o ResolvedSpectrum.o klein.o
objects2 = fit_Spectrum_Kl.o

$(TARGET_LIB): $(objects2) $(objects1)
	$(FC) -shared $(FFLAGS) -o $(TARGET_LIB) $(objects2) $(objects1)

$(objects2): fit_Spectrum_Kl.for
	$(FC) -o $(objects2) $(FFLAGS) -c fit_Spectrum_Kl.for

Calc_GS_Spec_hom.o : 
	$(FC) -c $(FFLAGS) Calc_GS_Spec_hom.for
angular.o : 
	$(FC) -c $(FFLAGS) angular.for
spidrsub.o : 
	$(FC) -c $(FFLAGS) spidrsub.for
FitFun.o : 
	$(FC) -c $(FFLAGS) FitFun.for
ResolvedSpectrum.o : 
	$(FC) -c $(FFLAGS) ResolvedSpectrum.for
klein.o : 
	$(FC) -c $(FFLAGS) klein.f

.PHONY : clean
clean :
	-rm -f $(objects1) $(objects2)
	echo Clean done