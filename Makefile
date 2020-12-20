#-----------------------------------------------------------------------
RM  = rm -f
NETCDFC=/mnt/central/spack/opt/spack/linux-centos8-cascadelake/gcc-10.2.0/netcdf-c-4.7.4-ktahonwirysyyun4puqkddjdwtuivyti
NETCDFF=/mnt/central/spack/opt/spack/linux-centos8-cascadelake/gcc-10.2.0/netcdf-fortran-4.5.3-ezwkaotiq57ps4lcd5m7f6wxkdg4zyfk
FFLAGS = -I${NETCDFC}/include -I${NETCDFF}/include 
LIBS=-L${NETCDFC}/lib/ -L${NETCDFF}/lib/ -lnetcdf -lnetcdff
SRC_DIR=./src

# -- GFORTRAN -- 
FTN		= gfortran
FREEFLAGS       = -ffree-form -ffree-line-length-none -g -C -w -fallow-argument-mismatch

#-----------------------------------------------------------------------
#
# Dependencies
#
#-----------------------------------------------------------------------
.SUFFIXES:
.SUFFIXES: .o .f90

%.o: %.f90
	$(FTN) $(FFLAGS) $(FREEFLAGS) -c -o $@ $<

JOINWRFEXE = joinwrfh

JOINWRFOBJS = \
	$(SRC_DIR)/joinwrf.o \
	$(SRC_DIR)/fjoinwrf.o \
	$(SRC_DIR)/timelib3d.o

default: $(JOINWRFEXE)

$(JOINWRFEXE): $(JOINWRFOBJS) 
	$(FTN) $(LDFLAGS) -o $@ $(JOINWRFOBJS) $(LIBS)
	@ls -l $(JOINWRFEXE)

clean:
	-$(RM) -f joinwrfh $(SRC_DIR)/*.o

