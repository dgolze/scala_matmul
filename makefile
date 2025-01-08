MPIFC = mpif90
FC = mpif90
FFLAGS = -O3 -ffree-line-length-none -mtune=native -march=native
F90FLAGS = $(FFLAGS)
FFLAGS_NO = -O0 -ffree-line-length-none -g
LAPACKBLAS = -L/work/modules/Ubuntu/14.04/amd64/t304/intel/2013.3.163/mkl/lib/intel64/ -lmkl_blas95_lp64 -lmkl_lapack95_lp64 \
              -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lpthread
SCALAPACK = -L/work/modules/Ubuntu/14.04/amd64/t304/intel/2013.3.163/mkl/lib/intel64/ -lmkl_scalapack_lp64 -lmkl_blacs_openmpi_lp64

PROGRAM  = mm_multiply

# "make" builds all
all: $(PROGRAM) realclean 

mm_multiply: kinds.o mm_types.o read_input.o setup_and_output.o scalapack_test.o
	$(MPIFC) $(FFLAGS) $(F90FLAGS) -o $@ kinds.o mm_types.o read_input.o setup_and_output.o  scalapack_test.o $(SCALAPACK) $(LAPACKBLAS) 

%.o: %.f90
	$(FC) $(FFLAGS) $(F90FLAGS) -o $@ -c $<

# Utility targets
.PHONY: realclean distclean 

realclean:
	rm -rf *.o *.mod

distclean: realclean
	rm -f *~ $(PROGRAM) 

