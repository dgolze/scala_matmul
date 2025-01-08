# Compiler settings
MPIFC = mpif90
FC = mpif90
FFLAGS = -O3 -ffree-line-length-none -mtune=native -march=native -J$(BUILDDIR)
F90FLAGS = $(FFLAGS)
FFLAGS_NO = -O0 -ffree-line-length-none -g -J$(BUILDDIR)
LAPACKBLAS = -L$(MKLROOT)/lib/intel64 -lmkl_blas95_lp64 -lmkl_lapack95_lp64 \
              -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lpthread
SCALAPACK = -L$(MKLROOT)/lib/intel64 -lmkl_scalapack_lp64 -lmkl_blacs_intelmpi_lp64

# Program name
PROGRAM  = mm_multiply

# Directories
SRCDIR = src
BUILDDIR = build

# Source and object files in order
SRC = $(SRCDIR)/kinds.f90 $(SRCDIR)/mm_types.f90 $(SRCDIR)/read_input.f90 \
      $(SRCDIR)/setup_and_output.f90 $(SRCDIR)/scalapack_test.f90
OBJ = $(patsubst $(SRCDIR)/%.f90, $(BUILDDIR)/%.o, $(SRC))

# Default target
all: $(PROGRAM)

# Link the program
$(PROGRAM): $(OBJ)
	$(MPIFC) $(FFLAGS) -o $@ $^ $(SCALAPACK) $(LAPACKBLAS)

# Compile source files into object files
$(BUILDDIR)/%.o: $(SRCDIR)/%.f90 | $(BUILDDIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Create the build directory if it doesn't exist
$(BUILDDIR):
	mkdir -p $(BUILDDIR)

# Utility targets
.PHONY: clean realclean distclean

clean:
	rm -rf $(BUILDDIR)/*.o $(BUILDDIR)/*.mod

realclean: clean
	rm -rf $(PROGRAM)

distclean: realclean
	rm -rf $(BUILDDIR)

