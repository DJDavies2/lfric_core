##############################################################################
# (C) Crown copyright 2025 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
# Various things specific to the Nvidia Fortran compiler.
##############################################################################
#

F_MOD_DESTINATION_ARG = -module$(SPACE)

FFLAGS_COMPILER           = -DNVHPC
FFLAGS_COMPILER          += -Mfree -Mpreprocess
FFLAGS_NO_OPTIMISATION    = -O0
FFLAGS_SAFE_OPTIMISATION  = -O2
FFLAGS_RISKY_OPTIMISATION = -O4
FFLAGS_DEBUG              = -g -traceback
FFLAGS_RUNTIME            =
# Option for checking code meets Fortran standard (not available for PGI)
FFLAGS_FORTRAN_STANDARD   =

LDFLAGS_COMPILER = -g

# Flags for OpenMP threading / OpenMP offloading / OpenACC Offloading
# The LFRIC_OFFLOAD_DIRECTIVES env_variable is also queried in the PSyclone
# script to generate matching directives
ifeq ("$(LFRIC_OFFLOAD_DIRECTIVES)", "omp")
	FFLAGS_OPENMP  = -mp=gpu -gpu=mem:managed
	LDFLAGS_OPENMP = -mp=gpu -gpu=mem:managed -cuda
else ifeq ("$(LFRIC_OFFLOAD_DIRECTIVES)", "acc")
	FFLAGS_OPENMP  = -acc=gpu -gpu=mem:managed -mp=multicore
	LDFLAGS_OPENMP = -acc=gpu -gpu=mem:managed -mp=multicore -cuda
else
	FFLAGS_OPENMP  = -mp
	LDFLAGS_OPENMP = -mp
endif

FPPFLAGS = -P -DNVHPC

