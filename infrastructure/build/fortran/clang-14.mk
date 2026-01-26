##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################
# Various things specific to the GNU Fortran compiler.
##############################################################################
#
# This macro is evaluated now (:= syntax) so it may be used as many times as
# desired without wasting time rerunning it.
#
#GFORTRAN_VERSION := $(shell $(FC) -dumpversion 2>&1 \
#                    | awk -F . '{ printf "%02i%02i%02i", $$1, $$2, $$3 }')
#$(info ** Chosen GNU Fortran compiler version $(GFORTRAN_VERSION))

#ifeq ($(shell test $(GFORTRAN_VERSION) -lt 040900; echo $$?), 0)
#  $(error GFortran is too old to build dynamo. Must be at least 4.9.0)
#endif

F_MOD_DESTINATION_ARG     = -J
F_MOD_SOURCE_ARG          = -I

FFLAGS_OPENMP  = -fopenmp
LDFLAGS_OPENMP = -fopenmp

FFLAGS_COMPILER           = 
FFLAGS_NO_OPTIMISATION    = -O0
FFLAGS_SAFE_OPTIMISATION  = -O0
FFLAGS_RISKY_OPTIMISATION = -O0
FFLAGS_DEBUG              = -g
FFLAGS_WARNINGS           = 
FFLAGS_UNIT_WARNINGS      = 
FFLAGS_INIT               = 
# fast-debug flags set separately as Intel compiler needs platform-specific control on them
FFLAGS_FASTD_INIT         = $(FFLAGS_INIT)
FFLAGS_FASTD_RUNTIME      = $(FFLAGS_RUNTIME)

# Option for checking code meets Fortran standard - currently 2008
FFLAGS_FORTRAN_STANDARD   = 

LDFLAGS_COMPILER =

utilities/traceback_mod.o utilities/traceback_mod.mod: private FFLAGS_EXTRA = -fall-intrinsics

# TODO - Remove the -fallow-arguments-mismatch flag when MPICH no longer fails
#        to build as a result of its mismatched arguments (see ticket summary
#        for #2549 for reasoning).
#ifeq ($(shell test $(GFORTRAN_VERSION) -ge 100000; echo $$?), 0)
#	FFLAGS_COMPILER += -fallow-argument-mismatch
#endif

FPPFLAGS = -P -DAOCC
