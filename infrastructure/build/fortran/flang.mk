##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE which you
# should have received as part of this distribution.
##############################################################################
# Various things specific to the GNU Fortran compiler.
##############################################################################
#
# This macro is evaluated now (:= syntax) so it may be used as many times as
# desired without wasting time rerunning it.
#

F_MOD_DESTINATION_ARG     = -J
F_MOD_SOURCE_ARG          = -I

FFLAGS_OPENMP  = -fopenmp
LDFLAGS_OPENMP = -fopenmp

FFLAGS_COMPILER           = -ffree-line-length-none
FFLAGS_NO_OPTIMISATION    = -O0
FFLAGS_SAFE_OPTIMISATION  = -Og
FFLAGS_RISKY_OPTIMISATION = -Ofast
FFLAGS_DEBUG              = -g
FFLAGS_WARNINGS           = 
FFLAGS_UNIT_WARNINGS      = 
FFLAGS_INIT               = 
FFLAGS_RUNTIME            = 
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

FPPFLAGS = -P
