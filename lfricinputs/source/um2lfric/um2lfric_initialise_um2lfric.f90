! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file LICENCE
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE um2lfric_initialise_um2lfric_mod

IMPLICIT NONE

PRIVATE

PUBLIC :: um2lfric_initialise_um2lfric

CONTAINS

SUBROUTINE um2lfric_initialise_um2lfric()

! um2lfric modules
USE um2lfric_namelist_mod,             ONLY: um2lfric_config
USE lfricinp_initialise_um_mod,        ONLY: um_input_file
USE um2lfric_check_input_data_mod,     ONLY: um2lfric_check_input_data

! lfricinputs modules
USE lfricinp_stashmaster_mod, ONLY: lfricinp_read_stashmaster
USE lfricinp_stash_to_lfric_map_mod, ONLY: lfricinp_init_stash_to_lfric_map
USE lfricinp_um_grid_mod, ONLY: lfricinp_set_grid_from_file
IMPLICIT NONE

! Read in UM stashmaster
CALL lfricinp_read_stashmaster(um2lfric_config%stashmaster_file)

! Map stashcodes to lfric field names
CALL lfricinp_init_stash_to_lfric_map()

! Check input file
CALL um2lfric_check_input_data(um_input_file)

! Initialise the grid
CALL lfricinp_set_grid_from_file(um_input_file,                     &
                                 um2lfric_config%num_snow_layers,   &
                                 um2lfric_config%num_surface_types)

END SUBROUTINE um2lfric_initialise_um2lfric

END MODULE um2lfric_initialise_um2lfric_mod
