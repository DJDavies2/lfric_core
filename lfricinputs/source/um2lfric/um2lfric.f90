! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file LICENCE
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
PROGRAM um2lfric

! lfricinputs modules
USE lfricinp_read_command_line_args_mod, ONLY: lfricinp_read_command_line_args
USE lfricinp_um_parameters_mod, ONLY: fnamelen
USE lfricinp_lfric_driver_mod, ONLY: lfricinp_initialise_lfric,        &
    lfricinp_finalise_lfric, mesh_id, twod_mesh_id, lfric_fields
USE lfricinp_ancils_mod, ONLY: lfricinp_create_ancil_fields, ancil_fields
USE lfricinp_create_lfric_fields_mod, ONLY: lfricinp_create_lfric_fields
USE lfricinp_um_grid_mod, ONLY: um_grid
USE lfricinp_initialise_um_mod, ONLY: lfricinp_initialise_um, &
    lfricinp_finalise_um, um_input_file
USE lfricinp_regrid_options_mod, ONLY: lfricinp_init_regrid_options
! um2lfric modules
USE um2lfric_namelist_mod, ONLY: um2lfric_config, required_lfric_namelists
USE um2lfric_initialise_um2lfric_mod, ONLY: um2lfric_initialise_um2lfric
USE um2lfric_regrid_fields_mod, ONLY: um2lfric_regrid_fields

! LFRic modules
USE log_mod,         ONLY: log_event, LOG_LEVEL_INFO

USE lfric_xios_write_mod, ONLY: write_state

! External libraries
USE xios, ONLY: xios_context_finalize

IMPLICIT NONE

CHARACTER(LEN=fnamelen) :: lfric_fname
CHARACTER(LEN=fnamelen) :: um2lfric_fname

CALL lfricinp_read_command_line_args(um2lfric_fname, lfric_fname)

! Read um2lfric configuration namelist
CALL um2lfric_config%load_namelist(um2lfric_fname)

! Read in global regrid options
CALL lfricinp_init_regrid_options(um2lfric_fname)

! Initialise LFRic Infrastructure
CALL lfricinp_initialise_lfric(program_name_arg="um2lfric", &
     lfric_nl_fname=lfric_fname,                            &
     required_lfric_namelists = required_lfric_namelists)

! Open the UM file
CALL log_event('Initialising UM input file', LOG_LEVEL_INFO)
CALL lfricinp_initialise_um(um2lfric_config%um_file)

! Initialise um2lfric
CALL um2lfric_initialise_um2lfric()

! Initialise LFRic field collection
CALL lfricinp_create_lfric_fields(mesh_id, twod_mesh_id,  &
                                  lfric_fields, um2lfric_config%stash_list, &
                                  um_grid, um_input_file)

! Initialise LFRic ancils field collection
CALL lfricinp_create_ancil_fields(ancil_fields, mesh_id, twod_mesh_id)

! Regrid from UM fields to lfric fields
CALL um2lfric_regrid_fields()

! Write lfric fields to output
CALL write_state(lfric_fields)

! Unloads data from memory and closes UM input file
CALL lfricinp_finalise_um()

! Finalizes XIOS file contents
CALL xios_context_finalize()
CALL log_event( 'UM2LFRic completed', LOG_LEVEL_INFO )

! Finalise YAXT, XIOS, MPI, logging
CALL lfricinp_finalise_lfric()

END PROGRAM um2lfric
