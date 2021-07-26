!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!>@brief   The top level program for the tangent linear tests.
!>@details The default is to run all available tests - which
!!         test whether the linear code is tangent linear to the
!!         corresponding nonlinear code.
program tl_test

  use mod_wait,           only : init_wait
  use mpi_mod,            only : initialise_comm, &
                                 finalise_comm
  use xios,               only : xios_initialize, &
                                 xios_finalize
  use log_mod,            only : log_event,       &
                                 LOG_LEVEL_ERROR, &
                                 LOG_LEVEL_INFO
  use tl_test_driver_mod, only : initialise,                 &
                                 finalise,                   &
                                 run_kinetic_energy_gradient

  implicit none

  character(*), parameter :: xios_id = "linear"

  character(:), allocatable :: filename
  integer                   :: world_communicator = -999
  integer                   :: model_communicator = -999

  ! Variables used for parsing command line arguments
  integer :: length, status, nargs
  character(len=0) :: dummy
  character(len=:), allocatable :: program_name, test_flag

  ! Flags which determine the tests that will be carried out
  logical :: do_test_kinetic_energy_gradient = .false.

  ! Usage message to print
  character(len=256) :: usage_message

  ! Initialse mpi and create the default communicator: mpi_comm_world
  call initialise_comm( world_communicator )

  ! Initialise XIOS and get back the split mpi communicator. This requires
  ! an iodef.xml file to be available (even if use_xios_io is .false.).
  call init_wait()
  call xios_initialize(xios_id, return_comm = model_communicator)

  call log_event( 'TL testing running ...', LOG_LEVEL_INFO )

  ! Parse command line parameters
  call get_command_argument( 0, dummy, length, status )
  allocate(character(length)::program_name)
  call get_command_argument( 0, program_name, length, status )
  nargs = command_argument_count()

  ! Print out usage message if wrong number of arguments is specified
  if (nargs /= 2) then
     write(usage_message,*) "Usage: ",trim(program_name), &
          " <namelist filename> "      // &
          " test_XXX with XXX in { "   // &
          " kinetic_energy_gradient, " // &
          " } "
     call log_event( trim(usage_message), LOG_LEVEL_ERROR )
  end if

  call get_command_argument( 1, dummy, length, status )
  allocate( character(length) :: filename )
  call get_command_argument( 1, filename, length, status )

  call get_command_argument( 2, dummy, length, status )
  allocate(character(length)::test_flag)
  call get_command_argument( 2, test_flag, length, status )

  ! Choose test case depending on flag provided in the first command
  ! line argument
  select case (trim(test_flag))
  case ("test_kinetic_energy_gradient")
     do_test_kinetic_energy_gradient = .true.
  case default
     call log_event( "Unknown test", LOG_LEVEL_ERROR )
  end select

  call initialise( filename, model_communicator )
  deallocate( filename )

  if (do_test_kinetic_energy_gradient) then
    call run_kinetic_energy_gradient()
  endif

  call finalise()

  ! Finalise XIOS
  call xios_finalize()

  ! Finalise mpi and release the communicator
  call finalise_comm()

end program tl_test
