!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @brief A simple algorithm for testing the PSy layer.
!! @details Computes the Galerkin projection for different W spaces

module dynamo_algorithm_mod

  use field_mod,  only : field_type
  use log_mod,      only: log_event, log_scratch_space, LOG_LEVEL_INFO
  use psy,          only: invoke_rhs_w3, invoke_w3_solver_kernel,            &
                          invoke_rhs_w2, invoke_rhs_w1,                      &
                          invoke_assign_coordinate_kernel
  use solver_mod,   only: solver_algorithm
  use argument_mod, only: w0, w1, w2, w3

  implicit none

  private
  public :: dynamo_algorithm

contains

  !> @brief Galerkin projections for different W spaces
  !! @details Computes the RHS for each of the W1, W2 and W3
  !! then calls a the GP kernel. Which is a trivial solver for W3
  !! and a generic solver_algorithm for W2 and W1. The solver is a special
  !! pattern so its not a kernel itself but a sub algorithm
  subroutine dynamo_algorithm( pressure_density, rhs, &
                               flux_velocity, rhs_w2, &
                               circulation,   rhs_w1, &
                               chi)

    implicit none

    type( field_type ), intent( in )    :: pressure_density
    type( field_type ), intent( in )    :: rhs
    type( field_type ), intent( inout ) :: flux_velocity
    type( field_type ), intent( inout ) :: rhs_w2
    type( field_type ), intent( inout ) :: circulation
    type( field_type ), intent( inout ) :: rhs_w1
    type( field_type ), intent( inout ) :: chi(3)

    call log_event( "Dynamo: computing W0 coordinate fields", LOG_LEVEL_INFO )
    call invoke_assign_coordinate_kernel( chi )
    
    call log_event( "Dynamo: Galerkin Projection for W3 ", LOG_LEVEL_INFO )
    !Construct PSy layer given a list of kernels. This is the line the code
    !generator may parse and do its stuff.

    !PSY call invoke ( w3_rhs_kernel_type(rhs),                            &
    !                  w3_solver_kernel_type(pressure_density,rhs) )
    call invoke_rhs_w3( rhs )

    call solver_algorithm(pressure_density, rhs, chi, w3)

    call log_event( "Dynamo:Starting Galerkin projection for W2 ...",      &
         LOG_LEVEL_INFO)

    !PSY call invoke( rhs_w2_type(rhs_w2) )
    call invoke_rhs_w2(rhs_w2)
    call solver_algorithm(flux_velocity, rhs_w2, chi, w2)
    
    call log_event( "Dynamo:Starting Galerkin projection for W1 ...",      &
         LOG_LEVEL_INFO)

    !PSY call invoke( rhs_w1_type(rhs_w1) )
    call invoke_rhs_w1(rhs_w1)
    call solver_algorithm(circulation, rhs_w1, chi, w1)

  end subroutine dynamo_algorithm

end module dynamo_algorithm_mod
