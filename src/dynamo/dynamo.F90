!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @mainpage Dynamo
!> PsyKAl is the architecture for Gung Ho. Whlist the computational and optimisation
!> infrastructure is being developed, the science code is being developed using 
!> a hand-rolled Psy layer, Psy-lite. A PsyKAl-lite needs a dynamo!
!> Eventually, PsyKAlite is replaced with the real Psy and Dynamo becomes Gung Ho.

!> @brief Main program used to illustrate dynamo functionality.

!> @details Creates the function spaces and alls the set_up to populate them (
!> either read or compute) then individual calls to the psy-layer with kernels
!> as if the code has been pre-processed by Psyclone.
!> Comments starting with !PSY are what the code would lool like before Psyclone
!> generated anything.



program dynamo

  use lfric
  use log_mod,                 only: log_event, log_scratch_space, &
                                     LOG_LEVEL_INFO
!PSY use v3_kernel_mod,        only: v3_kernel_type
!PSY use v3_solver_kernel_mod, only: v3_solver_kernel_type
  use psy,                     only: invoke_rhs_v3, invoke_v3_solver_kernel
  use set_up_mod,              only: set_up

  implicit none

  type(function_space_type)      :: v3_function_space, v2_function_space, & 
                                    v1_function_space, v0_function_space
  type(field_type)               :: pressure_density,rhs
  type(gaussian_quadrature_type) :: gq

  integer        :: cell
  integer        :: num_cells,num_dofs,num_unique_dofs,num_layers

  call log_event( 'Dynamo running...', LOG_LEVEL_INFO )

  call set_up(v0_function_space,v1_function_space,v2_function_space,      &
  v3_function_space, num_layers)

  gq = gaussian_quadrature_type()

  pressure_density = field_type(vector_space = v3_function_space,         &
       gq = gq,                                                           &
       num_layers = num_layers)

  rhs = field_type(vector_space = v3_function_space,                      &
       gq = gq,                                                           &
       num_layers = num_layers)

  !Construct PSy layer given a list of kernels. This is the line the code
  !generator may parse and do its stuff.

  call log_event( "Dynamo: calling 1st kernel", LOG_LEVEL_INFO )
  !PSY call invoke (v3_kernel_type(rhs) )
  call invoke_rhs_v3(rhs)

  call log_event( "Dynamo:calling 2nd kernel", LOG_LEVEL_INFO )
  !PSY call invoke (v3_solver_kernel_type(pressure_density,rhs) )
  call invoke_v3_solver_kernel(pressure_density,rhs)

  call print_field( 'RHS field...', rhs )
  call print_field( 'LHS field...', pressure_density )

  call log_event( 'Dynamo completed', LOG_LEVEL_INFO )

end program dynamo

!> Send a field to the log.
!>
subroutine print_field( title, field )

  use lfric
  use log_mod, only : log_event, log_scratch_space, LOG_LEVEL_INFO

  implicit none

  character( * ),     intent( in ) :: title
  type( field_type ), intent( in ) :: field

  integer                   :: cell
  integer                   :: layer
  integer                   :: df
  integer,          pointer :: map( : )

  call log_event( title, LOG_LEVEL_INFO )

  do cell=1,field%vspace%get_ncell()
    call field%vspace%get_cell_dofmap(cell,map)
    do df=1,field%vspace%get_ndf()
      do layer=0,field%get_nlayers()-1
        write( log_scratch_space, '( I4, I4, I4, F8.2 )' ) &
             cell, df, layer+1, field%data( map( df ) + layer )
        call log_event( log_scratch_space, LOG_LEVEL_INFO )
      end do
    end do
  end do

end subroutine print_field
