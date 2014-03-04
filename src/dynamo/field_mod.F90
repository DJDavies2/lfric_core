!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Provides access to the members of the fields class.

!> @details Accessor functions for the fields class are defined in this module.

!> @param get_num_cell   Returns the number of cells in the field
!> @param get_nlayers    Returns the number of layers in the field

module field_mod
use function_space_mod, only: function_space_type
use constants_mod, only: dp
implicit none
private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public :: field_type
  private
  integer :: nlayers, undf
  type(function_space_type), pointer, public :: vspace
  real(kind=dp), allocatable, public :: data(:)
contains
  procedure :: get_ncell
  procedure :: get_nlayers
end type field_type

!overload the default structure constructor for field
  interface field_type
     module procedure field_constructor
  end interface

  public get_ncell
  public get_nlayers
contains
!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

  type(field_type) function field_constructor( &
       vector_space,                                   &
       num_layers)                                     &
       result(self)
    ! constructor
    type(function_space_type), target, intent(in) :: vector_space
    integer, intent(in) :: num_layers
    
    self%vspace => vector_space
    self%nlayers = num_layers
    self%undf = (num_layers ) * self%vspace%get_undf()

    ! allocate the array in memory
    allocate(self%data(self%undf))    
    
  end function field_constructor

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
  integer function get_ncell(self)
    class(field_type) :: self
    get_ncell=self%vspace%get_ncell()
    return
  end function get_ncell

  integer function get_nlayers(self)
    class(field_type) :: self
    get_nlayers=self%nlayers
    return
  end function get_nlayers

end module field_mod

