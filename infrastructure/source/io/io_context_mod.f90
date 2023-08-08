!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Specifies the interface for all I/O context classes.
!>
module io_context_mod

  use clock_mod,       only : clock_type
  use model_clock_mod, only : model_clock_type
  use constants_mod,   only : i_native, r_second
  use event_mod,       only : event_actor_type
  use field_mod,       only : field_type
  use linked_list_mod, only : linked_list_type

  implicit none

  private

  !> @brief All context classes inherit this interface.
  !>
  type, public, abstract, extends(event_actor_type) :: io_context_type
    private
  contains
    private
    procedure(get_filelist_if),    public, deferred :: get_filelist
    procedure(set_current_if) , public, deferred :: set_current
  end type io_context_type

  abstract interface
    !> Gets the list of files associated with this context.
    !>
    !> @return Linked list of file objects.
    !>
    function get_filelist_if( this ) result(filelist)
      import linked_list_type, io_context_type
      implicit none
      class(io_context_type), intent(in), target :: this
      type(linked_list_type), pointer :: filelist
    end function get_filelist_if
  end interface

  abstract interface
    !> Sets the context as current
    !>
    subroutine set_current_if( this )
      import io_context_type
      implicit none
      class(io_context_type), intent(inout) :: this
    end subroutine set_current_if
  end interface

  abstract interface
    !> @brief Callback interface for bespoke IO configuration
    !> @param[in] clock     Clock to be passed in at call site
    subroutine callback_clock_arg(clock)
      use clock_mod, only : clock_type
      implicit none
      class(clock_type), intent(in) :: clock
    end subroutine callback_clock_arg
  end interface

  public :: callback_clock_arg

contains

end module io_context_mod
