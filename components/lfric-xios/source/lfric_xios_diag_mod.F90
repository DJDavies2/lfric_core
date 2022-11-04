!-------------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------
!> @brief XIOS wrapper for use in diagnostics
!> @details XIOS wrapper to be used in the initialisation of diagnostic fields

module lfric_xios_diag_mod

  use constants_mod,                  only: l_def, i_def, str_def
#ifdef UNIT_TEST
  use lfric_xios_mock_mod,            only:                                   &
                                        xios_is_valid_file,                   &
                                        xios_is_defined_file_attr,            &
                                        xios_get_file_attr,                   &
                                        xios_is_valid_field,                  &
                                        xios_is_defined_field_attr,           &
                                        xios_get_field_attr,                  &
                                        xios_get_axis_attr,                   &
                                        lfric_xios_mock_pull_in
#else
  use lfric_xios_mock_mod,            only: lfric_xios_mock_pull_in
  use xios,                           only:                                   &
                                        xios_is_valid_file,                   &
                                        xios_is_defined_file_attr,            &
                                        xios_get_file_attr,                   &
                                        xios_is_valid_field,                  &
                                        xios_is_defined_field_attr,           &
                                        xios_get_field_attr,                  &
                                        xios_get_axis_attr

#endif

  use log_mod,                         only:                                  &
                                        log_event,                            &
                                        log_level_error,                      &
                                        log_level_warning,                    &
                                        log_scratch_space

  implicit none

  private

  public ::                                                                    &
    file_is_enabled,                                                           &
    field_is_valid,                                                            &
    field_is_enabled,                                                          &
    get_field_order,                                                           &
    get_field_grid_ref,                                                        &
    get_field_domain_ref,                                                      &
    get_field_axis_ref,                                                        &
    get_axis_dimension

contains

  !> @brief Return true if an only if an XIOS output file is enabled.
  !> @param[in]    unique_id  XIOS id of the file
  !> @return                  Boolean representing enabled/disabled status
  function file_is_enabled(file_id) result(enabled)
    implicit none
    character(*), intent(in) :: file_id
    logical(l_def) :: has_enabled_flag
    logical(l_def) :: enabled
    if (.not. xios_is_valid_file(file_id)) then
      write(log_scratch_space, '(A, A)')                                      &
        'Invalid XIOS file:', file_id
        call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end if
    call xios_is_defined_file_attr(file_id, enabled=has_enabled_flag)
    if (has_enabled_flag) then
       call xios_get_file_attr(file_id, enabled=enabled)
       ! explicitly enabled/disabled
    else
       enabled = .true. ! enabled by default
    end if
  end function file_is_enabled

  !> @brief Return true if an only if an XIOS field id is valid.
  !> @param[in]    unique_id    XIOS id of the field
  !> @return                    Boolean representing validity
  function field_is_valid(unique_id) result(valid)
    implicit none
    character(*), intent(in) :: unique_id
    logical(l_def) :: valid
    valid = xios_is_valid_field(unique_id)
  end function field_is_valid

  !> @brief Return true if an only if an XIOS output field is enabled.
  !> @param[in]              unique_id    XIOS id of the field
  !> @return                    Boolean representing enabled/disabled status
  function field_is_enabled(unique_id) result(enabled)
    implicit none
    character(*), intent(in) :: unique_id
    logical(l_def) :: has_enabled_flag
    logical(l_def) :: enabled
    if (.not. xios_is_valid_field(unique_id)) then
      write(log_scratch_space, '(A, A)')                                      &
      'Invalid XIOS field:', unique_id
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    else
      call xios_is_defined_field_attr(unique_id, enabled=has_enabled_flag)
      if (has_enabled_flag) then
        call xios_get_field_attr(unique_id, enabled=enabled)
        ! explicitly enabled/disabled
      else
        enabled = .true. ! enabled by default
      end if
    end if
  end function field_is_enabled

  !> @brief Return the interpolation order of a field.
  !> @param[in]    unique_id    XIOS id of the field
  !> @return                    The interpolation order
  function get_field_order(unique_id) result(order)
   implicit none
   character(*), intent(in) :: unique_id
   integer(i_def) :: order
   order = 0 ! FOR NOW, to be extracted from the field comment?
  end function get_field_order

  !> @brief Return the XIOS grid reference of a field.
  !> @param[in]    unique_id    XIOS id of the field
  !> @return                    The grid reference
  function get_field_grid_ref(unique_id) result(grid_ref)
    implicit none
    character(*), intent(in) :: unique_id
    character(str_def) :: grid_ref
    logical(l_def) :: has_grid
    call xios_is_defined_field_attr(unique_id, grid_ref=has_grid)
    if (has_grid) then
       call xios_get_field_attr(unique_id, grid_ref=grid_ref)
    else
       grid_ref = ""
    end if
  end function get_field_grid_ref

  !> @brief Return the XIOS domain reference of a field.
  !> @param[in]    unique_id    XIOS id of the field
  !> @return                    The domain reference
  function get_field_domain_ref(unique_id) result(domain_ref)
    implicit none
    character(*), intent(in) :: unique_id
    character(str_def) :: domain_ref
    logical(l_def) :: has_domain
    call xios_is_defined_field_attr(unique_id, domain_ref=has_domain)
    if (has_domain) then
       call xios_get_field_attr(unique_id, domain_ref=domain_ref)
    else
       domain_ref = ""
    end if
  end function get_field_domain_ref

  !> @brief Return the XIOS axis reference of a field.
  !> @param[in]    unique_id    XIOS id of the field
  !> @return                    The axis reference
  function get_field_axis_ref(unique_id) result(axis_ref)
    implicit none
    character(*), intent(in) :: unique_id
    character(str_def) :: axis_ref
    logical(l_def) :: has_axis
    call xios_is_defined_field_attr(unique_id, axis_ref=has_axis)
    if (has_axis) then
       call xios_get_field_attr(unique_id, axis_ref=axis_ref)
    else
       axis_ref = ""
    end if
  end function get_field_axis_ref

  !> @brief Return the dimension of an XIOS axis object.
  !> @param[in]    unique_id    XIOS id of the axis
  !> @return                    The dimension
  function get_axis_dimension(unique_id) result(dim)
    implicit none
    character(*), intent(in) :: unique_id
    integer(i_def) :: dim
    call xios_get_axis_attr(unique_id, n_glo=dim)
  end function get_axis_dimension

end module lfric_xios_diag_mod
