!-------------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------
!> @brief Field formatting for XIOS

module lfric_xios_format_mod

  use constants_mod,                  only: i_def
  use lfric_xios_constants_mod,       only: dp_xios, xios_max_int
  use field_parent_mod,               only: field_parent_proxy_type
  use field_r32_mod,                  only: field_r32_proxy_type
  use field_r64_mod,                  only: field_r64_proxy_type
  use integer_field_mod,              only: integer_field_proxy_type
  use log_mod,                        only:                                   &
                                        log_event,                            &
                                        log_level_warning,                    &
                                        log_level_error

  implicit none

  public :: format_field

  private
contains

!> @brief Formats field data array in accordance with XIOS conventions.
!> @details The field data form a matrix stored as a 1D array, which is
!> transformed from column-major ("contiguous columns") to row-major form
!> ("contiguous rows").
!> Note that the contiguous columns of the input matrix are
!> vertical slices (ordered by level).
!> The row-major form of a matrix is the column-major for of its transposed.
!> Therefore, the formatting amounts to a transposition of the matrix.
!> @pre The total dimension must be a multiple of the number of rows.
!> @param[out] xios_data         Data array to be sent to XIOS
!> @param[in]  field_name        Field name for error reporting
!> @param[in]  fpxy              Field proxy of LFRic field to be formatted
!> @param[in]  m                 Number of rows in input matrix
!> @param[in]  n                 Number of columns in input matrix

subroutine format_field(xios_data, field_name, fpxy, m, n)
  implicit none

  real(dp_xios),                  intent(in out):: xios_data(:)
  character(len=*),               intent(in)    :: field_name
  class(field_parent_proxy_type), intent(in)    :: fpxy
  integer(i_def),                 intent(in)    :: m
  integer(i_def),                 intent(in)    :: n

  integer(i_def) :: mn
  integer(i_def) :: i

  mn = m*n

  ! sanity check
  if (mn /= size(xios_data)) then
    call log_event('assertion failed for field ' // field_name                &
      // ': mn == size(xios_data)',  log_level_error)
  end if

  select type(fpxy)
    type is (field_r32_proxy_type)
      ! this elegant call can be up to 4 times slower than the loop below:
      ! xios_data = reshape(transpose(reshape(fpxy%data(:), (/m, n/))), (/mn/))
      do i = 1, m
        xios_data(1+(i-1)*n:i*n) = fpxy%data(i:mn:m)
      end do

    type is (field_r64_proxy_type)
      ! xios_data = reshape(transpose(reshape(fpxy%data(:), (/m, n/))), (/mn/))
      do i = 1, m
        xios_data(1+(i-1)*n:i*n) = fpxy%data(i:mn:m)
      end do

    type is (integer_field_proxy_type)
      if ( any( abs(fpxy%data(:)) > xios_max_int) ) then
        call log_event( 'data for integer field ' //                          &
          trim(adjustl(field_name)) //                                        &
          ' contains values too large for 16-bit precision', log_level_warning)
      end if
      ! xios_data = reshape(transpose(reshape(fpxy%data(:), (/m, n/))), (/mn/))
      do i = 1, m
        xios_data(1+(i-1)*n:i*n) = fpxy%data(i:mn:m)
      end do

    class default
      call log_event( "invalid type for input field proxy", log_level_error )
  end select

end subroutine format_field

end module lfric_xios_format_mod
