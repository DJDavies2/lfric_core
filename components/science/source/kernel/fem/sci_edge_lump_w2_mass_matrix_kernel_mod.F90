!-----------------------------------------------------------------------------
! (C) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!------------------------------------------------------------------------------
!> @brief Apply a mass lumping to cells on the edge of the domain
!> @details To avoid propagating errors in from the edge of the domain
!!          the W2 mass matrix is lumped along the edge (where there are
!!          no neighbour cells in at least one direction).
!!          This means that with each application of the mass
!!          matrix (such as in a solver) the values on the edge do not
!!          get mapped to any other points.
module sci_edge_lump_w2_mass_matrix_kernel_mod

  use argument_mod,              only : arg_type, func_type,   &
                                        GH_FIELD, GH_OPERATOR, &
                                        GH_REAL,               &
                                        GH_READ, GH_READWRITE, &
                                        STENCIL, CROSS2D,      &
                                        CELL_COLUMN
  use constants_mod,             only : r_def, i_def
  use fs_continuity_mod,         only : W2
  use kernel_mod,                only : kernel_type
  implicit none

  private

  !-------------------------------------------------------------------------
  ! Public types
  !-------------------------------------------------------------------------

  type, public, extends(kernel_type) :: edge_lump_w2_mass_matrix_kernel_type
    private
    type(arg_type) :: meta_args(2) = (/                                     &
         arg_type(GH_OPERATOR, GH_REAL, GH_READWRITE, W2, W2),              &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,      W2, STENCIL(CROSS2D)) &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: edge_lump_w2_mass_matrix_code
  end type

  !-------------------------------------------------------------------------
  ! Contained functions/subroutines
  !-------------------------------------------------------------------------
  public :: edge_lump_w2_mass_matrix_code

contains

!> @param[in]     cell         Horizontal cell index
!> @param[in]     nlayers      Number of layers
!> @param[in]     ncell        Number of cells in the 3d domain
!> @param[in,out] mm_w2        W2 mass matrix
!> @param[in]     dummy_field  Unused field, only required in order to get a
!!                             stencil
!> @param[in]     smap_sizes   Stencil sizes
!> @param[in]     max_length   Maximum stencil branch length
!> @param[in]     smap         Stencil dofmap
!> @param[in]     ndf_w2       Number of dofs per cell for the W2 space
!> @param[in]     undf_w2      Global number of dofs for the W2 space
!> @param[in]     map_w2       Indirection map for this cell in the W2 space
subroutine edge_lump_w2_mass_matrix_code( cell,         &
                                          nlayers,      &
                                          ncell,        &
                                          mm_w2,        &
                                          dummy_field,  &
                                          smap_sizes,   &
                                          max_length,   &
                                          smap,         &
                                          ndf_w2,       &
                                          undf_w2,      &
                                          map_w2 )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers,  &
                                     cell,     &
                                     ncell,    &
                                     ndf_w2,   &
                                     undf_w2,  &
                                     max_length

  integer(kind=i_def), dimension(4),                     intent(in) :: smap_sizes
  integer(kind=i_def), dimension(ndf_w2, max_length, 4), intent(in) :: smap
  integer(kind=i_def), dimension(ndf_w2),                intent(in) :: map_w2

  real(kind=r_def), dimension(undf_w2),             intent(in)    :: dummy_field
  real(kind=r_def), dimension(ncell,ndf_w2,ndf_w2), intent(inout) :: mm_w2

  ! Internal variables
  integer(kind=i_def) :: k, ik, df
  real(kind=r_def)    :: row_sum

  ! If any smap_sizes == 1 then there are no neighbours
  ! in the direction and this can then be used as a proxy
  ! for being on the edge of the domain
  if ( minval(smap_sizes) == 1 ) then
    do k = 1, nlayers
      ik = (cell - 1) * nlayers + k
      do df = 1, ndf_w2
        row_sum = sum( mm_w2(ik, df, :) )
        mm_w2(ik, df, :) = 0.0_r_def
        mm_w2(ik, df, df) = row_sum
      end do
    end do
  end if

end subroutine edge_lump_w2_mass_matrix_code

end module sci_edge_lump_w2_mass_matrix_kernel_mod
