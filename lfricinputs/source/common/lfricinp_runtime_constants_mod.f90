! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file LICENCE
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE lfricinp_runtime_constants_mod
  use constants_mod,                     only: i_def, r_def
  use field_mod,                         only: field_type
  use runtime_tools_mod,                 only: primary_mesh_label,      &
                                               twod_mesh_label
  use mesh_mod,                          only: mesh_type
IMPLICIT NONE
PRIVATE
PUBLIC :: lfricinp_create_runtime_constants

CONTAINS

SUBROUTINE lfricinp_create_runtime_constants(mesh, twod_mesh, &
                                      chi,                   &
                                      panel_id, dt)
! Description:
  ! This routine contains a subset of the contents of create_runtime_constants
  ! as required by LFRic Inputs. Ideally we would use the centrally provided
  ! function, but this brings in infrastructure that is causing errors that need
  ! time to debug.

    use geometric_constants_mod,     only: create_geometric_constants
    use runtime_tools_mod,           only: init_mesh_id_list

    implicit none

    type(mesh_type),       intent(in),  pointer :: mesh
    type(mesh_type),       intent(in),  pointer :: twod_mesh
    type(field_type),      target,   intent(in) :: chi(:)
    type(field_type),      target,   intent(in) :: panel_id
    real(r_def),                     intent(in) :: dt

    ! Internal variables
    integer(kind=i_def)                         :: num_meshes, mesh_counter, j
    integer(kind=i_def),            allocatable :: mesh_id_list(:)
    integer(kind=i_def),            allocatable :: label_list(:)
    type(field_type),               allocatable :: chi_list(:,:)
    type(field_type),               allocatable :: panel_id_list(:)

    !==========================================================================!
    ! Turn all the mesh IDs and coordinate fields into lists
    !==========================================================================!
    num_meshes = 2_i_def ! We should always have primary mesh_id and twod_mesh_id

    allocate(mesh_id_list(num_meshes))
    allocate(chi_list(3,num_meshes))
    allocate(panel_id_list(num_meshes))
    allocate(label_list(num_meshes))

    ! Populate these lists
    mesh_counter = 1_i_def
    label_list(mesh_counter) = primary_mesh_label
    mesh_id_list(mesh_counter) = mesh%get_id()
    call panel_id%copy_field(panel_id_list(mesh_counter))
    do j = 1, 3
      call chi(j)%copy_field(chi_list(j, mesh_counter))
    end do

    ! Primary 2D mesh
    mesh_counter = mesh_counter + 1_i_def
    label_list(mesh_counter) = twod_mesh_label
    mesh_id_list(mesh_counter) = twod_mesh%get_id()
    call panel_id%copy_field(panel_id_list(mesh_counter))
    do j = 1, 3
      call chi(j)%copy_field(chi_list(j, mesh_counter))
    end do


    !==========================================================================!
    ! Set up runtime_constants for each category
    !==========================================================================!

    call init_mesh_id_list(mesh_id_list)

    call create_geometric_constants(mesh_id_list,      &
                                    chi_list,          &
                                    panel_id_list,     &
                                    label_list         )

    deallocate(mesh_id_list)
    deallocate(label_list)

END SUBROUTINE lfricinp_create_runtime_constants

END MODULE lfricinp_runtime_constants_mod