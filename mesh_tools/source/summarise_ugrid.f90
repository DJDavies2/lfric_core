!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
! Load a UGRID format file and print some summary information.
!
! Usage:
!     summarise_ugrid <filename>
!
!     filename - UGRID file
!
program summarise_ugrid

  use cli_mod,         only : get_initial_filename
  use constants_mod,   only : i_def, str_def
  use iso_fortran_env, only : output_unit
  use ncdf_quad_mod,   only : ncdf_quad_type
  use ugrid_2d_mod,    only : ugrid_2d_type
  use ugrid_file_mod,  only : ugrid_file_type
  use log_mod,         only : log_event, log_scratch_space, &
                              LOG_LEVEL_ERROR, LOG_LEVEL_INFO


  use ESMF

  implicit none

  character(:), allocatable :: filename

  class(ugrid_file_type), allocatable :: ugrid_file
  type(ugrid_2d_type)                 :: infile

  integer(i_def) :: n_meshes, i
  character(str_def), allocatable :: mesh_names(:)

  integer(i_def) :: nodes, edges, faces
  integer(i_def) :: nodes_per_face, edges_per_face
  integer(i_def) :: nodes_per_edge, max_faces_per_node

  type(ESMF_VM)  :: vm
  integer        :: rc

  CALL ESMF_Initialize(vm=vm, defaultlogfilename="summarise.Log", &
                  logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to initialise ESMF.', &
                                          LOG_LEVEL_ERROR )



  call get_initial_filename( filename, 'UGRID mesh file' )

  allocate(ncdf_quad_type::ugrid_file)

  call infile%set_file_handler(ugrid_file)

  call infile%get_nmeshes( trim(filename), n_meshes )

  allocate( mesh_names(n_meshes) )
  call infile%get_mesh_names( trim(filename), mesh_names )

  call log_event(                                                         &
      '================================================================', &
      LOG_LEVEL_INFO )
  write (log_scratch_space,'(A)') &
      'File ('// trim(adjustl(filename))// ') contains ugrid mesh(es):'
  call log_event( trim(log_scratch_space), LOG_LEVEL_INFO )
  call log_event(                                                         &
      '================================================================', &
      LOG_LEVEL_INFO )

  do i=1, n_meshes

    call infile%set_from_file_read( trim(mesh_names(i)), &
                                    trim(adjustl(filename)) )

    call infile%get_dimensions( nodes, edges, faces, nodes_per_face, &
                                edges_per_face, nodes_per_edge,      &
                                max_faces_per_node )

    write (log_scratch_space, '(A)') &
      trim(mesh_names(i))//', with dimensions:'
    call log_event( trim(log_scratch_space), LOG_LEVEL_INFO )

    write ( log_scratch_space, '(A,19X,I0)' ) '  Nodes: ', nodes
    call log_event( trim(log_scratch_space), LOG_LEVEL_INFO )

    write ( log_scratch_space, '(A,19X,I0)' ) '  Edges: ', edges
    call log_event( trim(log_scratch_space), LOG_LEVEL_INFO )

    write ( log_scratch_space, '(A,19X,I0)' ) '  Faces: ', faces
    call log_event( trim(log_scratch_space), LOG_LEVEL_INFO )

    write ( log_scratch_space, '(A,10X,I0)' ) &
        '  Nodes per face: ', nodes_per_face
    call log_event( trim(log_scratch_space), LOG_LEVEL_INFO )

    write ( log_scratch_space, '(A,10X,I0)' ) &
        '  Edges per face: ', edges_per_face
    call log_event( trim(log_scratch_space), LOG_LEVEL_INFO )

    write ( log_scratch_space, '(A,10X,I0)' ) &
        '  Nodes per edge: ', nodes_per_edge
    call log_event( trim(log_scratch_space), LOG_LEVEL_INFO )

    write ( log_scratch_space, '(A,2X,I0)' ) &
        '  Maximum facese per node: ', max_faces_per_node
    call log_event( trim(log_scratch_space), LOG_LEVEL_INFO )

  end do

  deallocate( mesh_names )


end program summarise_ugrid
