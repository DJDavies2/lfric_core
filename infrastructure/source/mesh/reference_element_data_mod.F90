!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Topology of a unit reference element for data output layout
! includes ordering of topological entities and lookups for dofs
! in order to get the correct layout of field data for output
! Currently only includes a cube
!-------------------------------------------------------------------------------

module reference_element_data_mod

 use constants_mod,          only : i_def, r_def, IMDI
 use reference_element_mod,  only : W, S, E, N, B, T, &
                                    SWB, SEB, NEB, NWB, &
                                    SWT, SET, NET, NWT, &
                                    WB, SB, EB, NB, SW, &
                                    SE, NE, NW, WT, ST, &
                                    ET, NT, &
                                    nverts, nfaces, nedges

implicit none

! Vertex coodinates of the unit ref element
real(kind=r_def), allocatable :: vert_coords( :,: )
! Edge coodinates of the unit ref element
real(kind=r_def), allocatable :: edge_coords( :,: )
! Face coodinates of the unit ref element
real(kind=r_def), allocatable :: face_coords( :,: )
! Volume coodinates of the unit ref element
real(kind=r_def), allocatable :: vol_coords( :,: )

! Geometric information about the reference element
integer(i_def) :: nvols


! Select entities in the function space
type select_data_entity_type
  integer(i_def), allocatable :: vols(:)
  integer(i_def), allocatable :: faces(:)
  integer(i_def), allocatable :: edges(:)
  integer(i_def), allocatable :: verts(:)
end type select_data_entity_type

type(select_data_entity_type), target ::       &
  select_data_entity_all, select_data_entity_theta, &
  select_data_entity_w2v, select_data_entity_w2h


!> Describes the centre of the cell (volume)
integer(i_def), parameter :: V=1


!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains 

subroutine reference_cube_data()
  !-----------------------------------------------------------------------------
  ! Subroutine that defines topology of a reference unit cube 
  !-----------------------------------------------------------------------------
  implicit none
  
  ! 2D cell information

  nvols = 1

  ! Allocate arrays for coords
  allocate ( vert_coords(nverts,3) )
  allocate ( edge_coords(nedges,3) )
  allocate ( face_coords(nfaces,3) )
  allocate ( vol_coords(nvols,3) )

  ! Allocate arrays for derived types
  ! all entities
  allocate ( select_data_entity_all % vols(nvols) ) 
  allocate ( select_data_entity_all % faces(nfaces) )
  allocate ( select_data_entity_all % edges(nedges) )
  allocate ( select_data_entity_all % verts(nverts) )
  ! theta - a subset of entities
  allocate ( select_data_entity_theta % vols(nvols) ) 
  allocate ( select_data_entity_theta % faces(nfaces) )
  allocate ( select_data_entity_theta % edges(nedges) )
  allocate ( select_data_entity_theta % verts(nverts) )
  ! w2v - a subset of entities
  allocate ( select_data_entity_w2v % vols(nvols) ) 
  allocate ( select_data_entity_w2v % faces(nfaces) )
  allocate ( select_data_entity_w2v % edges(nedges) )
  allocate ( select_data_entity_w2v % verts(nverts) )
  ! w2h - a subset of entities
  allocate ( select_data_entity_w2h % vols(nvols) ) 
  allocate ( select_data_entity_w2h % faces(nfaces) )
  allocate ( select_data_entity_w2h % edges(nedges) )
  allocate ( select_data_entity_w2h % verts(nverts) )

  ! Define vertex coordinates in unit reference space
  vert_coords(SWB,:) = (/ 0.0_r_def, 0.0_r_def, 0.0_r_def /)
  vert_coords(SEB,:) = (/ 1.0_r_def, 0.0_r_def, 0.0_r_def /)
  vert_coords(NEB,:) = (/ 1.0_r_def, 1.0_r_def, 0.0_r_def /)
  vert_coords(NWB,:) = (/ 0.0_r_def, 1.0_r_def, 0.0_r_def /)

  vert_coords(SWT,:) = (/ 0.0_r_def, 0.0_r_def, 1.0_r_def /)
  vert_coords(SET,:) = (/ 1.0_r_def, 0.0_r_def, 1.0_r_def /)
  vert_coords(NET,:) = (/ 1.0_r_def, 1.0_r_def, 1.0_r_def /)
  vert_coords(NWT,:) = (/ 0.0_r_def, 1.0_r_def, 1.0_r_def /)

  ! Define edge coordinates in unit reference space
  edge_coords(WB,:) = (/ 0.0_r_def, 0.5_r_def, 0.0_r_def /)
  edge_coords(SB,:) = (/ 0.5_r_def, 0.0_r_def, 0.0_r_def /)
  edge_coords(EB,:) = (/ 1.0_r_def, 0.5_r_def, 0.0_r_def /)
  edge_coords(NB,:) = (/ 0.5_r_def, 1.0_r_def, 0.0_r_def /)

  edge_coords(SW,:) = (/ 0.0_r_def, 0.0_r_def, 0.5_r_def /)
  edge_coords(SE,:) = (/ 1.0_r_def, 0.0_r_def, 0.5_r_def /)
  edge_coords(NE,:) = (/ 1.0_r_def, 1.0_r_def, 0.5_r_def /)
  edge_coords(NW,:) = (/ 0.0_r_def, 1.0_r_def, 0.5_r_def /)

  edge_coords(WT,:) = (/ 0.0_r_def, 0.5_r_def, 1.0_r_def /)
  edge_coords(ST,:) = (/ 0.5_r_def, 0.0_r_def, 1.0_r_def /)
  edge_coords(ET,:) = (/ 1.0_r_def, 0.5_r_def, 1.0_r_def /)
  edge_coords(NT,:) = (/ 0.5_r_def, 1.0_r_def, 1.0_r_def /)

  ! Define face coordinates in unit reference space
  face_coords(W,:) = (/ 0.0_r_def, 0.5_r_def, 0.5_r_def /)
  face_coords(S,:) = (/ 0.5_r_def, 0.0_r_def, 0.5_r_def /)
  face_coords(E,:) = (/ 1.0_r_def, 0.5_r_def, 0.5_r_def /)
  face_coords(N,:) = (/ 0.5_r_def, 1.0_r_def, 0.5_r_def /)

  face_coords(B,:) = (/ 0.5_r_def, 0.5_r_def, 0.0_r_def /)
  face_coords(T,:) = (/ 0.5_r_def, 0.5_r_def, 1.0_r_def /)

  ! Define volume coordinates in unit reference space

  vol_coords(V,:) = (/ 0.5_r_def, 0.5_r_def, 0.5_r_def /)

  
  ! Entity select
  ! all entities
  select_data_entity_all % vols = (/ V /)   
  select_data_entity_all % faces = (/ B, W, S, E, N, T /)
  select_data_entity_all % edges = (/ WB, SB, EB, NB, SW, SE, NE, NW, WT, ST, ET, NT /)
  select_data_entity_all % verts = (/ SWB, SEB, NEB, NWB, SWT, SET, NET, NWT /)
  ! theta - a subset of entities
  select_data_entity_theta % vols = IMDI
  select_data_entity_theta % faces = (/ B, T, IMDI, IMDI, IMDI, IMDI /)
  select_data_entity_theta % edges = IMDI
  select_data_entity_theta % verts = IMDI
  ! w2v - a subset of entities
  select_data_entity_w2v % vols = IMDI
  select_data_entity_w2v % faces = (/ B, T, IMDI, IMDI, IMDI, IMDI /)
  select_data_entity_w2v % edges = IMDI
  select_data_entity_w2v % verts = IMDI
  ! w2h - a subset of entities
  select_data_entity_w2h % vols = IMDI
  select_data_entity_w2h % faces = (/ W, S, E, N, IMDI, IMDI/)
  select_data_entity_w2h % edges = IMDI
  select_data_entity_w2h % verts = IMDI

end subroutine reference_cube_data


subroutine deallocate_data_reference()

  ! deallocate data reference element data

  if(allocated( vert_coords ))deallocate ( vert_coords )
  if(allocated( edge_coords ))deallocate ( edge_coords )
  if(allocated( face_coords ))deallocate ( face_coords )
  if(allocated( vol_coords ))deallocate ( vol_coords )

  if(allocated( select_data_entity_all % vols ))deallocate ( select_data_entity_all % vols )
  if(allocated( select_data_entity_all % faces ))deallocate ( select_data_entity_all % faces )
  if(allocated( select_data_entity_all % edges ))deallocate ( select_data_entity_all % edges )
  if(allocated( select_data_entity_all % verts ))deallocate ( select_data_entity_all % verts )

  if(allocated( select_data_entity_theta % vols ))deallocate ( select_data_entity_theta % vols )
  if(allocated( select_data_entity_theta % faces ))deallocate ( select_data_entity_theta % faces )
  if(allocated( select_data_entity_theta % edges ))deallocate ( select_data_entity_theta % edges )
  if(allocated( select_data_entity_theta % verts ))deallocate ( select_data_entity_theta % verts )

  if(allocated( select_data_entity_w2v % vols ))deallocate ( select_data_entity_w2v % vols )
  if(allocated( select_data_entity_w2v % faces ))deallocate ( select_data_entity_w2v % faces )
  if(allocated( select_data_entity_w2v % edges ))deallocate ( select_data_entity_w2v % edges )
  if(allocated( select_data_entity_w2v % verts ))deallocate ( select_data_entity_w2v % verts )

  if(allocated( select_data_entity_w2h % vols ))deallocate ( select_data_entity_w2h % vols )
  if(allocated( select_data_entity_w2h % faces ))deallocate ( select_data_entity_w2h % faces )
  if(allocated( select_data_entity_w2h % edges ))deallocate ( select_data_entity_w2h % edges )
  if(allocated( select_data_entity_w2h % verts ))deallocate ( select_data_entity_w2h % verts )

end subroutine deallocate_data_reference

end module reference_element_data_mod
