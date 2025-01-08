MODULE mm_types
 
 USE kinds,    ONLY: dp, default_string_length
 
 IMPLICIT NONE
 
 PRIVATE

 PUBLIC :: input_type, matrix_structure_type, para_env_type

!***************************************************************************
 TYPE input_type
   LOGICAL                              :: have_matrix_file, have_dimension ,&
                                           have_block 
   LOGICAL                              :: debug, output 
   CHARACTER(LEN=default_string_length) :: matrix_file
   CHARACTER(LEN=default_string_length) :: mdimension, mblock
 END TYPE input_type
!***************************************************************************
 TYPE matrix_structure_type
   INTEGER                  :: rowA, colA 
   INTEGER                  :: rowB, colB
   INTEGER                  :: rowA_local, colA_local
   INTEGER                  :: nblock      !block size of matrix
   INTEGER                  :: nprow, npcol   ! proc grid
   INTEGER                  :: myprow, mypcol  !proc ids
   INTEGER, DIMENSION(2)    :: first_p_pos 
   INTEGER                  :: icontxt     !for blacs context
   INTEGER, DIMENSION(9)    :: descriptor  !for scalapack
 END TYPE matrix_structure_type

!***************************************************************************
 TYPE para_env_type
   LOGICAL                  :: owns_group
   INTEGER                  :: mepos, num_proc, source
 END TYPE para_env_type

END MODULE mm_types
