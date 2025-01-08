!**********************************************************************************
!< brief multiplies two matrices, i.e. AB=C. Note that A and B are square matrices. 
!**********************************************************************************
program parallel_matrix_multiply

  USE read_input,       ONLY: parse_command_line,&
                              read_or_generate_matrix
  USE kinds,            ONLY: dp
  USE mm_types,         ONLY: input_type,&
                              matrix_structure_type,&
                              para_env_type
  USE setup_and_output, ONLY: init_mpi, init_blacs,&
                              init_local_matrices,&
                              copy_to_global,&
                              test_result_dgemm 

  IMPLICIT NONE

  include 'mpif.h'

  INTEGER                                     :: ierror
  INTEGER, DIMENSION(9)                       :: descA, descB, descC  !scalapack descriptors
  REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE  :: matrix_A, matrix_B,&
                                                 matrix_C 
  REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE  :: matrix_A_local,&
                                                 matrix_B_local,&
                                                 matrix_C_local
  TYPE(input_type)                            :: input
  TYPE(matrix_structure_type)                 :: structure
  TYPE(para_env_type)                         :: para_env


  CALL parse_command_line(input)
  CALL init_mpi(para_env,structure)
  CALL read_or_generate_matrix(input,structure,matrix_A,matrix_B)
  CALL init_blacs(para_env,structure)

  ALLOCATE(matrix_C(structure%rowA,structure%colB))
  matrix_C = 0.0_dp
  CALL init_local_matrices(para_env,structure,matrix_A, matrix_B,&
                           matrix_A_local,matrix_B_local,matrix_C_local)
 
  descA(:) = structure%descriptor 
  descB(:) = structure%descriptor 
  descC(:) = structure%descriptor

  CALL pdgemm("N","N",structure%rowA,structure%colB,structure%colA,&
              1.0_dp,matrix_A_local,1,1,descA,matrix_B_local,1,1,descB,&
              0.0_dp,matrix_C_local,1,1,descC ) 

  CALL copy_to_global(para_env,structure,matrix_C,matrix_C_local,output=input%output)

  IF(input%debug) THEN
    CALL test_result_dgemm(para_env,structure,matrix_A,matrix_B,matrix_C)    
  ENDIF

  DEALLOCATE(matrix_A_local, matrix_B_local, matrix_C_local) 
  DEALLOCATE(matrix_A, matrix_B, matrix_C) 
  CALL blacs_gridexit(structure%icontxt)
  CALL MPI_Finalize(ierror)
  !CALL blacs_exit(0)

end program parallel_matrix_multiply
