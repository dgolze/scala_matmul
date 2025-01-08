MODULE setup_and_output

 USE kinds,     ONLY: dp
 USE mm_types,  ONLY: para_env_type,&
                      matrix_structure_type

 IMPLICIT NONE
 
 PRIVATE

 PUBLIC :: init_mpi, init_blacs, init_local_matrices,&
           copy_to_global, test_result_dgemm
 include 'mpif.h'

CONTAINS

! **************************************************************************************************
!> intialize mpi environment and processor grid
! **************************************************************************************************
 SUBROUTINE init_mpi(para_env,structure)
   
   TYPE(para_env_type), INTENT(INOUT)                  :: para_env
   TYPE(matrix_structure_type), INTENT(INOUT)          :: structure
   
   INTEGER                                             :: rank, size, &
                                                          nprow, ierror
   
   CALL MPI_Init(ierror)
   
   CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
   CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
   
   para_env%owns_group = .TRUE.
   para_env%mepos = rank
   para_env%num_proc = size 
   para_env%source = 0
   
   
   !*** get processor grid
   
   DO nprow = INT(SQRT(REAL(para_env%num_proc,dp)+0.5_dp)),1,-1
     IF(MOD(para_env%num_proc,nprow) == 0) EXIT
   ENDDO
   structure%nprow = nprow
   structure%npcol = para_env%num_proc / structure%nprow
   
   IF(para_env%mepos==para_env%source) write(*,'(2x,a,i6,a,i6)') &
     '| processor grid: ',structure%nprow,' X ', structure%npcol

   structure%first_p_pos = (/0,0/)
   
 END SUBROUTINE init_mpi

! **************************************************************************************************
!> brief initialize blacs context; get proc grid and maximal local structure
! **************************************************************************************************

 SUBROUTINE init_blacs(para_env,structure)

   TYPE(para_env_type), INTENT(INOUT)                  :: para_env
   TYPE(matrix_structure_type), INTENT(INOUT)          :: structure

   INTEGER                                             :: rowA_local, colA_local
   INTEGER                                             :: numroc
   ! Initialize blacs processor grid

     CALL blacs_pinfo(para_env%mepos,para_env%num_proc) !not necessary
     CALL blacs_get (0, 0, structure%icontxt)  !necessary?
     CALL blacs_gridinit(structure%icontxt, 'R', structure%nprow, structure%npcol)
     CALL blacs_gridinfo(structure%icontxt, structure%nprow, structure%npcol,&
          structure%myprow, structure%mypcol)

     rowA_local = numroc(structure%rowA, structure%nblock, structure%myprow, &
                         structure%first_p_pos(1), structure%nprow)
     colA_local = numroc(structure%colA, structure%nblock, structure%mypcol,&
                         structure%first_p_pos(2), structure%npcol)

     IF(para_env%mepos == para_env%source) THEN     
        WRITE(*,*)"Size of global array is ", structure%rowA,"x", structure%colA

        WRITE(*,*)"Size of block is        ", structure%nblock
     ENDIF

     WRITE(*,*)"Proc ",para_env%mepos,": myprow, mypcol in p-array is ",&
                structure%myprow, structure%mypcol, " , ", "Size of local array is  ",&
                rowA_local," x ", colA_local 
     structure%rowA_local = rowA_local 
     structure%colA_local = colA_local 

  END SUBROUTINE  init_blacs         

! **************************************************************************************************
!> brief copy content of global matrices to local matrices; set the scalapack descriptors
! **************************************************************************************************

  SUBROUTINE init_local_matrices(para_env,structure,matrix_A, matrix_B,matrix_A_local,&
                                 matrix_B_local,matrix_C_local)

     TYPE(para_env_type), INTENT(IN)               :: para_env
     TYPE(matrix_structure_type), INTENT(IN)       :: structure
     REAL(KIND=dp), DIMENSION(:,:), INTENT(IN)     :: matrix_A, matrix_B
     REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE, &
       INTENT(OUT)                                 :: matrix_A_local, matrix_B_local, matrix_C_local
   
     INTEGER, EXTERNAL                             :: indxl2g
     INTEGER                                       :: icol_local, irow_local, irow_global, icol_global
     INTEGER                                       :: stat 

     ALLOCATE(matrix_A_local(structure%rowA_local,structure%colA_local))
     ALLOCATE(matrix_B_local(structure%rowA_local,structure%colA_local))
     ALLOCATE(matrix_C_local(structure%rowA_local,structure%colA_local))

     DO icol_local=1,structure%colA_local
       icol_global=indxl2g(icol_local,structure%nblock,structure%mypcol,&
                           structure%first_p_pos(2),structure%npcol) 
       DO irow_local=1,structure%rowA_local
           irow_global=indxl2g(irow_local,structure%nblock,structure%myprow,&
                               structure%first_p_pos(1),structure%nprow)
           matrix_A_local(irow_local,icol_local)=matrix_A(irow_global,icol_global)
           matrix_B_local(irow_local,icol_local)=matrix_B(irow_global,icol_global)
           matrix_C_local(irow_local,icol_local)=0.0_dp
           !write(*,*) "irow, licol", para_env%mepos,structure%myprow, structure%mypcol,&
           !             irow_local, icol_local, irow_global, icol_global, &
           !             matrix_A_local(irow_local,icol_local)
       ENDDO
     ENDDO

     CALL descinit(structure%descriptor,structure%rowA, structure%colA,&
                   structure%nblock,structure%nblock,0,0,structure%icontxt,&
                   structure%rowA_local,stat)

  END SUBROUTINE init_local_matrices

! **************************************************************************************************
!> brief copy local matrix to big final global matrix
! **************************************************************************************************
  SUBROUTINE copy_to_global(para_env,structure,matrix_C,matrix_C_local,output)

     TYPE(para_env_type), INTENT(IN)               :: para_env
     TYPE(matrix_structure_type), INTENT(IN)       :: structure
     REAL(KIND=dp), DIMENSION(:,:), INTENT(INOUT)  :: matrix_C
     REAL(KIND=dp), DIMENSION(:,:), INTENT(IN)     :: matrix_C_local
     LOGICAL, INTENT(IN)                           :: output

     INTEGER, EXTERNAL                             :: indxl2g
     INTEGER                                       :: icol_local, irow_local, irow_global, icol_global
     INTEGER                                       :: scount, rcount, ierr
   
     DO icol_local=1,structure%colA_local
       icol_global=indxl2g(icol_local,structure%nblock,structure%mypcol,&
                   structure%first_p_pos(2), structure%npcol) 
       DO irow_local=1,structure%rowA_local
           irow_global=indxl2g(irow_local,structure%nblock,structure%myprow, &
                       structure%first_p_pos(1), structure%nprow)
           matrix_C(irow_global,icol_global)=matrix_C_local(irow_local,icol_local)
           !write(*,*) "irow, icol", para_env%mepos,structure%myprow, structure%mypcol,&
           !             irow_local, icol_local, irow_global, icol_global, &
           !             matrix_C_local(irow_local,icol_local)
       ENDDO
     ENDDO
    
     !point to point; isend,irecv would be more elegant here... 
     CALL mpi_allreduce(MPI_IN_PLACE,matrix_C,SIZE(matrix_C),&
              MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
     IF(output) THEN
        IF(para_env%mepos == para_env%source) THEN
          DO irow_global=1,structure%rowA
            DO icol_global=1,structure%colA
               write(*,*) "irow,jcol,matrix_C", irow_global,icol_global,&
                           matrix_C(irow_global,icol_global)
            ENDDO
          ENDDO
        ENDIF
     ENDIF

  END SUBROUTINE copy_to_global

! **************************************************************************************************
!> brief compare matrix computed with pdgemm to dgemm result
! **************************************************************************************************
  SUBROUTINE test_result_dgemm(para_env,structure,matrix_A,matrix_B,matrix_C)

     TYPE(para_env_type), INTENT(IN)               :: para_env
     TYPE(matrix_structure_type), INTENT(IN)       :: structure
     REAL(KIND=dp), DIMENSION(:,:), INTENT(IN)     :: matrix_A, matrix_B, matrix_C

     INTEGER                                       :: rowA, colA, rowB, colB,&
                                                      irow, icol, counter
     REAL(KIND=dp)                                 :: diff
     REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE    :: temp_C

     rowA=structure%rowA
     colA=structure%colA
     rowB=structure%rowB
     colB=structure%rowA

     ALLOCATE(temp_C(rowA,colB))
     temp_C = 0.0_dp
     CALL dgemm("N","N",rowA,colB,colA,1.0_dp,matrix_A,rowA,matrix_B,rowB,0.0_dp,temp_C,rowA)     

     IF(para_env%mepos == para_env%source) THEN
       counter = 0
       DO icol=1,colB
          DO irow=1,rowA
             diff=ABS(matrix_C(irow,icol)-temp_C(irow,icol))
             IF(diff > 1.E-13_dp) THEN
               write(*,*) "i,j,matrix_C,temp_C", irow,icol,matrix_C(irow,icol), temp_C(irow,icol)
               counter = counter + 1
             ENDIF
          ENDDO
       ENDDO
       write(*,'(A30,I10)') "Number of wrong matrix elements", counter
     ENDIF
     
     DEALLOCATE(temp_C)
  END SUBROUTINE test_result_dgemm
END MODULE setup_and_output                  
