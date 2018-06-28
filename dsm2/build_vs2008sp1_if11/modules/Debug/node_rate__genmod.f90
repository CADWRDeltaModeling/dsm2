        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 25 16:27:08 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NODE_RATE__genmod
          INTERFACE 
            SUBROUTINE NODE_RATE(INTNODE,DIRECTION,GROUP_NDX,OBJFLOW,   &
     &MASSRATE)
              INTEGER(KIND=4) :: INTNODE
              INTEGER(KIND=4) :: DIRECTION
              INTEGER(KIND=4) :: GROUP_NDX
              REAL(KIND=8) :: OBJFLOW
              REAL(KIND=8) :: MASSRATE(24)
            END SUBROUTINE NODE_RATE
          END INTERFACE 
        END MODULE NODE_RATE__genmod
