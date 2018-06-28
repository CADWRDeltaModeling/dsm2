        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 25 16:27:00 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RES_RATE__genmod
          INTERFACE 
            SUBROUTINE RES_RATE(RES,DIRECTION,GROUP_NDX,OBJFLOW,MASSRATE&
     &)
              INTEGER(KIND=4) :: RES
              INTEGER(KIND=4) :: DIRECTION
              INTEGER(KIND=4) :: GROUP_NDX
              REAL(KIND=8) :: OBJFLOW
              REAL(KIND=8) :: MASSRATE(24)
            END SUBROUTINE RES_RATE
          END INTERFACE 
        END MODULE RES_RATE__genmod
