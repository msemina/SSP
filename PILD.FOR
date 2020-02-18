C
C     ..................................................................
C
C        SUBROUTINE PILD
C
C        PURPOSE
C           EVALUATE POLYNOMIAL AND ITS FIRST DERIVATIVE FOR A GIVEN
C           ARGUMENT
C
C        USAGE
C           CALL PILD(POLY,DVAL,ARGUM,X,IDIMX)
C
C        DESCRIPTION OF PARAMETERS
C           POLY  - VALUE OF POLYNOMIAL
C           DVAL  - DERIVATIVE
C           ARGUM - ARGUMENT
C           X     - VECTOR OF COEFFICIENTS FOR POLYNOMIAL, ORDERED
C                   FROM SMALLEST TO LARGEST POWER
C           IDIMX - DIMENSION OF X
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           PQSD
C
C        METHOD
C           EVALUATION IS DONE BY MEANS OF SUBROUTINE PQSD (QUADRATIC
C           SYNTHETIC DIVISION)
C
C     ..................................................................
C
      SUBROUTINE PILD (POLY,DVAL,ARGUM,X,IDIMX)
      DIMENSION X(1)
C
      P=ARGUM+ARGUM
      Q=-ARGUM*ARGUM
C
      CALL PQSD (DVAL,POLY,P,Q,X,IDIMX)
C
      POLY=ARGUM*DVAL+POLY
C
      RETURN
      END
