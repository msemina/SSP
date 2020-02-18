C
C     ..................................................................
C
C        SUBROUTINE PQSD
C
C        PURPOSE
C           PERFORM QUADRATIC SYNTHETIC DIVISION
C
C        USAGE
C           CALL PQSD(A,B,P,Q,X,IDIMX)
C
C        DESCRIPTION OF PARAMETERS
C           A     - COEFFICIENT OF Z IN REMAINDER (CALCULATED)
C           B     - CONSTANT TERM IN REMAINDER (CALCULATED)
C           P     - COEFFICIENT OF Z IN QUADRATIC POLYNOMIAL
C           Q     - CONSTANT TERM IN QUADRATIC POLYNOMIAL
C           X     - COEFFICIENT VECTOR FOR GIVEN POLYNOMIAL, ORDERED
C                   FROM SMALLEST TO LARGEST POWER
C           IDIMX - DIMENSION OF X
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           POLYNOMIAL IS DIVIDED BY THE QUADRATIC Z**2-P*Z-Q GIVING
C           THE LINEAR REMAINDER A*Z+B
C
C     ..................................................................
C
      SUBROUTINE PQSD(A,B,P,Q,X,IDIMX)
      DIMENSION X(1)
C
      A=0.
      B=0.
      J=IDIMX
    1 IF(J)3,3,2
    2 Z=P*A+B
      B=Q*A+X(J)
      A=Z
      J=J-1
      GO TO 1
    3 RETURN
      END
