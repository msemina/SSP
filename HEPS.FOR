C
C     ..................................................................
C
C        SUBROUTINE HEPS
C
C        PURPOSE
C           COMPUTES THE VALUE OF AN N-TERM EXPANSION IN HERMITE
C           POLYNOMIALS WITH COEFFICIENT VECTOR C FOR ARGUMENT VALUE X.
C
C        USAGE
C           CALL HEPS(Y,X,C,N)
C
C        DESCRIPTION OF PARAMETERS
C           Y     - RESULT VALUE
C           X     - ARGUMENT VALUE
C           C     - COEFFICIENT VECTOR OF GIVEN EXPANSION
C                   COEFFICIENTS ARE ORDERED FROM LOW TO HIGH
C           N     - DIMENSION OF COEFFICIENT VECTOR C
C
C        REMARKS
C           OPERATION IS BYPASSED IN CASE N LESS THAN 1
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           DEFINITION
C           Y=SUM(C(I)*H(I-1,X), SUMMED OVER I FROM 1 TO N).
C           EVALUATION IS DONE BY MEANS OF UPWARD RECURSION
C           USING THE RECURRENCE EQUATION FOR HERMITE POLYNOMIALS
C           H(N+1,X)=2*(X*H(N,X)-N*H(N-1,X)).
C
C     ..................................................................
C
      SUBROUTINE HEPS(Y,X,C,N)
C
      DIMENSION C(1)
C
C        TEST OF DIMENSION
      IF(N)1,1,2
    1 RETURN
C
    2 Y=C(1)
      IF(N-2)1,3,3
C
C        INITIALIZATION
    3 H0=1.
      H1=X+X
C
      DO 4 I=2,N
      H2=X*H1-FLOAT(I-1)*H0
      H0=H1
      H1=H2+H2
    4 Y=Y+C(I)*H0
      RETURN
      END
