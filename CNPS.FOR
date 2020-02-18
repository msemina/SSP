C
C     ..................................................................
C
C        SUBROUTINE CNPS
C
C        PURPOSE
C           COMPUTES THE VALUE OF AN N-TERM EXPANSION IN CHEBYSHEV
C           POLYNOMIALS WITH COEFFICIENT VECTOR C FOR ARGUMENT VALUE X.
C
C        USAGE
C           CALL CNPS(Y,X,C,N)
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
C           Y=SUM(C(I)*T(I-1,X), SUMMED OVER I FROM 1 TO N).
C           EVALUATION IS DONE BY MEANS OF BACKWARD RECURSION
C           USING THE RECURRENCE EQUATION FOR CHEBYSHEV POLYNOMIALS
C           T(N+1,X)=2*X*T(N,X)-T(N-1,X).
C
C     ..................................................................
C
      SUBROUTINE CNPS(Y,X,C,N)
C
      DIMENSION C(1)
C
C        TEST OF DIMENSION
      IF(N)1,1,2
    1 RETURN
C
    2 IF(N-2)3,4,4
    3 Y=C(1)
      RETURN
C
C        INITIALIZATION
    4 ARG=X+X
      H1=0.
      H0=0.
C
      DO 5 I=1,N
      K=N-I
      H2=H1
      H1=H0
    5 H0=ARG*H1-H2+C(K+1)
      Y=0.5*(C(1)-H2+H0)
      RETURN
      END
