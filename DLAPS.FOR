C
C     ..................................................................
C
C        SUBROUTINE DLAPS
C
C        PURPOSE
C           COMPUTES THE VALUE OF AN N-TERM EXPANSION IN LAGUERRE
C           POLYNOMIALS WITH COEFFICIENT VECTOR C FOR ARGUMENT VALUE X.
C
C        USAGE
C           CALL DLAPS(Y,X,C,N)
C
C        DESCRIPTION OF PARAMETERS
C           Y     - RESULT VALUE
C                   DOUBLE PRECISION VARIABLE
C           X     - ARGUMENT VALUE
C                   DOUBLE PRECISION VARIABLE
C           C     - COEFFICIENT VECTOR OF GIVEN EXPANSION
C                   COEFFICIENTS ARE ORDERED FROM LOW TO HIGH
C                   DOUBLE PRECISION VECTOR
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
C           Y=SUM(C(I)*L(I-1,X), SUMMED OVER I FROM 1 TO N).
C           EVALUATION IS DONE BY MEANS OF UPWARD RECURSION
C           USING THE RECURRENCE EQUATION FOR LAGUERRE POLYNOMIALS
C           L(N+1,X)=2*L(N,X)-L(N-1,X)-((1+X)*L(N,X)-L(N-1,X))/(N+1).
C
C     ..................................................................
C
      SUBROUTINE DLAPS(Y,X,C,N)
C
      DIMENSION C(1)
      DOUBLE PRECISION C,Y,X,H0,H1,H2,T
C
C        TEST OF DIMENSION
      IF(N)1,1,2
    1 RETURN
C
    2 Y=C(1)
      IF(N-2)1,3,3
C
C        INITIALIZATION
    3 H0=1.D0
      H1=1.D0-X
      T=1.D0+X
      DO 4 I=2,N
      H2=H1-H0+H1-(T*H1-H0)/DFLOAT(I)
      H0=H1
      H1=H2
    4 Y=Y+C(I)*H0
      RETURN
      END
