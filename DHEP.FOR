C
C     ..................................................................
C
C        SUBROUTINE DHEP
C
C        PURPOSE
C           COMPUTE THE VALUES OF THE HERMITE POLYNOMIALS H(N,X)
C           FOR ARGUMENT VALUE X AND ORDERS 0 UP TO N.
C
C        USAGE
C           CALL DHEP(Y,X,N)
C
C        DESCRIPTION OF PARAMETERS
C           Y     - RESULT VECTOR OF DIMENSION N+1 CONTAINING THE VALUES
C                   OF HERMITE POLYNOMIALS OF ORDER 0 UP TO N
C                   FOR GIVEN ARGUMENT X.
C                   DOUBLE PRECISION VECTOR.
C                   VALUES ARE ORDERED FROM LOW TO HIGH ORDER
C           X     - ARGUMENT OF HERMITE POLYNOMIAL
C                   DOUBLE PRECISION VARIABLE.
C           N     - ORDER OF HERMITE POLYNOMIAL
C
C        REMARKS
C           N LESS THAN 0 IS TREATED AS IF N WERE 0
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           EVALUATION IS BASED ON THE RECURRENCE EQUATION FOR
C           HERMITE POLYNOMIALS H(N,X)
C           H(N+1,X)=2*(X*H(N,X)-N*H(N-1,X))
C           WHERE THE FIRST TERM IN BRACKETS IS THE INDEX,
C           THE SECOND IS THE ARGUMENT.
C           STARTING VALUES ARE H(0,X)=1, H(1,X)=2*X.
C
C     ..................................................................
C
      SUBROUTINE DHEP(Y,X,N)
C
      DIMENSION Y(1)
      DOUBLE PRECISION Y,X,F
C
C        TEST OF ORDER
      Y(1)=1.D0
      IF(N)1,1,2
    1 RETURN
C
    2 Y(2)=X+X
      IF(N-1)1,1,3
C
    3 DO 4 I=2,N
      F=X*Y(I)-DFLOAT(I-1)*Y(I-1)
    4 Y(I+1)=F+F
      RETURN
      END
