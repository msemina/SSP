C
C     ..................................................................
C
C        SUBROUTINE CNP
C
C        PURPOSE
C           COMPUTE THE VALUES OF THE CHEBYSHEV POLYNOMIALS T(N,X)
C           FOR ARGUMENT VALUE X AND ORDERS 0 UP TO N.
C
C        USAGE
C           CALL CNP(Y,X,N)
C
C        DESCRIPTION OF PARAMETERS
C           Y     - RESULT VECTOR OF DIMENSION N+1 CONTAINING THE VALUES
C                   OF CHEBYSHEV POLYNOMIALS OF ORDER 0 UP TO N
C                   FOR GIVEN ARGUMENT X.
C           Y     - RESULT VALUE
C                   VALUES ARE ORDERED FROM LOW TO HIGH ORDER
C           X     - ARGUMENT OF CHEBYSHEV POLYNOMIAL
C           N     - ORDER OF CHEBYSHEV POLYNOMIAL
C
C        REMARKS
C           N LESS THAN 0 IS TREATED AS IF N WERE 0
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           EVALUATION IS BASED ON THE RECURRENCE EQUATION FOR
C           CHEBYSHEV POLYNOMIALS T(N,X)
C           T(N+1,X)=2*X*T(N,X)-T(N-1,X),
C           WHERE THE FIRST TERM IN BRACKETS IS THE ORDER,
C           THE SECOND IS THE ARGUMENT.
C           STARTING VALUES ARE T(0,X)=1, T(1,X)=X.
C
C     ..................................................................
C
      SUBROUTINE CNP(Y,X,N)
C
      DIMENSION Y(1)
      Y(1)=1.
      IF(N)1,1,2
    1 RETURN
C
    2 Y(2)=X
      IF(N-1)1,1,3
C
C        INITIALIZATION
    3 F=X+X
C
      DO 4 I=2,N
    4 Y(I+1)=F*Y(I)-Y(I-1)
      RETURN
      END
