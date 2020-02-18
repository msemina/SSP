C
C     ..................................................................
C
C        SUBROUTINE CSP
C
C        PURPOSE
C           COMPUTE THE VALUES OF THE SHIFTED CHEBYSHEV POLYNOMIALS
C           TS(N,X) FOR ARGUMENT X AND ORDERS 0 UP TO N.
C
C        USAGE
C           CALL CSP(Y,X,N)
C
C        DESCRIPTION OF PARAMETERS
C           Y     - RESULT VECTOR OF DIMENSION N+1 CONTAINING THE VALUES
C                   OF SHIFTED CHEBYSHEV POLYNOMIALS OF ORDER 0 UP TO N
C                   FOR GIVEN ARGUMENT X.
C                   VALUES ARE ORDERED FROM LOW TO HIGH ORDER
C           X     - ARGUMENT OF SHIFTED CHEBYSHEV POLYNOMIAL
C           N     - ORDER OF SHIFTED CHEBYSHEV POLYNOMIAL
C
C        REMARKS
C           N LESS THAN 0 IS TREATED AS IF N WERE 0
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           EVALUATION IS BASED ON THE RECURRENCE EQUATION FOR
C           SHIFTED CHEBYSHEV POLYNOMIALS TS(N,X)
C           TS(N+1,X)=(4*X-2)*TS(N,X)-TS(N-1,X),
C           WHERE THE FIRST TERM IN BRACKETS IS THE ORDER,
C           THE SECOND IS THE ARGUMENT.
C           STARTING VALUES ARE TS(0,X)=1, TS(1,X)=2*X-1.
C
C     ..................................................................
C
      SUBROUTINE CSP(Y,X,N)
C
      DIMENSION Y(1)
C
C        TEST OF ORDER
      Y(1)=1.
      IF(N)1,1,2
    1 RETURN
C
    2 Y(2)=X+X-1.
      IF(N-1)1,1,3
C
C        INITIALIZATION
    3 F=Y(2)+Y(2)
C
      DO 4 I=2,N
    4 Y(I+1)=F*Y(I)-Y(I-1)
      RETURN
      END
