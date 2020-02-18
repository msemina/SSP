C
C     ..................................................................
C
C        SUBROUTINE LEP
C
C        PURPOSE
C           COMPUTE THE VALUES OF THE LEGENDRE POLYNOMIALS P(N,X)
C           FOR ARGUMENT VALUE X AND ORDERS 0 UP TO N.
C
C        USAGE
C           CALL LEP(Y,X,N)
C
C        DESCRIPTION OF PARAMETERS
C           Y     - RESULT VECTOR OF DIMENSION N+1 CONTAINING THE VALUES
C                   OF LEGENDRE POLYNOMIALS OF ORDER 0 UP TO N
C                   FOR GIVEN ARGUMENT X.
C                   VALUES ARE ORDERED FROM LOW TO HIGH ORDER
C           X     - ARGUMENT OF LEGENDRE POLYNOMIAL
C           N     - ORDER OF LEGENDRE POLYNOMIAL
C
C        REMARKS
C           N LESS THAN 0 IS TREATED AS IF N WERE 0
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           EVALUATION IS BASED ON THE RECURRENCE EQUATION FOR
C           LEGENDRE POLYNOMIALS P(N,X)
C           P(N+1,X)=2*X*P(N,X)-P(N-1,X)-(X*P(N,X)-P(N-1,X))/(N+1),
C           WHERE THE FIRST TERM IN BRACKETS IS THE ORDER,
C           THE SECOND IS THE ARGUMENT.
C           STARTING VALUES ARE P(0,X)=1, P(1,X)=X.
C
C     ..................................................................
C
      SUBROUTINE LEP(Y,X,N)
C
      DIMENSION Y(1)
C
C        TEST OF ORDER
      Y(1)=1.
      IF(N)1,1,2
    1 RETURN
C
    2 Y(2)=X
      IF(N-1)1,1,3
C
    3 DO 4 I=2,N
      G=X*Y(I)
    4 Y(I+1)=G-Y(I-1)+G-(G-Y(I-1))/FLOAT(I)
      RETURN
      END
