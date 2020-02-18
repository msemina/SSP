C
C     ..................................................................
C
C        SUBROUTINE DLAP
C
C        PURPOSE
C           COMPUTE THE VALUES OF THE LAGUERRE POLYNOMIALS L(N,X)
C           FOR ARGUMENT VALUE X AND ORDERS 0 UP TO N.
C
C        USAGE
C           CALL DLAP(Y,X,N)
C
C        DESCRIPTION OF PARAMETERS
C           Y     - RESULT VECTOR OF DIMENSION N+1 CONTAINING THE VALUES
C                   OF LAGUERRE POLYNOMIALS OF ORDER 0 UP TO N
C                   FOR GIVEN ARGUMENT X.
C                   DOUBLE PRECISION VECTOR.
C                   VALUES ARE ORDERED FROM LOW TO HIGH ORDER
C           X     - ARGUMENT OF LAGUERRE POLYNOMIAL
C                   DOUBLE PRECISION VARIABLE.
C           N     - ORDER OF LAGUERRE POLYNOMIAL
C
C        REMARKS
C           N LESS THAN 0 IS TREATED AS IF N WERE 0
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           EVALUATION IS BASED ON THE RECURRENCE EQUATION FOR
C           LAGUERRE POLYNOMIALS L(N,X)
C           L(N+1,X)=2*L(N,X)-L(N-1,X)-((1+X)*L(N,X)-L(N-1,X))/(N+1),
C           WHERE THE FIRST TERM IN BRACKETS IS THE ORDER,
C           THE SECOND IS THE ARGUMENT.
C           STARTING VALUES ARE L(0,X)=1, L(1,X)=1.-X.
C
C     ..................................................................
C
      SUBROUTINE DLAP(Y,X,N)
C
      DIMENSION Y(1)
      DOUBLE PRECISION Y,X,T
C
C        TEST OF ORDER
      Y(1)=1.D0
      IF(N)1,1,2
    1 RETURN
C
    2 Y(2)=1.D0-X
      IF(N-1)1,1,3
C
C        INITIALIZATION
    3 T=1.D0+X
C
      DO 4 I=2,N
    4 Y(I+1)=Y(I)-Y(I-1)+Y(I)-(T*Y(I)-Y(I-1))/DFLOAT(I)
      RETURN
      END
