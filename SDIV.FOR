C
C     ..................................................................
C
C        SUBROUTINE SDIV
C
C        PURPOSE
C           DIVIDE EACH ELEMENT OF A MATRIX BY A SCALAR TO FORM A
C           RESULTANT MATRIX
C
C        USAGE
C           CALL SDIV(A,C,R,N,M,MS)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF INPUT MATRIX
C           C - SCALAR
C           R - NAME OF OUTPUT MATRIX
C           N - NUMBER OF ROWS IN MATRIX A AND R
C           M - NUMBER OF COLUMNS IN MATRIX A AND R
C           MS  - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A (AND R)
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C
C        REMARKS
C           IF SCALAR IS ZERO, DIVISION IS PERFORMED ONLY ONCE TO CAUSE
C           FLOATING POINT OVERFLOW CONDITION
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           EACH ELEMENT OF MATRIX IS DIVIDED BY SCALAR
C
C     ..................................................................
C
      SUBROUTINE SDIV(A,C,R,N,M,MS)
      DIMENSION A(1),R(1)
C
C        COMPUTE VECTOR LENGTH, IT
C
      CALL LOC(N,M,IT,N,M,MS)
C
C        DIVIDE BY SCALAR (IF SCALAR IS ZERO, DIVIDE ONLY ONCE)
C
      IF(C) 2,1,2
    1 IT=1
    2 DO 3 I=1,IT
    3 R(I)=A(I)/C
      RETURN
      END
