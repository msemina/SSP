C
C     ..................................................................
C
C        SUBROUTINE SMPY
C
C        PURPOSE
C           MULTIPLY EACH ELEMENT OF A MATRIX BY A SCALAR TO FORM A
C           RESULTANT MATRIX
C
C        USAGE
C           CALL SMPY(A,C,R,N,M,MS)
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
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           SCALAR IS MULTIPLIED BY EACH ELEMENT OF MATRIX
C
C     ..................................................................
C
      SUBROUTINE SMPY(A,C,R,N,M,MS)
      DIMENSION A(1),R(1)
C
C        COMPUTE VECTOR LENGTH, IT
C
      CALL LOC(N,M,IT,N,M,MS)
C
C        MULTIPLY BY SCALAR
C
      DO 1 I=1,IT
    1 R(I)=A(I)*C
      RETURN
      END
