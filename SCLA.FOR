C
C     ..................................................................
C
C        SUBROUTINE SCLA
C
C        PURPOSE
C           SET EACH ELEMENT OF A MATRIX EQUAL TO A GIVEN SCALAR
C
C        USAGE
C           CALL SCLA (A,C,N,M,MS)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF INPUT MATRIX
C           C - SCALAR
C           N - NUMBER OF ROWS IN MATRIX A
C           M - NUMBER OF COLUMNS IN MATRIX A
C           MS  - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
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
C           EACH ELEMENT OF MATRIX A IS REPLACED BY SCALAR C
C
C     ..................................................................
C
      SUBROUTINE SCLA(A,C,N,M,MS)
      DIMENSION A(1)
C
C        COMPUTE VECTOR LENGTH, IT
C
      CALL LOC(N,M,IT,N,M,MS)
C
C        REPLACE BY SCALAR
C
      DO 1 I=1,IT
    1 A(I)=C
      RETURN
      END
