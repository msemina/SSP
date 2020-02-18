C
C        ...............................................................
C
C        SUBROUTINE MCPY
C
C        PURPOSE
C           COPY ENTIRE MATRIX
C
C        USAGE
C           CALL MCPY (A,R,N,M,MS)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF INPUT MATRIX
C           R - NAME OF OUTPUT MATRIX
C           N - NUMBER OF ROWS IN A OR R
C           M - NUMBER OF COLUMNS IN A OR R
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
C           EACH ELEMENT OF MATRIX A IS MOVED TO THE CORRESPONDING
C           ELEMENT OF MATRIX R
C
C     ..................................................................
C
      SUBROUTINE MCPY(A,R,N,M,MS)
      DIMENSION A(1),R(1)
C
C        COMPUTE VECTOR LENGTH, IT
C
      CALL LOC(N,M,IT,N,M,MS)
C
C        COPY MATRIX
C
      DO 1 I=1,IT
    1 R(I)=A(I)
      RETURN
      END
