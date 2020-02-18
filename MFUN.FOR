C
C     ..................................................................
C
C        SUBROUTINE MFUN
C
C        PURPOSE
C           APPLY A FUNCTION TO EACH ELEMENT OF A MATRIX TO FORM A
C           RESULTANT MATRIX
C
C        USAGE
C           CALL MFUN (A,F,R,N,M,MS)
C           AN EXTERNAL STATEMENT MUST PRECEDE CALL STATEMENT IN ORDER
C           TO IDENTIFY PARAMETER F AS THE NAME OF A FUNCTION
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF INPUT MATRIX
C           F - NAME OF FORTRAN-FURNISHED OR USER FUNCTION SUBPROGRAM
C           R - NAME OF OUTPUT MATRIX
C           N - NUMBER OF ROWS IN MATRIX A AND R
C           M - NUMBER OF COLUMNS IN MATRIX A AND R
C           MS  - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A (AND R)
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C
C        REMARKS
C           PRECISION IS DEPENDENT UPON PRECISION OF FUNCTION USED
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           FUNCTION F IS APPLIED TO EACH ELEMENT OF MATRIX A
C           TO FORM MATRIX R
C
C     ..................................................................
C
      SUBROUTINE MFUN(A,F,R,N,M,MS)
      DIMENSION A(1),R(1)
C
C        COMPUTE VECTOR LENGTH, IT
C
      CALL LOC(N,M,IT,N,M,MS)
C
C        BUILD MATRIX R FOR ANY STORAGE MODE
C
      DO 5 I=1,IT
    5 R(I)=F(A(I))
      RETURN
      END
