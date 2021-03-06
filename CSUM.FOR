C
C     ..................................................................
C
C        SUBROUTINE CSUM
C
C        PURPOSE
C           SUM ELEMENTS OF EACH COLUMN TO FORM ROW VECTOR
C
C        USAGE
C           CALL CSUM(A,R,N,M,MS)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF INPUT MATRIX
C           R - NAME OF VECTOR OF LENGTH M
C           N - NUMBER OF ROWS IN A
C           M - NUMBER OF COLUMNS IN A
C           MS  - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C
C        REMARKS
C           VECTOR R CANNOT BE IN THE SAME LOCATION AS MATRIX A
C           UNLESS A IS GENERAL
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           ELEMENTS ARE SUMMED DOWN EACH COLUMN INTO A CORRESPONDING
C           ELEMENT OF OUTPUT ROW VECTOR R
C
C     ..................................................................
C
      SUBROUTINE CSUM(A,R,N,M,MS)
      DIMENSION A(1),R(1)
C
      DO 3 J=1,M
C
C        CLEAR OUTPUT LOCATION
C
      R(J)=0.0
C
      DO 3 I=1,N
C
C        LOCATE ELEMENT FOR ANY MATRIX STORAGE MODE
C
      CALL LOC(I,J,IJ,N,M,MS)
C
C        TEST FOR ZERO ELEMENT IN DIAGONAL MATRIX
C
      IF(IJ) 2,3,2
C
C        ACCUMULATE IN OUTPUT VECTOR
C
    2 R(J)=R(J)+A(IJ)
    3 CONTINUE
      RETURN
      END
