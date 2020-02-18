C
C     ..................................................................
C
C        SUBROUTINE CCPY
C
C        PURPOSE
C           COPY SPECIFIED COLUMN OF A MATRIX INTO A VECTOR
C
C        USAGE
C           CALL CCPY(A,L,R,N,M,MS)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF INPUT MATRIX
C           L - COLUMN OF A TO BE MOVED TO R
C           R - NAME OF OUTPUT VECTOR OF LENGTH N
C           N - NUMBER OR ROWS IN A
C           M - NUMBER OF COLUMNS IN A
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
C           ELEMENTS OF COLUMN L ARE MOVED TO CORRESPONDING POSITIONS
C           OF VECTOR R
C
C     ..................................................................
C
      SUBROUTINE CCPY(A,L,R,N,M,MS)
      DIMENSION A(1),R(1)
C
      DO 3 I=1,N
C
C        LOCATE ELEMENT FOR ANY MATRIX STORAGE MODE
C
      CALL LOC(I,L,IL,N,M,MS)
C
C        TEST FOR ZERO ELEMENT IN DIAGONAL MATRIX
C
      IF(IL) 1,2,1
C
C        MOVE ELEMENT TO R
C
    1 R(I)=A(IL)
      GO TO 3
    2 R(I)=0.0
    3 CONTINUE
      RETURN
      END
