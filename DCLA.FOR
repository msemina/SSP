C
C     ..................................................................
C
C        SUBROUTINE DCLA
C
C        PURPOSE
C           SET EACH DIAGONAL ELEMENT OF A MATRIX EQUAL TO A SCALAR
C
C        USAGE
C           CALL DCLA (A,C,N,MS)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF INPUT MATRIX
C           C - SCALAR
C           N - NUMBER OF ROWS AND COLUMNS IN MATRIX A
C           MS  - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C
C        REMARKS
C           INPUT MATRIX MUST BE A SQUARE MATRIX
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           EACH ELEMENT ON DIAGONAL OF MATRIX IS REPLACED BY SCALAR C
C
C     ..................................................................
C
      SUBROUTINE DCLA(A,C,N,MS)
      DIMENSION A(1)
C
      DO 3 I=1,N
C
C        LOCATE DIAGONAL ELEMENT FOR ANY MATRIX STORAGE MODE
C
      CALL LOC(I,I,ID,N,N,MS)
C
C        REPLACE DIAGONAL ELEMENTS
C
    3 A(ID)=C
      RETURN
      END
