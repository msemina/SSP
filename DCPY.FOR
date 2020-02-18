C
C     ..................................................................
C
C        SUBROUTINE DCPY
C
C        PURPOSE
C           COPY DIAGONAL ELEMENTS OF A MATRIX INTO A VECTOR
C
C        USAGE
C           CALL DCPY (A,R,N,MS)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF INPUT MATRIX
C           R - NAME OF OUTPUT VECTOR OF LENGTH N
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
C           ELEMENTS ON DIAGONAL OF MATRIX ARE MOVED TO CORRESPONDING
C           POSITIONS OF VECTOR R
C
C     ..................................................................
C
      SUBROUTINE DCPY(A,R,N,MS)
      DIMENSION A(1),R(1)
C
      DO 3 J=1,N
C
C        LOCATE DIAGONAL ELEMENT FOR ANY MATRIX STORAGE MODE
C
      CALL LOC(J,J,IJ,N,N,MS)
C
C        MOVE DIAGONAL ELEMENT TO VECTOR R
C
    3 R(J)=A(IJ)
      RETURN
      END
