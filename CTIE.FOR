C
C     ..................................................................
C
C        SUBROUTINE CTIE
C
C        PURPOSE
C           ADJOIN TWO MATRICES WITH SAME ROW DIMENSION TO FORM ONE
C           RESULTANT MATRIX (SEE METHOD)
C
C        USAGE
C           CALL CTIE(A,B,R,N,M,MSA,MSB,L)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF FIRST INPUT MATRIX
C           B - NAME OF SECOND INPUT MATRIX
C           R - NAME OF OUTPUT MATRIX
C           N - NUMBER OF ROWS IN A,B,R
C           M - NUMBER OF COLUMNS IN A
C           MSA - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C           MSB - SAME AS MSA EXCEPT FOR MATRIX B
C           L - NUMBER OF COLUMNS IN B
C
C        REMARKS
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRICES A OR B
C           MATRIX R IS ALWAYS A GENERAL MATRIX
C           MATRIX A MUST HAVE THE SAME NUMBER OF ROWS AS MATRIX B
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           MATRIX B IS ATTACHED TO THE RIGHT OF MATRIX A .
C           THE RESULTANT MATRIX R CONTAINS N ROWS AND M+L COLUMNS
C
C     ..................................................................
C
      SUBROUTINE CTIE(A,B,R,N,M,MSA,MSB,L)
      DIMENSION A(1),B(1),R(1)
C
      MM=M
      IR=0
      MSX=MSA
      DO 6 JJ=1,2
      DO 5 J=1,MM
      DO 5 I=1,N
      IR=IR+1
      R(IR)=0.0
C
C        LOCATE ELEMENT FOR ANY MATRIX STORAGE MODE
C
      CALL LOC(I,J,IJ,N,MM,MSX)
C
C        TEST FOR ZERO ELEMENT IN DIAGONAL MATRIX
C
      IF(IJ) 2,5,2
C
C        MOVE ELEMENT TO MATRIX R
C
    2 GO TO(3,4),JJ
    3 R(IR)=A(IJ)
      GO TO 5
    4 R(IR)=B(IJ)
    5 CONTINUE
C
C        REPEAT ABOVE FOR MATRIX B
C
      MSX=MSB
      MM=L
    6 CONTINUE
      RETURN
      END
