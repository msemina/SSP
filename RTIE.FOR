C
C     ..................................................................
C
C        SUBROUTINE RTIE
C
C        PURPOSE
C           ADJOIN TWO MATRICES WITH SAME COLUMN DIMENSION TO FORM ONE
C           RESULTANT MATRIX (SEE METHOD)
C
C        USAGE
C           CALL RTIE(A,B,R,N,M,MSA,MSB,L)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF FIRST INPUT MATRIX
C           B - NAME OF SECOND INPUT MATRIX
C           R - NAME OF OUTPUT MATRIX
C           N - NUMBER OF ROWS IN A
C           M - NUMBER OF COLUMNS IN A,B,R
C           MSA - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C           MSB - SAME AS MSA EXCEPT FOR MATRIX B
C           L - NUMBER OF ROWS IN B
C
C        REMARKS
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRICES A OR B
C           MATRIX R IS ALWAYS A GENERAL MATRIX
C           MATRIX A MUST HAVE THE SAME NUMBER OF COLUMNS AS MATRIX B
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           MATRIX B IS ATTACHED TO THE BOTTOM OF MATRIX A .
C           THE RESULTANT MATRIX R CONTAINS N+L ROWS AND M COLUMNS.
C
C     ..................................................................
C
      SUBROUTINE RTIE(A,B,R,N,M,MSA,MSB,L)
      DIMENSION A(1),B(1),R(1)
C
      NN=N
      IR=0
      NX=NN
      MSX=MSA
      DO 9 J=1,M
      DO 8 II=1,2
      DO 7 I=1,NN
      IR=IR+1
      R(IR)=0.0
C
C        LOCATE ELEMENT FOR ANY MATRIX STORAGE MODE
C
      CALL LOC(I,J,IJ,NN,M,MSX)
C
C        TEST FOR ZERO ELEMENT IN DIAGONAL MATRIX
C
      IF(IJ) 2,7,2
C
C        MOVE ELEMENT TO MATRIX R
C
    2 GO TO(3,4),II
    3 R(IR)=A(IJ)
      GO TO 7
    4 R(IR)=B(IJ)
    7 CONTINUE
C
C        REPEAT ABOVE FOR MATRIX B
C
      MSX=MSB
    8 NN=L
C
C        RESET FOR NEXT COLUMN
C
      MSX=MSA
    9 NN=NX
      RETURN
      END
