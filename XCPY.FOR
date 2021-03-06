C
C     ..................................................................
C
C        SUBROUTINE XCPY
C
C        PURPOSE
C           COPY A PORTION OF A MATRIX
C
C        USAGE
C           CALL XCPY(A,R,L,K,NR,MR,NA,MA,MS)
C
C        DESCRIPTION OF PARAMETERS
C           A  - NAME OF INPUT MATRIX
C           R  - NAME OF OUTPUT MATRIX
C           L  - ROW OF A WHERE FIRST ELEMENT OF R CAN BE FOUND
C           K  - COLUMN OF A WHERE FIRST ELEMENT OF R CAN BE FOUND
C           NR - NUMBER OF ROWS TO BE COPIED INTO R
C           MR - NUMBER OF COLUMNS TO BE COPIED INTO R
C           NA - NUMBER OF ROWS IN A
C           MA - NUMBER OF COLUMNS IN A
C           MS  - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C
C        REMARKS
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRIX A
C           MATRIX R IS ALWAYS A GENERAL MATRIX
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           MATRIX R IS FORMED BY COPYING A PORTION OF MATRIX A. THIS
C           IS DONE BY EXTRACTING NR ROWS AND MR COLUMNS OF MATRIX A,
C           STARTING WITH ELEMENT AT ROW L, COLUMN K
C
C     ..................................................................
C
      SUBROUTINE XCPY(A,R,L,K,NR,MR,NA,MA,MS)
      DIMENSION A(1),R(1)
C
C        INITIALIZE
C
      IR=0
      L2=L+NR-1
      K2=K+MR-1
C
      DO 5 J=K,K2
      DO 5 I=L,L2
      IR=IR+1
      R(IR)=0.0
C
C        LOCATE ELEMENT FOR ANY MATRIX STORAGE MODE
C
      CALL LOC(I,J,IA,NA,MA,MS)
C
C        TEST FOR ZERO ELEMENT IN DIAGONAL MATRIX
C
      IF(IA) 4,5,4
    4 R(IR)=A(IA)
    5 CONTINUE
      RETURN
      END
 