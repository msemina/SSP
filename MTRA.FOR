C
C     ..................................................................
C
C        SUBROUTINE MTRA
C
C        PURPOSE
C           TRANSPOSE A MATRIX
C
C        USAGE
C           CALL MTRA(A,R,N,M,MS)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF MATRIX TO BE TRANSPOSED
C           R - NAME OF OUTPUT MATRIX
C           N - NUMBER OF ROWS OF A AND COLUMNS OF R
C           M - NUMBER OF COLUMNS OF A AND ROWS OF R
C           MS  - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A (AND R)
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C
C        REMARKS
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRIX A
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           MCPY
C
C        METHOD
C           TRANSPOSE N BY M MATRIX A TO FORM M BY N MATRIX R BY MOVING
C           EACH ROW OF A INTO THE CORRESPONDING COLUMN OF R. IF MATRIX
C           A IS SYMMETRIC OR DIAGONAL, MATRIX R IS THE SAME AS A.
C
C     ..................................................................
C
      SUBROUTINE MTRA(A,R,N,M,MS)
      DIMENSION A(1),R(1)
C
C        IF MS IS 1 OR 2, COPY A
C
      IF(MS) 10,20,10
   10 CALL MCPY(A,R,N,N,MS)
      RETURN
C
C        TRANSPOSE GENERAL MATRIX
C
   20 IR=0
      DO 30 I=1,N
      IJ=I-N
      DO 30 J=1,M
      IJ=IJ+N
      IR=IR+1
   30 R(IR)=A(IJ)
      RETURN
      END
