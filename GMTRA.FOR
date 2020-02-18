C
C     ..................................................................
C
C        SUBROUTINE GMTRA
C
C        PURPOSE
C           TRANSPOSE A GENERAL MATRIX
C
C        USAGE
C           CALL GMTRA(A,R,N,M)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF MATRIX TO BE TRANSPOSED
C           R - NAME OF RESULTANT MATRIX
C           N - NUMBER OF ROWS OF A AND COLUMNS OF R
C           M - NUMBER OF COLUMNS OF A AND ROWS OF R
C
C        REMARKS
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRIX A
C           MATRICES A AND R MUST BE STORED AS GENERAL MATRICES
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           TRANSPOSE N BY M MATRIX A TO FORM M BY N MATRIX R
C
C     ..................................................................
C
      SUBROUTINE GMTRA(A,R,N,M)
      DIMENSION A(1),R(1)
C
      IR=0
      DO 10 I=1,N
      IJ=I-N
      DO 10 J=1,M
      IJ=IJ+N
      IR=IR+1
   10 R(IR)=A(IJ)
      RETURN
      END
