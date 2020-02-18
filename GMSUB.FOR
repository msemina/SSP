C
C     ..................................................................
C
C        SUBROUTINE GMSUB
C
C        PURPOSE
C           SUBTRACT ONE GENERAL MATRIX FROM ANOTHER TO FORM RESULTANT
C           MATRIX
C
C        USAGE
C           CALL GMSUB(A,B,R,N,M)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF FIRST INPUT MATRIX
C           B - NAME OF SECOND INPUT MATRIX
C           R - NAME OF OUTPUT MATRIX
C           N - NUMBER OF ROWS IN A,B,R
C           M - NUMBER OF COLUMNS IN A,B,R
C
C        REMARKS
C           ALL MATRICES MUST BE STORED AS GENERAL MATRICES
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           MATRIX B ELEMENTS ARE SUBTRACTED FROM CORRESPONDING MATRIX A
C           ELEMENTS
C
C     ..................................................................
C
      SUBROUTINE GMSUB(A,B,R,N,M)
      DIMENSION A(1),B(1),R(1)
C
C        CALCULATE NUMBER OF ELEMENTS
C
      NM=N*M
C
C        SUBTRACT MATRICES
C
      DO 10 I=1,NM
   10 R(I)=A(I)-B(I)
      RETURN
      END
