C
C     ..................................................................
C
C        SUBROUTINE GMADD
C
C        PURPOSE
C           ADD TWO GENERAL MATRICES TO FORM RESULTANT GENERAL MATRIX
C
C        USAGE
C           CALL GMADD(A,B,R,N,M)
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
C           ADDITION IS PERFORMED ELEMENT BY ELEMENT
C
C     ..................................................................
C
      SUBROUTINE GMADD(A,B,R,N,M)
      DIMENSION A(1),B(1),R(1)
C
C        CALCULATE NUMBER OF ELEMENTS
C
      NM=N*M
C
C        ADD MATRICES
C
      DO 10 I=1,NM
   10 R(I)=A(I)+B(I)
      RETURN
      END
