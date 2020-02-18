C
C     ..................................................................
C
C        SUBROUTINE GTPRD
C
C        PURPOSE
C           PREMULTIPLY A GENERAL MATRIX BY THE TRANSPOSE OF ANOTHER
C           GENERAL MATRIX
C
C        USAGE
C           CALL GTPRD(A,B,R,N,M,L)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF FIRST INPUT MATRIX
C           B - NAME OF SECOND INPUT MATRIX
C           R - NAME OF OUTPUT MATRIX
C           N - NUMBER OF ROWS IN A AND B
C           M - NUMBER OF COLUMNS IN A AND ROWS IN R
C           L - NUMBER OF COLUMNS IN B AND R
C
C        REMARKS
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRIX A
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRIX B
C           ALL MATRICES MUST BE STORED AS GENERAL MATRICES
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           MATRIX TRANSPOSE OF A IS NOT ACTUALLY CALCULATED. INSTEAD,
C           ELEMENTS OF MATRIX A ARE TAKEN COLUMNWISE RATHER THAN
C           ROWWISE FOR POSTMULTIPLICATION BY MATRIX B.
C
C     ..................................................................
C
      SUBROUTINE GTPRD(A,B,R,N,M,L)
      DIMENSION A(1),B(1),R(1)
C
      IR=0
      IK=-N
      DO 10 K=1,L
      IJ=0
      IK=IK+N
      DO 10 J=1,M
      IB=IK
      IR=IR+1
      R(IR)=0
      DO 10 I=1,N
      IJ=IJ+1
      IB=IB+1
   10 R(IR)=R(IR)+A(IJ)*B(IB)
      RETURN
      END
