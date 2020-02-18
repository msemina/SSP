C
C     ..................................................................
C
C        SUBROUTINE TPRD
C
C        PURPOSE
C           TRANSPOSE A MATRIX AND POSTMULTIPLY BY ANOTHER TO FORM
C           A RESULTANT MATRIX
C
C        USAGE
C           CALL TPRD(A,B,R,N,M,MSA,MSB,L)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF FIRST INPUT MATRIX
C           B - NAME OF SECOND INPUT MATRIX
C           R - NAME OF OUTPUT MATRIX
C           N - NUMBER OF ROWS IN A AND B
C           M - NUMBER OF COLUMNS IN A AND ROWS IN R
C           MSA - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C           MSB - SAME AS MSA EXCEPT FOR MATRIX B
C           L - NUMBER OF COLUMNS IN B AND R
C
C        REMARKS
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRICES A OR B
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           MATRIX TRANSPOSE OF A IS NOT ACTUALLY CALCULATED. INSTEAD,
C           ELEMENTS IN MATRIX A ARE TAKEN COLUMNWISE RATHER THAN
C           ROWWISE FOR MULTIPLICATION BY MATRIX B.
C           THE FOLLOWING TABLE SHOWS THE STORAGE MODE OF THE OUTPUT
C           MATRIX FOR ALL COMBINATIONS OF INPUT MATRICES
C                         A                B                R
C                      GENERAL          GENERAL          GENERAL
C                      GENERAL          SYMMETRIC        GENERAL
C                      GENERAL          DIAGONAL         GENERAL
C                      SYMMETRIC        GENERAL          GENERAL
C                      SYMMETRIC        SYMMETRIC        GENERAL
C                      SYMMETRIC        DIAGONAL         GENERAL
C                      DIAGONAL         GENERAL          GENERAL
C                      DIAGONAL         SYMMETRIC        GENERAL
C                      DIAGONAL         DIAGONAL         DIAGONAL
C
C     ..................................................................
C
      SUBROUTINE TPRD(A,B,R,N,M,MSA,MSB,L)
      DIMENSION A(1),B(1),R(1)
C
C        SPECIAL CASE FOR DIAGONAL BY DIAGONAL
C
      MS=MSA*10+MSB
      IF(MS-22) 30,10,30
   10 DO 20 I=1,N
   20 R(I)=A(I)*B(I)
      RETURN
C
C        MULTIPLY TRANSPOSE OF A BY B
C
   30 IR=1
      DO 90 K=1,L
      DO 90 J=1,M
      R(IR)=0.0
      DO 80 I=1,N
      IF(MS) 40,60,40
   40 CALL LOC(I,J,IA,N,M,MSA)
      CALL LOC(I,K,IB,N,L,MSB)
      IF(IA) 50,80,50
   50 IF(IB) 70,80,70
   60 IA=N*(J-1)+I
      IB=N*(K-1)+I
   70 R(IR)=R(IR)+A(IA)*B(IB)
   80 CONTINUE
   90 IR=IR+1
      RETURN
      END
