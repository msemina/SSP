C
C     ..................................................................
C
C        SUBROUTINE MPRD
C
C        PURPOSE
C           MULTIPLY TWO MATRICES TO FORM A RESULTANT MATRIX
C
C        USAGE
C           CALL MPRD(A,B,R,N,M,MSA,MSB,L)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF FIRST INPUT MATRIX
C           B - NAME OF SECOND INPUT MATRIX
C           R - NAME OF OUTPUT MATRIX
C           N - NUMBER OF ROWS IN A AND R
C           M - NUMBER OF COLUMNS IN A AND ROWS IN B
C           MSA - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C           MSB - SAME AS MSA EXCEPT FOR MATRIX B
C           L - NUMBER OF COLUMNS IN B AND R
C
C        REMARKS
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRICES A OR B
C           NUMBER OF COLUMNS OF MATRIX A MUST BE EQUAL TO NUMBER OF ROW
C           OF MATRIX B
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           THE M BY L MATRIX B IS PREMULTIPLIED BY THE N BY M MATRIX A
C           AND THE RESULT IS STORED IN THE N BY L MATRIX R. THIS IS A
C           ROW INTO COLUMN PRODUCT.
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
      SUBROUTINE MPRD(A,B,R,N,M,MSA,MSB,L)
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
C        ALL OTHER CASES
C
   30 IR=1
      DO 90 K=1,L
      DO 90 J=1,N
      R(IR)=0
      DO 80 I=1,M
      IF(MS) 40,60,40
   40 CALL LOC(J,I,IA,N,M,MSA)
      CALL LOC(I,K,IB,M,L,MSB)
      IF(IA) 50,80,50
   50 IF(IB) 70,80,70
   60 IA=N*(I-1)+J
      IB=M*(K-1)+I
   70 R(IR)=R(IR)+A(IA)*B(IB)
   80 CONTINUE
   90 IR=IR+1
      RETURN
      END
