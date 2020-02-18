C
C     ..................................................................
C
C        SUBROUTINE MATA
C
C        PURPOSE
C           PREMULTIPLY A MATRIX BY ITS TRANSPOSE TO FORM A
C           SYMMETRIC MATRIX
C
C        USAGE
C           CALL MATA(A,R,N,M,MS)
C
C        DESCRIPTION OF PARAMETERS
C           A  - NAME OF INPUT MATRIX
C           R  - NAME OF OUTPUT MATRIX
C           N  - NUMBER OF ROWS IN A
C           M  - NUMBER OF COLUMNS IN A. ALSO NUMBER OF ROWS AND
C                NUMBER OF COLUMNS OF R.
C           MS  - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C
C        REMARKS
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRIX A
C           MATRIX R IS ALWAYS A SYMMETRIC MATRIX WITH A STORAGE MODE=1
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           CALCULATION OF (A TRANSPOSE A) RESULTS IN A SYMMETRIC MATRIX
C           REGARDLESS OF THE STORAGE MODE OF THE INPUT MATRIX. THE
C           ELEMENTS OF MATRIX A ARE NOT CHANGED.
C
C     ..................................................................
C
      SUBROUTINE MATA(A,R,N,M,MS)
      DIMENSION A(1),R(1)
C
      DO 60 K=1,M
      KX=(K*K-K)/2
      DO 60 J=1,M
      IF(J-K) 10,10,60
   10 IR=J+KX
      R(IR)=0
      DO 60 I=1,N
      IF(MS) 20,40,20
   20 CALL LOC(I,J,IA,N,M,MS)
      CALL LOC(I,K,IB,N,M,MS)
      IF(IA) 30,60,30
   30 IF(IB) 50,60,50
   40 IA=N*(J-1)+I
      IB=N*(K-1)+I
   50 R(IR)=R(IR)+A(IA)*A(IB)
   60 CONTINUE
      RETURN
      END
