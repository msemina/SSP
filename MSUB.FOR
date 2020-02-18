C
C     ..................................................................
C
C        SUBROUTINE MSUB
C
C        PURPOSE
C           SUBTRACT TWO MATRICES ELEMENT BY ELEMENT TO FORM RESULTANT
C           MATRIX
C
C        USAGE
C           CALL MSUB(A,B,R,N,M,MSA,MSB)
C
C        DESCRIPTION OF PARAMETERS
C           A - NAME OF INPUT MATRIX
C           B - NAME OF INPUT MATRIX
C           R - NAME OF OUTPUT MATRIX
C           N - NUMBER OF ROWS IN A,B,R
C           M - NUMBER OF COLUMNS IN A,B,R
C           MSA - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C           MSB - SAME AS MSA EXCEPT FOR MATRIX B
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           STRUCTURE OF OUTPUT MATRIX IS FIRST DETERMINED. SUBTRACTION
C           OF MATRIX B ELEMENTS FROM CORRESPONDING MATRIX A ELEMENTS
C           IS THEN PERFORMED.
C           THE FOLLOWING TABLE SHOWS THE STORAGE MODE OF THE OUTPUT
C           MATRIX FOR ALL COMBINATIONS OF INPUT MATRICES
C                         A                B                 R
C                      GENERAL          GENERAL          GENERAL
C                      GENERAL          SYMMETRIC        GENERAL
C                      GENERAL          DIAGONAL         GENERAL
C                      SYMMETRIC        GENERAL          GENERAL
C                      SYMMETRIC        SYMMETRIC        SYMMETRIC
C                      SYMMETRIC        DIAGONAL         SYMMETRIC
C                      DIAGONAL         GENERAL          GENERAL
C                      DIAGONAL         SYMMETRIC        SYMMETRIC
C                      DIAGONAL         DIAGONAL         DIAGONAL
C
C     ..................................................................
C
      SUBROUTINE MSUB(A,B,R,N,M,MSA,MSB)
      DIMENSION A(1),B(1),R(1)
C
C        DETERMINE STORAGE MODE OF OUTPUT MATRIX
C
      IF(MSA-MSB) 7,5,7
    5 CALL LOC(N,M,NM,N,M,MSA)
      GO TO 100
    7 MTEST=MSA*MSB
      MSR=0
      IF(MTEST) 20,20,10
   10 MSR=1
   20 IF(MTEST-2) 35,35,30
   30 MSR=2
C
C        LOCATE ELEMENTS AND PERFORM SUBTRACTION
C
   35 DO 90 J=1,M
      DO 90 I=1,N
      CALL LOC(I,J,IJR,N,M,MSR)
      IF(IJR) 40,90,40
   40 CALL LOC(I,J,IJA,N,M,MSA)
      AEL=0.0
      IF(IJA) 50,60,50
   50 AEL=A(IJA)
   60 CALL LOC(I,J,IJB,N,M,MSB)
      BEL=0.0
      IF(IJB) 70,80,70
   70 BEL=B(IJB)
   80 R(IJR)=AEL-BEL
   90 CONTINUE
      RETURN
C
C        SUBTRACT MATRICES FOR OTHER CASES
C
  100 DO 110 I=1,NM
  110 R(I)=A(I)-B(I)
      RETURN
      END
