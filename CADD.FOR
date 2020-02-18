C
C     ..................................................................
C
C        SUBROUTINE CADD
C
C        PURPOSE
C           ADD COLUMN OF ONE MATRIX TO COLUMN OF ANOTHER MATRIX
C
C        USAGE
C           CALL CADD(A,ICA,R,ICR,N,M,MS,L)
C
C        DESCRIPTION OF PARAMETERS
C           A   - NAME OF INPUT MATRIX
C           ICA - COLUMN IN MATRIX A TO BE ADDED TO COLUMN ICR OF R
C           R   - NAME OF OUTPUT MATRIX
C           ICR - COLUMN IN MATRIX R WHERE SUMMATION IS DEVELOPED
C           N   - NUMBER OF ROWS IN A AND R
C           M   - NUMBER OF COLUMNS IN A
C           MS  - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C           L   - NUMBER OF COLUMNS IN R
C
C        REMARKS
C           MATRIX R MUST BE A GENERAL MATRIX
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRIX A UNLESS
C           A IS GENERAL
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           EACH ELEMENT OF COLUMN ICA OF MATRIX A IS ADDED TO
C           CORRESPONDING ELEMENT OF COLUMN ICR OF MATRIX R
C
C     ..................................................................
C
      SUBROUTINE CADD(A,ICA,R,ICR,N,M,MS,L)
      DIMENSION A(1),R(1)
C
      IR=N*(ICR-1)
      DO 2 I=1,N
      IR=IR+1
C
C        LOCATE INPUT ELEMENT FOR ANY MATRIX STORAGE MODE
C
      CALL LOC(I,ICA,IA,N,M,MS)
C
C        TEST FOR ZERO ELEMENT IN DIAGONAL MATRIX
C
      IF(IA) 1,2,1
C
C        ADD ELEMENTS
C
    1 R(IR)=R(IR)+A(IA)
    2 CONTINUE
      RETURN
      END
