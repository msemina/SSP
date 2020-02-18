C
C     ..................................................................
C
C        SUBROUTINE RANK
C
C        PURPOSE
C           RANK A VECTOR OF VALUES
C
C        USAGE
C           CALL RANK(A,R,N)
C
C        DESCRIPTION OF PARAMETERS
C           A - INPUT VECTOR OF N VALUES
C           R - OUTPUT VECTOR OF LENGTH N. SMALLEST VALUE IS RANKED 1,
C               LARGEST IS RANKED N. TIES ARE ASSIGNED AVERAGE OF TIED
C               RANKS
C           N - NUMBER OF VALUES
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           VECTOR IS SEARCHED FOR SUCCESSIVELY LARGER ELEMENTS. IF TIES
C           OCCUR, THEY ARE LOCATED AND THEIR RANK VALUE COMPUTED.
C           FOR EXAMPLE, IF 2 VALUES ARE TIED FOR SIXTH RANK, THEY ARE
C           ASSIGNED A RANK OF 6.5 (=(6+7)/2)
C
C     ..................................................................
C
      SUBROUTINE RANK(A,R,N)
      DIMENSION A(1),R(1)
C
C        INITIALIZATION
C
      DO 10 I=1,N
   10 R(I)=0.0
C
C        FIND RANK OF DATA
C
      DO 100 I=1,N
C
C        TEST WHETHER DATA POINT IS ALREADY RANKED
C
      IF(R(I)) 20, 20, 100
C
C        DATA POINT TO BE RANKED
C
   20 SMALL=0.0
      EQUAL=0.0
      X=A(I)
      DO 50 J=1,N
      IF(A(J)-X) 30, 40, 50
C        COUNT NUMBER OF DATA POINTS WHICH ARE SMALLER
C
C
   30 SMALL=SMALL+1.0
      GO TO 50
C
C        COUNT NUMBER OF DATA POINTS WHICH ARE EQUAL
C
   40 EQUAL=EQUAL+1.0
      R(J)=-1.0
   50 CONTINUE
C
C        TEST FOR TIE
C
      IF(EQUAL-1.0) 60, 60, 70
C
C        STORE RANK OF DATA POINT WHERE NO TIE
C
   60 R(I)=SMALL+1.0
      GO TO 100
C
C        CALCULATE RANK OF TIED DATA POINTS
C
   70 P=SMALL + (EQUAL + 1.0)*0.5
      DO 90 J=I,N
      IF(R(J)+1.0) 90, 80, 90
   80 R(J)=P
   90 CONTINUE
  100 CONTINUE
      RETURN
      END
