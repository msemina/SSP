C
C     ..................................................................
C
C        SUBROUTINE TIE
C
C        PURPOSE
C           CALCULATE CORRECTION FACTOR DUE TO TIES
C
C        USAGE
C           CALL TIE(R,N,KT,T)
C
C        DESCRIPTION OF PARAMETERS
C           R  - INPUT VECTOR OF RANKS OF LENGTH N CONTAINING VALUES
C                1 TO N
C           N  - NUMBER OF RANKED VALUES
C           KT - INPUT CODE FOR CALCULATION OF CORRECTION FACTOR
C                      1   SOLVE EQUATION 1
C                      2   SOLVE EQUATION 2
C           T  - CORRECTION FACTOR (OUTPUT)
C                    EQUATION 1   T=SUM(CT**3-CT)/12
C                    EQUATION 2   T=SUM(CT*(CT-1)/2)
C                  WHERE CT IS THE NUMBER OF OBSERVATIONS TIED FOR A
C                        GIVEN RANK
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           VECTOR IS SEARCHED FOR SUCCESSIVELY LARGER RANKS. TIES ARE
C           COUNTED AND CORRECTION FACTOR 1 OR 2 SUMMED.
C
C     ..................................................................
C
      SUBROUTINE TIE(R,N,KT,T)
      DIMENSION R(1)
C
C        INITIALIZATION
C
      T=0.0
      Y=0.0
    5 X=1.0E38
      IND=0
C
C        FIND NEXT LARGEST RANK
C
      DO 30 I=1,N
      IF(R(I)-Y) 30,30,10
   10 IF(R(I)-X) 20,30,30
   20 X=R(I)
      IND=IND+1
   30 CONTINUE
C
C        IF ALL RANKS HAVE BEEN TESTED, RETURN
C
      IF(IND) 90,90,40
   40 Y=X
      CT=0.0
C
C        COUNT TIES
C
      DO 60 I=1,N
      IF(R(I)-X) 60,50,60
   50 CT=CT+1.0
   60 CONTINUE
C
C        CALCULATE CORRECTION FACTOR
C
      IF(CT) 70,5,70
   70 IF(KT-1) 75,80,75
   75 T=T+CT*(CT-1.)/2.0
      GO TO 5
   80 T=T+(CT*CT*CT-CT)/12.0
      GO TO 5
   90 RETURN
      END
