C
C     ..................................................................
C
C        SUBROUTINE MOMEN
C
C        PURPOSE
C           TO FIND THE THE FIRST FOUR MOMENTS FOR GROUPED DATA ON
C           EQUAL CLASS INTERVALS.
C
C        USAGE
C           CALL MOMEN (F,UBO,NOP,ANS)
C
C        DESCRIPTION OF PARAMETERS
C           F   - GROUPED DATA (FREQUENCIES).  GIVEN AS A VECTOR OF
C                 LENGTH (UBO(3)-UBO(1))/UBO(2)
C           UBO - 3 CELL VECTOR, UBO(1) IS LOWER BOUND AND UBO(3) UPPER
C                 BOUND ON DATA.  UBO(2) IS CLASS INTERVAL.  NOTE THAT
C                 UBO(3) MUST BE GREATER THAN UBO(1).
C           NOP - OPTION PARAMETER.  IF NOP = 1, ANS(1) = MEAN.  IF
C                 NOP = 2, ANS(2) = SECOND MOMENT.  IF NOP = 3, ANS(3) =
C                 THIRD MOMENT.  IF NOP = 4, ANS(4) = FOURTH MOMENT.
C                 IF NOP = 5, ALL FOUR MOMENTS ARE FILLED IN.
C           ANS - OUTPUT VECTOR OF LENGTH 4 INTO WHICH MOMENTS ARE PUT.
C
C        REMARKS
C           NOTE THAT THE FIRST MOMENT IS NOT CENTRAL BUT THE VALUE OF
C           THE MEAN ITSELF.  THE MEAN IS ALWAYS CALCULATED.  MOMENTS
C           ARE BIASED AND NOT CORRECTED FOR GROUPING.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           REFER TO M. G. KENDALL, 'THE ADVANCED THEORY OF STATISTICS',
C           V.1, HAFNER PUBLISHING COMPANY, 1958, CHAPTER 3.
C
C     ..................................................................
C
      SUBROUTINE MOMEN (F,UBO,NOP,ANS)
      DIMENSION F(1),UBO(1),ANS(1)
C
      DO 100 I=1,4
  100 ANS(I)=0.0
C
C     CALCULATE THE NUMBER OF CLASS INTERVALS
C
      N=(UBO(3)-UBO(1))/UBO(2)+0.5
C
C     CALCULATE TOTAL FREQUENCY
C
      T=0.0
      DO 110 I=1,N
  110 T=T+F(I)
C
      IF(NOP-5) 130, 120, 115
  115 NOP=5
  120 JUMP=1
      GO TO 150
  130 JUMP=2
C
C        FIRST MOMENT
C
  150 DO 160 I=1,N
      FI=I
  160 ANS(1)=ANS(1)+F(I)*(UBO(1)+(FI-0.5)*UBO(2))
      ANS(1)=ANS(1)/T
C
      GO TO (350,200,250,300,200), NOP
C
C        SECOND MOMENT
C
  200 DO 210 I=1,N
      FI=I
  210 ANS(2)=ANS(2)+F(I)*(UBO(1)+(FI-0.5)*UBO(2)-ANS(1))**2
      ANS(2)=ANS(2)/T
      GO TO (250,350), JUMP
C
C        THIRD MOMENT
C
  250 DO 260 I=1,N
      FI=I
  260 ANS(3)=ANS(3)+F(I)*(UBO(1)+(FI-0.5)*UBO(2)-ANS(1))**3
      ANS(3)=ANS(3)/T
      GO TO (300,350), JUMP
C
C        FOURTH MOMENT
C
  300 DO 310 I=1,N
      FI=I
  310 ANS(4)=ANS(4)+F(I)*(UBO(1)+(FI-0.5)*UBO(2)-ANS(1))**4
      ANS(4)=ANS(4)/T
  350 RETURN
      END
