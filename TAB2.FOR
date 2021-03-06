C
C     ..................................................................
C
C	 SUBROUTINE TAB2
C
C	 PURPOSE
C	    PERFORM A TWO-WAY CLASSIFICATION FOR TWO VARIABLES IN AN
C	    OBSERVATION MATRIX (OR A MATRIX SUBSET) OF THE FREQUENCY,
C	    PERCENT FREQUENCY, AND OTHER STATISTICS OVER GIVEN CLASS
C	    INTERVALS.
C
C	 USAGE
C	    CALL TAB2(A,S,NOV,UBO,FREQ,PCT,STAT1,STAT2,NO,NV)
C
C	 DESCRIPTION OF PARAMETERS
C	    A	  - OBSERVATION MATRIX, NO BY NV
C	    S	  - INPUT VECTOR GIVING SUBSET OF A. ONLY THOSE
C		    OBSERVATIONS WITH A CORRESPONDING NON-ZERO S(J) ARE
C		    CONSIDERED. VECTOR LENGTH IS NO.
C	    NOV   - VARIABLES TO BE CROSS-TABULATED. NOV(1) IS VARIABLE
C			1, NOV(2) IS VARIABLE 2. VECTOR LENGTH IS 2. NOV
C			MUST BE GREATER THAN OR EQUAL TO 1 AND LESS THAN
C			OR EQUAL TO NV.
C	    UBO   - 3 BY 2 MATRIX GIVING LOWER LIMIT, NUMBER OF
C		    INTERVALS, AND UPPER LIMIT OF BOTH VARIABLES TO BE
C		    TABULATED (FIRST COLUMN FOR VARIABLE 1, SECOND
C		    COLUMN FOR VARIABLE 2). IF LOWER LIMIT IS EQUAL TO
C		    UPPER LIMIT FOR VARIABLE 1, THE PROGRAM USES THE
C		    MINIMUM AND MAXIMUM VALUES ON EACH VARIABLE. NUMBER
C		    OF INTERVALS MUST INCLUDE TWO CELLS FOR UNDER AND
C		    ABOVE LIMITS.
C	    FREQ  - OUTPUT MATRIX OF FREQUENCIES IN THE TWO-WAY
C		    CLASSIFICATION. ORDER OF MATRIX IS INT1 BY INT2,
C		    WHERE INT1 IS THE NUMBER OF INTERVALS OF VARIABLE 1
C		    AND INT2 IS THE NUMBER OF INTERVALS OF VARIABLE 2.
C		    INT1 AND INT2 MUST BE SPECIFIED IN THE SECOND
C		    POSITION OF RESPECTIVE COLUMN OF UBO MATRIX.
C	    PCT   - OUTPUT MATRIX OF PERCENT FREQUENCIES, SAME ORDER
C		    AS FREQ.
C	    STAT1 - OUTPUT MATRIX SUMMARIZING TOTALS, MEANS, AND
C		    STANDARD DEVIATIONS FOR EACH CLASS INTERVAL OF
C		    VARIABLE 1. ORDER OF MATRIX IS 3 BY INT1.
C	    STAT2 - SAME AS STAT1 BUT OVER VARIABLE 2. ORDER OF MATRIX
C		    IS 3 BY INT2.
C	    NO	  - NUMBER OF OBSERVATIONS. NO MUST BE GREATER THAN
C		    OR EQUAL TO 1.
C	    NV	  - NUMBER OF VARIABLES FOR EACH OBSERVATION. NV
C		    MUST BE GREATER THAN OR EQUAL TO 1.
C
C	 REMARKS
C	    IF S IS NULL, OUTPUT AREAS ARE SET TO ZERO
C
C	 SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C	    NONE
C
C	 METHOD
C	    INTERVAL SIZES FOR BOTH VARIABLES ARE CALCULATED FROM THE
C	    GIVEN INFORMATION OR OPTIONALLY FROM THE MINIMUM AND MAXIMUM
C	    VALUES. THE FREQUENCY AND PERCENT FREQUENCY MATRICES ARE
C	    DEVELOPED. MATRICES STAT1 AND STAT2 SUMMARIZING TOTALS,
C	    MEANS, AND STANDARD DEVIATIONS ARE THEN CALCULATED.
C	    THE DIVISOR FOR STANDARD DEVIATION IS ONE LESS THAN THE
C	    NUMBER OF OBSERVATIONS USED IN EACH CLASS INTERVAL.
C
C     ..................................................................
C
      SUBROUTINE TAB2(A,S,NOV,UBO,FREQ,PCT,STAT1,STAT2,NO,NV)
      DIMENSION A(1),S(1),NOV(2),UBO(3,2),FREQ(1),PCT(1),STAT1(1),
     1STAT2(2),SINT(2)
      DIMENSION WBO(3,2)
      DO 5 I=1,3
      DO 5 J=1,2
    5 WBO(I,J)=UBO(I,J)
C
C	 DETERMINE LIMITS
C
      DO 40 I=1,2
      IF(UBO(1,I)-UBO(3,I)) 40, 10, 40
   10 VMIN=1.0E38
      VMAX=-1.0E38
      IJ=NO*(NOV(I)-1)
      DO 35 J=1,NO
      IJ=IJ+1
      IF(S(J)) 15,35,15
   15 IF(A(IJ)-VMIN) 20,25,25
   20 VMIN=A(IJ)
   25 IF(A(IJ)-VMAX) 35,35,30
   30 VMAX=A(IJ)
   35 CONTINUE
      UBO(1,I)=VMIN
      UBO(3,I)=VMAX
   40 CONTINUE
C
C	 CALCULATE INTERVAL SIZE
C
   45 DO 50 I=1,2
   50 SINT(I)=ABS((UBO(3,I)-UBO(1,I))/(UBO(2,I)-2.0))
C
C        CLEAR OUTPUT AREAS
C
      INT1=UBO(2,1)
      INT2=UBO(2,2)
      INTT=INT1*INT2
      DO 55 I=1,INTT
      FREQ(I)=0.0
   55 PCT(I)=0.0
      INTY=3*INT1
      DO 60 I=1,INTY
   60 STAT1(I)=0.0
      INTZ=3*INT2
      DO 65 I=1,INTZ
   65 STAT2(I)=0.0
C
C        TEST SUBSET VECTOR
C
      SCNT=0.0
      INTY=INT1-1
      INTX=INT2-1
      IJ=NO*(NOV(1)-1)
      IJX=NO*(NOV(2)-1)
      DO 95 J=1,NO
      IJ=IJ+1
      IJX=IJX+1
      IF(S(J)) 70,95,70
   70 SCNT=SCNT+1.0
C
C        CALCULATE FREQUENCIES
C
      TEMP1=UBO(1,1)-SINT(1)
      DO 75 IY=1,INTY
      TEMP1=TEMP1+SINT(1)
      IF(A(IJ)-TEMP1) 80,75,75
   75 CONTINUE
      IY=INT1
   80 IYY=3*(IY-1)+1
      STAT1(IYY)=STAT1(IYY)+A(IJ)
      IYY=IYY+1
      STAT1(IYY)=STAT1(IYY)+1.0
      IYY=IYY+1
      STAT1(IYY)=STAT1(IYY)+A(IJ)*A(IJ)
      TEMP2=UBO(1,2)-SINT(2)
      DO 85 IX=1,INTX
      TEMP2=TEMP2+SINT(2)
      IF(A(IJX)-TEMP2) 90,85,85
   85 CONTINUE
      IX=INT2
   90 IJF=INT1*(IX-1)+IY
      FREQ(IJF)=FREQ(IJF)+1.0
      IX=3*(IX-1)+1
      STAT2(IX)=STAT2(IX)+A(IJX)
      IX=IX+1
      STAT2(IX)=STAT2(IX)+1.0
      IX=IX+1
      STAT2(IX)=STAT2(IX)+A(IJX)*A(IJX)
   95 CONTINUE
      IF (SCNT)98,151,98
C
C        CALCULATE PERCENT FREQUENCIES
C
   98 DO 100 I=1,INTT
  100 PCT(I)=FREQ(I)*100.0/SCNT
C
C        CALCULATE TOTALS, MEANS, STANDARD DEVIATIONS
C
      IXY=-1
      DO 120 I=1,INT1
      IXY=IXY+3
      ISD=IXY+1
      TEMP1=STAT1(IXY)
      SUM=STAT1(IXY-1)
      IF(TEMP1-1.0) 120,105,110
  105 STAT1(ISD)=0.0
      GO TO 115
  110 STAT1(ISD)=SQRT(ABS((STAT1(ISD)-SUM*SUM/TEMP1)/(TEMP1-1.0)))
  115 STAT1(IXY)=SUM/TEMP1
  120 CONTINUE
      IXX=-1
      DO 140 I=1,INT2
      IXX=IXX+3
      ISD=IXX+1
      TEMP2=STAT2(IXX)
      SUM=STAT2(IXX-1)
      IF(TEMP2-1.0) 140,125,130
  125 STAT2(ISD)=0.0
      GO TO 135
  130 STAT2(ISD)=SQRT(ABS((STAT2(ISD)-SUM*SUM/TEMP2)/(TEMP2-1.0)))
  135 STAT2(IXX)=SUM/TEMP2
  140 CONTINUE
      DO 150 I=1,3
      DO 150 J=1,2
  150 UBO(I,J)=WBO(I,J)
  151 RETURN
      END

      GO TO 115
  110 STAT1(ISD)=SQRT(ABS((STAT1(ISD)-SUM*SUM/TEMP1)/(TEMP1-1.0)))
  115 STAT1(IXY)=SUM/TEMP1
  120 CONT