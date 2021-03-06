C
C     ..................................................................
C
C	 FUNCTION RECP
C
C	 PURPOSE
C	    CALCULATE RECIPROCAL OF AN ELEMENT. THIS IS A FORTRAN
C	    FUNCTION SUBPROGRAM WHICH MAY BE USED AS AN ARGUMENT BY
C	    SUBROUTINE MFUN.
C
C	 USAGE
C	    RECP(E)
C
C	 DESCRIPTION OF PARAMETERS
C	    E - MATRIX ELEMENT
C
C	 REMARKS
C	    RECIPROCAL OF ZERO IS TAKEN TO BE 1.0E75
C
C	 SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C	    NONE
C
C	 METHOD
C	    RECIPROCAL OF ELEMENT E IS PLACED IN RECP
C
C     ..................................................................
C
      FUNCTION RECP(E)
C
      BIG=1.0E38
C
C	 TEST ELEMENT FOR ZERO
C
      IF(E) 1,2,1
C
C        IF NON-ZERO, CALCULATE RECIPROCAL
C
    1 RECP=1.0/E
      RETURN
C
C        IF ZERO, SET EQUAL TO INFINITY
C
    2 RECP=SIGN(BIG,E)
      RETURN
      END

