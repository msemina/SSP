C
C.......................................................................
C
C        SUBROUTINE NDTR
C
C        PURPOSE
C           COMPUTES Y = P(X) = PROBABILITY THAT THE RANDOM VARIABLE  U,
C           DISTRIBUTED NORMALLY(0,1), IS LESS THAN OR EQUAL TO X.
C           F(X), THE ORDINATE OF THE NORMAL DENSITY AT X, IS ALSO
C           COMPUTED.
C
C        USAGE
C           CALL NDTR(X,P,D)
C
C        DESCRIPTION OF PARAMETERS
C           X--INPUT SCALAR FOR WHICH P(X) IS COMPUTED.
C           P--OUTPUT PROBABILITY.
C           D--OUTPUT DENSITY.
C
C        REMARKS
C           MAXIMUM ERROR IS 0.0000007.
C
C        SUBROUTINES AND SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           BASED ON APPROXIMATIONS IN C. HASTINGS, APPROXIMATIONS FOR
C           DIGITAL COMPUTERS, PRINCETON UNIV. PRESS, PRINCETON, N.J.,
C           1955.  SEE EQUATION 26.2.17, HANDBOOK OF MATHEMATICAL
C           FUNCTIONS, ABRAMOWITZ AND STEGUN, DOVER PUBLICATIONS, INC.,
C           NEW YORK.
C
C.......................................................................
C
      SUBROUTINE NDTR(X,P,D)
C
      AX=ABS(X)
      T=1.0/(1.0+.2316419*AX)
      D=0.3989423*EXP(-X*X/2.0)
      P = 1.0 - D*T*((((1.330274*T - 1.821256)*T + 1.781478)*T -
     1  0.3565638)*T + 0.3193815)
      IF(X)1,2,2
    1 P=1.0-P
    2 RETURN
      END
