      SUBROUTINE SETBC()

      USE GENERAL_DATA, ONLY: IPERIODIC, JPERIODIC, KPERIODIC,
     >                        ISTART, JSTART, KSTART, IEND, JEND, KEND,
     >                        IMAX, JMAX, KMAX, ICMAX, JCMAX, KCMAX,
     >                        INFLOW, IOUTFLOW, NSTART, NADV, M,
     >                        NSCHEME
      USE VARIABLE_DATA, ONLY: Q, U, V, W, T, P, HF, H,
     >                         TIN, BOUND

      IMPLICIT NONE

      INTEGER :: I, J, K, I1, I2, J1, J2, K1, K2, NS
      REAL (KIND = 8) :: RKE
      REAL (KIND = 8), DIMENSION(1) :: EI

!# INFLOW
      IF ( ISTART == 1 .AND. .NOT. IPERIODIC ) THEN
         IF ( INFLOW == 1 .AND. NADV > NSTART-1 ) THEN

            I = 0

!# SETTING AT THE FAR GHOST POINTS INSTEAD OF PASSING THE INFORMATION.
!# THESE BOUNDS MUST MATCH THOSE IN CHAR_INFLOW()!
            IF ( .NOT. JPERIODIC ) THEN
               J1 = 1
               J2 = JCMAX
            ELSE
               IF ( NSCHEME .EQ. 2 ) THEN
                  J1 = 0
                  J2 = JCMAX + 1
               ELSE
                  J1 = -1
                  J2 = JCMAX + 2
               ENDIF
            ENDIF

            IF ( .NOT. KPERIODIC ) THEN
               K1 = 1
               K2 = KCMAX
            ELSE
               IF ( NSCHEME .EQ. 2 ) THEN
                  K1 = 0
                  K2 = KCMAX + 1
               ELSE
                  K1 = -1
                  K2 = KCMAX + 2
               ENDIF
            ENDIF

            DO K = K1, K2
               DO J = J1, J2
                  Q(I,J,K,1,M) = BOUND(J,K,1)
                  U(I,J,K)     = BOUND(J,K,2)
                  V(I,J,K)     = BOUND(J,K,3)
                  W(I,J,K)     = BOUND(J,K,4)
                  T(I,J,K)     = BOUND(J,K,5)

                  CALL THERMAL_PROPS ( I, I, J,K, HF(-2,-2,-2,1),
     >                           HF(-2,-2,-2,2), HF(-2,-2,-2,3),
     >                           HF(-2,-2,-2,4), T )

                  P(I,J,K)     = Q(I,J,K,1,M) * HF(I,J,K,4) * TIN(J,K)

                  RKE = 0.5D0 * (U(I,J,K) * U(I,J,K) +
     >                           V(I,J,K) * V(I,J,K) +
     >                           W(I,J,K) * W(I,J,K))

                  CALL INTERNAL_ENERGY ( I, I, HF(I,J,K,1),
     >                                  HF(I,J,K,2), HF(I,J,K,3),
     >                                  HF(I,J,K,4), T(I,J,K), EI(1) )

                  Q(I,J,K,2,M) = U(I,J,K) * Q(I,J,K,1,M)
                  Q(I,J,K,3,M) = V(I,J,K) * Q(I,J,K,1,M)
                  Q(I,J,K,4,M) = W(I,J,K) * Q(I,J,K,1,M)
                  Q(I,J,K,5,M) = Q(I,J,K,1,M) * (EI(1) + RKE)

               ENDDO
            ENDDO

         ELSE

            I = 0

            IF ( .NOT. JPERIODIC ) THEN
               J1 = 1
               J2 = JCMAX
            ELSE
               IF ( NSCHEME .EQ. 2 ) THEN
                  J1 = 0
                  J2 = JCMAX + 1
               ELSE
                  J1 = -1
                  J2 = JCMAX + 2
               ENDIF
            ENDIF

            IF ( .NOT. KPERIODIC ) THEN
               K1 = 1
               K2 = KCMAX
            ELSE
               IF ( NSCHEME .EQ. 2 ) THEN
                  K1 = 0
                  K2 = KCMAX + 1
               ELSE
                  K1 = -1
                  K2 = KCMAX + 2
               ENDIF
            ENDIF

            DO K = K1, K2
               DO J = J1, J2

!# SET BOUNDARY CONDITIONS

                  U(I,J,K) = U(I+1,J,K)
                  V(I,J,K) = V(I+1,J,K)
                  W(I,J,K) = W(I+1,J,K)
                  T(I,J,K) = T(I+1,J,K)
                  P(I,J,K) = P(I+1,J,K)

                  HF(I,J,K,1:4) = HF(I+1,J,K,1:4)

!# UPDATE CONSERVED VARIABLES

                  RKE = 0.5D0 * (U(I,J,K) * U(I,J,K) +
     >                           V(I,J,K) * V(I,J,K) +
     >                           W(I,J,K) * W(I,J,K))

                  CALL INTERNAL_ENERGY ( I, I,HF(I,J,K,1),
     >                                  HF(I,J,K,2), HF(I,J,K,3),
     >                                  HF(I,J,K,4), T(I,J,K), EI(1) )

                  Q(I,J,K,1,M) = P(I,J,K) / (HF(I,J,K,4) * T(I,J,K))
                  Q(I,J,K,2,M) = U(I,J,K) * Q(I,J,K,1,M)
                  Q(I,J,K,3,M) = V(I,J,K) * Q(I,J,K,1,M)
                  Q(I,J,K,4,M) = W(I,J,K) * Q(I,J,K,1,M)
                  Q(I,J,K,5,M) = Q(I,J,K,1,M) * (EI(1) + RKE)

               ENDDO
            ENDDO
         ENDIF
      ENDIF

!# OUTFLOW BOUNDARY
      IF ( IEND == IMAX - 1 .AND. .NOT. IPERIODIC ) THEN

         IF ( IOUTFLOW == 0 ) THEN

            I = ICMAX+1

            J1 = 1
            J2 = JCMAX

            K1 = 1
            K2 = KCMAX

            DO K = K1, K2
               DO J = J1, J2

!# SET BOUNDARY CONDITIONS

                  U(I,J,K) =  U(I-1,J,K)
                  V(I,J,K) =  V(I-1,J,K)
                  W(I,J,K) =  W(I-1,J,K)
                  T(I,J,K) =  T(I-1,J,K)
                  P(I,J,K) =  P(I-1,J,K)

!# UPDATE CONSERVED VARIABLES

                  RKE = 0.5D0 * (U(I,J,K) * U(I,J,K) +
     >                           V(I,J,K) * V(I,J,K) +
     >                           W(I,J,K) * W(I,J,K))

                  CALL INTERNAL_ENERGY ( I, I, HF(I,J,K,1),
     >                                  HF(I,J,K,2), HF(I,J,K,3),
     >                                  HF(I,J,K,4), T(I,J,K), EI(1) )

                  Q(I,J,K,1,M) = P(I,J,K) / (HF(I,J,K,4) * T(I,J,K))
                  Q(I,J,K,2,M) = U(I,J,K) * Q(I,J,K,1,M)
                  Q(I,J,K,3,M) = V(I,J,K) * Q(I,J,K,1,M)
                  Q(I,J,K,4,M) = W(I,J,K) * Q(I,J,K,1,M)
                  Q(I,J,K,5,M) = Q(I,J,K,1,M) * (EI(1) + RKE)

               ENDDO
            ENDDO
         ENDIF
      ENDIF

!# UPPER WALL
      IF ( JEND == JMAX-1 .AND. .NOT. JPERIODIC ) THEN

         J = JCMAX + 1

         IF ( IPERIODIC ) THEN
            IF ( NSCHEME .EQ. 2 ) THEN
               I1 = 0
               I2 = ICMAX + 1
            ELSE
               I1 = -1
               I2 = ICMAX + 2
            ENDIF
         ELSE
            IF ( NSCHEME .EQ. 2 ) THEN
               I1 = 0
               I2 = ICMAX + 1
            ELSE
               IF ( ISTART == 1 ) THEN
                  I1 = 0
               ELSE
                  I1 = -1
               ENDIF
               IF ( IEND == IMAX-1 ) THEN
                  I2 = ICMAX + 1
               ELSE
                  I2 = ICMAX + 2
               ENDIF
            ENDIF
         ENDIF

         IF ( KPERIODIC ) THEN
            IF ( NSCHEME .EQ. 2 ) THEN
               K1 = 0
               K2 = KCMAX + 1
            ELSE
               K1 = -1
               K2 = KCMAX + 2
            ENDIF
         ELSE
            IF ( NSCHEME .EQ. 2 ) THEN
               K1 = 0
               K2 = KCMAX + 1
            ELSE
               IF ( KSTART == 1 ) THEN
                  K1 = 0
               ELSE
                  K1 = -1
               ENDIF
               IF ( KEND == KMAX - 1 ) THEN
                  K2 = KCMAX + 1
               ELSE
                  K2 = KCMAX + 2
               ENDIF
            ENDIF
         ENDIF

         CALL WALLBC ( 2, I1, I2, J, J-1, K1, K2, +1.0D0 )
      ENDIF

!# LOWER WALL: SLIP REFLECTING BOUNDARY IN CYL COOR
      IF ( JSTART == 1 .AND. .NOT. JPERIODIC ) THEN

         J = 0

         IF ( IPERIODIC ) THEN
            IF ( NSCHEME .EQ. 2 ) THEN
               I1 = 0
               I2 = ICMAX + 1
            ELSE
               I1 = -1 
               I2 = ICMAX + 2
            ENDIF 
         ELSE     
            IF ( NSCHEME .EQ. 2 ) THEN
               I1 = 0
               I2 = ICMAX + 1
            ELSE
               IF ( ISTART == 1 ) THEN
                  I1 = 0
               ELSE 
                  I1 = -1
               ENDIF
               IF ( IEND == IMAX-1 ) THEN
                  I2 = ICMAX + 1
               ELSE
                  I2 = ICMAX + 2
               ENDIF
            ENDIF
         ENDIF

         IF ( KPERIODIC ) THEN
            IF ( NSCHEME .EQ. 2 ) THEN
               K1 = 0
               K2 = KCMAX + 1
            ELSE
               K1 = -1
               K2 = KCMAX + 2
            ENDIF
         ELSE
            IF ( NSCHEME .EQ. 2 ) THEN
               K1 = 0
               K2 = KCMAX + 1
            ELSE
               IF ( KSTART == 1 ) THEN
                  K1 = 0
               ELSE
                  K1 = -1
               ENDIF
               IF ( KEND == KMAX - 1 ) THEN
                  K2 = KCMAX + 1
               ELSE
                  K2 = KCMAX + 2
               ENDIF
            ENDIF
         ENDIF

         CALL WALLBC ( 2, I1, I2, J, J+1, K1, K2, +1.0D0 )
      ENDIF

!# SIDE WALL
      IF ( KEND == KMAX-1 .AND. .NOT. KPERIODIC ) THEN

         K = KCMAX + 1

         IF ( IPERIODIC ) THEN
            IF ( NSCHEME .EQ. 2 ) THEN
               I1 = 0
               I2 = ICMAX + 1
            ELSE
               I1 = -1
               I2 = ICMAX + 2
            ENDIF
         ELSE
            IF ( NSCHEME .EQ. 2 ) THEN
               I1 = 0
               I2 = ICMAX + 1
            ELSE
               IF ( ISTART == 1 ) THEN
                  I1 = 0
               ELSE
                  I1 = -1
               ENDIF
               IF ( IEND == IMAX - 1 ) THEN
                  I2 = ICMAX + 1
               ELSE
                  I2 = ICMAX + 2
               ENDIF
            ENDIF
         ENDIF

         IF ( JPERIODIC ) THEN
            IF ( NSCHEME .EQ. 2 ) THEN
               J1 = 0
               J2 = JCMAX + 1
            ELSE
               J1 = -1
               J2 = JCMAX + 2
            ENDIF
         ELSE
            IF ( NSCHEME .EQ. 2 ) THEN
               J1 = 0
               J2 = JCMAX + 1
            ELSE 
               IF ( JSTART == 1 ) THEN
                  J1 = 0
               ELSE
                  J1 = -1
               ENDIF
               IF ( JEND == JMAX - 1 ) THEN
                  J2 = JCMAX + 1
               ELSE
                  J2 = JCMAX + 2
               ENDIF
            ENDIF
         ENDIF

         CALL WALLBC ( 3, I1, I2, J1, J2, K, K-1, +1.0D0 )
      ENDIF

!# SIDE WALL
      IF ( KSTART == 1 .AND. .NOT. KPERIODIC ) THEN

         K = 0

         IF ( IPERIODIC ) THEN
            IF ( NSCHEME .EQ. 2 ) THEN
               I1 = 0
               I2 = ICMAX + 1
            ELSE
               I1 = -1
               I2 = ICMAX + 2
            ENDIF
         ELSE
            IF ( NSCHEME .EQ. 2 ) THEN
               I1 = 0
               I2 = ICMAX + 1
            ELSE 
               IF ( ISTART == 1 ) THEN
                  I1 = 0
               ELSE
                  I1 = -1
               ENDIF
               IF ( IEND == IMAX - 1 ) THEN
                  I2 = ICMAX + 1
               ELSE
                  I2 = ICMAX + 2
               ENDIF
            ENDIF
         ENDIF 

         IF ( JPERIODIC ) THEN
            IF ( NSCHEME .EQ. 2 ) THEN
               J1 = 0
               J2 = JCMAX + 1
            ELSE
               J1 = -1
               J2 = JCMAX + 2
            ENDIF
         ELSE
            IF ( NSCHEME .EQ. 2 ) THEN
               J1 = 0
               J2 = JCMAX + 1
            ELSE
               IF ( JSTART == 1 ) THEN
                  J1 = 0
               ELSE
                  J1 = -1
               ENDIF
               IF ( JEND == JMAX - 1 ) THEN
                  J2 = JCMAX + 1
               ELSE
                  J2 = JCMAX + 2
               ENDIF
            ENDIF
         ENDIF

         CALL WALLBC ( 3, I1, I2, J1, J2, K, K+1, +1.0D0 )
      ENDIF

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE WALLBC ( IOPT, IST, IND, JST, JND, KST, KND, VFACT )

      USE GENERAL_DATA, ONLY: ISGSK, ICHEM, NSPECI, M
      USE VARIABLE_DATA, ONLY: Q, U, V, W, T, P, HF, H

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IOPT, IST, IND, JST, JND, KST, KND

      INTEGER :: I, J, K, NS

      REAL ( KIND = 8 ) :: VFACT, RKE
      REAL (KIND = 8), DIMENSION(1) :: EI

      IF ( IOPT == 1 ) THEN

         DO K = KST, KND 
            DO J = JST, JND

!# SET BOUNDARY CONDITIONS BASED ON PRIMATIVE VARIABLES
               
               U(IST,J,K) =       - U(IND,J,K)
               V(IST,J,K) = VFACT * V(IND,J,K)
               W(IST,J,K) = VFACT * W(IND,J,K)

               T(IST,J,K) =  T(IND,J,K)
               P(IST,J,K) =  P(IND,J,K)

               HF(IST,J,K,1:4) = HF(IND,J,K,1:4)

!# UPDATE CONSERVED VARIABLES BASED ON NEW PRIMIATIVES

               RKE = 0.5D0 * (U(IST,J,K) * U(IST,J,K) +
     >                        V(IST,J,K) * V(IST,J,K) +
     >                        W(IST,J,K) * W(IST,J,K))

               CALL INTERNAL_ENERGY ( IST, IST, HF(IST,J,K,1),
     >                                HF(IST,J,K,2), HF(IST,J,K,3),
     >                                HF(IST,J,K,4), T(IST,J,K), EI(1) )

               Q(IST,J,K,1,M) = P(IST,J,K) /
     >                             (HF(IST,J,K,4) * T(IST,J,K))
               Q(IST,J,K,2,M) = U(IST,J,K) * Q(IST,J,K,1,M)
               Q(IST,J,K,3,M) = V(IST,J,K) * Q(IST,J,K,1,M)
               Q(IST,J,K,4,M) = W(IST,J,K) * Q(IST,J,K,1,M)
               Q(IST,J,K,5,M) = Q(IST,J,K,1,M) * (EI(1) + RKE)

            ENDDO
         ENDDO

      ELSEIF ( IOPT == 2 ) THEN

         DO K = KST, KND
            DO I = IST, IND

!# SET BOUNDARY CONDITIONS BASED ON PRIMATIVE VARIABLES

               U(I,JST,K) = VFACT * U(I,JND,K)
               V(I,JST,K) =       - V(I,JND,K)
               W(I,JST,K) = VFACT * W(I,JND,K)

               T(I,JST,K) =  T(I,JND,K)
               P(I,JST,K) =  P(I,JND,K)

               HF(I,JST,K,1:4) = HF(I,JND,K,1:4)

!# UPDATE CONSERVED VARIABLES BASED ON NEW PRIMIATIVES

               RKE = 0.5D0 * (U(I,JST,K) * U(I,JST,K) +
     >                        V(I,JST,K) * V(I,JST,K) +
     >                        W(I,JST,K) * W(I,JST,K))

               CALL INTERNAL_ENERGY ( I, I, HF(I,JST,K,1),
     >                                HF(I,JST,K,2), HF(I,JST,K,3),
     >                                HF(I,JST,K,4), T(I,JST,K), EI(1) )

               Q(I,JST,K,1,M) = P(I,JST,K) /
     >                             (HF(I,JST,K,4) * T(I,JST,K))
               Q(I,JST,K,2,M) = U(I,JST,K) * Q(I,JST,K,1,M)
               Q(I,JST,K,3,M) = V(I,JST,K) * Q(I,JST,K,1,M)
               Q(I,JST,K,4,M) = W(I,JST,K) * Q(I,JST,K,1,M)
               Q(I,JST,K,5,M) = Q(I,JST,K,1,M) * (EI(1) + RKE)

            ENDDO
         ENDDO

      ELSEIF ( IOPT == 3 ) THEN

         DO J = JST, JND
            DO I = IST, IND

!# SET BOUNDARY CONDITIONS BASED ON PRIMATIVE VARIABLES

               U(I,J,KST) = VFACT * U(I,J,KND)
               V(I,J,KST) = VFACT * V(I,J,KND)
               W(I,J,KST) =       - W(I,J,KND)

               T(I,J,KST) =  T(I,J,KND)
               P(I,J,KST) =  P(I,J,KND)

               HF(I,J,KST,1:4) = HF(I,J,KND,1:4)

!# UPDATE CONSERVED VARIABLES BASED ON NEW PRIMIATIVES

               RKE = 0.5D0 * (U(I,J,KST) * U(I,J,KST) +
     >                        V(I,J,KST) * V(I,J,KST) +
     >                        W(I,J,KST) * W(I,J,KST))

               CALL INTERNAL_ENERGY ( I, I, HF(I,J,KST,1),
     >                               HF(I,J,KST,2), HF(I,J,KST,3),
     >                               HF(I,J,KST,4), T(I,J,KST), EI(1) )

               Q(I,J,KST,1,M) = P(I,J,KST) /
     >                             (HF(I,J,KST,4) * T(I,J,KST))
               Q(I,J,KST,2,M) = U(I,J,KST) * Q(I,J,KST,1,M)
               Q(I,J,KST,3,M) = V(I,J,KST) * Q(I,J,KST,1,M)
               Q(I,J,KST,4,M) = W(I,J,KST) * Q(I,J,KST,1,M)
               Q(I,J,KST,5,M) = Q(I,J,KST,1,M) * (EI(1) + RKE)

            ENDDO
         ENDDO

      ELSE

         CALL EJECT ( 'INVALID INPUT FOR WALLBC: STOPPING' )

      ENDIF

      RETURN
      END
