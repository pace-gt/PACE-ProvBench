      SUBROUTINE SETIV()

      USE GENERAL_DATA, ONLY: ICMAX, JCMAX, KCMAX,
     >                        CNUK, PRT, CEPSK,
     >                        ND, NSPECI, INFLOW,
     >                        UREF, TREF, PREF, CONC0, RMUREF, RHOREF,
     >                        RNUREF, CREF, CMU, TPROD, TSTND,
     >                        WTAIR, GMAIR, CPAIR, CVAIR, RGAIR,
     >                        RUNIV, RMWT, HFK, CPK, CVK, RGK,
     >                        RM00, T00, P00, R00,
     >                        ASTR2, EFORM, ISTART
      USE VARIABLE_DATA, ONLY: Q, DQ, U, V, W, T, P, H,
     >                        HF, BOUND,
     >                        UIN, VIN, WIN, TIN,
     >                        URMS, VRMS, WRMS, X, Y, Z

      IMPLICIT NONE

      INTEGER :: I, J, K, L, LL, LM, NN, NS
      REAL (KIND = 8) :: CP, CV, RC, DH, GAMMA, RMOLWT, GM1, GP1, GM2
      REAL (KIND = 8) :: IE, KE

      DO LL = 1,2
        DO K = -2, KCMAX+3
          DO J = -2, JCMAX+3
            DO I = -2, ICMAX+3
              Q(I,J,K,1,LL) = 1.0D+00
              DQ(I,J,K,1)   = 0.0D+00
            END DO
          END DO
        END DO
        DO LM = 2,ND
          DO K = -2, KCMAX+3 
            DO J = -2, JCMAX+3
              DO I = -2, ICMAX+3
                Q(I,J,K,LM,LL) = 0.0D+00
                DQ(I,J,K,LM)   = 0.0D+00
              END DO
            END DO
          END DO
        END DO
      END DO

      DO K = -2, KCMAX+3 
        DO J = -2, JCMAX+3
          DO I = -2, ICMAX+3
            U(I,J,K)    = UREF
            V(I,J,K)    = 0.0D+00
            W(I,J,K)    = 0.0D+00
            P(I,J,K)    = PREF
            T(I,J,K)    = TREF

           ENDDO
         ENDDO
      ENDDO

      IF ( INFLOW .EQ. 1 ) THEN 
         DO K = -2, KCMAX+3
            DO J = -2, JCMAX+3
               DO L = 1, ND
                  BOUND(J,K,L) = 0.0
               ENDDO
               UIN(J,K)  = 0.0 
               VIN(J,K)  = 0.0 
               WIN(J,K)  = 0.0 
               TIN(J,K)  = 0.0 
               URMS(J,K) = 0.0 
               VRMS(J,K) = 0.0 
               WRMS(J,K) = 0.0 
            ENDDO
         ENDDO
      ENDIF

      RMOLWT= WTAIR
      GAMMA = GMAIR
      RC    = RGAIR
      CP    = CPAIR
      CV    = CVAIR
      DH    = 0.0D0

      CREF   = SQRT(GAMMA * RC * TREF)
      RHOREF = PREF / (RC * TREF)
      RMUREF = RNUREF * RHOREF
      CMU    = RMUREF * (TREF + 110.0) / (TREF * SQRT(TREF))

      GM1 = 1.0D+00 / (GAMMA - 1.0D+00)
      GP1 = GAMMA + 1.0D+00
      GM2 = GAMMA * GM1

      RM00 = UREF / CREF
      T00  = TREF * (1.0 + (GAMMA - 1.0) * RM00**2 * 0.5D+00)
      P00  = PREF * (T00 / TREF)**GM2
      R00  = P00 / (RC * T00)
      ASTR2 = (2.0 / GP1) * GAMMA * (GAMMA - 1.0) * CV * T00

      EFORM = CP * (TPROD-TREF)

      DO K = -2,KCMAX+3
        DO J = -2,JCMAX+3
          DO I = -2,ICMAX+3
            HF(I,J,K,1) = DH
            HF(I,J,K,2) = CP
            HF(I,J,K,3) = CV
            HF(I,J,K,4) = RC
          END DO
        END DO
      END DO

      CALL TML ( )

      DO K = -2, KCMAX+3
         DO J = -2, JCMAX+3
            DO I = -2, ICMAX+3

               CALL THERMAL_PROPS ( I, I, J, K, HF(-2,-2,-2,1),
     >                              HF(-2,-2,-2,2), HF(-2,-2,-2,3),
     >                              HF(-2,-2,-2,4), T )

               IE = CVAIR * T(I,J,K)
               KE = 0.5D0 * (U(I,J,K)**2 + V(I,J,K)**2 + W(I,J,K)**2)

               Q(I,J,K,1,1) = P(I,J,K) / (HF(I,J,K,4) * T(I,J,K))
               Q(I,J,K,2,1) = Q(I,J,K,1,1) * U(I,J,K)
               Q(I,J,K,3,1) = Q(I,J,K,1,1) * V(I,J,K)
               Q(I,J,K,4,1) = Q(I,J,K,1,1) * W(I,J,K)
               Q(I,J,K,5,1) = Q(I,J,K,1,1) * (IE + KE)

               Q(I,J,K,:,2) = Q(I,J,K,:,1)

            ENDDO

            IF ( ISTART .EQ. 1 .AND. INFLOW .EQ. 1 ) THEN
               TIN(J,K) = T(0,J,K)
               UIN(J,K) = U(0,J,K)
               VIN(J,K) = V(0,J,K)
               WIN(J,K) = W(0,J,K)
            ENDIF

         ENDDO
      ENDDO

      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE TML ()

      USE GENERAL_DATA, ONLY: IMAX, JMAX, KMAX, XLEN, YLEN, ZLEN,
     >                        UREF, ICMAX, JCMAX, KCMAX, ISTART, JSTART,
     >                        KSTART, PERT3D, COEF_AMP, IAM
      USE VARIABLE_DATA, ONLY: U, V, W

      IMPLICIT NONE

      INTEGER :: LMAX, LC

      REAL (KIND = 8), ALLOCATABLE :: UM(:),UT(:,:,:),SECJ(:)
      REAL (KIND = 8), ALLOCATABLE, DIMENSION(:) :: AEX,BEX
      REAL (KIND = 8), ALLOCATABLE, DIMENSION(:) :: EPS,ALPA,PHASE,
     >                                              RK1,RK2,XPHS
      REAL (KIND = 8), ALLOCATABLE, DIMENSION(:) :: SK1,SK2,TK1,TK2
      REAL (KIND = 8), ALLOCATABLE, DIMENSION(:,:,:) :: SI, SI3, COX
      REAL (KIND = 8), ALLOCATABLE, DIMENSION(:) :: YA,YB

      INTEGER :: I, J, K, L, II, JJ, KK, L2
      REAL (KIND = 8) :: R, S, C, TT, DI, DJ, DK, BETA, DLET, XZRO, EL,
     >                   COEF, RIC, EQ, XLEN1, YLEN1, ZLEN1, PI, FQ

      LMAX = MAX(IMAX,JMAX)
      ALLOCATE( UM(-2:LMAX+2),UT(-2:LMAX+2,4,3),SECJ(-2:LMAX+2),
     >         AEX(3),BEX(3),
     >         EPS(3),ALPA(3),PHASE(3),RK1(3),RK2(3),XPHS(3),
     >         SK1(3),SK2(3),TK1(3),TK2(3),
     >         SI(-2:LMAX+2,2,3),SI3(-2:LMAX+2,2,3),
     >         COX(-2:LMAX+2,2,3),
     >         YA(-2:LMAX+2),YB(-2:LMAX+2))

      EPS(1) = 0.130 * COEF_AMP
      EPS(2) = 0.130 * COEF_AMP
      EPS(3) = 0.130 * COEF_AMP * PERT3D
      IF (IAM == 0) WRITE(*,*) "EPS",SNGL(EPS(1:3))

      RK1(1) = 0.475
      RK1(2) = 0.350
      RK1(3) = 0.475

      RK2(1) = 0.750
      RK2(2) = 0.600
      RK2(3) = 0.750

      ALPA(1) = 0.4446
      ALPA(2) = 0.2223
      ALPA(3) = 0.11115

      PHASE(1) = 0.0
      PHASE(2) = 1.57076
      PHASE(3) = 4.712388

      XPHS(1) = 1.57076
      XPHS(2) = 0.0
      XPHS(3) = 0.0

      BETA = 2.0 * ALPA(1)
      DLET = 0.1
      XZRO = 0.25

      LC = 4

      PI = 4.D0 * DATAN(1.D0)
      XLEN1 = dble(LC) * pi
      YLEN1 = dble(LC) * pi
      ZLEN1 = dble(LC) * pi

      DI = XLEN1 / DBLE(IMAX-1)
      DJ = YLEN1 / DBLE(JMAX-1)
      IF ( KMAX .GT. 2 ) THEN
         DK = ZLEN1 / DBLE(KMAX-1)
      ELSE
         DK = 0.0D+00
      ENDIF

      L2   = ((JMAX-1)/2 + 1)
      EL   = 1.0 / ALPA(1)
      COEF = SQRT(2.0 / XZRO**2)

      DO J = -2,JMAX+2
         S   =  (J - L2)* DJ * EL
         R   =  (J - 2) * DI * EL
         TT  =  (J - 2) * DK * EL
         C   = EXP(S)
         RIC = 1.0 / C
         UM(J) = 0.5 * UREF * (C - RIC) / (C + RIC)

         DO L = 1,3
            AEX(L)     = EXP(RK1(L) * S)
            BEX(L)     = EXP(RK2(L) * S)
            SI(J,1,L)  = COS(ALPA(L) * R + PHASE(L))
            SI(J,2,L)  = SIN(ALPA(L) * R + PHASE(L))
            SI3(J,1,L) = COS(ALPA(L)*R + XPHS(L))
            SI3(J,2,L) = SIN(ALPA(L)*R + XPHS(L))

            COX(J,1,1)   = COS(BETA*TT)
            SK1(L)       = 2.0/(AEX(L) + 1.0/AEX(L))
            SK2(L)       = 2.0/(BEX(L) + 1.0/BEX(L))
            TK2(L)       = (BEX(L)-1.0/BEX(L))/(BEX(L)+1.0/BEX(L))
            UT(J,1,L) = -RK1(L)*SK1(L)*(AEX(L) - 1.0/AEX(L))/
     >                                  (AEX(L) + 1.0/AEX(L))
            UT(J,2,L) =  RK2(L)*SK2(L)*(TK2(L)**2-SK2(L)**2)
            UT(J,3,L) = -ALPA(L) * SK1(L)
            UT(J,4,L) = -ALPA(L) * SK2(L) * TK2(L)
         END DO
      END DO

      IF (PERT3D == 0) THEN
!------------------
! 2-D PERTURBATIONS
!------------------
      DO K = -2, KCMAX+2
         DO J = -2, JCMAX+2
            DO I = -2, ICMAX+2
               JJ = J + JSTART - 1
               II = I + ISTART - 1

               EQ = EL * UREF
               U(I,J,K) = UM(JJ) -
     >            EQ * EPS(1) * (UT(JJ,1,1) * SI(II,1,1) +
     >                           UT(JJ,2,1) * SI(II,2,1)) -
     >            EQ * EPS(2) * (UT(JJ,1,2) * SI(II,1,2) +
     >                           UT(JJ,2,2) * SI(II,2,2)) -
     >            EQ * EPS(3) * (UT(JJ,1,3) * SI(II,1,3) +
     >                           UT(JJ,2,3) * SI(II,2,3))

               V(I,J,K) =
     >            EQ * EPS(1) * (UT(JJ,3,1) * SI(II,2,1) +
     >                           UT(JJ,4,1) * SI(II,1,1)) +
     >            EQ * EPS(2) * (UT(JJ,3,2) * SI(II,2,2) +
     >                           UT(JJ,4,2) * SI(II,1,2)) +
     >            EQ * EPS(3) * (UT(JJ,3,3) * SI(II,2,3) +
     >                           UT(JJ,4,3) * SI(II,1,3))

               W(I,J,K) = 0.0
            END DO
         END DO
      END DO
      ELSE
!-------------------
! 3-D  PERTURBATIONS
!-------------------
      DO K = -2,KCMAX+2
         KK = K + KSTART - 1
         DO J = -2,JCMAX+2
            JJ = J + JSTART - 1
            DO I = -2,ICMAX+2
               II = I + ISTART - 1

               EQ = EL * UREF
cmm Only valid for a cube grid (JMAX=KMAX)
               FQ = EQ * DLET * COX(KK,1,1)

               U(I,J,K) = UM(JJ) -
     >            EQ * EPS(1) * (UT(JJ,1,1) * SI(II,1,1) +
     >                           UT(JJ,2,1) * SI(II,2,1)) -
     >            EQ * EPS(2) * (UT(JJ,1,2) * SI(II,1,2) +
     >                           UT(JJ,2,2) * SI(II,2,2)) -
     >            EQ * EPS(3) * (UT(JJ,1,3) * SI(II,1,3) +
     >                           UT(JJ,2,3) * SI(II,2,3)) -
     >            FQ * EPS(1) * (UT(JJ,1,1) * SI3(II,1,1) +
     >                           UT(JJ,2,1) * SI3(II,2,1)) -
     >            FQ * EPS(2) * (UT(JJ,1,2) * SI3(II,1,2) +
     >                           UT(JJ,2,2) * SI3(II,2,2)) -
     >            FQ * EPS(3) * (UT(JJ,1,3) * SI3(II,1,3) +
     >                           UT(JJ,2,3) * SI3(II,2,3))

               V(I,J,K) =
     >            EQ * EPS(1) * (UT(JJ,3,1) * SI(II,2,1) +
     >                           UT(JJ,4,1) * SI(II,1,1)) +
     >            EQ * EPS(2) * (UT(JJ,3,2) * SI(II,2,2) +
     >                           UT(JJ,4,2) * SI(II,1,2)) +
     >            EQ * EPS(3) * (UT(JJ,3,3) * SI(II,2,3) +
     >                           UT(JJ,4,3) * SI(II,1,3)) +
     >            FQ * EPS(1) * (UT(JJ,3,1) * SI3(II,2,1) +
     >                           UT(JJ,4,1) * SI3(II,1,1)) +
     >            FQ * EPS(2) * (UT(JJ,3,2) * SI3(II,2,2) +
     >                           UT(JJ,4,2) * SI3(II,1,2)) +
     >            FQ * EPS(3) * (UT(JJ,3,3) * SI3(II,2,3) +
     >                           UT(JJ,4,3) * SI3(II,1,3))

               W(I,J,K)  = 0.0
            END DO
         END DO
      END DO
      ENDIF

      DEALLOCATE( UM,UT,SECJ,
     >         AEX,BEX,
     >         EPS,ALPA,PHASE,RK1,RK2,XPHS,
     >         SK1,SK2,TK1,TK2,
     >         SI,SI3,
     >         COX,
     >         YA,YB)

      RETURN
      END
