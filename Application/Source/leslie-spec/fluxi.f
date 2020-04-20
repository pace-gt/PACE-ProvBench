      SUBROUTINE FLUXI()

      USE GENERAL_DATA, ONLY: ICMAX, JCMAX, KCMAX, ND, NSCHEME,
     >                        IADD, VISCOUS
      USE VARIABLE_DATA, ONLY: UAV, VAV, WAV, SIX, SIY, SIZ,
     >                         U, V, W, QAV, PAV, DQ, DTV

      IMPLICIT NONE

      REAL (KIND = 8), ALLOCATABLE :: QS(:), FSI(:,:)

      INTEGER :: ERROR

      INTEGER :: I, J, K, L

      REAL (KIND = 8) :: QSP, QSPI

      ALLOCATE ( QS(0:ICMAX), FSI(0:ICMAX,ND), STAT = ERROR )
      IF ( ERROR .NE. 0 ) CALL EJECT ('ALLOCATION ERROR: FLUXI')

      CALL EXTRAPI ( )

      DO K = 1, KCMAX
         DO J = 1, JCMAX

            QS(0:ICMAX) = UAV(0:ICMAX,J,K) * SIX(0:ICMAX,J,K) +
     >                    VAV(0:ICMAX,J,K) * SIY(0:ICMAX,J,K) +
     >                    WAV(0:ICMAX,J,K) * SIZ(0:ICMAX,J,K)

            IF ( NSCHEME .EQ. 2 ) THEN
               DO I = 0, ICMAX
                  L = I + 1 - IADD
                  QSP = U(L,J,K) * SIX(I,J,K) +
     >                  V(L,J,K) * SIY(I,J,K) +
     >                  W(L,J,K) * SIZ(I,J,K)
                  QSPI = (QSP - QS(I)) * DBLE(1 - 2 * IADD)
                  IF ( QSPI .GT. 0.0D0 ) QS(I) = 0.5D0 * (QS(I) + QSP)
               ENDDO
            ENDIF

            FSI(0:ICMAX,1) =  QAV(0:ICMAX,J,K,1) * QS(0:ICMAX)
            FSI(0:ICMAX,2) =  QAV(0:ICMAX,J,K,2) * QS(0:ICMAX) +
     >                              PAV(0:ICMAX,J,K) * SIX(0:ICMAX,J,K)
            FSI(0:ICMAX,3) =  QAV(0:ICMAX,J,K,3) * QS(0:ICMAX) +
     >                              PAV(0:ICMAX,J,K) * SIY(0:ICMAX,J,K)
            FSI(0:ICMAX,4) =  QAV(0:ICMAX,J,K,4) * QS(0:ICMAX) +
     >                              PAV(0:ICMAX,J,K) * SIZ(0:ICMAX,J,K)
            FSI(0:ICMAX,5) = (QAV(0:ICMAX,J,K,5) + PAV(0:ICMAX,J,K)) *
     >                                             QS(0:ICMAX)

            IF ( VISCOUS ) CALL VISCI ( 0, ICMAX, J, K, FSI )

! SUM UP F \dot A ON I-FACES

            DO L = 1, 5
               DO I = 1, ICMAX
                  DQ(I,J,K,L) = -DTV(I,J,K) * (FSI(I,L) - FSI(I-1,L))
               ENDDO
            ENDDO

         ENDDO
      ENDDO

      DEALLOCATE ( QS, FSI )

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE EXTRAPI ( )

      USE GENERAL_DATA, ONLY: ICMAX, JCMAX, KCMAX, NSCHEME,
     >                        IPERIODIC, JPERIODIC, KPERIODIC,
     >                        ISTART, JSTART, KSTART,
     >                        IMAX, JMAX, KMAX,
     >                        EFORM, ZERO, HFK, CPK, CVK, RGK,
     >                        CPAIR, CVAIR, RGAIR
      USE VARIABLE_DATA, ONLY: TAV, QAV, UAV, VAV, WAV, HAV, PAV

      IMPLICIT NONE

      INTEGER :: I1, I2, J1, J2, K1, K2, I, J, K, L, IGL1, IGL2

      REAL (KIND = 8) :: KE

      IF(NSCHEME .EQ. 2) THEN
         K1 = 0
         K2 = KCMAX + 1

         J1 = 0
         J2 = JCMAX + 1
      ELSE
         IF(KPERIODIC) THEN
            K1 = -1
            K2 = KCMAX + 2
         ELSE
            K1 = MAX(     -1, 0    - KSTART + 1)
            K2 = MIN(KCMAX+2, KMAX - KSTART + 1)
         ENDIF

         IF(JPERIODIC) THEN
            J1 = -1
            J2 = JCMAX + 2
         ELSE
            J1 = MAX(     -1, 0    - JSTART + 1)
            J2 = MIN(JCMAX+2, JMAX - JSTART + 1)
         ENDIF
      ENDIF

      DO K = K1, K2
         DO J = J1, J2

            IF ( NSCHEME .EQ. 2 ) THEN
               CALL EXI2 ( 0, ICMAX, J, K )
            ELSE
               IF ( IPERIODIC ) THEN
                  CALL EXI4 ( 0, ICMAX, J, K)
               ELSE
                  I1 = MAX(    0, 1        - ISTART + 1)
                  I2 = MIN(ICMAX, IMAX - 2 - ISTART + 1)

                  CALL EXI4 ( I1, I2, J, K)

                  I = 0 - ISTART + 1
                  IF ( I .GE. 0 .AND. I .LE. ICMAX )
     >                                       CALL EXI2 ( I, I, J, K )

                  I = IMAX - 1 - ISTART + 1
                  IF ( I .GE. 0 .AND. I .LE. ICMAX )
     >                                       CALL EXI2 ( I, I, J, K )
               ENDIF
            ENDIF
         ENDDO
      ENDDO

      DO K = K1, K2
         DO J = J1, J2
            DO I = 0, ICMAX
               KE = 0.5D+00 * (UAV(I,J,K) * UAV(I,J,K) +
     >                         VAV(I,J,K) * VAV(I,J,K) +
     >                         WAV(I,J,K) * WAV(I,J,K))

               QAV(I,J,K,2) = QAV(I,J,K,1) * UAV(I,J,K)
               QAV(I,J,K,3) = QAV(I,J,K,1) * VAV(I,J,K)
               QAV(I,J,K,4) = QAV(I,J,K,1) * WAV(I,J,K)
               QAV(I,J,K,5) = QAV(I,J,K,1) * (CVAIR * TAV(I,J,K) + KE)

               PAV(I,J,K)   = QAV(I,J,K,1) * RGAIR * TAV(I,J,K)

            ENDDO
         ENDDO
      ENDDO

      RETURN
      END
!---------------------------------------------------------------------
      SUBROUTINE EXI2 (IST, IND, J, K)

      USE GENERAL_DATA, ONLY: IADD, N, ND
      USE VARIABLE_DATA, ONLY: QAV, UAV, VAV, WAV, TAV,
     >                         Q, U, V, W, T

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: IST, IND, J, K

      INTEGER :: I, II

      IF ( IADD .EQ. 1 ) THEN
         QAV(IST:IND,J,K,1) = Q(IST+1:IND+1,J,K,1,N)
         UAV(IST:IND,J,K)   = U(IST+1:IND+1,J,K)
         VAV(IST:IND,J,K)   = V(IST+1:IND+1,J,K)
         WAV(IST:IND,J,K)   = W(IST+1:IND+1,J,K)
         TAV(IST:IND,J,K)   = T(IST+1:IND+1,J,K)
      ELSE
         QAV(IST:IND,J,K,1) = Q(IST:IND,J,K,1,N)
         UAV(IST:IND,J,K)   = U(IST:IND,J,K)
         VAV(IST:IND,J,K)   = V(IST:IND,J,K)
         WAV(IST:IND,J,K)   = W(IST:IND,J,K)
         TAV(IST:IND,J,K)   = T(IST:IND,J,K)
      ENDIF

      RETURN
      END
!---------------------------------------------------------------------
      SUBROUTINE EXI4 ( IST, IND, J, K )

      USE GENERAL_DATA, ONLY: IADD, IBDD, N
      USE VARIABLE_DATA, ONLY: DS, DS1, QAV, Q, UAV, VAV, WAV, TAV,
     >                         U, V, W, T

      IMPLICIT NONE

      INTEGER I, J, K, NS, IST, IND, II1, II2, IB1, IB2, IC1, IC2, L

      REAL (KIND = 8) :: AF(IST:IND), BF(IST:IND), CF(IST:IND)

      REAL (KIND = 8), PARAMETER :: R23 = 2.0D0 / 3.0D0,
     >                              R13 = 1.0D0 / 3.0D0

      IF ( IADD .EQ. 1 ) THEN
         DO I = IST, IND
            AF(I) = R23 * (1.0D0 - DS(I,J,K,1) * DS1(I,J,K,1))
            BF(I) = R13 * (1.0D0 + DS(I+1,J,K,1) * DS1(I+1,J,K,1) +
     >                   2.0D0 * DS(I,J,K,1) * DS1(I,J,K,1))
            CF(I) = -R13 * DS(I+1,J,K,1) * DS1(I+1,J,K,1)
         ENDDO
      ELSE
         DO I = IST, IND
            AF(I) = R23 * (1.0D0 - DS(I+1,J,K,1) * DS1(I,J,K,1))
            BF(I) = R13 * (1.0D0 + DS(I,J,K,1) * DS1(I-1,J,K,1) +
     >                       2.0D0 * DS(I+1,J,K,1) * DS1(I,J,K,1))
            CF(I) = -R13 * DS(I,J,K,1) * DS1(I-1,J,K,1)
         ENDDO
      ENDIF

      II1 = IST + IADD
      II2 = IND + IADD

      IB1 = II1 - IBDD
      IB2 = II2 - IBDD

      IC1 = II1 + IBDD
      IC2 = II2 + IBDD
 
      QAV(IST:IND,J,K,1) = BF(IST:IND) * Q(II1:II2,J,K,1,N) +
     >                     AF(IST:IND) * Q(IB1:IB2,J,K,1,N) +
     >                     CF(IST:IND) * Q(IC1:IC2,J,K,1,N)
 
      UAV(IST:IND,J,K)   = BF(IST:IND) * U(II1:II2,J,K) +
     >                     AF(IST:IND) * U(IB1:IB2,J,K) +
     >                     CF(IST:IND) * U(IC1:IC2,J,K)
 
      VAV(IST:IND,J,K)   = BF(IST:IND) * V(II1:II2,J,K) +
     >                     AF(IST:IND) * V(IB1:IB2,J,K) +
     >                     CF(IST:IND) * V(IC1:IC2,J,K)
 
      WAV(IST:IND,J,K)   = BF(IST:IND) * W(II1:II2,J,K) +
     >                     AF(IST:IND) * W(IB1:IB2,J,K) +
     >                     CF(IST:IND) * W(IC1:IC2,J,K)
 
      TAV(IST:IND,J,K)   = BF(IST:IND) * T(II1:II2,J,K) +
     >                     AF(IST:IND) * T(IB1:IB2,J,K) +
     >                     CF(IST:IND) * T(IC1:IC2,J,K)
 
      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE VISCI ( I1, I2, J, K, FSI )

      USE GENERAL_DATA, ONLY: ISTART, JSTART, KSTART,
     >                        ICMAX, IBDD, IADD, R2D3, ND, PR, CMU,
     >                        IPERIODIC, JPERIODIC, KPERIODIC,
     >                        IMAX, JMAX, KMAX, NSCHEME, R6I, R12I
      USE VARIABLE_DATA, ONLY: U, V, W, T, UAV, VAV, WAV, TAV,
     >                         T11, T12, T13, T21, T22, T23,
     >                         T31, T32, T33, HF, SIX, SIY, SIZ

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: I1, I2, J, K
      INTEGER :: IG, JG, KG, I, II, IBD, ICD
      REAL (KIND = 8) :: ABD
      REAL (KIND = 8) :: DUDXI, DVDXI, DWDXI, DTDXI
      REAL (KIND = 8) :: DUDET, DVDET, DWDET, DTDET
      REAL (KIND = 8) :: DUDZT, DVDZT, DWDZT, DTDZT
      REAL (KIND = 8) :: DUDX, DVDX, DWDX, DTDX
      REAL (KIND = 8) :: DUDY, DVDY, DWDY, DTDY
      REAL (KIND = 8) :: DUDZ, DVDZ, DWDZ, DTDZ
      REAL (KIND = 8) :: DIV, TEMP, UAVE, VAVE, WAVE, CPAV
      REAL (KIND = 8) :: RMU, RK, RLMBDA, QX, QY, QZ, SX, SY, SZ
      REAL (KIND = 8) :: TXX, TYY, TXY, TXZ, TYX, TYZ, TZZ, TZY, TZX

      REAL (KIND = 8), INTENT(INOUT), DIMENSION(0:ICMAX,ND) :: FSI

      JG = J + JSTART - 1
      KG = K + KSTART - 1

      ABD = DBLE(IBDD)

      DO I = I1, I2
         IG = I + ISTART - 1

         II  =  I + IADD
         IBD = II - IBDD
         ICD = II + IBDD

         IF((.NOT. IPERIODIC .AND. (IG .EQ. 0 .OR. IG .EQ. IMAX-1)).OR.
     >                                         NSCHEME .EQ. 2) THEN
            DUDXI = U(I+1,J,K) - U(I,J,K)
            DVDXI = V(I+1,J,K) - V(I,J,K)
            DWDXI = W(I+1,J,K) - W(I,J,K)
            DTDXI = T(I+1,J,K) - T(I,J,K)
         ELSE
            DUDXI = ABD * ((U(IBD,J,K) - U(ICD,J,K)) +
     >                   8.0D+00 * (U( II,J,K) - U(IBD,J,K))) * R6I
            DVDXI = ABD * ((V(IBD,J,K) - V(ICD,J,K)) +
     >                   8.0D+00 * (V( II,J,K) - V(IBD,J,K))) * R6I
            DWDXI = ABD * ((W(IBD,J,K) - W(ICD,J,K)) +
     >                   8.0D+00 * (W( II,J,K) - W(IBD,J,K))) * R6I
            DTDXI = ABD * ((T(IBD,J,K) - T(ICD,J,K)) +
     >                   8.0D+00 * (T( II,J,K) - T(IBD,J,K))) * R6I
         ENDIF

         DUDX = T11(I,J,K,1) * DUDXI
         DVDX = T11(I,J,K,1) * DVDXI
         DWDX = T11(I,J,K,1) * DWDXI
         DTDX = T11(I,J,K,1) * DTDXI

         DUDY = T21(I,J,K,1) * DUDXI
         DVDY = T21(I,J,K,1) * DVDXI
         DWDY = T21(I,J,K,1) * DWDXI
         DTDY = T21(I,J,K,1) * DTDXI

         DUDZ = T31(I,J,K,1) * DUDXI
         DVDZ = T31(I,J,K,1) * DVDXI
         DWDZ = T31(I,J,K,1) * DWDXI
         DTDZ = T31(I,J,K,1) * DTDXI

         IF((.NOT. JPERIODIC .AND. (JG .EQ. 1 .OR. JG .EQ. JMAX-1)) .OR.
     >                                          NSCHEME .EQ. 2) THEN
            DUDET = (UAV(I,J+1,K) - UAV(I,J-1,K)) * 0.5D+00
            DVDET = (VAV(I,J+1,K) - VAV(I,J-1,K)) * 0.5D+00
            DWDET = (WAV(I,J+1,K) - WAV(I,J-1,K)) * 0.5D+00
            DTDET = (TAV(I,J+1,K) - TAV(I,J-1,K)) * 0.5D+00
         ELSE
            DUDET = R12I * (UAV(I,J-2,K) - UAV(I,J+2,K) +
     >                   8.0D+00 * (UAV(I,J+1,K) - UAV(I,J-1,K)))
            DVDET = R12I * (VAV(I,J-2,K) - VAV(I,J+2,K) +
     >                   8.0D+00 * (VAV(I,J+1,K) - VAV(I,J-1,K)))
            DWDET = R12I * (WAV(I,J-2,K) - WAV(I,J+2,K) +
     >                   8.0D+00 * (WAV(I,J+1,K) - WAV(I,J-1,K)))
            DTDET = R12I * (TAV(I,J-2,K) - TAV(I,J+2,K) +
     >                   8.0D+00 * (TAV(I,J+1,K) - TAV(I,J-1,K)))
         ENDIF

         DUDX = DUDX + T12(I,J,K,1) * DUDET
         DVDX = DVDX + T12(I,J,K,1) * DVDET
         DWDX = DWDX + T12(I,J,K,1) * DWDET
         DTDX = DTDX + T12(I,J,K,1) * DTDET
 
         DUDY = DUDY + T22(I,J,K,1) * DUDET
         DVDY = DVDY + T22(I,J,K,1) * DVDET
         DWDY = DWDY + T22(I,J,K,1) * DWDET
         DTDY = DTDY + T22(I,J,K,1) * DTDET
 
         DUDZ = DUDZ + T32(I,J,K,1) * DUDET
         DVDZ = DVDZ + T32(I,J,K,1) * DVDET
         DWDZ = DWDZ + T32(I,J,K,1) * DWDET
         DTDZ = DTDZ + T32(I,J,K,1) * DTDET
 
         IF((.NOT. KPERIODIC .AND. (KG .EQ. 1 .OR. KG .EQ. KMAX-1)) .OR.
     >                                          NSCHEME .EQ. 2) THEN
            DUDZT = (UAV(I,J,K+1) - UAV(I,J,K-1)) * 0.5D+00
            DVDZT = (VAV(I,J,K+1) - VAV(I,J,K-1)) * 0.5D+00
            DWDZT = (WAV(I,J,K+1) - WAV(I,J,K-1)) * 0.5D+00
            DTDZT = (TAV(I,J,K+1) - TAV(I,J,K-1)) * 0.5D+00
         ELSE
            DUDZT = R12I * (UAV(I,J,K-2) - UAV(I,J,K+2) +
     >                   8.0D+00 * (UAV(I,J,K+1) - UAV(I,J,K-1)))
            DVDZT = R12I * (VAV(I,J,K-2) - VAV(I,J,K+2) +
     >                   8.0D+00 * (VAV(I,J,K+1) - VAV(I,J,K-1)))
            DWDZT = R12I * (WAV(I,J,K-2) - WAV(I,J,K+2) +
     >                   8.0D+00 * (WAV(I,J,K+1) - WAV(I,J,K-1)))
            DTDZT = R12I * (TAV(I,J,K-2) - TAV(I,J,K+2) +
     >                   8.0D+00 * (TAV(I,J,K+1) - TAV(I,J,K-1)))
         ENDIF

         DUDX = DUDX + T13(I,J,K,1) * DUDZT
         DVDX = DVDX + T13(I,J,K,1) * DVDZT
         DWDX = DWDX + T13(I,J,K,1) * DWDZT
         DTDX = DTDX + T13(I,J,K,1) * DTDZT
 
         DUDY = DUDY + T23(I,J,K,1) * DUDZT
         DVDY = DVDY + T23(I,J,K,1) * DVDZT
         DWDY = DWDY + T23(I,J,K,1) * DWDZT
         DTDY = DTDY + T23(I,J,K,1) * DTDZT
 
         DUDZ = DUDZ + T33(I,J,K,1) * DUDZT
         DVDZ = DVDZ + T33(I,J,K,1) * DVDZT
         DWDZ = DWDZ + T33(I,J,K,1) * DWDZT
         DTDZ = DTDZ + T33(I,J,K,1) * DTDZT

         TEMP = 0.5D+00 * (T(I,J,K)    + T(I+1,J,K))
         UAVE = 0.5D+00 * (U(I,J,K)    + U(I+1,J,K))
         VAVE = 0.5D+00 * (V(I,J,K)    + V(I+1,J,K))
         WAVE = 0.5D+00 * (W(I,J,K)    + W(I+1,J,K))
         CPAV = 0.5D+00 * (HF(I,J,K,2) + HF(I+1,J,K,2))

         RMU    = CMU * TEMP * SQRT(TEMP) / (TEMP + 110.0D0)
         DIV    = DUDX + DVDY + DWDZ
         RK     = CPAV * RMU / PR
         RLMBDA = -R2D3 * RMU

         TXX = -2.0D+00 * RMU * DUDX - RLMBDA * DIV
         TXY = -RMU * (DUDY + DVDX)
         TXZ = -RMU * (DWDX + DUDZ)
         TYY = -2.0D+00 * RMU * DVDY - RLMBDA * DIV
         TYX =  TXY
         TYZ = -RMU * (DVDZ + DWDY)
         TZZ = -2.0D+00 * RMU * DWDZ - RLMBDA * DIV
         TZX =  TXZ
         TZY =  TYZ

         QX  = -RK * DTDX
         QY  = -RK * DTDY
         QZ  = -RK * DTDZ

         SX = SIX(I,J,K)
         SY = SIY(I,J,K)
         SZ = SIZ(I,J,K)

         FSI(I,2) = FSI(I,2) + TXX * SX + TXY * SY + TXZ * SZ
         FSI(I,3) = FSI(I,3) + TYX * SX + TYY * SY + TYZ * SZ
         FSI(I,4) = FSI(I,4) + TZX * SX + TZY * SY + TZZ * SZ
         FSI(I,5) = FSI(I,5) +
     >                      (TXX*UAVE + TXY*VAVE + TXZ*WAVE + QX) * SX
     >                     +(TYX*UAVE + TYY*VAVE + TYZ*WAVE + QY) * SY
     >                     +(TZX*UAVE + TZY*VAVE + TZZ*WAVE + QZ) * SZ

      ENDDO

      RETURN
      END
