      SUBROUTINE FLUXK()

      USE GENERAL_DATA, ONLY: VISCOUS, IEND, IOUTFLOW, IMAX, ICMAX,
     >                        JCMAX, KCMAX, ND, NSCHEME, KADD
      USE VARIABLE_DATA, ONLY: UAV, VAV, WAV, SKX, SKY, SKZ,
     >                         U, V, W, QAV, PAV, DQ, DTV

      IMPLICIT NONE

      REAL (KIND = 8), ALLOCATABLE :: QS(:), FSK(:,:,:)

      INTEGER :: ERROR

      INTEGER :: IND, I, J, K, L

      REAL (KIND = 8) :: QSP, QSPK

      IF ( IEND .EQ. IMAX-1 .AND. IOUTFLOW .EQ. 1 ) THEN
         IND = ICMAX+1
      ELSE
         IND = ICMAX
      ENDIF

      ALLOCATE ( QS(IND), FSK(IND,0:KCMAX,ND), STAT = ERROR )
      IF ( ERROR .NE. 0 ) CALL EJECT ('ALLOCATION ERROR: FLUXK')

      I = SIZE(FSK)
      CALL EXTRAPK ( FSK, I )

      DO J = 1, JCMAX
        DO K = 0, KCMAX

           QS(1:IND) = UAV(1:IND,J,K) * SKX(1:IND,J,K) +
     >                 VAV(1:IND,J,K) * SKY(1:IND,J,K) +
     >                 WAV(1:IND,J,K) * SKZ(1:IND,J,K)

           IF ( NSCHEME .EQ. 2 ) THEN
              L = K + 1 - KADD
              DO I = 1, IND
                 QSP = U(I,J,L) * SKX(I,J,K) +
     >                 V(I,J,L) * SKY(I,J,K) +
     >                 W(I,J,L) * SKZ(I,J,K)
                 QSPK = (QSP - QS(I)) * DBLE(1 - 2 * KADD)
                 IF (QSPK .GT. 0.0D+00) QS(I) = 0.5D+00 * (QS(I) + QSP)
              ENDDO
           ENDIF
   
           FSK(1:IND,K,1) =  QAV(1:IND,J,K,1) * QS(1:IND)
           FSK(1:IND,K,2) =  QAV(1:IND,J,K,2) * QS(1:IND) +
     >                             PAV(1:IND,J,K) * SKX(1:IND,J,K)
           FSK(1:IND,K,3) =  QAV(1:IND,J,K,3) * QS(1:IND) +
     >                             PAV(1:IND,J,K) * SKY(1:IND,J,K)
           FSK(1:IND,K,4) =  QAV(1:IND,J,K,4) * QS(1:IND) +
     >                             PAV(1:IND,J,K) * SKZ(1:IND,J,K)
           FSK(1:IND,K,5) = (QAV(1:IND,J,K,5) + PAV(1:IND,J,K)) *
     >                                                   QS(1:IND)

           IF ( VISCOUS ) CALL VISCK ( 1, IND, J, K, FSK )
        ENDDO

        DO K = 1, KCMAX
           DO L = 1, 5
              DQ(1:IND,J,K,L) = DQ(1:IND,J,K,L) - 
     >             DTV(1:IND,J,K) * (FSK(1:IND,K,L) - FSK(1:IND,K-1,L))
           ENDDO
        ENDDO
      ENDDO

      DEALLOCATE ( QS, FSK )

      RETURN
      END
!---------------------------------------------------------------------
      SUBROUTINE EXTRAPK ( )

      USE GENERAL_DATA, ONLY: ICMAX, JCMAX, KCMAX, NSCHEME,
     >                        IPERIODIC, JPERIODIC, KPERIODIC,
     >                        ISTART, JSTART, KSTART,
     >                        IMAX, JMAX, KMAX,
     >                        EFORM, ZERO, HFK, CPK, CVK, RGK,
     >                        CPAIR, CVAIR, RGAIR
      USE VARIABLE_DATA, ONLY: TAV, QAV, UAV, VAV, WAV, HAV, PAV

      IMPLICIT NONE

      INTEGER :: J, K, L, I1, I2, J1, J2, K1, K2, IGL1, IGL2, JGL1, JGL2
      INTEGER :: KG

      REAL (KIND = 8), ALLOCATABLE, DIMENSION(:) :: RWRK

      IF(NSCHEME .EQ. 2) THEN
         I1 = 0
         I2 = ICMAX + 1

         J1 = 0
         J2 = JCMAX + 1
      ELSE
         IF(IPERIODIC) THEN
            I1 = -1
            I2 = ICMAX + 2
         ELSE
            IGL1 = 0
            IGL2 = IMAX

            I1 = MAX(     -1, IGL1 - ISTART + 1)
            I2 = MIN(ICMAX+2, IGL2 - ISTART + 1)
         ENDIF

         IF(JPERIODIC) THEN
            J1 = -1
            J2 = JCMAX + 2
         ELSE
            JGL1 = 0
            JGL2 = JMAX

            J1 = MAX(     -1, JGL1 - JSTART + 1)
            J2 = MIN(JCMAX+2, JGL2 - JSTART + 1)
         ENDIF
      ENDIF

      ALLOCATE(RWRK(I1:I2))

      DO K = 0, KCMAX
         KG = K + KSTART - 1
         DO J = J1, J2

            IF ( NSCHEME .EQ. 2 .OR. (.NOT. KPERIODIC .AND.
     >                   (KG .EQ. 0 .OR. KG .EQ. KMAX-1))) THEN
               CALL EXK2 ( I1, I2, J, K )
            ELSE
               CALL EXK4 ( I1, I2, J, K )
            ENDIF

            RWRK(I1:I2) = 0.5D+00 * (UAV(I1:I2,J,K) * UAV(I1:I2,J,K) +
     >                                 VAV(I1:I2,J,K) * VAV(I1:I2,J,K) +
     >                                 WAV(I1:I2,J,K) * WAV(I1:I2,J,K))

            QAV(I1:I2,J,K,2) = QAV(I1:I2,J,K,1) * UAV(I1:I2,J,K)
            QAV(I1:I2,J,K,3) = QAV(I1:I2,J,K,1) * VAV(I1:I2,J,K)
            QAV(I1:I2,J,K,4) = QAV(I1:I2,J,K,1) * WAV(I1:I2,J,K)
            QAV(I1:I2,J,K,5) = QAV(I1:I2,J,K,1) *
     >                         (TAV(I1:I2,J,K) * CVAIR + RWRK(I1:I2))

            PAV(I1:I2,J,K) = QAV(I1:I2,J,K,1) * RGAIR *
     >                                              TAV(I1:I2,J,K)

         ENDDO
      ENDDO

      DEALLOCATE(RWRK)

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE EXK2 ( I1, I2, J, K )

      USE GENERAL_DATA, ONLY: KADD, N, ND
      USE VARIABLE_DATA, ONLY: QAV, UAV, VAV, WAV, TAV,
     >                         Q, U, V, W, T

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: I1, I2, J, K

      INTEGER :: KK
  
      KK = K + KADD

      QAV(I1:I2,J,K,1) = Q(I1:I2,J,KK,1,N)
      UAV(I1:I2,J,K)   = U(I1:I2,J,KK)
      VAV(I1:I2,J,K)   = V(I1:I2,J,KK)
      WAV(I1:I2,J,K)   = W(I1:I2,J,KK)
      TAV(I1:I2,J,K)   = T(I1:I2,J,KK)

      RETURN
      END
!---------------------------------------------------------------------
      SUBROUTINE EXK4 ( IST, IND, J, K )

      USE GENERAL_DATA, ONLY: KADD, KBDD, N
      USE VARIABLE_DATA, ONLY: DS, DS1, QAV, Q, UAV, VAV, WAV, TAV,
     >                         U, V, W, T

      IMPLICIT NONE

      INTEGER IST, IND, J, K, NS, KK, KBD, KCD

      REAL (KIND = 8) :: AF(IST:IND), BF(IST:IND), CF(IST:IND)

      REAL (KIND = 8), PARAMETER :: R23 = 2.0D0 / 3.0D0,
     >                              R13 = 1.0D0 / 3.0D0

      KK  = K + KADD
      KBD = KK - KBDD
      KCD = KK + KBDD

      IF ( KADD .EQ. 1 ) THEN
         AF(IST:IND) = R23 * (1.0D0 -
     >                       DS(IST:IND,J,K,3) * DS1(IST:IND,J,K,3))
         BF(IST:IND) = R13 * (1.0D0 + DS(IST:IND,J,K+1,3) *
     >                                            DS1(IST:IND,J,K+1,3) +
     >                 2.0D0 * DS(IST:IND,J,K,3) * DS1(IST:IND,J,K,3))
         CF(IST:IND) = -R13 * DS(IST:IND,J,K+1,3) * DS1(IST:IND,J,K+1,3)
      ELSE
         AF(IST:IND) = R23 * (1.0D0 -
     >                       DS(IST:IND,J,K+1,3) * DS1(IST:IND,J,K,3))
         BF(IST:IND) = R13 * (1.0D0 +
     >                        DS(IST:IND,J,K,3) * DS1(IST:IND,J,K-1,3) +
     >                 2.0D0 * DS(IST:IND,J,K+1,3) * DS1(IST:IND,J,K,3))
         CF(IST:IND) = -R13 * DS(IST:IND,J,K,3) * DS1(IST:IND,J,K-1,3)
      END IF

      QAV(IST:IND,J,K,1) = BF(IST:IND) * Q(IST:IND,J, KK,1,N)
      QAV(IST:IND,J,K,1) = QAV(IST:IND,J,K,1) +
     >                     AF(IST:IND) * Q(IST:IND,J,KBD,1,N)
      QAV(IST:IND,J,K,1) = QAV(IST:IND,J,K,1) +
     >                     CF(IST:IND) * Q(IST:IND,J,KCD,1,N)

      UAV(IST:IND,J,K)   = BF(IST:IND) * U(IST:IND,J, KK)
      UAV(IST:IND,J,K)   = UAV(IST:IND,J,K) +
     >                     AF(IST:IND) * U(IST:IND,J,KBD)
      UAV(IST:IND,J,K)   = UAV(IST:IND,J,K) +
     >                     CF(IST:IND) * U(IST:IND,J,KCD)

      VAV(IST:IND,J,K)   = BF(IST:IND) * V(IST:IND,J, KK)
      VAV(IST:IND,J,K)   = VAV(IST:IND,J,K) +
     >                     AF(IST:IND) * V(IST:IND,J,KBD)
      VAV(IST:IND,J,K)   = VAV(IST:IND,J,K) +
     >                     CF(IST:IND) * V(IST:IND,J,KCD)

      WAV(IST:IND,J,K)   = BF(IST:IND) * W(IST:IND,J, KK)
      WAV(IST:IND,J,K)   = WAV(IST:IND,J,K) +
     >                     AF(IST:IND) * W(IST:IND,J,KBD)
      WAV(IST:IND,J,K)   = WAV(IST:IND,J,K) +
     >                     CF(IST:IND) * W(IST:IND,J,KCD)

      TAV(IST:IND,J,K)   = BF(IST:IND) * T(IST:IND,J, KK)
      TAV(IST:IND,J,K)   = TAV(IST:IND,J,K) +
     >                     AF(IST:IND) * T(IST:IND,J,KBD)
      TAV(IST:IND,J,K)   = TAV(IST:IND,J,K) +
     >                     CF(IST:IND) * T(IST:IND,J,KCD)

      RETURN
      END
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      SUBROUTINE VISCK (I1, I2, J, K, FSK)

      USE GENERAL_DATA, ONLY: ISTART, JSTART, KSTART,
     >                        KCMAX, KBDD, KADD, R2D3, ND, PR, CMU,
     >                        IPERIODIC, JPERIODIC, KPERIODIC,
     >                        IMAX, JMAX, KMAX, NSCHEME, R6I, R12I,
     >                        R2, R8, R2I, R3, R4, HALF
      USE VARIABLE_DATA, ONLY: U, V, W, T, UAV, VAV, WAV, TAV,
     >                         T11, T12, T13, T21, T22, T23,
     >                         T31, T32, T33, HF, SKX, SKY, SKZ

      IMPLICIT NONE

      REAL (KIND = 8), INTENT(INOUT), DIMENSION(I1:I2,0:KCMAX,ND) :: FSK

      INTEGER, INTENT(IN) :: I1, I2, J, K
      INTEGER :: IG, JG, KG, I, KK, KBD, KCD
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

      KK  = K + KADD
      KBD = KK - KBDD
      KCD = KK + KBDD
      ABD = DBLE(KBDD)

      JG = J + JSTART - 1
      KG = K + KSTART - 1

      DO I = I1, I2
         IG = I + ISTART - 1

         IF(.NOT. IPERIODIC .AND. IG .EQ. IMAX) THEN
            DUDXI = HALF *
     >          (R3 * U(I,J,K) - R4 * U(I-1,J,K) - U(I-2,J,K))
            DVDXI = HALF *
     >          (R3 * V(I,J,K) - R4 * V(I-1,J,K) - V(I-2,J,K))
            DWDXI = HALF *
     >          (R3 * W(I,J,K) - R4 * W(I-1,J,K) - W(I-2,J,K))
            DTDXI = HALF *
     >          (R3 * T(I,J,K) - R4 * T(I-1,J,K) - T(I-2,J,K))
         ELSE IF((.NOT. IPERIODIC .AND.
     >                        (IG .EQ. 1 .OR. IG .EQ. IMAX-1)) .OR.
     >                                         NSCHEME .EQ. 2) THEN
            DUDXI = (UAV(I+1,J,K) - UAV(I-1,J,K)) * R2I
            DVDXI = (VAV(I+1,J,K) - VAV(I-1,J,K)) * R2I
            DWDXI = (WAV(I+1,J,K) - WAV(I-1,J,K)) * R2I
            DTDXI = (TAV(I+1,J,K) - TAV(I-1,J,K)) * R2I
         ELSE
            DUDXI = R12I * (UAV(I-2,J,K) - UAV(I+2,J,K) +
     >                   R8 * (UAV(I+1,J,K) - UAV(I-1,J,K)))
            DVDXI = R12I * (VAV(I-2,J,K) - VAV(I+2,J,K) +
     >                   R8 * (VAV(I+1,J,K) - VAV(I-1,J,K)))
            DWDXI = R12I * (WAV(I-2,J,K) - WAV(I+2,J,K) +
     >                   R8 * (WAV(I+1,J,K) - WAV(I-1,J,K)))
            DTDXI = R12I * (TAV(I-2,J,K) - TAV(I+2,J,K) +
     >                   R8 * (TAV(I+1,J,K) - TAV(I-1,J,K)))
         ENDIF

         DUDX = T11(I,J,K,3) * DUDXI
         DVDX = T11(I,J,K,3) * DVDXI
         DWDX = T11(I,J,K,3) * DWDXI
         DTDX = T11(I,J,K,3) * DTDXI

         DUDY = T21(I,J,K,3) * DUDXI
         DVDY = T21(I,J,K,3) * DVDXI
         DWDY = T21(I,J,K,3) * DWDXI
         DTDY = T21(I,J,K,3) * DTDXI

         DUDZ = T31(I,J,K,3) * DUDXI
         DVDZ = T31(I,J,K,3) * DVDXI
         DWDZ = T31(I,J,K,3) * DWDXI
         DTDZ = T31(I,J,K,3) * DTDXI

         IF((.NOT. JPERIODIC .AND. (JG .EQ. 1 .OR. JG .EQ. JMAX-1)) .OR.
     >                                          NSCHEME .EQ. 2) THEN
            DUDET = (UAV(I,J+1,K) - UAV(I,J-1,K)) * R2I
            DVDET = (VAV(I,J+1,K) - VAV(I,J-1,K)) * R2I
            DWDET = (WAV(I,J+1,K) - WAV(I,J-1,K)) * R2I
            DTDET = (TAV(I,J+1,K) - TAV(I,J-1,K)) * R2I
         ELSE
            DUDET = R12I * (UAV(I,J-2,K) - UAV(I,J+2,K) +
     >                   R8 * (UAV(I,J+1,K) - UAV(I,J-1,K)))
            DVDET = R12I * (VAV(I,J-2,K) - VAV(I,J+2,K) +
     >                   R8 * (VAV(I,J+1,K) - VAV(I,J-1,K)))
            DWDET = R12I * (WAV(I,J-2,K) - WAV(I,J+2,K) +
     >                   R8 * (WAV(I,J+1,K) - WAV(I,J-1,K)))
            DTDET = R12I * (TAV(I,J-2,K) - TAV(I,J+2,K) +
     >                   R8 * (TAV(I,J+1,K) - TAV(I,J-1,K)))
         ENDIF

         DUDX = DUDX + T12(I,J,K,3) * DUDET
         DVDX = DVDX + T12(I,J,K,3) * DVDET
         DWDX = DWDX + T12(I,J,K,3) * DWDET
         DTDX = DTDX + T12(I,J,K,3) * DTDET

         DUDY = DUDY + T22(I,J,K,3) * DUDET
         DVDY = DVDY + T22(I,J,K,3) * DVDET
         DWDY = DWDY + T22(I,J,K,3) * DWDET
         DTDY = DTDY + T22(I,J,K,3) * DTDET

         DUDZ = DUDZ + T32(I,J,K,3) * DUDET
         DVDZ = DVDZ + T32(I,J,K,3) * DVDET
         DWDZ = DWDZ + T32(I,J,K,3) * DWDET
         DTDZ = DTDZ + T32(I,J,K,3) * DTDET

         IF((.NOT. KPERIODIC .AND. (KG .EQ. 0 .OR. KG .EQ. KMAX-1)) .OR.
     >                                         NSCHEME .EQ. 2) THEN
            DUDZT = U(I,J,K+1) - U(I,J,K)
            DVDZT = V(I,J,K+1) - V(I,J,K)
            DWDZT = W(I,J,K+1) - W(I,J,K)
            DTDZT = T(I,J,K+1) - T(I,J,K)
         ELSE
            DUDZT = ABD * ((U(I,J,KBD) - U(I,J,KCD)) +
     >                   R8 * (U(I,J, KK) - U(I,J,KBD))) * R6I
            DVDZT = ABD * ((V(I,J,KBD) - V(I,J,KCD)) +
     >                   R8 * (V(I,J, KK) - V(I,J,KBD))) * R6I
            DWDZT = ABD * ((W(I,J,KBD) - W(I,J,KCD)) +
     >                   R8 * (W(I,J, KK) - W(I,J,KBD))) * R6I
            DTDZT = ABD * ((T(I,J,KBD) - T(I,J,KCD)) +
     >                   R8 * (T(I,J, KK) - T(I,J,KBD))) * R6I
         ENDIF

         DUDX = DUDX + T13(I,J,K,3) * DUDZT
         DVDX = DVDX + T13(I,J,K,3) * DVDZT
         DWDX = DWDX + T13(I,J,K,3) * DWDZT
         DTDX = DTDX + T13(I,J,K,3) * DTDZT

         DUDY = DUDY + T23(I,J,K,3) * DUDZT
         DVDY = DVDY + T23(I,J,K,3) * DVDZT
         DWDY = DWDY + T23(I,J,K,3) * DWDZT
         DTDY = DTDY + T23(I,J,K,3) * DTDZT

         DUDZ = DUDZ + T33(I,J,K,3) * DUDZT
         DVDZ = DVDZ + T33(I,J,K,3) * DVDZT
         DWDZ = DWDZ + T33(I,J,K,3) * DWDZT
         DTDZ = DTDZ + T33(I,J,K,3) * DTDZT

         TEMP = R2I * (T(I,J,K)    + T(I,J,K+1))
         UAVE = R2I * (U(I,J,K)    + U(I,J,K+1))
         VAVE = R2I * (V(I,J,K)    + V(I,J,K+1))
         WAVE = R2I * (W(I,J,K)    + W(I,J,K+1))
         CPAV = R2I * (HF(I,J,K,2) + HF(I,J,K+1,2))

         RMU    = CMU * TEMP * SQRT(TEMP) / (TEMP + 110.0D0)
         DIV    = DUDX + DVDY + DWDZ
         RK     = RMU * CPAV / PR
         RLMBDA = -R2D3 * RMU

         TXX = -R2 * RMU * DUDX - RLMBDA * DIV
         TXY = -RMU * (DUDY + DVDX)
         TXZ = -RMU * (DWDX + DUDZ)
         TYY = -R2 * RMU * DVDY - RLMBDA * DIV
         TYX =  TXY
         TYZ = -RMU * (DVDZ + DWDY)
         TZZ = -R2 * RMU * DWDZ - RLMBDA * DIV
         TZX =  TXZ
         TZY =  TYZ

         QX = -RK * DTDX
         QY = -RK * DTDY
         QZ = -RK * DTDZ

         SX = SKX(I,J,K)
         SY = SKY(I,J,K)
         SZ = SKZ(I,J,K)

         FSK(I,K,2) = FSK(I,K,2) + TXX * SX + TXY * SY + TXZ * SZ
         FSK(I,K,3) = FSK(I,K,3) + TYX * SX + TYY * SY + TYZ * SZ
         FSK(I,K,4) = FSK(I,K,4) + TZX * SX + TZY * SY + TZZ * SZ
         FSK(I,K,5) = FSK(I,K,5) +
     >               (TXX*UAVE + TXY*VAVE + TXZ*WAVE + QX) * SX +
     >               (TYX*UAVE + TYY*VAVE + TYZ*WAVE + QY) * SY +
     >               (TZX*UAVE + TZY*VAVE + TZZ*WAVE + QZ) * SZ
      ENDDO

      RETURN
      END
