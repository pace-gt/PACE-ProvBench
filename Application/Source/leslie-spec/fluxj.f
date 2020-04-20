      SUBROUTINE FLUXJ()

      USE GENERAL_DATA, ONLY: VISCOUS, IEND, IOUTFLOW, IMAX, ICMAX,
     >                        JCMAX, KCMAX, ND, NSCHEME, JADD
      USE VARIABLE_DATA, ONLY: UAV, VAV, WAV, SJX, SJY, SJZ,
     >                         U, V, W, QAV, PAV, DQ, DTV

      IMPLICIT NONE

      REAL (KIND = 8), ALLOCATABLE :: QS(:), FSJ(:,:,:)

      INTEGER :: ERROR

      INTEGER :: I, J, K, L, IND

      REAL (KIND = 8) :: QSP, QSPJ

      IF (IEND .EQ. IMAX-1 .AND. IOUTFLOW .EQ. 1) THEN
         IND = ICMAX+1
      ELSE
         IND = ICMAX
      ENDIF

      ALLOCATE ( QS(IND), FSJ(IND,0:JCMAX,ND), STAT = ERROR )
      IF ( ERROR .NE. 0 ) CALL EJECT ('ALLOCATION ERROR: FLUXJ')

      L = SIZE (FSJ)
      CALL EXTRAPJ ( FSJ, L )

      DO K = 1, KCMAX
         DO J = 0, JCMAX

            QS(1:IND) = UAV(1:IND,J,K) * SJX(1:IND,J,K) +
     >                  VAV(1:IND,J,K) * SJY(1:IND,J,K) +
     >                  WAV(1:IND,J,K) * SJZ(1:IND,J,K)

            IF (NSCHEME .EQ. 2) THEN
               L = J + 1 - JADD
               DO I = 1, IND
                  QSP = U(I,L,K) * SJX(I,J,K) +
     >                  V(I,L,K) * SJY(I,J,K) +
     >                  W(I,L,K) * SJZ(I,J,K)
                  QSPJ = (QSP - QS(I)) * DBLE(1 - 2 * JADD)
                  IF (QSPJ .GT. 0.0D+00) QS(I) = 0.5D+00 * (QS(I) + QSP)
               ENDDO
            ENDIF

           FSJ(1:IND,J,1) =  QAV(1:IND,J,K,1) * QS(1:IND)
           FSJ(1:IND,J,2) =  QAV(1:IND,J,K,2) * QS(1:IND) +
     >                           PAV(1:IND,J,K) * SJX(1:IND,J,K)
           FSJ(1:IND,J,3) =  QAV(1:IND,J,K,3) * QS(1:IND) +
     >                           PAV(1:IND,J,K) * SJY(1:IND,J,K)
           FSJ(1:IND,J,4) =  QAV(1:IND,J,K,4) * QS(1:IND) +
     >                           PAV(1:IND,J,K) * SJZ(1:IND,J,K)
           FSJ(1:IND,J,5) = (QAV(1:IND,J,K,5) + PAV(1:IND,J,K)) *
     >                                                  QS(1:IND)

           IF ( VISCOUS ) CALL VISCJ (1, IND, J, K, FSJ)
        ENDDO

        DO J = 1, JCMAX
           DO L = 1, 5
              DQ(1:IND,J,K,L) = DQ(1:IND,J,K,L) - 
     >             DTV(1:IND,J,K) * (FSJ(1:IND,J,L) - FSJ(1:IND,J-1,L))
           ENDDO
        ENDDO
      ENDDO

      DEALLOCATE ( QS, FSJ )

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE EXTRAPJ ( )

      USE GENERAL_DATA, ONLY: ICMAX, JCMAX, KCMAX, NSCHEME,
     >                        IPERIODIC, JPERIODIC, KPERIODIC,
     >                        ISTART, JSTART, KSTART,
     >                        IMAX, JMAX, KMAX,
     >                        EFORM, ZERO, HFK, CPK, CVK, RGK,
     >                        CPAIR, CVAIR, RGAIR
      USE VARIABLE_DATA, ONLY: TAV, QAV, UAV, VAV, WAV, HAV, PAV

      IMPLICIT NONE

      INTEGER :: J, K, L, I1, I2, J1, J2, K1, K2
 
      REAL (KIND = 8), ALLOCATABLE, DIMENSION(:) :: RWRK

      IF (NSCHEME .EQ. 2) THEN
         I1 = 0
         I2 = ICMAX + 1

         K1 = 0
         K2 = KCMAX + 1
      ELSE
         IF (IPERIODIC) THEN
            I1 = -1
            I2 = ICMAX + 2
         ELSE
            I1 = MAX(     -1, 0    - ISTART + 1)
            I2 = MIN(ICMAX+2, IMAX - ISTART + 1)
         ENDIF

         IF (KPERIODIC) THEN
            K1 = -1
            K2 = KCMAX + 2
         ELSE
            K1 = MAX(     -1, 0    - KSTART + 1)
            K2 = MIN(KCMAX+2, KMAX - KSTART + 1)
         ENDIF
      ENDIF

      ALLOCATE(RWRK(I1:I2))

      DO K = K1, K2
         DO J = 0, JCMAX
            L = J + JSTART - 1

            IF ( NSCHEME .EQ. 2 .OR. (.NOT. JPERIODIC .AND.
     >                   (L .EQ. 0 .OR. L .EQ. JMAX-1))) THEN
               CALL EXJ2 ( I1, I2, J, K )
            ELSE
               CALL EXJ4 ( I1, I2, J, K )
            ENDIF

            RWRK(I1:I2) = 0.5D+00 * (UAV(I1:I2,J,K) * UAV(I1:I2,J,K) +
     >                                 VAV(I1:I2,J,K) * VAV(I1:I2,J,K) +
     >                                 WAV(I1:I2,J,K) * WAV(I1:I2,J,K))

            QAV(I1:I2,J,K,2) = QAV(I1:I2,J,K,1) * UAV(I1:I2,J,K)
            QAV(I1:I2,J,K,3) = QAV(I1:I2,J,K,1) * VAV(I1:I2,J,K)
            QAV(I1:I2,J,K,4) = QAV(I1:I2,J,K,1) * WAV(I1:I2,J,K)
            QAV(I1:I2,J,K,5) = QAV(I1:I2,J,K,1) *
     >                       (TAV(I1:I2,J,K) * CVAIR + RWRK(I1:I2))

            PAV(I1:I2,J,K) =
     >               QAV(I1:I2,J,K,1) * RGAIR * TAV(I1:I2,J,K)

         ENDDO
      ENDDO
 
      DEALLOCATE(RWRK)
 
      RETURN
      END
!---------------------------------------------------------------------
      SUBROUTINE EXJ2(I1, I2, J, K)

      USE GENERAL_DATA, ONLY: JADD, N, ND
      USE VARIABLE_DATA, ONLY: QAV, UAV, VAV, WAV, TAV,
     >                         Q, U, V, W, T

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: I1, I2, J, K

      INTEGER :: JJ

      JJ = J + JADD

      QAV(I1:I2,J,K,1) = Q(I1:I2,JJ,K,1,N)
      UAV(I1:I2,J,K)   = U(I1:I2,JJ,K)
      VAV(I1:I2,J,K)   = V(I1:I2,JJ,K)
      WAV(I1:I2,J,K)   = W(I1:I2,JJ,K)
      TAV(I1:I2,J,K)   = T(I1:I2,JJ,K)

      RETURN
      END
!---------------------------------------------------------------------
      SUBROUTINE EXJ4 ( IST, IND, J, K )

      USE GENERAL_DATA, ONLY: JADD, JBDD, N
      USE VARIABLE_DATA, ONLY: DS, DS1, QAV, Q, UAV, VAV, WAV, TAV,
     >                         U, V, W, T

      IMPLICIT NONE

      INTEGER :: IST, IND, J, K, NS, JJ, JBD, JCD

      REAL (KIND = 8) :: AF(IST:IND), BF(IST:IND), CF(IST:IND)

      REAL (KIND = 8), PARAMETER :: R23 = 2.0D0 / 3.0D0,
     >                              R13 = 1.0D0 / 3.0D0

      JJ  =  J + JADD
      JBD = JJ - JBDD
      JCD = JJ + JBDD

      IF ( JADD .EQ. 1 ) THEN
         AF(IST:IND) = R23 * (1.0D0 -
     >                      DS(IST:IND,J,K,2) * DS1(IST:IND,J,K,2))
         BF(IST:IND) = R13 * (1.0D0 + DS(IST:IND,J+1,K,2) *
     >                                           DS1(IST:IND,J+1,K,2) +
     >                 2.0D0 * DS(IST:IND,J,K,2) * DS1(IST:IND,J,K,2))
         CF(IST:IND) = -R13 * DS(IST:IND,J+1,K,2) * DS1(IST:IND,J+1,K,2)
      ELSE
         AF(IST:IND) = R23 * (1.0D0 - DS(IST:IND,J+1,K,2) *
     >                                            DS1(IST:IND,J,K,2))
         BF(IST:IND) = R13 * (1.0D0 + DS(IST:IND,J,K,2) *
     >                                          DS1(IST:IND,J-1,K,2) +
     >                2.0D0 * DS(IST:IND,J+1,K,2) * DS1(IST:IND,J,K,2))
         CF(IST:IND) = -R13 * DS(IST:IND,J,K,2) * DS1(IST:IND,J-1,K,2)
      ENDIF

      QAV(IST:IND,J,K,1) =       BF(IST:IND) * Q(IST:IND, JJ,K,1,N)
      QAV(IST:IND,J,K,1) = QAV(IST:IND,J,K,1) +
     >                           AF(IST:IND) * Q(IST:IND,JBD,K,1,N)
      QAV(IST:IND,J,K,1) = QAV(IST:IND,J,K,1) +
     >                           CF(IST:IND) * Q(IST:IND,JCD,K,1,N)

      UAV(IST:IND,J,K)   =       BF(IST:IND) * U(IST:IND, JJ,K)
      UAV(IST:IND,J,K)   = UAV(IST:IND,J,K) +
     >                           AF(IST:IND) * U(IST:IND,JBD,K)
      UAV(IST:IND,J,K)   = UAV(IST:IND,J,K) +
     >                           CF(IST:IND) * U(IST:IND,JCD,K)

      VAV(IST:IND,J,K)   =       BF(IST:IND) * V(IST:IND, JJ,K)
      VAV(IST:IND,J,K)   = VAV(IST:IND,J,K) +
     >                           AF(IST:IND) * V(IST:IND,JBD,K)
      VAV(IST:IND,J,K)   = VAV(IST:IND,J,K) +
     >                           CF(IST:IND) * V(IST:IND,JCD,K)

      WAV(IST:IND,J,K)   =       BF(IST:IND) * W(IST:IND, JJ,K)
      WAV(IST:IND,J,K)   = WAV(IST:IND,J,K) +
     >                           AF(IST:IND) * W(IST:IND,JBD,K)
      WAV(IST:IND,J,K)   = WAV(IST:IND,J,K) +
     >                           CF(IST:IND) * W(IST:IND,JCD,K)

      TAV(IST:IND,J,K)   =       BF(IST:IND) * T(IST:IND, JJ,K)
      TAV(IST:IND,J,K)   = TAV(IST:IND,J,K) +
     >                           AF(IST:IND) * T(IST:IND,JBD,K)
      TAV(IST:IND,J,K)   = TAV(IST:IND,J,K) +
     >                           CF(IST:IND) * T(IST:IND,JCD,K)

      RETURN
      END
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      SUBROUTINE VISCJ (I1, I2, J, K, FSJ)

      USE GENERAL_DATA, ONLY: ISTART, JSTART, KSTART,
     >                        JCMAX, JBDD, JADD, R2D3, ND, PR, CMU,
     >                        IPERIODIC, JPERIODIC, KPERIODIC,
     >                        IMAX, JMAX, KMAX, NSCHEME, R6I, R12I,
     >                        R2, R8, R2I, R3, R4, HALF
      USE VARIABLE_DATA, ONLY: U, V, W, T, UAV, VAV, WAV, TAV,
     >                         T11, T12, T13, T21, T22, T23,
     >                         T31, T32, T33, HF, SJX, SJY, SJZ

      IMPLICIT NONE

      REAL (KIND = 8), INTENT(INOUT), DIMENSION(I1:I2,0:JCMAX,ND) :: FSJ

      INTEGER, INTENT(IN) :: I1, I2, J, K
      INTEGER :: IG, JG, KG, I, JJ, JBD, JCD
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

      JJ  = J + JADD
      JBD = JJ - JBDD
      JCD = JJ + JBDD
      ABD = DBLE(JBDD)

      JG = J + JSTART - 1
      KG = K + KSTART - 1

      DO I = I1, I2
         IG = I + ISTART - 1

         IF (.NOT. IPERIODIC .AND. IG .EQ. IMAX) THEN
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

         DUDX = T11(I,J,K,2) * DUDXI
         DVDX = T11(I,J,K,2) * DVDXI
         DWDX = T11(I,J,K,2) * DWDXI
         DTDX = T11(I,J,K,2) * DTDXI
 
         DUDY = T21(I,J,K,2) * DUDXI
         DVDY = T21(I,J,K,2) * DVDXI
         DWDY = T21(I,J,K,2) * DWDXI
         DTDY = T21(I,J,K,2) * DTDXI
 
         DUDZ = T31(I,J,K,2) * DUDXI
         DVDZ = T31(I,J,K,2) * DVDXI
         DWDZ = T31(I,J,K,2) * DWDXI
         DTDZ = T31(I,J,K,2) * DTDXI
 
         IF ((.NOT. JPERIODIC .AND.
     >               (JG .EQ. 0 .OR. JG .EQ. JMAX-1)) .OR.
     >                                         NSCHEME .EQ. 2) THEN
            DUDET = U(I,J+1,K) - U(I,J,K)
            DVDET = V(I,J+1,K) - V(I,J,K)
            DWDET = W(I,J+1,K) - W(I,J,K)
            DTDET = T(I,J+1,K) - T(I,J,K)
         ELSE
            DUDET = ABD * ((U(I,JBD,K) - U(I,JCD,K)) +
     >                   R8 * (U(I,JJ ,K) - U(I,JBD,K))) * R6I
            DVDET = ABD * ((V(I,JBD,K) - V(I,JCD,K)) +
     >                   R8 * (V(I,JJ ,K) - V(I,JBD,K))) * R6I
            DWDET = ABD * ((W(I,JBD,K) - W(I,JCD,K)) +
     >                   R8 * (W(I,JJ ,K) - W(I,JBD,K))) * R6I
            DTDET = ABD * ((T(I,JBD,K) - T(I,JCD,K)) +
     >                   R8 * (T(I,JJ ,K) - T(I,JBD,K))) * R6I
         ENDIF

         DUDX = DUDX + T12(I,J,K,2) * DUDET
         DVDX = DVDX + T12(I,J,K,2) * DVDET
         DWDX = DWDX + T12(I,J,K,2) * DWDET
         DTDX = DTDX + T12(I,J,K,2) * DTDET
 
         DUDY = DUDY + T22(I,J,K,2) * DUDET
         DVDY = DVDY + T22(I,J,K,2) * DVDET
         DWDY = DWDY + T22(I,J,K,2) * DWDET
         DTDY = DTDY + T22(I,J,K,2) * DTDET
 
         DUDZ = DUDZ + T32(I,J,K,2) * DUDET
         DVDZ = DVDZ + T32(I,J,K,2) * DVDET
         DWDZ = DWDZ + T32(I,J,K,2) * DWDET
         DTDZ = DTDZ + T32(I,J,K,2) * DTDET
 
         IF ((.NOT. KPERIODIC .AND.
     >               (KG .EQ. 1 .OR. KG .EQ. KMAX-1)) .OR.
     >                                              NSCHEME .EQ. 2) THEN
            DUDZT = (UAV(I,J,K+1) - UAV(I,J,K-1)) * R2I
            DVDZT = (VAV(I,J,K+1) - VAV(I,J,K-1)) * R2I
            DWDZT = (WAV(I,J,K+1) - WAV(I,J,K-1)) * R2I
            DTDZT = (TAV(I,J,K+1) - TAV(I,J,K-1)) * R2I
         ELSE
            DUDZT = R12I * (UAV(I,J,K-2) - UAV(I,J,K+2) +
     >                R8 * (UAV(I,J,K+1) - UAV(I,J,K-1)))
            DVDZT = R12I * (VAV(I,J,K-2) - VAV(I,J,K+2) +
     >                R8 * (VAV(I,J,K+1) - VAV(I,J,K-1)))
            DWDZT = R12I * (WAV(I,J,K-2) - WAV(I,J,K+2) +
     >                R8 * (WAV(I,J,K+1) - WAV(I,J,K-1)))
            DTDZT = R12I * (TAV(I,J,K-2) - TAV(I,J,K+2) +
     >                R8 * (TAV(I,J,K+1) - TAV(I,J,K-1)))
         ENDIF

         DUDX = DUDX + T13(I,J,K,2) * DUDZT
         DVDX = DVDX + T13(I,J,K,2) * DVDZT
         DWDX = DWDX + T13(I,J,K,2) * DWDZT
         DTDX = DTDX + T13(I,J,K,2) * DTDZT
 
         DUDY = DUDY + T23(I,J,K,2) * DUDZT
         DVDY = DVDY + T23(I,J,K,2) * DVDZT
         DWDY = DWDY + T23(I,J,K,2) * DWDZT
         DTDY = DTDY + T23(I,J,K,2) * DTDZT
 
         DUDZ = DUDZ + T33(I,J,K,2) * DUDZT
         DVDZ = DVDZ + T33(I,J,K,2) * DVDZT
         DWDZ = DWDZ + T33(I,J,K,2) * DWDZT
         DTDZ = DTDZ + T33(I,J,K,2) * DTDZT
 
         TEMP = R2I * (T(I,J,K) +    T(I,J+1,K)) 
         UAVE = R2I * (U(I,J,K) +    U(I,J+1,K))
         VAVE = R2I * (V(I,J,K) +    V(I,J+1,K))
         WAVE = R2I * (W(I,J,K) +    W(I,J+1,K))
         CPAV = R2I * (HF(I,J,K,2) + HF(I,J+1,K,2))

         RMU    = CMU * TEMP * SQRT(TEMP) / (TEMP + 110.0D0)
         DIV    = DUDX + DVDY + DWDZ
         RK     = RMU * CPAV /PR
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

         QX  = -RK * DTDX
         QY  = -RK * DTDY
         QZ  = -RK * DTDZ

         SX = SJX(I,J,K)
         SY = SJY(I,J,K)
         SZ = SJZ(I,J,K)

         FSJ(I,J,2) = FSJ(I,J,2) + TXX * SX + TXY * SY + TXZ * SZ
         FSJ(I,J,3) = FSJ(I,J,3) + TYX * SX + TYY * SY + TYZ * SZ
         FSJ(I,J,4) = FSJ(I,J,4) + TZX * SX + TZY * SY + TZZ * SZ
         FSJ(I,J,5) = FSJ(I,J,5) +
     >               (TXX*UAVE + TXY*VAVE + TXZ*WAVE + QX) * SX +
     >               (TYX*UAVE + TYY*VAVE + TYZ*WAVE + QY) * SY +
     >               (TZX*UAVE + TZY*VAVE + TZZ*WAVE + QZ) * SZ
      ENDDO

      RETURN
      END
