!---------------------------------------------------------------------
!-- SIMPLE CALL ROUTINES FOR COMPUTING THERMAL PROPERTIES
!---------------------------------------------------------------------
      SUBROUTINE THERMAL_PROPS ( I1, I2, J, K, HF, CP, CV, RG, T )

      USE GENERAL_DATA, ONLY: ICHEM, CPAIR, CVAIR, RGAIR,
     >                        ICMAX, JCMAX, KCMAX

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: I1, I2, J, K

      REAL (KIND = 8), DIMENSION (-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3) ::
     >             HF, CP, CV, RG, T

      IF ( ICHEM .EQ. 0 ) THEN
         CP(I1:I2,J,K) = CPAIR
         CV(I1:I2,J,K) = CVAIR
         RG(I1:I2,J,K) = RGAIR
      ELSE 
         CALL EJECT ('ICHEM NOT SET PROPERLY')
      ENDIF

      RETURN
      END
!---------------------------------------------------------------------
      SUBROUTINE INTERNAL_ENERGY ( I1, I2, HF, CP, CV, RG, T, IE )

      USE GENERAL_DATA, ONLY:

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: I1, I2

      REAL (KIND = 8), DIMENSION (I1:I2), INTENT (IN) ::
     >                                HF, CP, CV, RG, T

      REAL (KIND = 8), DIMENSION (I1:I2), INTENT (OUT) :: IE

      IE(I1:I2) = CV(I1:I2) * T(I1:I2)

      RETURN
      END
