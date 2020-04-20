      SUBROUTINE ALLOC ()

      USE GENERAL_DATA, ONLY: ICMAX, JCMAX, KCMAX, ND, INFLOW
      USE VARIABLE_DATA

      IMPLICIT NONE

      INTEGER :: ISTATUS

      INTEGER (KIND = 8) :: NWORDS = 0

      ALLOCATE ( X(-2:ICMAX+4,-2:JCMAX+4,-2:KCMAX+4),
     >           Y(-2:ICMAX+4,-2:JCMAX+4,-2:KCMAX+4),
     >           Z(-2:ICMAX+4,-2:JCMAX+4,-2:KCMAX+4), STAT = ISTATUS )
      IF ( ISTATUS .NE. 0 ) THEN
         CALL EJECT ( 'ALLOC: XYZ' )
      ENDIF
      NWORDS = NWORDS + 3 * (ICMAX + 7) * (JCMAX + 7) * (KCMAX + 7)

      ALLOCATE ( VOL(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           DTV(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           SIX(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           SIY(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           SIZ(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           SJX(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           SJY(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           SJZ(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           SKX(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           SKY(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           SKZ(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >        SIJKSQ(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >                                     STAT = ISTATUS )
      IF ( ISTATUS .NE. 0 ) THEN
         CALL EJECT ( 'ALLOC: SURFACE AREAS' )
      ENDIF
      NWORDS = NWORDS + 13 * (ICMAX + 6) * (JCMAX + 6) * (KCMAX + 6)

      ALLOCATE (  Q(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,ND,2),
     >          QAV(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,ND),
     >           DQ(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,ND),
     >                                     STAT = ISTATUS )
      IF ( ISTATUS .NE. 0 ) THEN
         CALL EJECT ( 'ALLOC: Q' )
      ENDIF
      NWORDS = NWORDS + 4 * ND * (ICMAX + 6) * (JCMAX + 6) * (KCMAX + 6)

      ALLOCATE ( U(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           V(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           W(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           T(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >           P(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >          HF(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,4),
     >         UAV(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >         VAV(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >         WAV(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >         TAV(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >         PAV(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3),
     >                                     STAT = ISTATUS )
      IF ( ISTATUS .NE. 0 ) THEN
         CALL EJECT ( 'ALLOC: UVW' )
      ENDIF
      NWORDS = NWORDS + 14 * (ICMAX + 6) * (JCMAX + 6) * (KCMAX + 6)

      IF ( INFLOW .EQ. 1 ) THEN
         ALLOCATE ( UIN(-2:JCMAX+3,-2:KCMAX+3),
     >              VIN(-2:JCMAX+3,-2:KCMAX+3),
     >              WIN(-2:JCMAX+3,-2:KCMAX+3),
     >              TIN(-2:JCMAX+3,-2:KCMAX+3),
     >            BOUND(-2:JCMAX+3,-2:KCMAX+3,ND),
     >             URMS(-2:JCMAX+3,-2:KCMAX+3),
     >             VRMS(-2:JCMAX+3,-2:KCMAX+3),
     >             WRMS(-2:JCMAX+3,-2:KCMAX+3),
     >                                     STAT = ISTATUS )
         IF ( ISTATUS .NE. 0 ) THEN
            CALL EJECT ( 'ALLOC: INFLOW-BC' )
         ENDIF
         NWORDS = NWORDS + (8 + ND) * (JCMAX + 6) * (KCMAX + 6)
      ENDIF

      ALLOCATE ( T11(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,3),
     >           T12(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,3),
     >           T13(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,3),
     >           T21(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,3),
     >           T22(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,3),
     >           T23(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,3),
     >           T31(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,3),
     >           T32(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,3),
     >           T33(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,3),
     >           DS1(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,3),
     >            DS(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3,3),
     >                                     STAT = ISTATUS )
      IF ( ISTATUS .NE. 0 ) THEN
         CALL EJECT ( 'ALLOC: METRICS' )
      ENDIF
      NWORDS = NWORDS +
     >       (36 + 6) * (ICMAX + 6) * (JCMAX + 6) * (KCMAX + 6)

1000  FORMAT(' PROCESS: ',I4,' TOTAL ALLOCATED SPACE (KB) = ',I8)

      END SUBROUTINE ALLOC
