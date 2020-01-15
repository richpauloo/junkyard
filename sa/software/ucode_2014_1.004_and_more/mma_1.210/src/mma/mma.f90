PROGRAM MMA
!
! This program reads a file with user provided
! root name and paths to model results that are to be
! analyzed (the path must include the rootfilename).
! This program gathers data from those directories, analyzes
! it and prints the results.
!
  USE GLOBAL_DATA, ONLY: AMESSAGE, HYPHENS, ERRSUB, MAX_STRING_LEN, VERSIONID
  USE UTILITIES
  USE MMA_MOD, ONLY: &
    ! subroutines
    MMA_OPEN, MMA_INI, MMA_INI_COLHD, MMA_INI_MODELS, MMA_INI_PREDS, &
    MMA_EVA_EXTREMES,  MMA_EVA_DEVALUE, MMA_EVA_PCREAS,  &
    MMA_READ_DM, MMA_READ_DM_INI, &
    MMA_EVA_ALLOC,  MMA_EVA_CENT, MMA_EVA_LINES, &
    MMA_EVA_PC, MMA_EVA_PREDYPD, MMA_EVA_TIME, &
    MMA_ANL_ALL, MMA_ANL_RANK, MMA_CLN, &
    ! Variables
    CONVREAS, COLHD, NINCL, IUMSG, IUMPTHS, IUOUT, IUOUTG, IUOUTXYZT, MPR, &
    NAMEMODEL, NAMEPATH, NCOL, NMPR, K1, K2, K3, NMPTHS, NOTCONV, NOTREAS, &
    PARAMEQNL, STATS, STATSPERCENT, TIME_USED, XYZFILE
  !
  IMPLICIT NONE
  LOGICAL                                        :: AVGPREDL = .FALSE.
  LOGICAL                                        :: AVGPARAML = .FALSE.
  DOUBLE PRECISION                               :: DENOM = 1.D+30
  LOGICAL                                        :: CHECKPARAM
  LOGICAL                                        :: CONVERGED = .FALSE.
  INTEGER                                        :: I1
  INTEGER                                        :: I2
  INTEGER                                        :: I3
  INTEGER                                        :: IA
  INTEGER                                        :: II
  INTEGER                                        :: IIOP
  INTEGER                                        :: IFAIL = 0
  INTEGER                                        :: IMODEL = 0
  CHARACTER(LEN=MAX_STRING_LEN)                  :: INNAM = ' '
  INTEGER                                        :: MAXNOBS = 0
  INTEGER                                        :: MNOBS = 0
  INTEGER                                        :: NOBS = 0
  INTEGER                                        :: NOBSMPR = 0
  CHARACTER(LEN=MAX_STRING_LEN)                  :: OUTNAM = ' '
  DOUBLE PRECISION                               :: VERSION = 1.210D0
  CHARACTER(LEN=5)                               :: VERSIONTMP = ' '
  CHARACTER(LEN=20)                              :: VERSIONMIN = '1.4.0'
  !
  10 FORMAT(//,80('*'),/,80('*'),/,1X,'Executing MMA,  Version: ',F7.3,A5, &
            /,1X,'Constructed using the JUPITER API, Version: ',A14,/,80('*'))
  11  FORMAT('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
  19  FORMAT(/,1X,'Models Evaluated: ',/)
  23 FORMAT(I6,' ANALYZED:                    ',A12,' in ',A)
  103 FORMAT(/,'MODELS WILL BE LISTED AS ONE OF THE FOLLOWING:',/, &
               '  ANALYZED',/, &
               '  NOT CONVERGED, or',/, &
               '  UNREASONABLE PARAMETERS (given the user specified ', &
               'parameter equations)',/)
  197 FORMAT(/,1X,I6,' MODELS were evaluated, of those: ',/,7X,I5, &
            ' models CONVERGED and ',/,7X,I5, &
            ' models DID NOT CONVERGE ',/)
  198 FORMAT(/,1X,I6,' MODELS CONVERGED, of those: ',/,7X,I5, &
            ' models had REASONABLE PARAMETERS and ',/,7X,I5, &
            ' models had UNREASONABLE PARAMETERS ',/)
  199 FORMAT(/,1X,I6,' MODELS will be ranked and weighted',/)
  200 FORMAT(1X,'"ID#"      "',A12,'"',13(1X,'"',A12,'"'))
  201 FORMAT(1X,'"ID#"      "',A12,'"',11(1X,'"',A12,'"'))
  202 FORMAT(1X,'"ID#"      "',A12,'"',22(1X,'"',A12,'"'))
  203 FORMAT(I6,A20,3I15,9(1PE15.7),1X,A)
  204 FORMAT(I6,A20,10(1PE15.7),1X,A)
  205 FORMAT(I6,A20,21(1PE15.7),1X,A)
  300 FORMAT(' "ID#"      "MODEL       " "NPE         " "NOBS        "', &
    ' "NPR         " "SWSR_PR-O   " "CEV_PR-O    " "MLOF_PR-O   "', &
    ' "AIC_PR-O    " "AICC_PR-O   " "BIC_PR-O    " "KIC_PR-O    "', &
    ' "XTWX_PR-O   " "PriModP_PR-O" "PATHANDROOT "')
   301 FORMAT(' "ID#"      "MODEL       " "R2_OSPR-O   " ', &
    '"INT_OSPR-O  " "SLP_OSPR-O  " "R2_WSPR-O   " "INT_WSPR-O  " ', &
    '"SLP_WSPR-O  " "R2_WRWOPR-O " "INT_WRWOPR-O" "SLP_WRWOPR-O" ', &
    '"RN2PR-O     " "PATHANDROOT "')
  400 FORMAT(' "ID#"      "MODEL       " "NPE         " "NOBS        "', &
    ' "NPR         " "SWSR_%      " "CEV_%       " "MLOF_%      "', &
     ' "AIC_%       " "AICC_%      " "BIC_%       " "KIC_%       ', &
     '" "XTWX_%      " "PriModP_%   " "PATHANDROOT "')
  401 FORMAT(' "ID#"      "MODEL       " "R2_OS_%     " ', &
    '"INT_OS_%    " "SLP_OS_%    " "R2_WS_%     " "INT_WS_%    " ', &
    '"SLP_WS_%    " "R2_WRWO_%   " "INT_WRWO_%  " "SLP_WRWO_%  " ', &
    '"RN2_%       " "PATHANDROOT "')
  900 FORMAT(/,' FOR MODEL: ',A,' PATH ',/,A,//, &
               ' ERROR! MODELS MUST HAVE THE SAME NUMBER OF OBSERVATIONS.'/)
  901 FORMAT(/,' FOR MODEL: ',A,' PATH ',/,A,//, &
               ' OBSERVATION NAMES DO NOT MATCH OR DO NOT OCCUR IN THE ',/, &
               ' SAME ORDER AS IN OTHER UNDERSCORE FILES.',//, &
               ' CALCULATIONS WOULD BE INCORRECT, SO MMA IS TERMINATING.',//, &
               ' MIGHT THERE BE A MIX OF REGRESSION OUTPUTS IN THE PATH? ',/)
  902 FORMAT(/,' FOR MODEL: ',A,' PATH ',/,A,//, &
               ' OBSERVATIONS DO NOT OCCUR IN THE SAME ORDER IN _w & _r ',//, &
               ' CALCULATIONS WILL BE INCORRECT, SO MMA IS TERMINATING.',//, &
               ' MIGHT THERE BE A MIX OF REGRESSION OUTPUTS IN THE PATH? ',/)
  903 FORMAT(/,' NONE OF THE MODELS QUALIFY FOR EVALUATION',//, &
             ' EITHER THEY DID NOT CONVERGE OR',/, &
             ' THEY DID NOT MEET THE REASONABLE PARAMETER CRITERION SET', &
             ' FORTH IN THE PARAM_EQNS INPUT BLOCK.',/)
  904 FORMAT(/,' EVALUATED MODELS DO NOT HAVE CONSISTENT UNITS',/)
  905 FORMAT(/,' Error: insufficient number of observations in xyzt file',/)  ! Command line should include 1) InputFileName 2) RootNameForOutputFiles
!write(*,*)' enter in file' ! only for debugging
!read(*,*) INNAM ! only for debugging
  ! Get command-line argument for input file name
  INNAM = UTL_GETARG(1)
  ! Open the input data file
  CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUMPTHS)
  IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN to open '//TRIM(INNAM)//ERRSUB)
!write(*,*)' enter root' ! only for debugging
!read(*,*) OUTNAM ! only for debugging
  ! Get command-line argument for rootname of output files
  OUTNAM = UTL_GETARG(2)
  ! Open MMA output files and write intial information
  CALL MMA_INI(IFAIL,OUTNAM,VERSION,VERSIONTMP,VERSIONMIN)
  IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
  ! CHECK VERSION OF API COMPATIBILITY WITH MMA
  WRITE(*,10)VERSION,VERSIONTMP,VERSIONID
  CALL UTL_VERSION_CHECK(VERSIONMIN,IFAIL)
  IF(IFAIL < 0) THEN
    AMESSAGE = ' Programming error:  Version '//TRIM(VERSIONMIN)//   &
               ' of JUPITER API is required.  Version in use is '   &
               //TRIM(VERSIONID)//' -- Please download newer version '   &
               //'of JUPITER API from U.S. Geological Survey'//   &
               ' JUPITER web site.'
    CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    CLOSE(IUMSG)
    CALL UTL_STOP()
  ENDIF
  ! Read Input
  CALL MMA_INI_MODELS(IFAIL,AVGPARAML)
  IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
  ! Initialize Column Headings
  CALL MMA_INI_COLHD(IFAIL)
  IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
  ! CHECK FOR MAXIMUM NUMBER OF OBSERVATIONS
  MAXNOBS = 0
  DO IMODEL = 1,NMPTHS
    ! Get units, dtla, stats RN2_dep RN2_dep_pri from _dm and check same # obs
    CALL MMA_READ_DM_INI(IFAIL,IMODEL,MNOBS)
    IF(MNOBS > MAXNOBS)MAXNOBS = MNOBS
  ENDDO
  ! Process files for each model
  DO IMODEL = 1,NMPTHS
    ! Get units, dtla, stats RN2_dep RN2_dep_pri from _dm and check same # obs
    CALL MMA_READ_DM(IFAIL,IMODEL,MAXNOBS,CONVERGED)
    IF(IFAIL > 0) GO TO 9004
    ! First time through NOBS=0, set, after that check that all have same # OBS
    IF (NOBS.EQ.0) THEN
      NOBS = NINCL
      NOBSMPR = NOBS + MPR
      CALL MMA_EVA_ALLOC(IFAIL,NOBS,NOBSMPR)
      IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
    ENDIF
    ! ALL MODELS MUST HAVE SAME NUMBER OF OBS, IF NOT, QUIT and WRITE Message
    IF(NINCL .NE. MAXNOBS) GO TO 9000
    ! If this model converged and # obs are correct then calulate stats
    IF(CONVERGED) THEN
      ! EVALUATE IF OPTIMAL PARAMETERS ARE REASONABLE BASED ON PARAMETER EQNS
      IF(PARAMEQNL) THEN
        CHECKPARAM = .TRUE.
        CALL MMA_EVA_PCREAS(IFAIL,IMODEL,CHECKPARAM)
        IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
        IF(.NOT. CHECKPARAM) THEN
          CALL MMA_EVA_EXTREMES(IFAIL,IMODEL)
          IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
          CYCLE
        ENDIF
      ENDIF
      ! Calulate R2 intercept and slope for os ww ws
      CALL MMA_EVA_LINES(IFAIL,IMODEL,NOBS)
      IF(IFAIL .NE. 0) GO TO 9001
      ! Calculate Centroids and related stats
      ! because it has r and w associated with xyzt
      CALL MMA_EVA_CENT(IFAIL,IMODEL,NOBS)
      IF(IFAIL .EQ. -1) GO TO 9005
      IF(IFAIL .EQ. 1) GO TO 9002
      ! Calculate R2 intercept and slope for residuals vs time
      IF(TIME_USED) THEN
        CALL MMA_EVA_TIME(IFAIL,IMODEL,NOBS)
        IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
        IF(IMODEL == 1) THEN
          WRITE(*,*)
          WRITE(*,'(A)')HYPHENS(1:80)
          WRITE(*,'(A)')HYPHENS(1:80)
          WRITE(IUMSG,'(A)')HYPHENS(1:80)
          WRITE(IUMSG,'(A)')HYPHENS(1:80)
          WRITE(IUMSG,103)
          WRITE(*,19)
          WRITE(IUMSG,19)
        ENDIF
      ENDIF
      IF(STATS(IMODEL,4) < 1.D+29) THEN
        WRITE(*,23)IMODEL,TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
        WRITE(IUMSG,23)IMODEL,TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
      ENDIF
    ELSE
    ! If this model did not converge fill stats with extremes to rank them low
      CALL MMA_EVA_EXTREMES(IFAIL,IMODEL)
      IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
    ENDIF
  ENDDO
  CONVREAS = NMPTHS - NOTCONV - NOTREAS
  WRITE(IUMSG,197)NMPTHS,NMPTHS-NOTCONV,NOTCONV
  WRITE(IUMSG,198)NMPTHS-NOTCONV,NMPTHS-NOTCONV-NOTREAS,NOTREAS
  WRITE(IUMSG,199)CONVREAS
  WRITE(IUMSG,'(A)')HYPHENS(1:80)
  WRITE(IUMSG,'(A)')HYPHENS(1:80)
  WRITE(*,197)NMPTHS,NMPTHS-NOTCONV,NOTCONV
  WRITE(*,198)NMPTHS-NOTCONV,NMPTHS-NOTCONV-NOTREAS,NOTREAS
  WRITE(*,199)CONVREAS
  WRITE(*,'(A)')HYPHENS(1:80)
  WRITE(*,'(A)')HYPHENS(1:80)
  IF(CONVREAS <= 0) GO TO 9003
  !Devalue measures that depend on prior if prior are not the same in all models
  CALL MMA_EVA_DEVALUE()
  ! Write column headers in _mma file
  WRITE(IUOUT,200)COLHD(NCOL-1,1),(COLHD(IA,1),IA=1,3), &
                  (COLHD(IA,1),IA=4,K1,2),COLHD(NCOL,1)
  WRITE(IUOUTG,201)COLHD(NCOL-1,1),(COLHD(IA,1),IA=K1+1,K2),COLHD(NCOL,1)
  IF(XYZFILE) WRITE(IUOUTXYZT,202) &
                  COLHD(NCOL-1,1),(COLHD(IA,1),IA=K3+1,K3+21),COLHD(NCOL,1)
  ! Write model measures obs only to _mma file
  DO IMODEL = 1,NMPTHS
    I1 = STATS(IMODEL,1)
    I2 = STATS(IMODEL,2)
    I3 = STATS(IMODEL,3)
    WRITE(IUOUT,203)IMODEL,TRIM(NAMEMODEL(IMODEL)),I1,I2,I3, &
                    (STATS(IMODEL,II),II=4,K1,2),TRIM(NAMEPATH(IMODEL))
    WRITE(IUOUTG,204)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
                    (STATS(IMODEL,II),II=K1+1,K2),TRIM(NAMEPATH(IMODEL))
    IF(XYZFILE) WRITE(IUOUTXYZT,205)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
                    (STATS(IMODEL,II),II=K3+1,K3+21),TRIM(NAMEPATH(IMODEL))
  ENDDO
  IF(NMPR > 0) THEN
    WRITE(IUOUT,200)COLHD(NCOL-1,1),(COLHD(IA,1),IA=1,3), &
                    (COLHD(IA,1),IA=5,K1,2),COLHD(K1,1),COLHD(NCOL,1)
    WRITE(IUOUTG,201)COLHD(NCOL-1,1),(COLHD(IA,1),IA=K2+1,K3),COLHD(NCOL,1)
    ! Write model measures w Prior to _mma file
    DO IMODEL = 1,NMPTHS
      I1 = STATS(IMODEL,1)
      I2 = STATS(IMODEL,2)
      I3 = STATS(IMODEL,3)
      WRITE(IUOUT,203)IMODEL,TRIM(NAMEMODEL(IMODEL)),I1,I2,I3, &
           (STATS(IMODEL,II),II=5,K1,2),STATS(IMODEL,K1),TRIM(NAMEPATH(IMODEL))
      WRITE(IUOUTG,204)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
                      (STATS(IMODEL,II),II=K2+1,K3),TRIM(NAMEPATH(IMODEL))
    ENDDO
    WRITE(IUOUT,300)
    WRITE(IUOUTG,301)
    ! Write model measures diff ObOnly and Prior to _mma file
    I1 = 0
    I2 = 0
    I3 = 0
    DO IMODEL = 1,NMPTHS
      WRITE(IUOUT,203)IMODEL,TRIM(NAMEMODEL(IMODEL)),I1,I2,I3, &
      ((STATS(IMODEL,II+1)-STATS(IMODEL,II)),II=4,K1-1,2),0.0D0,TRIM(NAMEPATH(IMODEL))

      WRITE(IUOUTG,204)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
      ((STATS(IMODEL,II+10)-STATS(IMODEL,II)),II=K1+1,K2),TRIM(NAMEPATH(IMODEL))
      IIOP = 2
      DO II = 1,8
        IIOP = IIOP + 1
!        DENOM = ((STATS(IMODEL,II+IIOP+1)+STATS(IMODEL,II+IIOP)))/2.D0
        DENOM = STATS(IMODEL,II+IIOP)
        IF(DENOM .NE. 0.D0 .AND. STATS(IMODEL,II+IIOP+1) < 1.0D+30) &
          STATSPERCENT(IMODEL,II) = &
                100.D0*((STATS(IMODEL,II+IIOP+1)-STATS(IMODEL,II+IIOP))/DENOM)
      ENDDO
      DO II = 1,10
!        DENOM = ((STATS(IMODEL,K2+II)+STATS(IMODEL,K1+II)))/2.D0
        DENOM = STATS(IMODEL,K1+II)
        IF(DENOM .NE. 0.D0 .AND. STATS(IMODEL,K2+II) < 1.0D+30) &
          STATSPERCENT(IMODEL,9+II) = &
                100.D0*((STATS(IMODEL,K2+II)-STATS(IMODEL,K1+II))/DENOM)
      ENDDO
      STATSPERCENT(IMODEL,20) = 0.D0
    ENDDO
    WRITE(IUOUT,400)
    WRITE(IUOUTG,401)
    ! Write model measures diff ObOnly and Prior to _mma file
    DO IMODEL = 1,NMPTHS
      WRITE(IUOUT,203)IMODEL,TRIM(NAMEMODEL(IMODEL)),I1,I2,I3, &
      (STATSPERCENT(IMODEL,II),II=1,9),TRIM(NAMEPATH(IMODEL))
      WRITE(IUOUTG,204)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
      (STATSPERCENT(IMODEL,II),II=10,19),TRIM(NAMEPATH(IMODEL))
    ENDDO
  ENDIF
  ! Rank Models
  CALL MMA_ANL_RANK(IFAIL)
  IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
  ! Check if user entered names of predictions to be averaged
  ! If so read and allocate arrays
  CALL MMA_INI_PREDS(IFAIL,AVGPREDL)
  IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
  ! If the user entered a pred block with at least one prediction listed:
  ! Read predictions made with each model. If AVGPREDL returns FALSE
  ! Skip averaging predictions because some converged model does not have
  ! _linp file, or not all of the listed predictions are included in every file
  ! Otherwise CALCULATE MODEL-AVERAGED PREDICTIONS AND VARIANCE
  IF(AVGPREDL)CALL MMA_EVA_PREDYPD(IFAIL,AVGPREDL)
  IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
  ! If the user entered a param block with at least one parameter listed:
  ! If some specified and converged model
  ! does not have the necessary info
  ! AVGPARAML returns FALSE and MMA will skip averaging parameters
  IF(AVGPARAML)CALL MMA_EVA_PC(IFAIL,AVGPARAML)
  IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
  ! DO ANALYSES and as appropriate, model-average predictions and parameters
  CALL MMA_ANL_ALL(IFAIL,AVGPARAML,AVGPREDL)
  IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
  !
  IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: Failed in MM_CLN ')
  ! Indicate completion status
  ! Successful Completion
  WRITE(*,*)
  WRITE(*,'(A)')HYPHENS(1:80)
  WRITE(*,*)' MMA COMPLETED SUCCESSFULLY '
  WRITE(*,'(A)')HYPHENS(1:80)
  WRITE(IUMSG,*)
  WRITE(IUMSG,'(A)')HYPHENS(1:80)
  WRITE(IUMSG,*)' MMA COMPLETED SUCCESSFULLY '
  WRITE(IUMSG,'(A)')HYPHENS(1:80)
  GO TO 9100
  ! Unsuccessful Completion Different # Obs in evaluated models
  ! Model selection is only valid for cases with the same observations
  9000 WRITE(*,*)
  WRITE(*,11)
  WRITE(*,900)TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
  WRITE(*,11)
  WRITE(IUMSG,*)
  WRITE(IUMSG,11)
  WRITE(IUMSG,900)TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
  WRITE(IUMSG,11)
  GO TO 9100
  ! Unsuccessful Completion Different Obs Names
  ! Model selection is only valid for cases with the same observations
  9001 WRITE(*,*)
  WRITE(*,11)
  WRITE(*,901)TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
  WRITE(*,11)
  WRITE(IUMSG,*)
  WRITE(IUMSG,11)
  WRITE(IUMSG,901)TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
  WRITE(IUMSG,11)
  GO TO 9100
  ! Unsuccessful Completion Different Obs or Order in w and r files
  ! Model selection is only valid for cases with the same observations
  9002 WRITE(*,*)
  WRITE(*,11)
  WRITE(*,902)TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
  WRITE(*,11)
  WRITE(IUMSG,*)
  WRITE(IUMSG,11)
  WRITE(IUMSG,902)TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
  WRITE(IUMSG,11)
  GO TO 9100
  ! Unsuccessful Completion: none of the models converged
  !                          or had reasonable parameter values
  9003 WRITE(*,*)
  WRITE(*,11)
  WRITE(*,903)
  WRITE(*,11)
  WRITE(IUMSG,*)
  WRITE(IUMSG,11)
  WRITE(IUMSG,903)
  WRITE(IUMSG,11)
  GO TO 9100
  ! Unsuccessful Completion: models have inconsistent units
  9004 WRITE(*,*)
  WRITE(*,11)
  WRITE(*,904)
  WRITE(*,11)
  WRITE(IUMSG,*)
  WRITE(IUMSG,11)
  WRITE(IUMSG,904)
  WRITE(IUMSG,11)
  CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
  GO TO 9100
  ! Unsuccessful Completion Insufficient Number of Observations in XYZT file
  ! Model selection is only valid for cases with the same observations
  9005 WRITE(*,*)
  WRITE(*,11)
  WRITE(*,905)
  WRITE(*,11)
  WRITE(IUMSG,*)
  WRITE(IUMSG,11)
  WRITE(IUMSG,905)
  WRITE(IUMSG,11)
  CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
  ! Final Closure of Program and Files
  9100 CONTINUE
  CALL MMA_CLN(IFAIL)
  IF(IFAIL > 0) CALL UTL_STOP(' Called from MAIN: '//ERRSUB)
END PROGRAM MMA
