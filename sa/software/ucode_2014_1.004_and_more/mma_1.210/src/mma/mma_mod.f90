MODULE MMA_MOD
  USE GLOBAL_DATA, ONLY: AMESSAGE, BIGREAL, ERRSUB, IVERB, &
                         HYPHENS, LENDNAM, MAX_STRING_LEN
  USE BASIC, ONLY: &
    ! subroutines
    BAS_INI_GETOPTIONS, BAS_CLN
  USE DATATYPES
  USE EQUATION
  USE PRIOR_INFORMATION, ONLY: LLPTRPRIOR
  USE SENSITIVITY, ONLY: SEN_UEV_DX_WRITE_MATRIX
  USE UTILITIES
  USE UTLUCODE
  IMPLICIT NONE
  SAVE
  ! Public subprograms
  PUBLIC &
    MMA_OPEN, MMA_INI, MMA_INI_COLHD, MMA_INI_PREDS, &
    MMA_INI_MODELS, MMA_EVA_EXTREMES, MMA_EVA_DEVALUE, MMA_EVA_PCREAS,  &
    MMA_READ_DM, MMA_READ_DM_INI,  &
    MMA_EVA_ALLOC,  MMA_EVA_CENT, MMA_EVA_LINES, &
    MMA_EVA_PC, MMA_EVA_PREDYPD, MMA_EVA_TIME, &
    MMA_ANL_ALL, MMA_ANL_RANK, MMA_CLN, &
  ! PUBLIC variables
    CONVREAS, COLHD, NINCL, IUMSG, IUMPTHS, IUOUT, IUOUTG, IUOUTXYZT, MPR, &
    NAMEMODEL, NAMEPATH, NCOL, NMPR, K1, K2, K3, NMPTHS, NOTCONV, NOTREAS, &
    PARAMEQNL, STATS, STATSPERCENT, TIME_USED, XYZFILE
  PRIVATE
  ! Module variables
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)   :: ANALYSIS
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:,:) :: AVGARRAY
  LOGICAL, ALLOCATABLE, DIMENSION(:)             :: AVGL
  LOGICAL, ALLOCATABLE, DIMENSION(:)             :: AVGPARAMLGP
  LOGICAL, ALLOCATABLE, DIMENSION(:,:)           :: AVGPARAMLGPI
  DOUBLE PRECISION                               :: CEV = 1.D+30
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:,:) :: COLHD
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)   :: COLHDNAME
  INTEGER                                        :: CONVREAS = 0
  LOGICAL, ALLOCATABLE, DIMENSION(:)             :: COORDL
  LOGICAL                                        :: COORDS = .TRUE.
  CHARACTER(LEN=12)                              :: DEFGROUP='Default'
  INTEGER, ALLOCATABLE, DIMENSION(:,:)           :: EQNARRAY
  LOGICAL, ALLOCATABLE, DIMENSION(:)             :: EQNL
  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:)     :: GROUPS
  INTEGER, ALLOCATABLE, DIMENSION(:,:)           :: ILOGARRAY
  INTEGER, ALLOCATABLE, DIMENSION(:,:)           :: ILOGARRAYORIG
  INTEGER, ALLOCATABLE, DIMENSION(:,:)           :: ILOGARRAYSHORT
  INTEGER                                        :: INITOBS = 0
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: IPTRNMODELS
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: IPLOT
  INTEGER                                        :: IPRED = 0
  INTEGER                                        :: ISTART = 0
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: ITER
  INTEGER                                        :: ITERP
  INTEGER                                        :: IUANAL = 0
  INTEGER                                        :: IUMSG = 0
  INTEGER                                        :: IUMPTHS = 0
  INTEGER                                        :: IUOUT = 0
  INTEGER                                        :: IUOUTG = 0
  INTEGER                                        :: IUOUTXYZT = 0
  INTEGER                                        :: IUPARAMNATIVE = 0
  INTEGER                                        :: IUVARNATIVE = 0
  INTEGER                                        :: IUPARAMREGRESS = 0
  INTEGER                                        :: IUVARREGRESS = 0
  INTEGER                                        :: IURANK = 0
  INTEGER                                        :: IURANKG = 0
  INTEGER                                        :: IURANKXYZT = 0
  INTEGER                                        :: KMGP = 0
  CHARACTER(LEN=12)                              :: LENGTH_UNITS = 'NA'
  DOUBLE PRECISION                               :: LNDETFP = 1.D+30
  DOUBLE PRECISION                               :: LNDETFN = 1.D+30
  CHARACTER(LEN=12)                              :: MASS_UNITS = 'NA'
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: MAXAVG
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: MAXEQN
  INTEGER                                        :: MAXMAXAVG
  INTEGER                                        :: MAXMAXEQN
  INTEGER                                        :: MAXNPE = 0
  INTEGER                                        :: MPR = 0
  INTEGER                                        :: MINMPR = 0
  CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:)      :: NAMEL
  CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:)      :: NAMELSHORT
  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:)     :: NAMEMODEL
  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:)     :: NAMEMODELSHORT
  CHARACTER(LEN=MAX_STRING_LEN),ALLOCATABLE,DIMENSION(:) :: NAMEPATH
  CHARACTER(LEN=MAX_STRING_LEN),ALLOCATABLE,DIMENSION(:) :: NAMEPATHSHORT
  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:)     :: NAMEPATHGP
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: NAMEPATHGPN
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: NAMEPATHGPNSHORT
  INTEGER                                        :: NCOL = 63
  INTEGER                                        :: NINCL = 0
  INTEGER                                        :: K1 = 20
  INTEGER                                        :: K2 = 30
  INTEGER                                        :: K3 = 40
  INTEGER                                        :: NMPR = 0
  INTEGER                                        :: NMPTHS = -1
  INTEGER                                        :: NTOTOBS = 0
  INTEGER                                        :: NOTCONV = 0
  INTEGER                                        :: NOTREAS = 0
  INTEGER                                        :: NPARGPSG
  INTEGER                                        :: NPE = 0
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: OBSNAME
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: OBSNAMEALL
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: ORDER
  CHARACTER(LEN=MAX_STRING_LEN)                  :: OUTNAM = ' '
  LOGICAL                                        :: PARAMEQNL = .FALSE.
  DOUBLE PRECISION, ALLOCATABLE,DIMENSION(:,:)   :: PARARRAY
  DOUBLE PRECISION, ALLOCATABLE,DIMENSION(:,:)   :: PARARRAYORIG
  DOUBLE PRECISION, ALLOCATABLE,DIMENSION(:,:)   :: PARARRAYSHORT
  DOUBLE PRECISION, ALLOCATABLE,DIMENSION(:,:)   :: PARARRAYNATIVE
  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:)     :: PARAVG
  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:)     :: PARAVGGP
  INTEGER,ALLOCATABLE,DIMENSION(:)               :: PARAVGGPN
  CHARACTER(LEN=MAX_STRING_LEN),ALLOCATABLE,DIMENSION(:) :: PAREQN
  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:)     :: PAREQNGP
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: PAREQNGPN
  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:)     :: PAREQNNAME
  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:,:)   :: PNARRAY
  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:,:)   :: PNARRAYORIG
  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:,:)   :: PNARRAYSHORT
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: PREDPLOT
  CHARACTER(LEN=20), ALLOCATABLE, DIMENSION(:)   :: PREDNAM
  CHARACTER(LEN=20), ALLOCATABLE, DIMENSION(:)   :: PREDNAMLC
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: PRD
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: PRIMODWT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: PRIMODWTSHORT
  LOGICAL                                        :: PRIORMEASUSED = .FALSE.
  CHARACTER(LEN=20), ALLOCATABLE, DIMENSION(:)   :: PRINAME
  LOGICAL                                        :: PRIORSAME = .TRUE.
  LOGICAL                                        :: PRIORSAMENUM = .TRUE.
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: PVARCIIND
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: PVARCISIM
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: PVARCIINF
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: PVARPIIND
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: PVARPISIM
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: PVARPIINF
  DOUBLE PRECISION, ALLOCATABLE,DIMENSION(:,:)   :: PVARRAY
  DOUBLE PRECISION, ALLOCATABLE,DIMENSION(:,:)   :: PVARRAYORIG
  DOUBLE PRECISION, ALLOCATABLE,DIMENSION(:,:)   :: PVARRAYLOG
  DOUBLE PRECISION, ALLOCATABLE,DIMENSION(:,:)   :: PVARRAYLOGORIG
  DOUBLE PRECISION, ALLOCATABLE,DIMENSION(:,:)   :: PVARRAYSHORT
  DOUBLE PRECISION, ALLOCATABLE,DIMENSION(:,:)   :: PVARRAYNATIVE
  INTEGER, ALLOCATABLE, DIMENSION(:,:)           :: RANK
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: RANKNORM
  LOGICAL                                        :: SKIPANALYSIS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: STATS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: STATSPERCENT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: STATSHORT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: TCOORD
  LOGICAL, ALLOCATABLE, DIMENSION(:)             :: TCOORDL
  LOGICAL                                        :: TCOORDS = .TRUE.
  LOGICAL                                        :: TIME_USED = .FALSE.
  CHARACTER(LEN=12)                              :: TIME_UNITS = 'NA'
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: VAL1
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: VAL2
  DOUBLE PRECISION                               :: VERS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: WEIGHT
  LOGICAL                                        :: WRTPARAMNATIVE = .FALSE.
  CHARACTER(LEN=3)                               :: WRITEPARAMNATIVE = 'no'
  LOGICAL                                        :: WRTPARAMREGRESS = .FALSE.
  CHARACTER(LEN=3)                               :: WRITEPARAMREGRESS = 'no'
  LOGICAL                                        :: WRTPREDS = .FALSE.
  CHARACTER(LEN=3)                               :: WRITEPREDS = 'no'
  LOGICAL                                        :: YPFILE = .FALSE.
  LOGICAL                                        :: YPFILEGEN = .FALSE.
  LOGICAL                                        :: XYZFILE = .FALSE.
  LOGICAL                                        :: XYZFILEEXIST = .TRUE.
  !   For MODEL_GROUPS input, do not define a default order
  INTEGER,           PARAMETER                      :: NMODGPCOLS = 0
  CHARACTER(LEN=40), DIMENSION(1), TARGET           :: MODGPCOL  = (/' '/)
  !
  !   For PARAM_EQNS input, define default column order
  INTEGER,           PARAMETER                      :: NPAREQNCOLS = 3
  CHARACTER(LEN=40), DIMENSION(NPAREQNCOLS), TARGET    :: PAREQNCOL = &
      (/ 'PAREQNNAME  ','PAREQN      ','GROUPNAME   '/)
  !
  !   For PARAM_AVGS input, define default column order
  INTEGER,           PARAMETER                      :: NPARAVGCOLS = 3
  CHARACTER(LEN=40), DIMENSION(NPARAVGCOLS), TARGET    :: PARAVGCOL = &
      (/ 'PARAMAVGNAME','GROUPNAME   ','AVG         '/)
  !
  !   For MODEL_PATHS input, define default column order
  INTEGER,           PARAMETER                      :: NMODPTHCOLS = 3
  CHARACTER(LEN=40), DIMENSION(NMODPTHCOLS), TARGET    :: MODPTHCOL = &
      (/ 'PATHANDROOT ','PRIORMODPROB','GROUPNAME   '/)
  !
  !   For PREDICTIONS input, define default column order
  INTEGER,           PARAMETER                      :: NPREDSCOLS = 1
  CHARACTER(LEN=40), DIMENSION(NPREDSCOLS), TARGET    :: PREDCOL = &
      (/ 'PREDICTION  '/)
  !
  !   For ANALYSES input, define default column order
  INTEGER,           PARAMETER                      :: NANALCOLS = 3
  CHARACTER(LEN=40), DIMENSION(NANALCOLS), TARGET    :: ANALCOL = &
      (/ 'ANALYSISLABEL ','CRITEQN       ','PREQN         '/)
  !
  CONTAINS
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_OPEN(IFAIL,NAM,NOW,TASK,IU)
    ! Gets a unit #, Opens a file, Reports errors
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,            INTENT(INOUT)         :: IFAIL
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN) :: NAM
    CHARACTER(LEN=7),   INTENT(IN)            :: NOW
    CHARACTER(LEN=5),   INTENT(IN)            :: TASK
    INTEGER,            INTENT(OUT)           :: IU
    ! Internal variables
    INTEGER                                   :: ISTAT
    ! Formats
    1 FORMAT(1X,'OPEN UNIT',I4,' FOR ',A5,' STATUS = ',A7,' NAME IS ',A)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_OPEN'
    IFAIL = 0
    IU = UTL_GETUNIT(10,99)
    IF(IVERB > 2 .AND. ISTART > 0)WRITE(IUMSG,1)IU,TASK,NOW,TRIM(NAM)
    OPEN(UNIT=IU,FILE=NAM,STATUS=NOW,ACTION=TASK,IOSTAT=ISTAT)
    IF (ISTAT .NE. 0) THEN
      WRITE(*,*)ERRSUB
      WRITE(*,*)' For file "',TRIM(NAM),'"'
      WRITE(*,*)' File open failed--status = ',ISTAT
      WRITE(*,*)' Attempted task = ',TASK
      IFAIL = 1
    ENDIF
    RETURN
    END SUBROUTINE MMA_OPEN
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_INI(IFAIL,OUTROOT,VERSION,VERSIONTMP,VERSIONMIN)
    ! Reads OPTIONS OUTPUT_CONTROL
    ! OPENs files
    ! Writes prelminary information
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,                       INTENT(INOUT)  :: IFAIL
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN)     :: OUTROOT
    DOUBLE PRECISION,              INTENT(IN)     :: VERSION
    CHARACTER(LEN=5),              INTENT(IN)     :: VERSIONTMP
    CHARACTER(LEN=20),             INTENT(IN)     :: VERSIONMIN
    ! Local Variables
    CHARACTER(LEN=10)                             :: DATE = ' '
    INTEGER                                       :: IERR = 0
    CHARACTER(LEN=MAX_STRING_LEN)                 :: ONAM = ' '
    CHARACTER(LEN=10)                             :: TIME  = ' '
    INTEGER, DIMENSION(8)                         :: VS = 0
    CHARACTER(LEN=5)                              :: ZONE2
    TYPE (LLIST), POINTER                         :: OCHEAD
    TYPE (LLIST), POINTER                         :: TAIL
    CHARACTER(LEN=40), DIMENSION(1)               :: OCCOL
    INTEGER                                       :: NOC
    ! formats
    5 FORMAT(/,80('*'),/,1X,'Executing MMA,  Version: ',F7.3,A5, /,1X, &
         'Constructed using the JUPITER API, Version: ',A14,/,80('*'),/)
    10 FORMAT(/,' Messages ',/,'    will be written to unit: ',I5,/, &
                '    File: ',A)
    20 FORMAT(/,' Global Model Measures ',/, &
               '    will be written to  unit: ',I5,/,'    File: ',A)
    21 FORMAT(/,' Statistical Model Measures ',/, &
               '    will be written to  unit: ',I5,/,'    File: ',A)
    22 FORMAT(/,' Spatial Model Measures ',/, &
               '    will be written to  unit: ',I5,/,'    File: ',A)
    30 FORMAT(/,' Model Ranks for Global Measures ',/, &
               '    will be written to unit: ',I5,/,'     File: ',A)
    31 FORMAT(/,' Model Ranks for Statistical Measures ',/, &
               '    will be written to unit: ',I5,/,'     File: ',A)
    32 FORMAT(/,' Model Ranks for Spatial Measures ',/, &
               '    will be written to unit: ',I5,/,'     File: ',A)
    40 FORMAT(/,' Normalized Model Ranks for ALL Measures ' &
              ,/,'    unit: ',I5,/,'    File: ',A,/)
    100 FORMAT(' NAME_OF_ANALYSIS: "',A,'"')
    102 FORMAT(' DATE_that_ANALYSIS_was_EXECUTED',2X,I2,'-',I2,'-',I4, &
               ' TIME ',I2,':',I2,':',I2,' ZONE ',A5,//)
    NULLIFY(OCHEAD)
    NULLIFY(TAIL)
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_INI'
    IFAIL = 0
    !   Read options
    OUTNAM = TRIM(OUTROOT)
    VERS = VERSION
    ! Start message output file
    ONAM = TRIM(OUTNAM)//'.#mout'
    CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUMSG)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB)
    WRITE(*,*)
    WRITE(*,'(A)')HYPHENS(1:80)
    WRITE(*,'(A)')HYPHENS(1:80)
    WRITE(IUMSG,'(A)')HYPHENS(1:80)
    WRITE(IUMSG,'(A)')HYPHENS(1:80)
    WRITE(*,5)VERSION,VERSIONTMP,VERSIONMIN
    WRITE(IUMSG,5)VERSION,VERSIONTMP,VERSIONMIN
    WRITE(*,10)IUMSG,TRIM(ONAM)
    ! write date and time of setup and of execution
    ! this may help resolve confusion if paths change between setup & execution
    CALL DATE_AND_TIME(DATE,TIME,ZONE2,VS)
    WRITE(IUMSG,100)TRIM(OUTNAM)
    WRITE(IUMSG,102)VS(2),VS(3),VS(1),VS(5),VS(6),VS(7),ZONE2
    CALL BAS_INI_GETOPTIONS(IUMPTHS,IUMSG)
    NOC = 0
    ! Write the ANALYSIS_NAME BLOCK
    CALL UTL_READBLOCK(0,'OUTPUT_CONTROL',OCCOL,IUMPTHS,IUMSG,  &
        '*',.FALSE.,OCHEAD,TAIL,NOC)
    !   Check list of output controls
    IF (NOC>0) THEN
      CALL UTL_FILTER(IERR,OCHEAD,IUMSG,'WRITEPARAMNATIVE',WRITEPARAMNATIVE)
      CALL UTL_CASE(TRIM(WRITEPARAMNATIVE),WRITEPARAMNATIVE,-1)
      IF(WRITEPARAMNATIVE .EQ. 'yes') WRTPARAMNATIVE=.TRUE.
      CALL UTL_FILTER(IERR,OCHEAD,IUMSG,'WRITEPARAMREGRESS',WRITEPARAMREGRESS)
      CALL UTL_CASE(TRIM(WRITEPARAMREGRESS),WRITEPARAMREGRESS,-1)
      IF(WRITEPARAMREGRESS .EQ. 'yes') WRTPARAMREGRESS=.TRUE.
      CALL UTL_FILTER(IERR,OCHEAD,IUMSG,'WRITEPREDS',WRITEPREDS)
      CALL UTL_CASE(TRIM(WRITEPREDS),WRITEPREDS,-1)
      IF(WRITEPREDS .EQ. 'yes') WRTPREDS=.TRUE.
    ENDIF
    ! Start main output files
    ONAM = TRIM(OUTNAM)//'._mma'
    CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUOUT)
    WRITE(*,20)IUOUT,TRIM(ONAM)
    WRITE(IUMSG,20)IUOUT,TRIM(ONAM)
    ONAM = TRIM(OUTNAM)//'._mma_gstats'
    CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUOUTG)
    WRITE(*,21)IUOUTG,TRIM(ONAM)
    WRITE(IUMSG,21)IUOUTG,TRIM(ONAM)
    ONAM = TRIM(OUTNAM)//'._mma_xyzt'
    CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUOUTXYZT)
    WRITE(*,22)IUOUTXYZT,TRIM(ONAM)
    WRITE(IUMSG,22)IUOUTXYZT,TRIM(ONAM)
    ! Start rank output file
    ONAM = TRIM(OUTNAM)//'._rank'
    CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IURANK)
    WRITE(*,30)IURANK,TRIM(ONAM)
    WRITE(IUMSG,30)IURANK,TRIM(ONAM)
    ! Start rank output file
    ONAM = TRIM(OUTNAM)//'._rank_gstats'
    CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IURANKG)
    WRITE(*,31)IURANKG,TRIM(ONAM)
    WRITE(IUMSG,31)IURANKG,TRIM(ONAM)
    ! Start rank output file
    ONAM = TRIM(OUTNAM)//'._rank_xyzt'
    CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IURANKXYZT)
    WRITE(*,32)IURANKXYZT,TRIM(ONAM)
    WRITE(IUMSG,32)IURANKXYZT,TRIM(ONAM)
    CALL TYP_DEALLOC(OCHEAD)
    RETURN
    END SUBROUTINE MMA_INI
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_INI_MODELS(IFAIL,AVGPARAML)
    ! READS MODEL_GROUPS PARAM_EQNS PARAM_AVGS MODEL_PATHS
    ! Allocates arrays of NMPTHS NCOL KMGP KMA KME MAXMAXEQN
    ! Sets up reasonable parameter equations
    ! Sets up parameter averaging for each group
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,                       INTENT(INOUT) :: IFAIL
    LOGICAL,                       INTENT(INOUT) :: AVGPARAML
    !  Local variables
    TYPE (LLIST), POINTER :: MGHEAD ! Pointer to head of list (model groups)
    TYPE (LLIST), POINTER :: MAHEAD ! Pointer to head of list (model avg)
    TYPE (LLIST), POINTER :: MEHEAD ! Pointer to head of list (model eqns)
    TYPE (LLIST), POINTER :: MPHEAD ! Pointer to head of list (paths)
    TYPE (LLIST), POINTER :: TAIL
    DOUBLE PRECISION                     :: PRIDIFF
    DOUBLE PRECISION                     :: SUMPRIMODWT
    INTEGER :: I, ICNT, IERR, J, KMA, KME, KMP, MAXCNT, MORE
    !   Formats
    100 FORMAT(/,1X,A)
    200 FORMAT(1X,I6,' Error(s) encountered -- STOP EXECUTION (MAININIT)')
    900 FORMAT(1X,'GROUP #: then EQUATION #s ASSOCIATED WITH GROUP',/, &
               1X,'LAST GROUP is the DEFAULT group')
    901 FORMAT(1X,'GROUP #: then PARAMETERS TO BE AVERAGED FOR GROUP',/, &
               1X,'LAST GROUP is the DEFAULT group')
    902 FORMAT(1X,'GROUP #, GROUP NAME: then MODEL PATH')
    903 FORMAT(1X,I6,4X,A12,1X,A)
    904 FORMAT(1X,'INSTALLED EQUATIONS:',//, &
               1X,'EQUATION #, EQUATION NAME: then EQUATION')
    905 FORMAT(1X,I6,4X,A12,1X,A)
    909 FORMAT( &
         /,1X,'*************************************************************', &
        //,1X,'********* SUM OF PRIOR MODEL PROBABILITIES IS  1.00 *********', &
        //,1X,'  SUM = ',1PE15.7, &
        //,1X,'FOR MODELS WITH NO USER-ASSIGNED PROBABILITY, THE PROBABILITY', &
         /,1X,'WAS SET TO 1/(NUMBER OF MODELS) AND USED IN THE SUM', &
         /,1X,'*************************************************************',/)
    910 FORMAT( &
         /,1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', &
        //,1X,'!!! WARNING: SUM OF PRIOR MODEL PROBABILITIES IS NOT 1.00 !!!', &
        //,1X,'  SUM = ',1PE15.7, &
        //,1X,'FOR MODELS WITH NO USER-ASSIGNED PROBABILITY, THE PROBABILITY', &
         /,1X,'WAS SET TO 1/(NUMBER OF MODELS) AND USED IN THE SUM', &
        //,1X,'IF THE SUM OF PRIOR MODEL PROBABILITIES IS BETWEEN ', &
         /,1X,'0.999 and 1.001 THE PROBABILITIES ARE NORMALIZED. ', &
         /,1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',/)
    911 FORMAT( &
         /,1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', &
        //,1X,'!!! SUM OF PRIOR MODEL PROBABILITIES IS TOO FAR FROM 1.00 !!!', &
        //,1X,'  SUM = ',1PE15.7, &
        //,1X,'!!!!!!!!!!!!!!!!!!! TERMINATING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!', &
         /,1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',/)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_INI_MODELS'
    IFAIL=0
    AVGPARAML = .FALSE.
    !
    NULLIFY(MGHEAD)
    NULLIFY(MAHEAD)
    NULLIFY(MEHEAD)
    NULLIFY(MPHEAD)
    NULLIFY(TAIL)
    ! Find next data block
    !   MODEL_GROUPS block
    KMGP = 0
    CALL UTL_READBLOCK(0,'MODEL_GROUPS',MODGPCOL,IUMPTHS,IUMSG,   &
        'GROUPNAME',.FALSE.,MGHEAD,TAIL,KMGP)
    IF(KMGP == 0) KMGP = 1
    IF (IVERB>2) THEN
      !   Write block information (model groups) to output file
      WRITE(IUMSG,'(A)')HYPHENS(1:80)
      WRITE(IUMSG,100)'Echo model-groups input:'
      CALL UTL_WRITEBLOCK(MGHEAD,IUMSG)
    ENDIF
    !
    ! Find next data block
    !   PARAM_EQNS block
    KME = 0
    CALL UTL_READBLOCK(3,'PARAM_EQNS',PAREQNCOL,IUMPTHS,IUMSG,   &
        'PAREQNNAME',.FALSE.,MEHEAD,TAIL,KME)
    IF(KME > 0) THEN
      PARAMEQNL = .TRUE.
      !   Insert model-group information into parameter-equations lists
      CALL UTL_GROUPLIST(DEFGROUP,MGHEAD,IUMSG,MEHEAD,KMGP,KME)
      !
      IF (IVERB>2) THEN
        !   Write block information (parameter equations) to output file
        WRITE(IUMSG,'(A)')HYPHENS(1:80)
        WRITE(IUMSG,100)'Echo param-eqns input:'
        CALL UTL_WRITEBLOCK(MEHEAD,IUMSG)
      ENDIF
    ENDIF
    !
    ! Find next data block
    !   PARAM_AVGS block
    KMA = 0
    CALL UTL_READBLOCK(3,'PARAM_AVGS',PARAVGCOL,IUMPTHS,IUMSG,   &
        'PARAMAVGNAME',.FALSE.,MAHEAD,TAIL,KMA)
    IF(KMA > 0) THEN
      AVGPARAML = .TRUE.
      !   Insert model-group information into parameter-equations lists
      CALL UTL_GROUPLIST(DEFGROUP,MGHEAD,IUMSG,MAHEAD,KMGP,KMA)
      !
      IF (IVERB>2) THEN
        !   Write block information (parameter averages) to output file
        WRITE(IUMSG,'(A)')HYPHENS(1:80)
        WRITE(IUMSG,100)'Echo param-avgs input:'
        CALL UTL_WRITEBLOCK(MAHEAD,IUMSG)
      ENDIF
    ENDIF
    !
    ! Find next data block
    !   MODEL_PATHS block
    KMP = 0
    CALL UTL_READBLOCK(3,'MODEL_PATHS',MODPTHCOL,IUMPTHS,IUMSG,   &
        'PATHANDROOT',.TRUE.,MPHEAD,TAIL,KMP)
    !   Insert model-group information into model-path list
    CALL UTL_GROUPLIST(DEFGROUP,MGHEAD,IUMSG,MPHEAD,KMGP,KMP)
    IF (IVERB>2) THEN
      !   Write block information (model paths) to output file
      WRITE(IUMSG,'(A)')HYPHENS(1:80)
      WRITE(IUMSG,100)'Echo model paths input:'
      CALL UTL_WRITEBLOCK(MPHEAD,IUMSG)
    ENDIF
    !
    NMPTHS = KMP
    ! ALLOCATE NMPTHS NCOL KMGP
    ALLOCATE (COORDL(NMPTHS),ITER(NMPTHS),ORDER(NMPTHS), &
              NAMEL(NMPTHS),NAMEMODEL(NMPTHS), &
              RANK(NMPTHS,NCOL-2),RANKNORM(NMPTHS,NCOL-2), &
              STATS(NMPTHS,NCOL-2),STATSPERCENT(NMPTHS,20),TCOORDL(NMPTHS))
    ALLOCATE (GROUPS(KMGP),EQNL(KMGP),AVGL(KMGP),PRIMODWT(NMPTHS), &
              NAMEPATH(NMPTHS),NAMEPATHGP(NMPTHS),NAMEPATHGPN(NMPTHS))
    ! INITIALIZE
    ITER = 0
    NAMEL = 'Y'
    NAMEMODEL = ' '
    ORDER = 0
    RANK = NMPTHS
    RANKNORM = DBLE(NMPTHS)
    STATS = 1.D+30
    STATSPERCENT = 1.D+30
    COORDL = .TRUE.
    TCOORDL = .TRUE.
    GROUPS = 'DEFAULT'
    DEFGROUP = 'DEFAULT'
    NAMEPATH = ' '
    NAMEPATHGP = 'DEFAULT'
    NAMEPATHGPN = KMGP
    ! DEFAULT PRIOR MODEL WEIGHT 1/#models
    ! This will have no effect if prior model weight is irrelevant
    PRIMODWT = 1.D0/DBLE(NMPTHS)
    IF(KMA > 0) THEN
      ! ALLOCATE KMGP KMA
      ALLOCATE (MAXAVG(KMGP),PARAVG(KMA),PARAVGGP(KMA),PARAVGGPN(KMA))
      ! INITIALIZE
      AVGL = .FALSE.
      AVGL(1) = .FALSE.
      PARAVG = ' '
      PARAVGGP = 'DEFAULT'
    ENDIF
    IF(KME > 0) THEN
      ! ALLOCATE KMGP KME
      ALLOCATE (MAXEQN(KMGP),PAREQN(KME),PAREQNGP(KME),PAREQNGPN(KME), &
                PAREQNNAME(KME))
      ! INITIALIZE
      EQNL = .FALSE.
      EQNL(1) = .FALSE.
      PAREQNNAME = ' '
      PAREQN = ' '
      PAREQNGP = 'DEFAULT'
    ENDIF
    !
    !   Filter information in linked list and populate arrays in the
    !   model/equations/parameters structure
    IERR = 0
    CALL UTL_FILTERLIST(MGHEAD,IUMSG,'GROUPNAME',KMGP,IERR,GROUPS,MORE,MORE)
    IF(IERR>0)CALL UTL_STOP &
     (' Input error MODEL_GROUPS Block GROUPNAME. See message in .#mout file')
      DO I=1,KMGP
        CALL UTL_CASE(TRIM(GROUPS(I)),GROUPS(I),-1)
      ENDDO
    IF(KMA > 0)CALL UTL_FILTERLIST(MGHEAD,IUMSG,'AVG',KMGP,IERR,AVGL,MORE)
    IF(IERR>0)CALL UTL_STOP &
     (' Input error MODEL_GROUPS Block AVG. See message in .#mout file')
    IF(KME > 0) THEN
      EQNL = .TRUE.
      CALL UTL_FILTERLIST(MEHEAD,IUMSG,'PAREQNNAME',KME,IERR,PAREQNNAME,MORE)
      IF(IERR>0)CALL UTL_STOP &
      (' Input error PARAM_EQNS Block PAREQNNAME. See message in .#mout file')
      CALL UTL_FILTERLIST(MEHEAD,IUMSG,'PAREQN',KME,IERR,PAREQN,MORE)
      IF(IERR>0)CALL UTL_STOP &
       (' Input error PARAM_EQNS Block PAREQN. See message in .#mout file')
      CALL UTL_FILTERLIST(MEHEAD,IUMSG,'GROUPNAME',KME,IERR,PAREQNGP,MORE)
      DO I=1,KME
        CALL UTL_CASE(TRIM(PAREQNGP(I)),PAREQNGP(I),-1)
      ENDDO
      IF(IERR>0)CALL UTL_STOP &
       (' Input error PARAM_EQNS Block GROUPNAME. See message in .#mout file')
      MAXMAXEQN = 0
      MAXEQN = 0
      MAXCNT = 0
      ! COUNT EQTNS FOR EACH GROUP,
      ! SET MAXIMUM # EQTNS ANY GROUP = MAXMAXEQN
      DO J=1,KMGP
        IF(J > 1)MAXEQN(J-1) = MAXCNT
        IF(EQNL(J)) THEN
          MAXCNT = 0
          DO I=1,KME
            IF(PAREQNGP(I) .EQ. GROUPS(J)) THEN
              PAREQNGPN(I) = J
              MAXCNT = MAXCNT +1
              CYCLE
            ENDIF
          ENDDO
          IF(MAXCNT > MAXMAXEQN) MAXMAXEQN = MAXCNT
        ENDIF
        IF(J == KMGP)MAXEQN(J) = MAXCNT
      ENDDO
      ! FILL AN ARRAY FOR EACH GROUP WITH THE APPROPRIATE EQUATION NUMBER
      ! ZEROS INDICATE NO MORE EQTNS FOR THAT GROUP
      ! ALLOCATE KMGP MAXMAXEQN
      ALLOCATE(EQNARRAY(KMGP,MAXMAXEQN))
      ! INITIALIZE
      EQNARRAY = 0
      DO J=1,KMGP
        ICNT = 0
        DO I=1,KME
          IF(PAREQNGP(I) .EQ. GROUPS(J)) THEN
            ICNT = ICNT + 1
            EQNARRAY(J,ICNT) = I
            IF(ICNT .EQ. MAXEQN(J)) EXIT
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    ! SORT PARAMETERS TO BE AVERAGED
    IF(KMA > 0) THEN
      IERR = 0
      CALL UTL_FILTERLIST(MAHEAD,IUMSG,'PARAMAVGNAME',KMA,IERR,PARAVG,MORE)
    IF(IERR>0)CALL UTL_STOP &
     (' Input error PARAM_AVGS Block PARAMAVGNAME. See message in .#mout file')
      CALL UTL_FILTERLIST(MAHEAD,IUMSG,'GROUPNAME',KMA,IERR,PARAVGGP,MORE)
    IF(IERR>0)CALL UTL_STOP &
     (' Input error PARAM_AVGS Block GROUPNAME. See message in .#mout file')
      DO I=1,KMA
        CALL UTL_CASE(TRIM(PARAVGGP(I)),PARAVGGP(I),-1)
      ENDDO
      MAXMAXAVG = 0
      MAXAVG = 0
      MAXCNT = 0
      ! COUNT PARAMETERS TO BE AVERAGED FOR EACH GROUP,
      ! SET MAXIMUM # AVERAGES ANY GROUP = MAXMAXAVG
      DO J=1,KMGP
        IF(J > 1)MAXAVG(J-1) = MAXCNT
        MAXCNT = 0
        IF(AVGL(J)) THEN
          DO I=1,KMA
            IF(PARAVGGP(I) .EQ. GROUPS(J)) THEN
              PARAVGGPN(I) = J
              MAXCNT = MAXCNT +1
              IF(J == KMGP)MAXAVG(J) = MAXCNT
              CYCLE
            ENDIF
          ENDDO
          IF(MAXCNT > MAXMAXAVG) MAXMAXAVG = MAXCNT
        ENDIF
      ENDDO
      ! FILL AN ARRAY FOR EACH GROUP WITH THE APPROPRIATE PARAMETER NAMES TO BE
      ! AVERAGED. ' ' INDICATES NO MORE EQTNS FOR THAT GROUP
      ! ALLOCATE KMGP MAXMAXAVG
      ALLOCATE(AVGARRAY(KMGP,MAXMAXAVG),AVGPARAMLGP(KMGP), &
               AVGPARAMLGPI(KMGP,MAXMAXAVG))
      ! INITIALIZE
      AVGARRAY = ' '
      AVGPARAMLGP = .FALSE.
      AVGPARAMLGPI = .FALSE.
      ! SETUP AVERAGING GROUPS
      DO J=1,KMGP
        ICNT = 0
        DO I=1,KMA
          IF(PARAVGGP(I) .EQ. GROUPS(J)) THEN
            ICNT = ICNT + 1
            AVGARRAY(J,ICNT) = PARAVG(I)
            IF(ICNT .EQ. MAXAVG(J)) EXIT
          ENDIF
        ENDDO
        IF(ICNT > 0) AVGPARAMLGP = .TRUE.
      ENDDO
      ! WRITE EXTENDED INFO
      IF(IVERB > 2) THEN
        WRITE(IUMSG,*)
        WRITE(IUMSG,'(A)')HYPHENS(1:80)
        WRITE(IUMSG,900)
        DO J=1,KMGP
          WRITE(IUMSG,*)J,(EQNARRAY(J,I),I=1,MAXMAXEQN)
        ENDDO
        WRITE(IUMSG,'(A)')HYPHENS(1:80)
        WRITE(IUMSG,901)
        DO J=1,KMGP
          WRITE(IUMSG,*)J,(AVGARRAY(J,I),I=1,MAXMAXAVG)
        ENDDO
        WRITE(IUMSG,'(A)')HYPHENS(1:80)
        WRITE(IUMSG,*)
      ENDIF
    ENDIF
    ! Filter information on models and populate arrays
    IERR = 0
    CALL UTL_FILTERLIST(MPHEAD,IUMSG,'PATHANDROOT',KMP,IERR,NAMEPATH,MORE)
    IF(IERR>0)CALL UTL_STOP &
     (' Input error MODEL_PATHS Block PATHANDROOT. See message in .#mout file')
    CALL UTL_FILTERLIST(MPHEAD,IUMSG,'PRIORMODPROB',KMP,IERR,PRIMODWT,MORE)
    IF(IERR>0)CALL UTL_STOP &
     (' Input error MODEL_PATHS Block PRIORMODPROB. See message in .#mout file')
    CALL UTL_FILTERLIST(MPHEAD,IUMSG,'GROUPNAME',KMP,IERR,NAMEPATHGP,MORE)
    IF(IERR>0)CALL UTL_STOP &
     (' Input error MODEL_PATHS Block GROUPNAME. See message in .#mout file')
    DO J=1,KMGP
      DO I=1,KMP
        IF(NAMEPATHGP(I) .EQ. ' ') NAMEPATHGP(I) = 'DEFAULT'
        CALL UTL_CASE(TRIM(NAMEPATHGP(I)),NAMEPATHGP(I),-1)
        IF(NAMEPATHGP(I) .EQ. GROUPS(J)) THEN
          NAMEPATHGPN(I) = J
          CYCLE
        ENDIF
      ENDDO
    ENDDO
    ! SUM PRIOR MODELS WEIGHTS
    SUMPRIMODWT = 0.D0
    DO J=1,NMPTHS
      SUMPRIMODWT = SUMPRIMODWT + PRIMODWT(J)
    ENDDO
    PRIDIFF = 1.D0 - SUMPRIMODWT
    ! IF SUM OF PRIOR MODEL WEIGHTS  IS MORE THAN 0.1% OFF
    !  WRITE MESSAGES, AND NORMALIZE
    IF(ABS(PRIDIFF) < 1.D-7) THEN
      WRITE(IUMSG,909)SUMPRIMODWT
      WRITE(*,909)SUMPRIMODWT
    ELSEIF(ABS(PRIDIFF) > 0.001) THEN
      WRITE(IUMSG,910)SUMPRIMODWT
      WRITE(*,910)SUMPRIMODWT
      WRITE(IUMSG,911)SUMPRIMODWT
      WRITE(*,911)SUMPRIMODWT
      CALL UTL_STOP()
    ELSE
      WRITE(IUMSG,910)SUMPRIMODWT
      WRITE(*,910)SUMPRIMODWT
    ENDIF
    ! WRITE EXTENDED INFO
    IF(IVERB > 2) THEN
      WRITE(IUMSG,*)
      WRITE(IUMSG,'(A)')HYPHENS(1:80)
      WRITE(IUMSG,902)
      DO J=1,NMPTHS
        WRITE(IUMSG,903)NAMEPATHGPN(J),GROUPS(NAMEPATHGPN(J)),NAMEPATH(J)
      ENDDO
      WRITE(IUMSG,'(A)')HYPHENS(1:80)
      WRITE(IUMSG,*)
    ENDIF
    IF(KME > 0) THEN
      ! INSTALL PARAMETER EQUATIONS
      IF(IVERB > 2) THEN
        WRITE(IUMSG,*)
        WRITE(IUMSG,'(A)')HYPHENS(1:80)
        WRITE(IUMSG,904)
        DO J=1,KME
          WRITE(IUMSG,905)J,PAREQNNAME(J),TRIM(PAREQN(J))
        ENDDO
        WRITE(IUMSG,'(A)')HYPHENS(1:80)
        WRITE(IUMSG,*)
      ENDIF
      ! THESE EQNS ARE EVALUATED IN MMA_EVA_PCREAS
      ! THEY ARE ELIMINATED BEFORE INSTALLING ANALYSIS EQUATIONS IN MMA_ANL
      ! AFTER THE MAIN MODEL EVALUATION LOOP, IS COMPLETED
      CALL EQN_INI(IFAIL,KME)
      IF (IFAIL.NE.0) THEN
        CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
        CALL UTL_STOP('EQN_INI reported failure')
      ENDIF
      DO J=1,KME
        CALL UTL_CASE(TRIM(PAREQN(J)),PAREQN(J),-1)
        CALL EQN_INI_INSTALL(IFAIL,J,PAREQNNAME(J),PAREQN(J))
        IF (IFAIL.NE.0) THEN
          CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
          CALL UTL_STOP('EQN_INI_INSTALL reported failure')
        ENDIF
      ENDDO
      DEALLOCATE(PAREQNGP)
    ENDIF
    IF(KMA > 0) DEALLOCATE(PARAVGGP)
    !
    CALL TYP_DEALLOC(MGHEAD)
    CALL TYP_DEALLOC(MAHEAD)
    CALL TYP_DEALLOC(MEHEAD)
    CALL TYP_DEALLOC(MPHEAD)
    RETURN
  END SUBROUTINE MMA_INI_MODELS
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_INI_PRIORCHECK(IFAIL)
  ! -- MMA_INI_PRIORCHECK checks if measures requiring consistent prior are used
  !    the equation has been checked before this call so no error functions here
  IMPLICIT NONE
  INTEGER,           INTENT(INOUT) :: IFAIL
  INTEGER :: I
  ! Initialize
  AMESSAGE = ' '
  ERRSUB=' Error in subroutine MMA_INI_PRIORCHECK'
  IFAIL=0
  PRIORMEASUSED = .FALSE.
  IF(IFAIL .NE. 0) STOP
  DO I=1,500
    IF(TRIM(ATERM(I)) == ' ')EXIT
    IF(TRIM(ATERM(I)) == 'swsrwpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'cevwpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'mlofwpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'aicwpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'aiccwpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'bicwpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'kicwpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'xtwxwpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'r2_oswpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'int_oswpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'slp_oswpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'r2_wswpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'int_wswpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'slp_wswpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'r2_wwwpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'int_wwwpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'slp_wwwpri') PRIORMEASUSED = .TRUE.
    IF(TRIM(ATERM(I)) == 'r2_nmwpri') PRIORMEASUSED = .TRUE.
  ENDDO
  ATERM = ' '
  50 CONTINUE
  RETURN
  END SUBROUTINE MMA_INI_PRIORCHECK
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_INI_COLHD(IFAIL)
    ! This subroutine sets up column headers
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,            INTENT(INOUT)  :: IFAIL
    ! Internal variables
    INTEGER I, N1, NC
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_INI_COLHD'
    IFAIL = 0
    ALLOCATE(COLHD(NCOL,3),COLHDNAME(NCOL))
    COLHD = ' '
    N1 = NCOL-1
    NC = NCOL
    ! COLUMN HEADS       POTENTIAL FOR SORTING   ORDER FROM BEST
    COLHD(N1,1)='Model       ';   COLHD(N1,2)='N';    COLHD(N1,3)='NONE'; !Name of Model
    COLHD( 1,1)='NPE         ';   COLHD( 1,2)='N';    COLHD( 1,3)='NONE'; !Number Parameters Estimated
    COLHD( 2,1)='NOBS        ';   COLHD( 2,2)='N';    COLHD( 2,3)='NONE'; !Number Observations
    COLHD( 3,1)='NPR         ';   COLHD( 3,2)='N';    COLHD( 3,3)='NONE'; !Number Prior
    COLHD( 4,1)='SWSRObs     ';   COLHD( 4,2)='Y';    COLHD( 4,3)='LOW '; !Sum Wieghted Residuals Squared
    COLHD( 5,1)='SWSRwPri    ';   COLHD( 5,2)='Y';    COLHD( 5,3)='LOW '; !sigma squared
    COLHD( 6,1)='CEVObs      ';   COLHD( 6,2)='Y';    COLHD( 6,3)='ONE '; !
    COLHD( 7,1)='CEVwPri     ';   COLHD( 7,2)='Y';    COLHD( 7,3)='ONE '; !
    COLHD( 8,1)='MLOFObs     ';   COLHD( 8,2)='Y';    COLHD( 8,3)='LOW '; !Maximum Likelihood Objective Function
    COLHD( 9,1)='MLOFwPri    ';   COLHD( 9,2)='Y';    COLHD( 9,3)='LOW '; !Maximum Likelihood Objective Function
    COLHD(10,1)='AICObs      ';   COLHD(10,2)='Y';    COLHD(10,3)='LOW '; !AIC
    COLHD(11,1)='AICwPri     ';   COLHD(11,2)='Y';    COLHD(11,3)='LOW '; !AIC
    COLHD(12,1)='AICcObs     ';   COLHD(12,2)='Y';    COLHD(12,3)='LOW '; !AICc
    COLHD(13,1)='AICcwPri    ';   COLHD(13,2)='Y';    COLHD(13,3)='LOW '; !AICc
    COLHD(14,1)='BICObs      ';   COLHD(14,2)='Y';    COLHD(14,3)='LOW '; !BIC
    COLHD(15,1)='BICwPri     ';   COLHD(15,2)='Y';    COLHD(15,3)='LOW '; !BIC
    COLHD(16,1)='KICObs      ';   COLHD(16,2)='Y';    COLHD(16,3)='LOW '; !KIC = Kashyap obs only
    COLHD(17,1)='KICwPri     ';   COLHD(17,2)='Y';    COLHD(17,3)='LOW '; !KIC = Kashyap w pri
    COLHD(18,1)='XTwXObs     ';   COLHD(18,2)='Y';    COLHD(18,3)='NONE'; !LN determinant XTwX obs
    COLHD(19,1)='XTwXwPri    ';   COLHD(19,2)='Y';    COLHD(19,3)='NONE'; !LN determinant XTwX obs & Pri
    COLHD(20,1)='PriModProb  ';   COLHD(20,2)='Y';    COLHD(20,3)='HIGH'; !Prior Model Probability
    !
    COLHD(K1+1 ,1)='R2_osObs    ';COLHD(K1+1 ,2)='Y'; COLHD(K1+1 ,3)='ONE '; !Rsquared for os - unwt sim eq vs unwt obs
    COLHD(K1+2 ,1)='Int_osObs   ';COLHD(K1+2 ,2)='Y'; COLHD(K1+2 ,3)='ZERO'; !Intercept for os
    COLHD(K1+3 ,1)='Slp_osObs   ';COLHD(K1+3 ,2)='Y'; COLHD(K1+3 ,3)='ONE '; !Slope for os
    COLHD(K1+4 ,1)='R2_wsObs    ';COLHD(K1+4 ,2)='Y'; COLHD(K1+4 ,3)='ZERO'; !!Rsquared for ws - wt res vs wt sim eq
    COLHD(K1+5 ,1)='Int_wsObs   ';COLHD(K1+5 ,2)='Y'; COLHD(K1+5 ,3)='ZERO'; !Intercept for ws
    COLHD(K1+6 ,1)='Slp_wsObs   ';COLHD(K1+6 ,2)='Y'; COLHD(K1+6 ,3)='ZERO'; !Slope for ws
    COLHD(K1+7 ,1)='R2_wwObs    ';COLHD(K1+7 ,2)='Y'; COLHD(K1+7 ,3)='ONE '; !Rsquared for ww - wt sim eq vs wt obs
    COLHD(K1+8 ,1)='Int_wwObs   ';COLHD(K1+8 ,2)='Y'; COLHD(K1+8 ,3)='ZERO'; !Intercept for ww
    COLHD(K1+9 ,1)='Slp_wwObs   ';COLHD(K1+9 ,2)='Y'; COLHD(K1+9 ,3)='ONE '; !Slope for ww
    COLHD(K1+10,1)='R2_NMObs    ';COLHD(K1+10,2)='Y'; COLHD(K1+10,3)='ONE '; !fit to normal distribution r2_nm for obs only
    !
    COLHD(K2+1 ,1)='R2_oswPri   ';COLHD(K2+1 ,2)='Y'; COLHD(K2+1 ,3)='ONE '; !Rsquared for os - unwt sim eq vs unwt obs
    COLHD(K2+2 ,1)='Int_oswPri  ';COLHD(K2+2 ,2)='Y'; COLHD(K2+2 ,3)='ZERO'; !Intercept for os
    COLHD(K2+3 ,1)='Slp_oswPri  ';COLHD(K2+3 ,2)='Y'; COLHD(K2+3 ,3)='ONE '; !Slope for os
    COLHD(K2+4 ,1)='R2_wswPri   ';COLHD(K2+4 ,2)='Y'; COLHD(K2+4 ,3)='ZERO'; !!Rsquared for ws - wt res vs wt sim eq
    COLHD(K2+5 ,1)='Int_wswPri  ';COLHD(K2+5 ,2)='Y'; COLHD(K2+5 ,3)='ZERO'; !Intercept for ws
    COLHD(K2+6 ,1)='Slp_wswPri  ';COLHD(K2+6 ,2)='Y'; COLHD(K2+6 ,3)='ZERO'; !Slope for ws
    COLHD(K2+7 ,1)='R2_wwwPri   ';COLHD(K2+7 ,2)='Y'; COLHD(K2+7 ,3)='ONE '; !Rsquared for ww - wt sim eq vs wt obs
    COLHD(K2+8 ,1)='Int_wwwPri  ';COLHD(K2+8 ,2)='Y'; COLHD(K2+8 ,3)='ZERO'; !Intercept for ww
    COLHD(K2+9 ,1)='Slp_wwwPri  ';COLHD(K2+9 ,2)='Y'; COLHD(K2+9 ,3)='ONE '; !Slope for ww
    COLHD(K2+10,1)='R2_NMwPri   ';COLHD(K2+10,2)='Y'; COLHD(K2+10,3)='ONE '; !fit to normal distrib r2_nm for obs and prior
    !
    COLHD(K3+1,1)='R2_rt       '; COLHD(K3+1,2)='Y'; COLHD(K3+1,3)='ZERO'; !Rsquared for rt - unwtres vs time
    COLHD(K3+2,1)='Int_rt      '; COLHD(K3+2,2)='Y'; COLHD(K3+2,3)='ZERO'; !Intercept for rt
    COLHD(K3+3,1)='Slp_rt      '; COLHD(K3+3,2)='Y'; COLHD(K3+3,3)='ZERO'; !Slope for rt
    COLHD(K3+4,1)='R2_wrt      '; COLHD(K3+4,2)='Y'; COLHD(K3+4,3)='ZERO'; !Rsquared for wt - wtres vs time
    COLHD(K3+5,1)='Int_wrt     '; COLHD(K3+5,2)='Y'; COLHD(K3+5,3)='ZERO'; !Intercept for wt
    COLHD(K3+6,1)='Slp_wrt     '; COLHD(K3+6,2)='Y'; COLHD(K3+6,3)='ZERO'; !Slope for wt
    !
    COLHD(K3+7 ,1)='cnt_locX    ';COLHD(K3+7 ,2)='N'; COLHD(K3+7 ,3)='NONE'; !x centroid of data locations
    COLHD(K3+8 ,1)='cnt_locY    ';COLHD(K3+8 ,2)='N'; COLHD(K3+8 ,3)='NONE'; !y centroid of data locations
    COLHD(K3+9 ,1)='cnt_locZ    ';COLHD(K3+9 ,2)='N'; COLHD(K3+9 ,3)='NONE'; !z centroid of data locations
    COLHD(K3+10,1)='difcnt_wX   ';COLHD(K3+10,2)='Y'; COLHD(K3+10,3)='ZERO'; !difference x centroid of wtd resid & data locations
    COLHD(K3+11,1)='difcnt_wY   ';COLHD(K3+11,2)='Y'; COLHD(K3+11,3)='ZERO'; !difference y centroid of wtd resid & data locations
    COLHD(K3+12,1)='difcnt_wZ   ';COLHD(K3+12,2)='Y'; COLHD(K3+12,3)='ZERO'; !difference z centroid of wtd resid & data locations
    COLHD(K3+13,1)='distcntw    ';COLHD(K3+13,2)='Y'; COLHD(K3+13,3)='LOW '; !absolute distance tween wtd resid & data locations
    COLHD(K3+14,1)='difcnt_sgnwX';COLHD(K3+14,2)='Y'; COLHD(K3+14,3)='ZERO'; !x centroid of sign of residuals +1-1
    COLHD(K3+15,1)='difcnt_sgnwY';COLHD(K3+15,2)='Y'; COLHD(K3+15,3)='ZERO'; !y centroid of sign of residuals +1-1
    COLHD(K3+16,1)='difcnt_sgnwZ';COLHD(K3+16,2)='Y'; COLHD(K3+16,3)='ZERO'; !z centroid of sign of residuals +1-1
    COLHD(K3+17,1)='distcnt_sgnw';COLHD(K3+17,2)='Y'; COLHD(K3+17,3)='LOW '; !difference centroid sign residuals & data locations
    COLHD(K3+18,1)='difcnt_MGwX ';COLHD(K3+18,2)='Y'; COLHD(K3+18,3)='ZERO'; !x centroid of absolute magnitude weighted residuals
    COLHD(K3+19,1)='difcnt_MGwY ';COLHD(K3+19,2)='Y'; COLHD(K3+19,3)='ZERO'; !y centroid of absolute magnitude weighted residuals
    COLHD(K3+20,1)='difcnt_MGwZ ';COLHD(K3+20,2)='Y'; COLHD(K3+20,3)='ZERO'; !z centroid of absolute magnitude weighted residuals
    COLHD(K3+21,1)='dist_cntMGw ';COLHD(K3+21,2)='Y'; COLHD(K3+21,3)='LOW '; !abs dist tween abs magnitude resid & data locations
    COLHD(NC,1)='PathAndRoot  '  ;COLHD(NC,2)='N'   ; COLHD(NC,3)='NONE'; !Path full or relative to where job is launched
    DO I = 1,NCOL
      CALL UTL_CASE(TRIM(COLHD(I,1)),COLHD(I,1),1)
      CALL UTL_CASE(TRIM(COLHD(I,1)),COLHDNAME(I),-1)
    ENDDO
    RETURN
  END SUBROUTINE MMA_INI_COLHD
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_INI_PREDS(IFAIL,AVGPREDL)
    ! This subroutine reads the PREDS block
    ! Allocates IPRED and CONVREAS
    ! Initialize pred related items
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,                    INTENT(INOUT)  :: IFAIL
    LOGICAL,                    INTENT(INOUT)  :: AVGPREDL
    ! Local variables
    INTEGER                                    :: I = 0
    INTEGER                                    :: IERR = 0
    INTEGER                                    :: MORE
    TYPE (LLIST), POINTER :: PRHEAD ! Pointer to head of list (preds)
    TYPE (LLIST), POINTER :: TAIL
    ! Formats
    10 FORMAT(/,1X,'PREDICTIONS TO BE MODEL-AVERAGED',/, &
                1X,'--------------------------------')
    20 FORMAT(1X,A)
    100 FORMAT(/,1X,A)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_INI_PREDS'
    IFAIL = 0
    !
    NULLIFY(PRHEAD)
    NULLIFY(TAIL)
    ! use first model to indicate generally whether to expect preds
    ! determine if prediction files exists
    INQUIRE(FILE=TRIM(NAMEPATH(1))//'._linp',EXIST=YPFILEGEN)
    ! Find next data block
    !   MODEL_GROUPS block
    IPRED = 0
    CALL UTL_READBLOCK(1,'PREDS',PREDCOL,IUMPTHS,IUMSG,   &
        'PREDICTION',.FALSE.,PRHEAD,TAIL,IPRED)
    IF (IVERB>2) THEN
      !   Write block information (predictions) to output file
      WRITE(IUMSG,'(A)')HYPHENS(1:80)
      WRITE(IUMSG,100)'Echo prediction input:'
      CALL UTL_WRITEBLOCK(PRHEAD,IUMSG)
    ENDIF
    IF(IPRED < 1) THEN
      AVGPREDL = .FALSE.
    ELSE
      AVGPREDL = .TRUE.
      ALLOCATE(PREDNAM(IPRED),PREDNAMLC(IPRED),PREDPLOT(IPRED), &
               PRD(CONVREAS,IPRED),PVARCIIND(CONVREAS,IPRED), &
               PVARCISIM(CONVREAS,IPRED), &
               PVARCIINF(CONVREAS,IPRED), &
               PVARPIIND(CONVREAS,IPRED), &
               PVARPISIM(CONVREAS,IPRED), &
               PVARPIINF(CONVREAS,IPRED))
      PREDNAM = ' '
      PREDNAMLC = ' '
      PREDPLOT = 0
      PRD = 1.D+30
      PVARCIIND = 1.D+30
      PVARCISIM = 1.D+30
      PVARCIINF = 1.D+30
      PVARPIIND = 1.D+30
      PVARPISIM = 1.D+30
      PVARPIINF = 1.D+30
      IERR = 0
      CALL UTL_FILTERLIST(PRHEAD,IUMSG,'PREDICTION',IPRED,IERR,PREDNAM,MORE)
      IF(IERR>0)CALL UTL_STOP &
        (' Input error PREDICTION Block PREDICTION. See message in .#mout file')
      DO I=1,IPRED
        CALL UTL_CASE(TRIM(PREDNAM(I)),PREDNAMLC(I),-1)
      ENDDO
    ENDIF
    CALL TYP_DEALLOC(PRHEAD)
    RETURN
  END SUBROUTINE MMA_INI_PREDS
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_READ_DM_INI(IFAIL,IMODEL,MNOBS)
    ! This subroutine reads the _dm file, getting basic info about each model
    ! Sets stats npe nobs mpr R2_NMObs R2_NMwPri.
    ! Determines if it converged.
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,                INTENT(INOUT)    :: IFAIL
    INTEGER,                   INTENT(IN)    :: IMODEL
    INTEGER,                  INTENT(OUT)    :: MNOBS
    ! Local variables
    CHARACTER(LEN=3)                           :: CDUM3
    CHARACTER(LEN=12)                          :: CDUM
    INTEGER                                    :: IDUM
    INTEGER                                    :: MMPR
    DOUBLE PRECISION                           :: RDUM
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_READ_DM_INI'
    IFAIL = 0
    ! READ _DM FILE
    CALL UTLUCODE_DX_READ_DM &
    (IFAIL,NAMEPATH(IMODEL),RDUM,RDUM,RDUM,RDUM,CDUM3,IDUM,RDUM,RDUM,RDUM, &
     CDUM,CDUM,CDUM,CDUM,MMPR,MNOBS,IDUM,IDUM,IDUM,IDUM,RDUM,RDUM,RDUM,RDUM)
    RETURN
    IF(IMODEL == 1) THEN
      MINMPR = MMPR
    ELSE
      IF(MMPR < MINMPR) THEN
        MINMPR = MMPR
        PRIORSAMENUM = .FALSE.
      ENDIF
    ENDIF
  END SUBROUTINE MMA_READ_DM_INI
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_READ_DM(IFAIL,IMODEL,MAXNOBS,CONVERGED)
    ! This subroutine reads the _dm file, getting basic info about each model
    ! Sets stats npe nobs mpr R2_NMObs R2_NMwPri.
    ! Determines if it converged.
    ! If so, calculates stats sigma2 AICc BIC HANNAN KIC(KASHYAP) cev
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,                INTENT(INOUT)    :: IFAIL
    INTEGER,                   INTENT(IN)    :: IMODEL
    INTEGER,                   INTENT(IN)    :: MAXNOBS
    LOGICAL,                  INTENT(OUT)    :: CONVERGED
    ! Local variables
    DOUBLE PRECISION                           :: AIC = 1.D+30 ! recalc in mma main
    DOUBLE PRECISION                           :: BIC = 1.D+30 ! recalc in mma main
    DOUBLE PRECISION                           :: CEV
    CHARACTER(LEN=200)                         :: CHECK = ' '
    DOUBLE PRECISION                           :: HQ = 1.D+30 ! recalc in mma main
    INTEGER                                    :: I
    CHARACTER(LEN=3)                           :: ICONVERGE = '   '
    INTEGER                                    :: IDUM
    INTEGER                                    :: ILOOP
    CHARACTER(LEN=MAX_STRING_LEN)              :: INNAM = ' '
    INTEGER                                    :: IUMODEL
    INTEGER                                    :: NPERD
    ! DOUBLE PRECISION :: KASHYAP = 1.D+30   ! read from _dm as STATS(IMODEL,16)
    ! DOUBLE PRECISION :: KASHYAPNP = 1.D+30 ! read from _dm as STATS(IMODEL,17)
    CHARACTER(LEN=200)                         :: LUNITS = 'NA'
    CHARACTER(LEN=200)                         :: MUNITS = 'NA'
    CHARACTER(LEN=20),ALLOCATABLE,DIMENSION(:) :: PRINAMETMP
    DOUBLE PRECISION                           :: RDUM
    DOUBLE PRECISION                           :: RDUM2
    DOUBLE PRECISION                           :: RDUM3
    DOUBLE PRECISION                           :: RK
    DOUBLE PRECISION                           :: RN
    DOUBLE PRECISION                           :: RNP
    DOUBLE PRECISION                           :: STDERR = 1.D+30
    CHARACTER(LEN=200)                         :: TUNITS = 'NA'
    ! formats
    10 FORMAT(/,' Evaluating model: ',A,/,12X,' Path: ',A,/)
    11 FORMAT(/,' Error for model: ',A12)
    22 FORMAT(I6,'               NOT CONVERGED: ',A12,' in ',A)
    30 FORMAT(/,' UNITS FOR MODEL: ',A,/,' DO NOT MATCH FIRST MODEL: ',A,/, &
                ' CHECK _dm files to evaluate')
    40 FORMAT(/,' Comparing prior names ',A20,' and ',A20)
    41 FORMAT(/,' Prior name ',A20,' does not match ',A20)
    ! Initialize
    AMESSAGE = ' '
    CONVERGED = .FALSE.
    ERRSUB=' Error in subroutine MMA_READ_DM'
    IFAIL = 0
    ! READ _DM FILE
    CALL UTLUCODE_DX_READ_DM &
    (IFAIL,NAMEPATH(IMODEL), &
    AIC,BIC,LNDETFN,HQ, &
    ICONVERGE,ITERP,STATS(IMODEL,16),STATS(IMODEL,8),STATS(IMODEL,9), &
    LENGTH_UNITS,MASS_UNITS,NAMEMODEL(IMODEL),TIME_UNITS,MPR,NINCL,NPE, &
    NPERD,IDUM,INITOBS,STATS(IMODEL,K1+10),STATS(IMODEL,K2+10), &
    STDERR,CEV,IUMSG,LNDETFP,STATS(IMODEL,17))
    IF(IVERB>4)WRITE(IUMSG,10)TRIM(NAMEMODEL(IMODEL)),NAMEPATH(IMODEL)
    IF(IMODEL == 1) THEN
      NMPR = MPR
      IF(MPR > 0) THEN
        ALLOCATE(PRINAME(MPR))
        ! READ_PR FILE
        INNAM = TRIM(NAMEPATH(IMODEL))//'._pr'
        CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUMODEL)
        ! READ Prior Equation Names
        ! Read header
        READ(IUMODEL,*)CHECK
        IF(IVERB>4) THEN
          WRITE(IUMSG,*)
          WRITE(IUMSG,*)' Reading ',TRIM(INNAM)
          WRITE(IUMSG,*)TRIM(CHECK)
        ENDIF
        DO I=1,NMPR
          READ(IUMODEL,*)PRINAME(I)
          CALL UTL_CASE(TRIM(PRINAME(I)),PRINAME(I),-1)
          IF(IVERB>4) WRITE(IUMSG,*)PRINAME(I)
        ENDDO
        CALL UTL_SHELLSORT(MPR,PRINAME)
        CLOSE(UNIT=IUMODEL)
      ENDIF
    ELSEIF(MPR .NE. NMPR) THEN
      PRIORSAMENUM = .FALSE.
    ELSEIF(MPR > 0 .AND. MPR == NMPR) THEN
      ! READ_PR FILE
      ALLOCATE(PRINAMETMP(MPR))
      PRINAMETMP = ' '
      IF(MPR > 0) THEN
        INNAM = TRIM(NAMEPATH(IMODEL))//'._pr'
        CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUMODEL)
        ! READ Prior Equation Names
        ! Read header
        READ(IUMODEL,*)CHECK
        IF(IVERB>4) THEN
          WRITE(IUMSG,*)
          WRITE(IUMSG,*)' Reading ',TRIM(INNAM)
          WRITE(IUMSG,*)TRIM(CHECK)
        ENDIF
        DO I=1,MPR
          READ(IUMODEL,*)PRINAMETMP(I)
          CALL UTL_CASE(TRIM(PRINAMETMP(I)),PRINAMETMP(I),-1)
        ENDDO
        CALL UTL_SHELLSORT(MPR,PRINAMETMP)
        DO I=1,NMPR
          IF(IVERB>4) WRITE(IUMSG,40)TRIM(PRINAMETMP(I)),TRIM(PRINAME(I))
          IF(TRIM(PRINAMETMP(I)) .NE. TRIM(PRINAME(I)))THEN
            PRIORSAME = .FALSE.
            IF(IVERB>4) WRITE(IUMSG,41)TRIM(PRINAMETMP(I)),TRIM(PRINAME(I))
          ENDIF
        ENDDO
        CLOSE(UNIT=IUMODEL)
      ENDIF
      IF(ALLOCATED(PRINAMETMP))DEALLOCATE(PRINAMETMP)
    ENDIF
    ! READ_SS FILE
    INNAM = TRIM(NAMEPATH(IMODEL))//'._ss'
    CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUMODEL)
    ! READ SWSR
    ! Read header
    READ(IUMODEL,*)CHECK
    IF(IVERB>4) THEN
      WRITE(IUMSG,*)
      WRITE(IUMSG,*)' Reading ',TRIM(INNAM)
      WRITE(IUMSG,*)TRIM(CHECK)
    ENDIF
    ! Note lines start at 0 to include starting values so loop to iterp
    STATS(IMODEL,4)=1.D+31
    STATS(IMODEL,5)=1.D+31
    DO I=1,ITERP+1
      READ(IUMODEL,*)IDUM,RDUM,RDUM2,RDUM3
      IF(RDUM3 < STATS(IMODEL,5)) THEN
        STATS(IMODEL,4)=RDUM
        STATS(IMODEL,5)=RDUM3
        ITER(IMODEL)=IDUM
      ENDIF
      IF(IVERB>4) WRITE(IUMSG,*)IDUM
    ENDDO
    IF(IVERB>4) THEN
      WRITE(IUMSG,*)ITER(IMODEL),STATS(IMODEL,4),STATS(IMODEL,5)
      WRITE(IUMSG,*)' Closing ',TRIM(INNAM)
      WRITE(IUMSG,*)
    ENDIF
    CLOSE (IUMODEL)
    ! NPERD actually estimated
    NPE = NPERD
    ! Correlation with normal deviates, OBS ONLY STATS(IMODEL,K1+10)
    ! Correlation with normal deviates, OBS+PRIOR STATS(IMODEL,K2+10)
    CALL UTL_CASE(TRIM(NAMEMODEL(IMODEL)),NAMEMODEL(IMODEL),1)
    CALL UTL_CASE(TRIM(LENGTH_UNITS),LENGTH_UNITS,1)
    CALL UTL_CASE(TRIM(TIME_UNITS),TIME_UNITS,1)
    CALL UTL_CASE(TRIM(MASS_UNITS),MASS_UNITS,1)
    IF(IMODEL .EQ. 1) THEN
      CALL UTL_CASE(TRIM(LENGTH_UNITS),LUNITS,-1)
      CALL UTL_CASE(TRIM(MASS_UNITS),MUNITS,-1)
      CALL UTL_CASE(TRIM(TIME_UNITS),TUNITS,-1)
    ELSE
      CALL UTL_CASE(TRIM(LENGTH_UNITS),LENGTH_UNITS,-1)
      CALL UTL_CASE(TRIM(MASS_UNITS),MASS_UNITS,-1)
      CALL UTL_CASE(TRIM(TIME_UNITS),TIME_UNITS,-1)
      IF(TRIM(LENGTH_UNITS) .NE. TRIM(LUNITS)) IFAIL = 1
      IF(TRIM(MASS_UNITS) .NE. TRIM(MUNITS)) IFAIL = 1
      IF(TRIM(TIME_UNITS) .NE. TRIM(TUNITS)) IFAIL = 1
      IF(IFAIL > 0) THEN
        AMESSAGE = ' TERMINATING: UNITS ARE NOT CONSISTENT'
        CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
        WRITE(*,30)TRIM(NAMEPATH(IMODEL)),TRIM(NAMEPATH(1))
        WRITE(IUMSG,30)TRIM(NAMEPATH(IMODEL)),TRIM(NAMEPATH(1))
        RETURN
      ENDIF
    ENDIF
    IF(NPE .GT. MAXNPE)MAXNPE = NPE
    STATS(IMODEL,1) = DBLE(NPE)
    STATS(IMODEL,2) = DBLE(NINCL)
    STATS(IMODEL,3) = DBLE(MPR)
    IF(STATS(IMODEL,3) .LT. 1.D0) COLHD(K2+16,3) = 'NONE'
    IF(ICONVERGE == 'YES') THEN
      NAMEL(IMODEL) = 'Y'
      CONVERGED = .TRUE.
    ELSE
      ! MODEL DID NOT CONVERGE, INCREMENT NOTCONV,
      ! SET LOGICAL FOR THIS MODEL TO N.
      ! SET STATS FOR THIS SUBROUTINE TO EXTREMES
      NAMEL(IMODEL) = 'N'
      NOTCONV = NOTCONV + 1
      WRITE(*,22)IMODEL,TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
      WRITE(IUMSG,22)IMODEL,TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
      DO ILOOP=4,20
        STATS(IMODEL,ILOOP)= 1.D+30
      ENDDO
      STATS(IMODEL,K1+10)= 1.D+30
      STATS(IMODEL,K2+10)= 1.D+30
      RETURN
    ENDIF
    ! Sigma squared
    RN=DBLE(NINCL)
    RNP=DBLE(NINCL+MPR)
    IF(RN < 1.D0) THEN
      IFAIL = 1
      WRITE(*,11)NAMEPATH(IMODEL)
      WRITE(IUMSG,11)NAMEPATH(IMODEL)
      AMESSAGE = ' TERMINATING: [(#Observations)+(#Prior)] < 1'
      CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
      RETURN
    ENDIF
    RK=DBLE(NPE)+1.D0
    IF((RN-RK-1.D0) < 1.D0) THEN
      IFAIL = 1
      AMESSAGE = ' TERMINATING: [(#Observations)-(#Parameters)-1] < 1'
      CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
      RETURN
    ENDIF
    ! obs only
    STATS(IMODEL,10) = STATS(IMODEL,8)+(2.D0*RK)  !AIC
    STATS(IMODEL,11) = STATS(IMODEL,9)+(2.D0*RK)  !AIC
    STATS(IMODEL,12) = STATS(IMODEL,8)+(2.D0*RK) &
                       +((2.D0*RK*(RK+1.D0))/(RN-RK-1.D0)) !AICc
    STATS(IMODEL,13) = STATS(IMODEL,9)+(2.D0*RK) &
                       +((2.D0*RK*(RK+1.D0))/(RNP-RK-1.D0)) !AICc
    STATS(IMODEL,14) = STATS(IMODEL,8)+(RK*LOG(RN)) !BIC
    STATS(IMODEL,15) = STATS(IMODEL,9)+(RK*LOG(RNP)) !BIC
    ! change ln|F| to ln|XTwX|  .... ln|XTwX| = ln|F| + N ln(sigmaML^2)
    STATS(IMODEL,18) = LNDETFN + LOG(((STATS(IMODEL,4)/RN))**(RK-1.D0))  !ln|XTwX| Obs
    STATS(IMODEL,19) = LNDETFP + LOG(((STATS(IMODEL,5)/RNP))**(RK-1.D0)) !ln|XTwX| Obs and Prior
    STATS(IMODEL,20) = PRIMODWT(IMODEL)
    IF((RN+STATS(IMODEL,3)-STATS(IMODEL,1)) < 1.D0) THEN
      IFAIL = 1
      AMESSAGE = ' TERMINATING: [(#Observations)+(#Prior)-(#Parameters)] < 1'
      CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
      RETURN
    ENDIF
    STATS(IMODEL,6) = STATS(IMODEL,4)/(RN-STATS(IMODEL,1)) ! CEVObs
    STATS(IMODEL,7) = STATS(IMODEL,5)/(RNP-STATS(IMODEL,1)) ! CEVwPri
    RETURN
    ! EOF WAS REACHED
    900 CALL MMA_EOF(IFAIL,INNAM)
    RETURN
  END SUBROUTINE MMA_READ_DM
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_PCREAS(IFAIL,IMODEL,CHECKPARAM)
    ! This subroutine evaluates whether this model should be included based on
    ! whether the optimal parameter values meet the reasonable criteria
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,                    INTENT(INOUT)    :: IFAIL
    INTEGER,                    INTENT(IN)       :: IMODEL
    LOGICAL,                    INTENT(INOUT)    :: CHECKPARAM
    ! Local variables
    CHARACTER(LEN=200)                           :: CHECK = ' '
    DOUBLE PRECISION                             :: DUMMY1
    DOUBLE PRECISION                             :: DUMMY2
    INTEGER                                      :: I
    INTEGER                                      :: ITYP
    INTEGER                                      :: J
    INTEGER                                      :: K
    INTEGER                                      :: ILOGF
    CHARACTER(LEN=MAX_STRING_LEN)                :: INNAM = ' '
    INTEGER                                      :: IUMODEL
    LOGICAL                                      :: LVAL
    INTEGER                                      :: NPARS
    CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: PNAM
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: PVAL
    DOUBLE PRECISION                             :: RVAL
    ! Formats
    22 FORMAT(I6,'   UNREASONABLE PARAMETERS:   ',A12,' in ',A)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_PCREAS'
    IFAIL = 0
    ITYP = 0
    RVAL = 0.D0
    LVAL = .TRUE.
    ! OPEN _PC TO READ OPTIMAL PARAMETERS
    INNAM = TRIM(NAMEPATH(IMODEL))//'._pc'
    CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUMODEL)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB)
    NPARS = INT(STATS(IMODEL,1))
    ALLOCATE(PNAM(NPARS),PVAL(NPARS))
    ! Read Optimal Parameters
    READ(IUMODEL,*,END=900)CHECK
    IF(IVERB>4) THEN
      WRITE(IUMSG,*)
      WRITE(IUMSG,*)' Reading ',TRIM(INNAM)
      WRITE(IUMSG,*)TRIM(CHECK)
    ENDIF
    DO I=1,NPARS
      READ(IUMODEL,*,END=900)PNAM(I),PVAL(I),DUMMY1,DUMMY2,ILOGF
      IF(IVERB>4)WRITE(IUMSG,*)PNAM(I),PVAL(I),DUMMY1,DUMMY2,ILOGF
    ENDDO
    IF(IVERB>4) THEN
      WRITE(IUMSG,*)' Closing ',TRIM(INNAM)
      WRITE(IUMSG,*)
    ENDIF
    ! MAKE PARAMETER NAMES LOWER CASE
    DO I=1,NPARS
      CALL UTL_CASE(TRIM(PNAM(I)),PNAM(I),-1)
    ENDDO
    CLOSE (IUMODEL)
    ! EVALUATE EQN DESCRIBING PARAMETER RELATIONSHIPS
    ! IF ANY ARE NOT TRUE, RETURN CHECKPARAM as FALSE
    J = NAMEPATHGPN(IMODEL) ! group for this model
    IF(PARAMEQNL) THEN
      IF(EQNL(J)) THEN
        K = MAXEQN(J)         ! # equations for this group
        DO I=1,K
          CALL EQN_EVALUATE(IFAIL,EQNARRAY(J,I),NPARS,PNAM,PVAL,ITYP,RVAL,LVAL)
          IF (IFAIL.NE.0) THEN
            CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
            CALL UTL_STOP('EQN_EVALUATE reported failure')
          ENDIF
          IF(.NOT. LVAL) THEN
            ! PARAMETERS DO NOT MEET THE SPECIFIED CRITERION FOR REASONABLE
            CHECKPARAM = .FALSE.
            NAMEL(IMODEL) = 'N'
            NOTREAS = NOTREAS + 1
            WRITE(*,22)IMODEL,TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
            WRITE(IUMSG,22)IMODEL,TRIM(NAMEMODEL(IMODEL)),TRIM(NAMEPATH(IMODEL))
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDIF
    DEALLOCATE(PNAM,PVAL)
    RETURN
    ! EOF WAS REACHED
    900 CALL MMA_EOF(IFAIL,INNAM)
    RETURN
  END SUBROUTINE MMA_EVA_PCREAS
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_ALLOC(IFAIL,NOBS,NOBSMPR)
    ! This subroutine allocates arrays dimensioned to NOBS and NOBSMPR
    ! and intializes
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,                     INTENT(INOUT)  :: IFAIL
    INTEGER,                     INTENT(IN)     :: NOBS
    INTEGER,                     INTENT(IN)     :: NOBSMPR
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_ALLOC'
    IFAIL = 0
    !
    ALLOCATE (VAL1(NOBSMPR),VAL2(NOBSMPR),IPLOT(NOBSMPR),OBSNAME(NOBSMPR), &
              OBSNAMEALL(NOBSMPR),TCOORD(NOBS))
    ! Initialize
    OBSNAME = ' '
    OBSNAMEALL = ' '
    TCOORD = 1.e-30
    RETURN
  END SUBROUTINE MMA_EVA_ALLOC
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_LINES(IFAIL,IMODEL,NOBS)
    ! This subroutine opens os ws ww files for this model
    ! Calls:
    ! MMA_EVA_RISXY1 MMA_EVA_RIS
    ! to get the xy data and evaluate slope intercept and r^2
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,                    INTENT(INOUT) :: IFAIL
    INTEGER,                    INTENT(IN)    :: IMODEL
    INTEGER,                    INTENT(IN)    :: NOBS
    ! Local variables
    INTEGER                                   :: IA
    INTEGER                                   :: I
    INTEGER                                   :: II
    CHARACTER(LEN=MAX_STRING_LEN)             :: INNAM = ' '
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_LINES'
    IFAIL = 0
    ! Evaluate frist os then ws then ww, 1to3 for obs only
      DO II = 1,3
        IF(II .EQ. 1) THEN
          OBSNAME(1) = ' '
          INNAM = TRIM(NAMEPATH(IMODEL))//'._os'
          IA = K1+1
        ELSEIF(II .EQ. 2) THEN
          INNAM = TRIM(NAMEPATH(IMODEL))//'._ws'
          IA = K1+4
        ELSE
          INNAM = TRIM(NAMEPATH(IMODEL))//'._ww'
          IA = K1+7
        ENDIF
        ! get the data
        CALL MMA_EVA_RISXY1(IFAIL,INNAM,MPR,NOBS)
        IF(IFAIL .NE. 0) RETURN
        ! evaluate the slope intercept and R^2
        CALL MMA_EVA_RIS(IFAIL,IMODEL,IA,NOBS)
        ! repeat with prior
        CALL MMA_EVA_RIS(IFAIL,IMODEL,IA+10,NOBS+MINMPR)
        IF(IFAIL .NE. 0) RETURN
      ENDDO
      IF(IMODEL == 1) THEN
        ! Save names of observations for first model
        NTOTOBS = NINCL + MPR
        DO I=1,NTOTOBS
          CALL UTL_CASE(TRIM(OBSNAME(I)),OBSNAMEALL(I),-1)
        ENDDO
        ! Sort the names array in alpha order
        CALL UTL_SHELLSORT (NTOTOBS,OBSNAMEALL)
      ELSE
        ! for later models check that they are using the same observations
        CALL MMA_INI_NAMCHK(IFAIL)
        IF(IFAIL > 0)CALL UTL_STOP(' Called from MMA_EVA_LINES: '//ERRSUB)
      ENDIF
      RETURN
    END SUBROUTINE MMA_EVA_LINES
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_TIME(IFAIL,IMODEL,NOBS)
    ! This subroutine opens files for spatial/temporal data
    ! Calls
    ! MMA_EVA_RISXY2 MMA_EVA_RIS
    ! to read the data and evaluate slope intercept and R^2
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,                    INTENT(INOUT) :: IFAIL
    INTEGER,                    INTENT(IN)    :: IMODEL
    INTEGER,                    INTENT(IN)    :: NOBS
    ! Local variables
    INTEGER                                   :: IA
    INTEGER                                   :: II
    INTEGER                                   :: ILOOP
    CHARACTER(LEN=MAX_STRING_LEN)             :: INNAM = ' '
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_TIME'
    IFAIL = 0
    ! Evaluate residuals and weighted residuals with respect to time
    IF(TCOORD(1) .NE. 1.E-30) THEN
      ! first for residuals then weighted-residuals 1then2
      DO II = 1,2
        IF(II .EQ. 1) THEN
          INNAM = TRIM(NAMEPATH(IMODEL))//'._r'
          IA = K1+11
        ELSE
          INNAM = TRIM(NAMEPATH(IMODEL))//'._w'
          IA = K1+14
        ENDIF
        ! get the data
        CALL MMA_EVA_RISXY2(IFAIL,INNAM,MPR,NOBS)
        IF(IFAIL .NE. 0) RETURN
        ! evaluate the slope intercept and R^2
        CALL MMA_EVA_RIS(IFAIL,IMODEL,IA,NOBS)
        IF(IFAIL .NE. 0) RETURN
      ENDDO
    ELSE
      ! No time coordinates for this model set
      TCOORDS = .FALSE.
      TCOORDL(IMODEL) = .FALSE.
      DO ILOOP=1,6
        STATS(IMODEL,K3+ILOOP) = 1.0D+30
      ENDDO
    ENDIF
    RETURN
  END SUBROUTINE MMA_EVA_TIME
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_RISXY1(IFAIL,INNAM,MPR,NOBS)
    ! This subroutine reads xy data from one file
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,                       INTENT(INOUT) :: IFAIL
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN)    :: INNAM
    INTEGER,                       INTENT(IN)    :: MPR
    INTEGER,                       INTENT(IN)    :: NOBS
    ! Internal variables
    CHARACTER(LEN=200)                           :: CHECK = ' '
    INTEGER                                      :: IOBS
    INTEGER                                      :: IUMODEL = 0
    INTEGER                                      :: TIPLOT
    CHARACTER(LEN=LENDNAM)                       :: TOBSNAME
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_RISXY1'
    IFAIL = 0
    ! Task
    ! Open file
    CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUMODEL)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB)
    READ(IUMODEL,*,END=900)CHECK
    IF(IVERB>4) THEN
      WRITE(IUMSG,*)
      WRITE(IUMSG,*)' Reading ',TRIM(INNAM)
      WRITE(IUMSG,*)TRIM(CHECK)
    ENDIF
    ! First read for this model
    IF(OBSNAME(1).EQ.' ') THEN
      DO IOBS = 1,NOBS+MINMPR
        READ (IUMODEL,*,END=900)VAL1(IOBS),VAL2(IOBS),IPLOT(IOBS),OBSNAME(IOBS)
        IF(IVERB>4)WRITE(IUMSG,*)VAL1(IOBS),VAL2(IOBS),IPLOT(IOBS),OBSNAME(IOBS)
        CALL UTL_CASE(TRIM(OBSNAME(IOBS)),OBSNAME(IOBS),-1)
      ENDDO
    ! Not first read for this model, check if names are different for
    ! underscore files in same directory and note possible error in the
    ! inversion runs
    ELSE
      DO IOBS = 1,NOBS+MINMPR
        READ (IUMODEL,*,END=900)VAL1(IOBS),VAL2(IOBS),TIPLOT,TOBSNAME
        IF(IVERB>4)WRITE(IUMSG,*)VAL1(IOBS),VAL2(IOBS),TIPLOT,TOBSNAME
        CALL UTL_CASE(TRIM(TOBSNAME),TOBSNAME,-1)
        IF(TOBSNAME .NE. OBSNAME(IOBS)) THEN
          IFAIL = 1
          CLOSE(IUMODEL)
          IF(IVERB>4)WRITE(IUMSG,*)' Closing ',TRIM(INNAM)
          RETURN
        ENDIF
      ENDDO
    ENDIF
    CLOSE(IUMODEL)
    RETURN
    ! EOF WAS REACHED
    900 CALL MMA_EOF(IFAIL,INNAM)
    RETURN
  END SUBROUTINE MMA_EVA_RISXY1
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_RISXY2(IFAIL,INNAM,MPR,NOBS)
    ! This subroutine reads spatial/temporal data from two files
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,                       INTENT(INOUT) :: IFAIL
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN)    :: INNAM
    INTEGER,                       INTENT(IN)    :: MPR
    INTEGER,                       INTENT(IN)    :: NOBS
    ! Internal variables
    CHARACTER(LEN=200)                           :: CHECK = ' '
    INTEGER                                      :: IOBS
    INTEGER                                      :: IIOBS
    INTEGER                                      :: IUMODEL = 0
    INTEGER                                      :: TIPLOT
    CHARACTER(LEN=LENDNAM)                       :: TOBSNAME
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_RISXY2'
    IFAIL = 0
    ! Task
    ! Open file
    CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUMODEL)
    READ(IUMODEL,*,END=900)CHECK
    IF(IVERB>4) THEN
      WRITE(IUMSG,*)
      WRITE(IUMSG,*)' Reading ',TRIM(INNAM)
      WRITE(IUMSG,*)TRIM(CHECK)
    ENDIF
    ! First read for this model was done in MMA_EVA_RISXY1, so check
    ! names in these files with the first read on names in this directory
    DO IOBS = 1,NOBS+MPR
      READ (IUMODEL,*,END=900)VAL2(IOBS),TIPLOT,TOBSNAME
      IF(IVERB>4) WRITE(IUMSG,*)VAL2(IOBS),TIPLOT,TOBSNAME
      CALL UTL_CASE(TRIM(TOBSNAME),TOBSNAME,-1)
      COMPARE : DO IIOBS=1,NOBS
        IFAIL = 0
        IF(TRIM(OBSNAME(IIOBS)) .EQ. TRIM(TOBSNAME)) THEN
          VAL1(IOBS) = TCOORD(IIOBS)
          EXIT COMPARE
        ENDIF
        IFAIL = 1
      ENDDO COMPARE
    ENDDO
    CLOSE(IUMODEL)
    IF(IVERB>4) WRITE(IUMSG,*)' Closing ',TRIM(INNAM)
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    RETURN
    ! EOF WAS REACHED
    900 CALL MMA_EOF(IFAIL,INNAM)
    RETURN
  END SUBROUTINE MMA_EVA_RISXY2
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_RIS(IFAIL,IMODEL,IA,NEVAL)
    ! This subroutine calculates R2, intercept and slope of best fit line
    ! to xy data
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,            INTENT(INOUT)  :: IFAIL
    INTEGER,            INTENT(IN)     :: IMODEL
    INTEGER,            INTENT(IN)     :: IA
    INTEGER,            INTENT(IN)     :: NEVAL
    !   Intrenal variables
    DOUBLE PRECISION                   :: SUMX
    DOUBLE PRECISION                   :: SUMX2
    DOUBLE PRECISION                   :: SUMY
    DOUBLE PRECISION                   :: SUMY2
    DOUBLE PRECISION                   :: SUMXY
    DOUBLE PRECISION                   :: SUMXS2
    DOUBLE PRECISION                   :: SUMYS2
    INTEGER IOBS
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_RIS'
    IFAIL = 0
    SUMX = 0.0D0
    SUMY = 0.0D0
    SUMX2 = 0.0D0
    SUMY2 = 0.0D0
    SUMXY = 0.0D0
    SUMXS2 = 0.0D0
    SUMYS2 = 0.0D0
    ! Calculate sums used to solve normal equations for straight line fit
    DO IOBS = 1,NEVAL
      SUMX = SUMX + VAL1(IOBS)
      SUMX2 = SUMX2 + (VAL1(IOBS)*VAL1(IOBS))
      SUMY = SUMY + VAL2(IOBS)
      SUMY2 = SUMY2 + (VAL2(IOBS)*VAL2(IOBS))
      SUMXY = SUMXY + (VAL1(IOBS)*VAL2(IOBS))
    ENDDO
    SUMXS2 = SUMX * SUMX
    SUMYS2 = SUMY * SUMY
    ! IA indicates column of STATS where data starts for this R2 I Slp
    ! R2
    STATS(IMODEL,IA)   = (NEVAL*SUMXY-SUMX*SUMY)/SQRT((NEVAL*SUMX2-SUMXS2)* &
                       (NEVAL*SUMY2-SUMYS2))
    STATS(IMODEL,IA)   = STATS(IMODEL,IA) * STATS(IMODEL,IA)
    ! INTERECPT
    STATS(IMODEL,IA+1) = (SUMY*SUMX2-SUMX*SUMXY)/(NEVAL*SUMX2-SUMXS2)
    ! SLOPE
    STATS(IMODEL,IA+2) = (NEVAL*SUMXY-SUMX*SUMY)/(NEVAL*SUMX2-SUMXS2)
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    RETURN
  END SUBROUTINE MMA_EVA_RIS
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_CENT(IFAIL,IMODEL,NOBS)
    ! This subroutine finds the centroid of obs locations and compares it
    ! with the centroid of weighted residuals
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER, INTENT(INOUT)          :: IFAIL
    INTEGER, INTENT(IN)             :: IMODEL
    INTEGER, INTENT(IN)             :: NOBS
    !   Internal variables
    CHARACTER(LEN=200)              :: CHECK = ' '
    INTEGER                         :: CHECKIPLOT
    CHARACTER(LEN=LENDNAM)          :: CHECKOBSNAME
    INTEGER                         :: IIOBS
    CHARACTER(LEN=MAX_STRING_LEN)   :: INNAM = ' '
    INTEGER                         :: IOBS
    INTEGER                         :: IUMODEL = 0
    INTEGER                         :: IUMODEL2 = 0
    INTEGER                         :: IUXYZ = 0
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: R
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: WR
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: XCOORD
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: YCOORD
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: ZCOORD
    DOUBLE PRECISION                :: SUMX
    DOUBLE PRECISION                :: SUMY
    DOUBLE PRECISION                :: SUMZ
    DOUBLE PRECISION                :: SUMW
    DOUBLE PRECISION                :: SUMDXW
    DOUBLE PRECISION                :: SUMDYW
    DOUBLE PRECISION                :: SUMDZW
    DOUBLE PRECISION                :: SUMDXSIGNW
    DOUBLE PRECISION                :: SUMDYSIGNW
    DOUBLE PRECISION                :: SUMDZSIGNW
    DOUBLE PRECISION                :: SUMDXMAGW
    DOUBLE PRECISION                :: SUMDYMAGW
    DOUBLE PRECISION                :: SUMDZMAGW
    CHARACTER(LEN=LENDNAM)          :: TOBSNAME
    DOUBLE PRECISION                :: TT
    DOUBLE PRECISION                :: TX
    DOUBLE PRECISION                :: TY
    DOUBLE PRECISION                :: TZ
    ! Formats
    11 FORMAT(/,' Error for model: ',A12)
    15 FORMAT(/,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', &
                '!!!!!!!!!!!!!!!!!!!!!!!!',/,'!!!!!!!!!!!!!!!!!!!!!!!!!!!', &
                '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',/)
    20 FORMAT(/,5X,'Directory from which MMA was executed does not contain', &
              /,7X,'an MMAroot.xyzt file. ', &
              /,5X,'Spatial measures will not be evaluated. They will be ', &
              /,7X,'printed as 1E+30.')
    ! Initialize
    ALLOCATE(R(NOBS),WR(NOBS),XCOORD(NOBS),YCOORD(NOBS),ZCOORD(NOBS))
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_CENT'
    IFAIL = 0
    XCOORD = 0.D0
    YCOORD = 0.D0
    ZCOORD = 0.D0
    TCOORD = 0.D0
    IF(.NOT. XYZFILEEXIST) RETURN
    ! Check whether an .xyz file exists
    INNAM = TRIM(OUTNAM)//'.xyzt'
    INQUIRE(FILE=TRIM(INNAM),EXIST=XYZFILE)
    IF(.NOT. XYZFILE) THEN
      IF(IMODEL == 1) THEN
        XYZFILE = .FALSE.
        XYZFILEEXIST = .FALSE.
        WRITE(IUMSG,15)
        WRITE(IUMSG,20)
        WRITE(IUMSG,15)
        WRITE(*,15)
        WRITE(*,20)
        WRITE(*,15)
      ENDIF
      RETURN
    ENDIF
    ! If xyz exists, open it
    CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUXYZ)
    IF(IVERB>4) THEN
      WRITE(IUMSG,*)
      WRITE(IUMSG,*)' Reading unit: ',IUXYZ,TRIM(INNAM)
    ENDIF
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB)
    ! and open associated _r file
    INNAM = TRIM(NAMEPATH(IMODEL))//'._r'
    CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUMODEL)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB)
    IF(IVERB>4) WRITE(IUMSG,*)' and Reading unit: ',IUMODEL,TRIM(INNAM)
    ! and open associated _w file
    INNAM = TRIM(NAMEPATH(IMODEL))//'._w'
    CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUMODEL2)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB)
    IF(IVERB>4) WRITE(IUMSG,*)' and Reading unit: ',IUMODEL2,TRIM(INNAM)
    READ(IUMODEL,*,END=900)CHECK
    IF(IVERB>4) WRITE(IUMSG,*)' Reading unit: ',IUMODEL,TRIM(CHECK)
    READ(IUMODEL2,*,END=900)
    IF(IVERB>4) WRITE(IUMSG,*)' Reading unit: ',IUMODEL2,TRIM(CHECK)
    READ(IUXYZ,*,END=900)CHECK
    IF(IVERB>4) WRITE(IUMSG,*)' Reading unit: ',IUXYZ,TRIM(CHECK)
    ! MATCH XYZ WITH OBS
    DO IOBS = 1,NOBS
      READ (IUMODEL,*,END=900)R(IOBS),IPLOT(IOBS),OBSNAME(IOBS)
      IF(IVERB>4) WRITE(IUMSG,*)' Reading unit: ', &
                  IUMODEL,R(IOBS),IPLOT(IOBS),TRIM(OBSNAME(IOBS))
      CALL UTL_CASE(TRIM(OBSNAME(IOBS)),OBSNAME(IOBS),-1)
      READ (IUMODEL2,*,END=900)WR(IOBS),CHECKIPLOT,CHECKOBSNAME
      IF(IVERB>4) WRITE(IUMSG,*)' Reading unit: ',IUMODEL2, &
                  WR(IOBS),CHECKIPLOT,TRIM(CHECKOBSNAME)
      CALL UTL_CASE(TRIM(CHECKOBSNAME),CHECKOBSNAME,-1)
      ! If observation name mismatch is found
      IF(CHECKOBSNAME .NE. OBSNAME(IOBS)) THEN
        IFAIL = 1
        CLOSE (IUMODEL)
        CLOSE (IUMODEL2)
        CLOSE (IUXYZ)
        IF(IVERB>4) WRITE(IUMSG,*)' Closing units: ',IUMODEL,IUMODEL2,IUXYZ
        IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
        RETURN
      ENDIF
    ENDDO
    !
    DO IOBS = 1,NOBS
      READ (IUXYZ,*,END=901)TOBSNAME,TX,TY,TZ,TT
      IF(IVERB>4) WRITE(IUMSG,*)' Reading unit: ',IUXYZ,TRIM(TOBSNAME), &
                  TX,TY,TZ,TT
      CALL UTL_CASE(TRIM(TOBSNAME),TOBSNAME,-1)
      COMPARE : DO IIOBS=1,NOBS
        IF(OBSNAME(IIOBS) .EQ. TOBSNAME) THEN
          XCOORD(IIOBS) = TX
          YCOORD(IIOBS) = TY
          ZCOORD(IIOBS) = TZ
          TCOORD(IIOBS) = TT
          EXIT COMPARE
        ENDIF
      ENDDO COMPARE
    ENDDO
    ! If coords were set to extremes then do not do spatial/temporal analyses
    DO IOBS = 1,NOBS
      IF(XCOORD(IOBS) .EQ. 1.D-30 .AND. &
         YCOORD(IOBS) .EQ. 1.D-30 .AND. &
         ZCOORD(IOBS) .EQ. 1.D-30 .AND. &
         TCOORD(IOBS) .EQ. 1.D-30) THEN
        COORDS = .FALSE.
        COORDL(IMODEL) = .FALSE.
        CLOSE (IUMODEL)
        CLOSE (IUMODEL2)
        CLOSE (IUXYZ)
        IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
        RETURN
      ENDIF
    ENDDO
    TT=TCOORD(1)
    DO IOBS = 2,NOBS
      IF(TCOORD(IOBS) .NE. TT) THEN
        TIME_USED = .TRUE.
        EXIT
      ENDIF
    ENDDO
    CLOSE (IUMODEL)
    CLOSE (IUMODEL2)
    CLOSE (IUXYZ)
    ! Initialize
    SUMX = 0.0D0
    SUMY = 0.0D0
    SUMZ = 0.0D0
    SUMW = 0.0D0
    SUMDXW = 0.0D0
    SUMDYW = 0.0D0
    SUMDZW = 0.0D0
    SUMDXSIGNW = 0.0D0
    SUMDYSIGNW = 0.0D0
    SUMDZSIGNW = 0.0D0
    SUMDXMAGW = 0.0D0
    SUMDYMAGW = 0.0D0
    SUMDZMAGW = 0.0D0
    ! Calculate sums used to solve normal equations for straight line fit
    DO IOBS = 1,NOBS
      SUMX = SUMX + XCOORD(IOBS)
      SUMY = SUMY + YCOORD(IOBS)
      SUMZ = SUMZ + ZCOORD(IOBS)
      SUMW = SUMW + ABS(WR(IOBS))
    ENDDO
    ! unweighted centroids x then y (centroid of observation locations)
    IF(NOBS < 1) THEN
      IFAIL = 1
      WRITE(*,11)NAMEPATH(IMODEL)
      WRITE(IUMSG,11)NAMEPATH(IMODEL)
      AMESSAGE = ' TERMINATING: #Observations < 1'
      CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
      RETURN
    ENDIF
    STATS(IMODEL,K3+7) = SUMX/NOBS
    STATS(IMODEL,K3+8) = SUMY/NOBS
    STATS(IMODEL,K3+9) = SUMZ/NOBS
    ! dist of obs from centroid of locations * weighted resiudal in x and y
    DO IOBS = 1,NOBS
      IF(WR(IOBS) .NE. 0.D0) THEN
        SUMDXW = SUMDXW + (STATS(IMODEL,K3+7)-XCOORD(IOBS))*WR(IOBS)
        SUMDYW = SUMDYW + (STATS(IMODEL,K3+8)-YCOORD(IOBS))*WR(IOBS)
        SUMDZW = SUMDZW + (STATS(IMODEL,K3+9)-ZCOORD(IOBS))*WR(IOBS)
        ! centroid of sign of weighted residuals
        SUMDXSIGNW = SUMDXSIGNW + ABS(STATS(IMODEL,K3+7)-XCOORD(IOBS))*WR(IOBS)/ &
                     ABS(WR(IOBS))
        SUMDYSIGNW = SUMDYSIGNW + ABS(STATS(IMODEL,K3+8)-YCOORD(IOBS))*WR(IOBS)/ &
                     ABS(WR(IOBS))
        SUMDZSIGNW = SUMDZSIGNW + ABS(STATS(IMODEL,K3+9)-ZCOORD(IOBS))*WR(IOBS)/ &
                     ABS(WR(IOBS))
        ! centroid of magnitude of weighted residuals
        SUMDXMAGW = SUMDXMAGW + (XCOORD(IOBS)-STATS(IMODEL,K3+7))*ABS(WR(IOBS))
        SUMDYMAGW = SUMDYMAGW + (YCOORD(IOBS)-STATS(IMODEL,K3+8))*ABS(WR(IOBS))
        SUMDZMAGW = SUMDZMAGW + (ZCOORD(IOBS)-STATS(IMODEL,K3+9))*ABS(WR(IOBS))
      ENDIF
    ENDDO
    IF(SUMW == 0.D0) THEN
      IFAIL = 1
      WRITE(*,11)NAMEPATH(IMODEL)
      WRITE(IUMSG,11)NAMEPATH(IMODEL)
      AMESSAGE = ' TERMINATING: Sum of Weighted Residuals = 0'
      CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
      STATS(IMODEL,K3+10) = 1.D+30
      STATS(IMODEL,K3+11) = 1.D+30
      STATS(IMODEL,K3+12) = 1.D+30
    ELSE
      ! (location - centroid) * weighted residual /SumWR for x then y
      STATS(IMODEL,K3+10) = SUMDXW/SUMW
      STATS(IMODEL,K3+11) = SUMDYW/SUMW
      STATS(IMODEL,K3+12) = SUMDZW/SUMW
    ENDIF
    ! wt residual centroid dist from location centroid
    STATS(IMODEL,K3+13) = SQRT(STATS(IMODEL,K3+10)*STATS(IMODEL,K3+10) &
        +STATS(IMODEL,K3+11)*STATS(IMODEL,K3+11)+STATS(IMODEL,K3+12)* &
        STATS(IMODEL,K3+12))
    ! (location - centroid) * sign of weighted residual /NOBS for x then y
    STATS(IMODEL,K3+14) = SUMDXSIGNW/NOBS
    STATS(IMODEL,K3+15) = SUMDYSIGNW/NOBS
    STATS(IMODEL,K3+16) = SUMDZSIGNW/NOBS
    ! diat if centroid of sign of wt residual from location centroid
    STATS(IMODEL,K3+17) = SQRT(STATS(IMODEL,K3+14)*STATS(IMODEL,K3+14) &
        +STATS(IMODEL,K3+15)*STATS(IMODEL,K3+15)+STATS(IMODEL,K3+16)* &
        STATS(IMODEL,K3+16))
    ! (location - centroid) * magnitude of weighted residual /NOBS for x then y
    IF(SUMW == 0.D0) THEN
      IFAIL = 1
      WRITE(*,11)NAMEPATH(IMODEL)
      WRITE(IUMSG,11)NAMEPATH(IMODEL)
      AMESSAGE = ' TERMINATING: Sum of Weighted Residuals = 0'
      CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
      STATS(IMODEL,K3+10) = 1.D+30
      STATS(IMODEL,K3+11) = 1.D+30
      STATS(IMODEL,K3+12) = 1.D+30
    ELSE
      STATS(IMODEL,K3+18) = SUMDXMAGW/SUMW
      STATS(IMODEL,K3+19) = SUMDYMAGW/SUMW
      STATS(IMODEL,K3+20) = SUMDZMAGW/SUMW
    ENDIF
    ! diag if centroid of magnitude of wt residual from location centroid
    STATS(IMODEL,K3+21) = SQRT(STATS(IMODEL,K3+18)*STATS(IMODEL,K3+18) &
        +STATS(IMODEL,K3+19)*STATS(IMODEL,K3+19)+STATS(IMODEL,K3+20) &
        *STATS(IMODEL,K3+20))
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    IF(ALLOCATED(R))DEALLOCATE(R)
    IF(ALLOCATED(WR))DEALLOCATE(WR)
    IF(ALLOCATED(XCOORD))DEALLOCATE(XCOORD)
    IF(ALLOCATED(YCOORD))DEALLOCATE(YCOORD)
    IF(ALLOCATED(ZCOORD))DEALLOCATE(ZCOORD)
    RETURN
    ! EOF WAS REACHED
    900 CALL MMA_EOF(IFAIL,INNAM)
    IF(ALLOCATED(R))DEALLOCATE(R)
    IF(ALLOCATED(WR))DEALLOCATE(WR)
    IF(ALLOCATED(XCOORD))DEALLOCATE(XCOORD)
    IF(ALLOCATED(YCOORD))DEALLOCATE(YCOORD)
    IF(ALLOCATED(ZCOORD))DEALLOCATE(ZCOORD)
    RETURN
    ! EOF XYZT FILE WAS REACHED
    901 IFAIL = -1
    IF(ALLOCATED(R))DEALLOCATE(R)
    IF(ALLOCATED(WR))DEALLOCATE(WR)
    IF(ALLOCATED(XCOORD))DEALLOCATE(XCOORD)
    IF(ALLOCATED(YCOORD))DEALLOCATE(YCOORD)
    IF(ALLOCATED(ZCOORD))DEALLOCATE(ZCOORD)
    RETURN
  END SUBROUTINE MMA_EVA_CENT
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_ORDERSTATS(IFAIL,NMODELS,VALUE,DELTA,LOWJ)
    ! This subroutine orders the columns of STATS from highest to lowest
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,            INTENT(INOUT)  :: IFAIL
    INTEGER,            INTENT(IN)     :: NMODELS
    DOUBLE PRECISION,   INTENT(IN)     :: VALUE(NMODELS)
    DOUBLE PRECISION,   INTENT(OUT)    :: DELTA(NMODELS)
    INTEGER,            INTENT(OUT)    :: LOWJ
    ! Internal variables
    INTEGER                            :: ACCT(NMODELS)
    DOUBLE PRECISION                   :: HIGHEST
    INTEGER                            :: IMODEL
    INTEGER                            :: JMODEL
    INTEGER                            :: LASTJ
    DOUBLE PRECISION                   :: LOWCRIT
    DOUBLE PRECISION                   :: LOWEST
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_ORDERSTATS'
    IFAIL = 0
    ACCT = 0
    ORDER=0
    LASTJ=1
    ! FIND HIGHEST
    HIGHEST = -1.D+31
    LOWCRIT = +1.D+31
    DO JMODEL=1,NMODELS
      IF(VALUE(JMODEL) .GT. HIGHEST) THEN
        HIGHEST = VALUE(JMODEL)
        LASTJ = JMODEL
      ENDIF
      IF(VALUE(JMODEL) .LT. LOWCRIT) THEN
        LOWCRIT = VALUE(JMODEL)
        LOWJ = JMODEL
      ENDIF
    ENDDO
    ORDER(1)=LASTJ
    ACCT(LASTJ)=1
    ! LOOP FINDING NEXT HIGHEST EACH TIME
    DO IMODEL = 2,NMODELS
      LOWEST = -1.D+30
      DO JMODEL = 1,NMODELS
        IF(VALUE(JMODEL) .LE. HIGHEST .AND. VALUE(JMODEL) &
           .GT. LOWEST .AND. ACCT(JMODEL) .EQ. 0)THEN
          ORDER(IMODEL)=JMODEL
          LOWEST=VALUE(JMODEL)
          LASTJ=JMODEL
        ENDIF
      ENDDO
      HIGHEST=VALUE(LASTJ)
      ACCT(LASTJ)=1
    ENDDO
      ! LOOP CALCULATING DELTAs
    DO IMODEL = 1,NMODELS
      DELTA(IMODEL) = VALUE(IMODEL) - LOWCRIT
    ENDDO
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    RETURN
  END SUBROUTINE MMA_EVA_ORDERSTATS
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_DEVALUE()
    ! This subroutine sets stats related to prior to 1D+30
    IMPLICIT NONE
    ! Local variables
    INTEGER IMODEL
    !Formats
    15 FORMAT(/,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', &
                '!!!!!!!!!!!!!!!!!!!!!!!!',/,'!!!!!!!!!!!!!!!!!!!!!!!!!!!', &
                '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',/)
    30 FORMAT(/,5X,'Models evaluated do not have the same number of prior', &
              /,7X,'equations. Model measures based on prior will be set ', &
              /,7X,'to 1.E+30.')
    31 FORMAT(/,5X,'Models evaluated do not have the same prior equation', &
              /,7X,' names. Model measures based on prior will be set ', &
              /,7X,'to 1.E+30.')
    ! check if all models have same prior
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_DEVALUE'
    IF(.NOT. PRIORSAMENUM .OR. .NOT. PRIORSAME) THEN
      WRITE(IUMSG,15)
      WRITE(*,15)
      IF(.NOT. PRIORSAMENUM) THEN
        WRITE(IUMSG,30)
        WRITE(*,30)
      ENDIF
      IF(.NOT. PRIORSAME) THEN
        WRITE(IUMSG,31)
        WRITE(*,31)
      ENDIF
      WRITE(IUMSG,15)
      WRITE(*,15)
      DO IMODEL=1,NMPTHS
        STATS(IMODEL,5)  = 1.0D+30
        STATS(IMODEL,7) = 1.0D+30
        STATS(IMODEL,9)  = 1.0D+30
        STATS(IMODEL,11)  = 1.0D+30
        STATS(IMODEL,13)  = 1.0D+30
        STATS(IMODEL,15)  = 1.0D+30
        STATS(IMODEL,17) = 1.0D+30
        STATS(IMODEL,19) = 1.0D+30
        STATS(IMODEL,20) = 1.0D+30
      ENDDO
    ENDIF
    RETURN
  END SUBROUTINE MMA_EVA_DEVALUE
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_EXTREMES(IFAIL,IMODEL)
    ! This subroutine sets stats to extreme values for nonconverged models
    ! and those with unreasonable parameter estimates
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,            INTENT(INOUT)  :: IFAIL
    INTEGER,            INTENT(IN)     :: IMODEL
    ! Local variables
    INTEGER :: ILOOP
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_EXTREMES'
    IFAIL = 0
    DO ILOOP=4,20
      STATS(IMODEL,ILOOP) = 1.0D+30
    ENDDO
    DO ILOOP=1,10
      STATS(IMODEL,K1+ILOOP) = 1.0D+30
      STATS(IMODEL,K2+ILOOP) = 1.0D+30
    ENDDO
    DO ILOOP=1,21
      STATS(IMODEL,K3+ILOOP) = 1.0D+30
    ENDDO
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    RETURN
  END SUBROUTINE MMA_EVA_EXTREMES
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_PREDYPD(IFAIL,AVGPREDL)
    ! This subroutine reads and stores the predictions and their variances for
    ! individual, simulataneous, and infinite confidence intervals, and for
    ! individual, simulataneous, and infinite prediction intervals
    ! on predictions
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,                     INTENT(INOUT)   :: IFAIL
    LOGICAL,                     INTENT(INOUT)   :: AVGPREDL
    ! Local variables
    CHARACTER(LEN=1)                             :: CHCK = ' '
    CHARACTER(LEN=LENDNAM)                       :: CHECK = ' '
    DOUBLE PRECISION                             :: CL
    DOUBLE PRECISION                             :: CU
    INTEGER                                      :: I
    INTEGER                                      :: ICNT
    INTEGER                                      :: IFOUND
    INTEGER                                      :: IMODEL
    CHARACTER(LEN=MAX_STRING_LEN)                :: INNAM = ' '
    INTEGER                                      :: IPLOT
    INTEGER, ALLOCATABLE, DIMENSION(:,:)         :: IPREDNUM
    INTEGER                                      :: IUYP
    INTEGER                                      :: J
    INTEGER                                      :: JJ
    INTEGER                                      :: K
    INTEGER                                      :: LINES
    INTEGER                                      :: NCNT
    DOUBLE PRECISION                             :: PR
    CHARACTER(LEN=20)                            :: PRNAME
    DOUBLE PRECISION                             :: PV
    ! Formats
    1 FORMAT(A)
    2 FORMAT(1X,'WARNING, PLOTSYMBOL= ',I6,' for prediction: ',A,' model: ', &
             A,/,5X,' differs from symbol for that prediction in previous ', &
             'models. The last symbol read will be used.')
    3 FORMAT(1X,A1)
    10 FORMAT(//,1X,80('!'),/,1X,' _linp file not found for model: ',A,/, &
                 1X,' PREDICTIONS WILL NOT BE AVERAGED',/, &
                 1X,' for model-averaged predictions to be valid,',/, &
                 1X,' predictions must be available for all models included ', &
                 'in the weighting',/,1X,80('!'),//)
    20 FORMAT(//,1X,80('!'),/,1X,' THE FOLLOWING PREDICTION NAMES WERE ', &
              'LISTED IN THE PREDS INPUT BLOCK',/,1X,' BUT WERE NOT FOUND ', &
              'IN THE MODEL FILES FOR MODEL: ',/,1X,A,/,1X,' Predictions ', &
              'will not be averaged',/,1X,' Please check and correct the ', &
              'input',/,1X,80('!'),//)
    30 FORMAT(1X,A)
    !
    ALLOCATE(IPREDNUM(IPRED,2))
    !
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_PREDYPD'
    IFAIL = 0
    ICNT = 0
    IPREDNUM = 0
    LINES = 0
    NCNT = 0
    CHECK = ' '
    GETPREDS: DO IMODEL=1,NMPTHS
      ! only gather prediction information if the model converged with
      ! reasonable values
      IF(STATS(IMODEL,4) < 1.E+29) THEN
        NCNT = NCNT +1
        ! Initialize
        ICNT = 0
        LINES = 0
        CHECK = ' '
        INNAM = TRIM(NAMEPATH(IMODEL))//'._linp'
        INQUIRE(FILE=TRIM(INNAM),EXIST=YPFILE)
        IF(YPFILE) THEN
          ! Open _linp file
          CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUYP)
          IF(IVERB>4) WRITE(IUMSG,*)' Reading: ',TRIM(INNAM)
          ! Read two header lines
          READ(IUYP,*,END=900)CHCK
          IF(IVERB>4) WRITE(IUMSG,*)' Reading unit: ',IUYP,TRIM(CHCK)
          READ(IUYP,*,END=900)CHCK
          IF(IVERB>4) WRITE(IUMSG,*)' Reading unit: ',IUYP,TRIM(CHCK)
          CHCK = ' '
          ! Count # preds in the model, there could be many more than the
          ! user wants to average
          ! There are 6 repreated sets of preds,
          ! each starting with a line that begins with ' "95%
          ! and with a second line that starts with  ' "PRED
          DO WHILE(CHCK .NE. '"')
            ! read next line to see if it is the next header
            READ(IUYP,3,END=900)CHCK
            IF(IVERB>4) WRITE(IUMSG,*)' Reading1 unit: ',IUYP,TRIM(CHCK)
            IF(CHCK .EQ. '"') THEN
              READ(IUYP,3,END=900)CHCK
              IF(IVERB>4) WRITE(IUMSG,*)' Reading1 unit: ',IUYP,TRIM(CHCK)
              CHCK = ' '
              EXIT
            ENDIF
            ! if not backspace and read the values
            BACKSPACE(IUYP)
            ! count preds in this model
            LINES = LINES+1
            READ(IUYP,*,END=900)PRNAME,PR,CL,CU,PV,IPLOT
            IF(IVERB>4) WRITE(IUMSG,*)' Reading1 unit: ',IUYP,TRIM(PRNAME), &
                                      PR,CL,CU,PV,IPLOT
            CALL UTL_CASE(TRIM(PRNAME),PRNAME,-1)
            ! check whether this lower case name matches one of the names
            ! in the pred block of the mma input, in lower case
            NAMEMATCH: DO K=1,IPRED
              IF(TRIM(PRNAME) .EQ. TRIM(PREDNAMLC(K))) THEN
                ! count the # of matches found
                ICNT = ICNT+1
                ! identify this pred number as one to be used
                ! IPREDNUM(1,K:1,2)-listed in order of occurence for this model
                ! the first item is the pred# for this model,
                ! the second is the pred# as listed in the mma input block
                IPREDNUM(ICNT,1)=LINES
                IPREDNUM(ICNT,2)=K
                ! if the plot symbol differs between models, warn the user
                ! in case this indicates a problem to them
                IF(PREDPLOT(K) .NE. IPLOT .AND. NCNT > 1) &
                  WRITE(IUMSG,2)IPLOT,TRIM(PREDNAM(K)),NAMEMODEL(IMODEL)
                PREDPLOT(K) = IPLOT
                ! put the prediction and the variance for the individual
                ! confidence interval in the proper arrayy for averaging later
                ! note upper and lower confidence intervals are read
                ! and variance is calculated
                PRD(NCNT,K) = PR
                PVARCIIND(NCNT,K) = ((CU-CL)/4.D0)**2
                EXIT NAMEMATCH
              ENDIF
            ENDDO NAMEMATCH
            ! if all requested preds in the pred block have been found read
            ! the rest just to count the number of preds for the model
            IF(ICNT .EQ. IPRED) THEN
              DO WHILE(CHCK .NE. '"')
                READ(IUYP,3,END=900)CHCK
                IF(IVERB>4) WRITE(IUMSG,*)' Reading2 unit: ',IUYP,TRIM(CHCK)
                IF(CHCK .EQ. '"') THEN
                  READ(IUYP,3,END=900)CHCK
                  IF(IVERB>4) WRITE(IUMSG,*)' Reading2 unit: ',IUYP,TRIM(CHCK)
                  EXIT
                ENDIF
                LINES = LINES+1
              ENDDO
            ENDIF
          ENDDO
          ! if the preds for this model did not include all the preds listed
          ! in the mma pred input block then note which ones are missing
          ! and do not do the pred averaging
          IF(ICNT .NE. IPRED) THEN
            AVGPREDL = .FALSE.
            WRITE(*,20)TRIM(NAMEPATH(IMODEL))
            WRITE(IUMSG,20)TRIM(NAMEPATH(IMODEL))
            DO J=1,IPRED
              IFOUND = 0
              DO JJ=1,ICNT
                IF(J .EQ. IPREDNUM(JJ,2)) THEN
                  IFOUND = 1
                  EXIT
                ENDIF
              ENDDO
              IF(IFOUND .EQ. 0) THEN
                WRITE(*,30) TRIM(PREDNAM(J))
                WRITE(IUMSG,30) TRIM(PREDNAM(J))
              ENDIF
            ENDDO
            CYCLE GETPREDS
          ENDIF
          ! if all requested preds were found the codes gets here and
          ! the variances are determined for those preds
          ! for simultaneous and infinite confidence intervals
          ! and then for individual, limited and infinite prediction intervals
          ICNT = 1
          ! this loop is for limited confidence interval variance
          DO I=1,LINES
            READ(IUYP,*,END=900)PRNAME,PR,CL,CU,PV,IPLOT
            IF(IVERB>4) WRITE(IUMSG,*)' Reading2 unit: ',IUYP,TRIM(PRNAME), &
                                      PR,CL,CU,PV,IPLOT
            IF(ICNT <= IPRED .AND. IPREDNUM(ICNT,1) .EQ. I) THEN
              PVARCISIM(NCNT,IPREDNUM(ICNT,2)) = ((CU-CL)/4.D0)**2
              ICNT = ICNT + 1
            ENDIF
          ENDDO
          READ(IUYP,*,END=900)CHECK
          IF(IVERB>4) WRITE(IUMSG,*)' Reading3 unit: ',IUYP,TRIM(CHECK)
          READ(IUYP,*,END=900)CHECK
          IF(IVERB>4) WRITE(IUMSG,*)' Reading3 unit: ',IUYP,TRIM(CHECK)
          ICNT = 1
          ! this loop is for infinite confidence interval variance
          DO I=1,LINES
            READ(IUYP,*,END=900)PRNAME,PR,CL,CU,PV,IPLOT
            IF(IVERB>4) WRITE(IUMSG,*)' Reading3 unit: ',IUYP,TRIM(PRNAME), &
                                      PR,CL,CU,PV,IPLOT
            IF(ICNT <= IPRED .AND. IPREDNUM(ICNT,1) .EQ. I) THEN
              PVARCIINF(NCNT,IPREDNUM(ICNT,2)) = ((CU-CL)/4.D0)**2
              ICNT = ICNT + 1
            ENDIF
          ENDDO
          READ(IUYP,*,END=900)CHECK
          IF(IVERB>4) WRITE(IUMSG,*)' Reading4 unit: ',IUYP,TRIM(CHECK)
          READ(IUYP,*,END=900)CHECK
          IF(IVERB>4) WRITE(IUMSG,*)' Reading4 unit: ',IUYP,TRIM(CHECK)
          ICNT = 1
          ! this loop is for individual prediction interval variance
          DO I=1,LINES
            READ(IUYP,*,END=900)PRNAME,PR,CL,CU,PV,IPLOT
            IF(IVERB>4) WRITE(IUMSG,*)' Reading4 unit: ',IUYP,TRIM(PRNAME), &
                                      PR,CL,CU,PV,IPLOT
            IF(ICNT <= IPRED .AND. IPREDNUM(ICNT,1) .EQ. I) THEN
              PVARPIIND(NCNT,IPREDNUM(ICNT,2)) = ((CU-CL)/4.D0)**2
              ICNT = ICNT + 1
            ENDIF
          ENDDO
          READ(IUYP,*,END=900)CHECK
          IF(IVERB>4) WRITE(IUMSG,*)' Reading5 unit: ',IUYP,TRIM(CHECK)
          READ(IUYP,*,END=900)CHECK
          IF(IVERB>4) WRITE(IUMSG,*)' Reading5 unit: ',IUYP,TRIM(CHECK)
          ICNT = 1
          ! this loop is for limited prediction interval variance
          DO I=1,LINES
            READ(IUYP,*,END=900)PRNAME,PR,CL,CU,PV,IPLOT
            IF(IVERB>4) WRITE(IUMSG,*)' Reading5 unit: ',IUYP,TRIM(PRNAME), &
                                      PR,CL,CU,PV,IPLOT
            IF(ICNT <= IPRED .AND. IPREDNUM(ICNT,1) .EQ. I) THEN
              PVARPISIM(NCNT,IPREDNUM(ICNT,2)) = ((CU-CL)/4.D0)**2
              ICNT = ICNT + 1
            ENDIF
          ENDDO
          READ(IUYP,*,END=900)CHECK
          IF(IVERB>4) WRITE(IUMSG,*)' Reading6 unit: ',IUYP,TRIM(CHECK)
          READ(IUYP,*,END=900)CHECK
          IF(IVERB>4) WRITE(IUMSG,*)' Reading6 unit: ',IUYP,TRIM(CHECK)
          ICNT = 1
          ! this loop is for infinite prediction interval variance
          DO I=1,LINES
            READ(IUYP,*,END=900)PRNAME,PR,CL,CU,PV,IPLOT
            IF(IVERB>4) WRITE(IUMSG,*)' Reading6 unit: ',IUYP,TRIM(PRNAME), &
                                      PR,CL,CU,PV,IPLOT
            IF(ICNT <= IPRED .AND. IPREDNUM(ICNT,1) .EQ. I) THEN
              PVARPIINF(NCNT,IPREDNUM(ICNT,2)) = ((CU-CL)/4.D0)**2
              ICNT = ICNT + 1
            ENDIF
          ENDDO
          CLOSE(IUYP)
        ELSE
          ! _linp file not found, model-averaged predictions not valid
          WRITE(*,10)INNAM
          WRITE(IUMSG,10)INNAM
          AVGPREDL = .FALSE.
        ENDIF
      ENDIF
    ENDDO GETPREDS
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    DEALLOCATE(IPREDNUM)
    RETURN
    ! EOF WAS REACHED
    900 CALL MMA_EOF(IFAIL,INNAM)
    DEALLOCATE(IPREDNUM)
    RETURN
  END SUBROUTINE MMA_EVA_PREDYPD
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EVA_PC(IFAIL,AVGPARAML)
    ! This subroutine:
    ! Reads parameters and values from the _pc file and sets flags
    ! for AVGPARAML - whether there is any averaging for any group
    ! for AVGPARAMLGP - whether there is any averaging for each group
    ! for AVGPARAMLGPI - whether there is averaging for param i of a group
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,                       INTENT(INOUT) :: IFAIL
    LOGICAL,                       INTENT(INOUT) :: AVGPARAML
    ! Local variables
    CHARACTER(LEN=200)                           :: CHECK = ' '
    DOUBLE PRECISION                             :: DUMMY
    INTEGER                                      :: I
    INTEGER                                      :: IMODEL
    INTEGER                                      :: J
    INTEGER                                      :: K
    INTEGER                                      :: KK
    CHARACTER(LEN=MAX_STRING_LEN)                :: INNAM = ' '
    INTEGER                                      :: IUMODEL
    INTEGER                                      :: NPARS
    DOUBLE PRECISION                             :: PUPPER
    ! Formats
    10 FORMAT(/,1X,' CHECK THAT PARAMETERS TO BE AVERAGED EXIST FOR ALL ', &
              'MODELS IN THE GROUP',/)
    20 FORMAT(/,1X,' PARAMETERS WILL NOT BE AVERAGED FOR GROUP: ',A12,/,1X, &
               ' BECAUSE THE SPECIFIED PARAMETERS DO NOT EXIST FOR MODEL: ', &
               A,//)
    ! ALLOCATE
    ALLOCATE(ILOGARRAYORIG(NMPTHS,MAXNPE),PARARRAYORIG(NMPTHS,MAXNPE), &
             PNARRAYORIG(NMPTHS,MAXNPE),PVARRAYORIG(NMPTHS,MAXNPE), &
             PVARRAYLOGORIG(NMPTHS,MAXNPE))
    ALLOCATE(ILOGARRAY(NMPTHS,MAXNPE),PARARRAY(NMPTHS,MAXNPE), &
             PNARRAY(NMPTHS,MAXNPE),PVARRAY(NMPTHS,MAXNPE), &
             PVARRAYLOG(NMPTHS,MAXNPE))
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_EVA_PC'
    IFAIL = 0
    PARARRAY = 0.D0
    PNARRAY = ' '
    PVARRAY = 0.D0
    PVARRAYLOG = 0.D0
    ILOGARRAY = 0
    WRITE(IUMSG,'(A)')HYPHENS(1:80)
    WRITE(IUMSG,10)
    ! CYCLE THROUGH MODELS
    DO IMODEL=1,NMPTHS
      ! If there is no model averaging for this group, skip it
      IF(.NOT. AVGPARAMLGP(NAMEPATHGPN(IMODEL))) CYCLE
      NPARS = INT(STATS(IMODEL,1))
      ! OPEN _PC TO READ OPTIMAL PARAMETERS
      INNAM = TRIM(NAMEPATH(IMODEL))//'._pc'
      CALL MMA_OPEN(IFAIL,INNAM,'OLD    ','READ ',IUMODEL)
      ! Read Optimal Parameters
      READ(IUMODEL,*,END=900)CHECK
      IF(IVERB>4) THEN
        WRITE(IUMSG,*)
        WRITE(IUMSG,*)' Reading unit: ',IUMODEL,TRIM(INNAM)
        WRITE(IUMSG,*)TRIM(CHECK)
      ENDIF
      DO I=1,NPARS
        READ(IUMODEL,*,END=900)PNARRAYORIG(IMODEL,I),PARARRAYORIG(IMODEL,I), &
                    DUMMY,PUPPER,ILOGARRAYORIG(IMODEL,I)
        IF(IVERB>4) WRITE(IUMSG,*)' Reading unit: ',IUMODEL, &
                    TRIM(PNARRAYORIG(IMODEL,I)),PARARRAYORIG(IMODEL,I),DUMMY, &
                    PUPPER,ILOGARRAYORIG(IMODEL,I)
        ! CALC Variance from limits
        PVARRAYORIG(IMODEL,I) = ((PUPPER-PARARRAYORIG(IMODEL,I))/1.96D0)**2
        IF(ILOGARRAYORIG(IMODEL,I) > 0)PVARRAYLOGORIG(IMODEL,I) = &
                    ((LOG10(PUPPER)-LOG10(PARARRAYORIG(IMODEL,I)))/1.96D0)**2
      ENDDO
      CLOSE (IUMODEL)
      IF(IVERB>4) WRITE(IUMSG,*)' Closing unit: ',IUMODEL,TRIM(INNAM)
      I = NAMEPATHGPN(IMODEL)
      ! Do for the number of parameters to be averaged for this group of models
      DO J=1,MAXAVG(I)
        AVGPARAMLGPI(I,J) = .FALSE.
        ! Search names of parameters associated with this model
        ! If the parameter indicated for averaging in this group exists
        ! for this model set flag to model-average it true. Othewise leave
        ! it false and write a message
        DO K=1,NPARS
          DO KK=1,NPARS
            IF(TRIM(AVGARRAY(I,J)) .EQ. TRIM(PNARRAYORIG(IMODEL,KK))) THEN
              PNARRAY(IMODEL,J) = PNARRAYORIG(IMODEL,KK)
              PARARRAY(IMODEL,J) = PARARRAYORIG(IMODEL,KK)
              ILOGARRAY(IMODEL,J) = ILOGARRAYORIG(IMODEL,KK)
              PVARRAY(IMODEL,J) = PVARRAYORIG(IMODEL,KK)
              PVARRAYLOG(IMODEL,J) = PVARRAYLOGORIG(IMODEL,KK)
              AVGPARAMLGPI(I,J) = .TRUE.
              EXIT
            ENDIF
            IF(KK .EQ. NPARS) THEN
              ! TERMINATE?
              WRITE(IUMSG,20)GROUPS(NAMEPATHGPN(IMODEL)),NAMEPATH(IMODEL)
              WRITE(*,*)
              WRITE(*,*)' !!!! WARNING PARAMETER MISMATCH !!!'
              WRITE(*,*)' NO MATCH FOR: ',TRIM(AVGARRAY(I,J))
              WRITE(*,*)' IN MODEL: ',TRIM(NAMEMODEL(IMODEL))
              WRITE(*,*)' PATH: ',TRIM(NAMEPATH(IMODEL))
              WRITE(IUMSG,*)' !!!! WARNING PARAMETER MISMATCH !!!'
              WRITE(IUMSG,*)' NO MATCH FOR: ',TRIM(AVGARRAY(I,J))
              WRITE(IUMSG,*)' IN MODEL: ',TRIM(NAMEMODEL(IMODEL))
              WRITE(IUMSG,*)' PATH: ',TRIM(NAMEPATH(IMODEL))
              WRITE(*,*)
              WRITE(*,*)' !!!! MMA TERMINATING !!!'
              WRITE(*,*)
              WRITE(IUMSG,*)
              WRITE(IUMSG,*)' !!!! MMA TERMINATING !!!'
              WRITE(IUMSG,*)
              CALL UTL_STOP(' ')
            ENDIF
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    ! CHECK GROUPS, IF NO PARAMS TO AVG AVGPARAML reset to FALSE
    AVGPARAML = .FALSE.
    AVGPARAMLGP = .FALSE.
    DO I=1,KMGP
      DO J=1,MAXAVG(I)
        IF(AVGPARAMLGPI(I,J)) THEN
          AVGPARAML = .TRUE.
          AVGPARAMLGP(I) = .TRUE.
          ! REVISE COUNT OF PARAMETERS TO BE AVERAGED BY GROUP
          DO K=J+1,MAXAVG(I)
            IF(AVGPARAMLGPI(I,J)) THEN
              MAXAVG(I) = K
            ELSE
              EXIT
            ENDIF
          ENDDO
          EXIT
        ENDIF
      ENDDO
    ENDDO
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    RETURN
    ! EOF WAS REACHED
    900 CALL MMA_EOF(IFAIL,INNAM)
    RETURN
  END SUBROUTINE MMA_EVA_PC
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_ANL_ALL(IFAIL,AVGPARAML,AVGPREDL)
    ! This subroutine reads the desired analyses then sets up and calls
    ! MMA_ANL_CRIT
    ! for calculation of model criterion for each analysis
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,                       INTENT(INOUT)   :: IFAIL
    LOGICAL,                       INTENT(IN)      :: AVGPARAML
    LOGICAL,                       INTENT(IN)      :: AVGPREDL
    ! Local variables
    TYPE (LLIST), POINTER                          :: ANHEAD ! Pointer to head of list (analyses)
    CHARACTER(LEN=2000), ALLOCATABLE, DIMENSION(:) :: CRITEQN
    INTEGER                                        :: I
    INTEGER                                        :: ICOLUMN
    INTEGER                                        :: IMODEL
    INTEGER                                        :: IERR = 0
    INTEGER                                        :: J
    INTEGER                                        :: KANAL
    INTEGER                                        :: MORE
    CHARACTER(LEN=MAX_STRING_LEN)                  :: ONAM
    DOUBLE PRECISION                               :: SUMPRIMODWT
    TYPE (LLIST), POINTER                          :: TAIL
    CHARACTER(LEN=12)                              :: TEMP
    CHARACTER(LEN=2000), ALLOCATABLE, DIMENSION(:) :: PREQN
    ! Formats
    10 FORMAT(/,' Analysis: ',A,' will be written to:  "',A,'"'/)
    15 FORMAT(/,'********************************************************', &
                '************************',/,'***************************', &
                '*****************************************************',/)
    20 FORMAT(5X,'Prior information equations are absent or ', &
              /,7X,'prior information names differ between models. ', &
              //,5X,'Default analyses will be conducted based on ', &
              /,7X,'Observations Only.')
    100 FORMAT(/,1X,A)
    200 FORMAT &
        (/,' NAMES OF ANALYZED MODELS  NORMALIZED PRIOR MODEL PROBABILITY',/, &
           ' ------------------------  -----------------------------------')
    201 FORMAT(13X,A12,15X,F6.4)
    601 FORMAT(' "ANALYSIS NAME:" "',A,'" "Criterion Equation:" "',A, &
               '" "Probability Equation:" "',A,'"')
    602 FORMAT(/,' Thoroughly check input. Consider if you may have confused', &
               /,' the Criterion and Probability Equation input: ',//, &
               ' ANALYSIS NAME: ',A,/,' Criterion Equation: ',A,/, &
               ' Probability Equation: ',A)
    ! READ AND RUN ANALYSES
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_ANL_ALL'
    IFAIL = 0
    TEMP = ' '
    ONAM = ' '
    I = 0
    SUMPRIMODWT = 0.D0
    !
    NULLIFY(ANHEAD)
    NULLIFY(TAIL)
    ! Allocate arrays for that # of accepted models
    ALLOCATE(STATSHORT(CONVREAS,NCOL-2),PRIMODWTSHORT(CONVREAS), &
       NAMEPATHGPNSHORT(CONVREAS),NAMELSHORT(CONVREAS), &
       NAMEPATHSHORT(CONVREAS),NAMEMODELSHORT(CONVREAS))
    ! If there will be model averaging,
    ! Allocate arrays
    IF(AVGPARAML) THEN
      ALLOCATE(ILOGARRAYSHORT(CONVREAS,MAXNPE), &
      PNARRAYSHORT(CONVREAS,MAXNPE), &
      PARARRAYNATIVE(CONVREAS,MAXNPE),PVARRAYNATIVE(CONVREAS,MAXNPE),  &
      PARARRAYSHORT(CONVREAS,MAXNPE),PVARRAYSHORT(CONVREAS,MAXNPE))
      PNARRAYSHORT = ' '
      PARARRAYSHORT = 1.D+30
    ENDIF
    ! Separate Converged models with reasonable parameters
    DO IMODEL=1,NMPTHS
      ! find converged models with reasonable parameters
      IF(STATS(IMODEL,4) < 1.D+29) THEN
        I = I + 1
        NAMEPATHGPNSHORT(I) = NAMEPATHGPN(IMODEL)
        NAMEMODELSHORT(I)   = NAMEMODEL(IMODEL)
        NAMEPATHSHORT(I)    = NAMEPATH(IMODEL)
        PRIMODWTSHORT(I)    = PRIMODWT(IMODEL)
        SUMPRIMODWT = SUMPRIMODWT + PRIMODWT(IMODEL)
        ! If there will be model averaging, populate arrays
        DO J=1,MAXNPE
          IF(AVGPARAML) THEN
            ILOGARRAYSHORT(I,J) = ILOGARRAY(IMODEL,J)
            PNARRAYSHORT(I,J)   = PNARRAY(IMODEL,J)
            PARARRAYNATIVE(I,J)  = PARARRAY(IMODEL,J)
            PVARRAYNATIVE(I,J)   = PVARRAY(IMODEL,J)
            IF(ILOGARRAYSHORT(I,J) > 0) THEN
              PARARRAYSHORT(I,J) = LOG10(PARARRAY(IMODEL,J))
              PVARRAYSHORT(I,J) = PVARRAYLOG(IMODEL,J)
            ELSE
              PARARRAYSHORT(I,J)  = PARARRAY(IMODEL,J)
              PVARRAYSHORT(I,J)   = PVARRAY(IMODEL,J)
            ENDIF
          ENDIF
        ENDDO
        ! short list accepted model names
        NAMELSHORT(I) = NAMEL(IMODEL)
        NAMEPATHSHORT(I)    = NAMEPATH(IMODEL)
        ! short list stats columns for accepted models
        DO ICOLUMN=1,NCOL-2
          STATSHORT(I,ICOLUMN) = STATS(IMODEL,ICOLUMN)
        ENDDO
      ENDIF
    ENDDO
    ! RESCALE PRIOR BASED ON ACCEPTED MODELS
    IF(SUMPRIMODWT <= 0.D0) THEN
      IFAIL = 1
      AMESSAGE = ' TERMINATING: [Sum of the Prior Weights on Models] <= 0'
      CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
      CALL TYP_DEALLOC(ANHEAD)
      RETURN
    ENDIF
    DO IMODEL=1,I
      PRIMODWTSHORT(IMODEL) = PRIMODWTSHORT(IMODEL)/SUMPRIMODWT
    ENDDO
    WRITE(IUMSG,200)
    DO IMODEL=1,I
      WRITE(IUMSG,201) TRIM(NAMEMODELSHORT(IMODEL)),PRIMODWTSHORT(IMODEL)
    ENDDO
    IF(AVGPARAML) DEALLOCATE(ILOGARRAY,PARARRAY,PNARRAY,PVARRAY)
    !
    ! Find next data block
    !   ANALYSES block
    KANAL = 0
    CALL UTL_READBLOCK(3,'ANALYSES',ANALCOL,IUMPTHS,IUMSG,   &
        'ANALYSISLABEL',.FALSE.,ANHEAD,TAIL,KANAL)
    IF (IVERB>2) THEN
      !   Write block information (model groups) to output file
      WRITE(IUMSG,'(A)')HYPHENS(1:80)
      WRITE(IUMSG,100)'Echo analysis input:'
      CALL UTL_WRITEBLOCK(ANHEAD,IUMSG)
    ENDIF
    ! Default Analysis
    IF (KANAL < 1) THEN
      ALLOCATE(ANALYSIS(5),CRITEQN(5),PREQN(5))
    ELSE
      ALLOCATE(ANALYSIS(KANAL),CRITEQN(KANAL),PREQN(KANAL))
    ENDIF
    ANALYSIS = "AICwPri"
    CRITEQN = "AICwPri"
    PREQN = "exp(-0.5D0*(valcrit-mincrit))*priormodprob"
    ! Filter Analysis data
    IERR = 0
    CALL UTL_FILTERLIST(ANHEAD,IUMSG,'ANALYSISLABEL',KANAL,IERR,ANALYSIS,MORE)
    IF(IERR>0)CALL UTL_STOP &
      (' Input error ANALYSIS Block ANALYSISLABEL. See message in .#mout file')
    CALL UTL_FILTERLIST(ANHEAD,IUMSG,'CRITEQN',KANAL,IERR,CRITEQN,MORE)
    IF(IERR>0)CALL UTL_STOP &
      (' Input error  ANALYSIS Block CRITEQN. See message in .#mout file')
    CALL UTL_FILTERLIST(ANHEAD,IUMSG,'PREQN',KANAL,IERR,PREQN,MORE)
    IF(IERR>0)CALL UTL_STOP &
      (' Input error ANALYSIS Block PREQN. See message in .#mout file')
    ! Default analysis
    IF (KANAL < 1) THEN
      KANAL = 5
      PREQN = "exp(-0.5D0*(valcrit-mincrit))*priormodprob"
      PREQN(5) = "1*priormodprob"
      IF(NMPR > 0 .AND. (PRIORSAMENUM .AND. PRIORSAME)) THEN
        ANALYSIS(1) ="AICwPri"
        CRITEQN(1) ="AICwPri"
        ANALYSIS(2) ="AICcwPri"
        CRITEQN(2) ="AICcwPri"
        ANALYSIS(3) ="BICwPri"
        CRITEQN(3) ="BICwPri"
        ANALYSIS(4) ="KICwPri"
        CRITEQN(4) ="KICwPri"
      ELSE
        WRITE(IUMSG,15)
        WRITE(IUMSG,20)
        WRITE(IUMSG,15)
        ANALYSIS(1) ="AICObs"
        CRITEQN(1) ="AICObs"
        ANALYSIS(2) ="AICcObs"
        CRITEQN(2) ="AICcObs"
        ANALYSIS(3) ="BICObs"
        CRITEQN(3) ="BICObs"
        ANALYSIS(4) ="KICObs"
        CRITEQN(4) = "KICObs"
      ENDIF
      ANALYSIS(5) ="PriModProb"
      CRITEQN(5) ="PriModProb"
    ENDIF
    !
    CALL EQN_CLN()
    ! Loop through analyses
    DO J=1,KANAL
      SKIPANALYSIS = .FALSE.
      ! INITILIZE EQUATION MODULE
      ! ANALYSIS NAME AND EQUATION
      TEMP=ANALYSIS(J)
      CALL UTL_CASE(TRIM(TEMP),TEMP,-1)
      CALL UTL_CASE(TRIM(CRITEQN(J)),CRITEQN(J),-1)
      CALL UTL_CASE(TRIM(PREQN(J)),PREQN(J),-1)
      !SETUP CRIT EQN
      CALL EQN_INI(IFAIL,1)
      IF (IFAIL.NE.0) THEN
        CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
        CALL UTL_STOP('EQN_INI reported failure')
      ENDIF
      CALL EQN_INI_INSTALL(IFAIL,1,TEMP,CRITEQN(J))
      IF (IFAIL.NE.0) THEN
        CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
        CALL UTL_STOP('EQN_INI_INSTALL reported failure')
      ENDIF
      CALL MMA_INI_PRIORCHECK(IFAIL)
      IF(PRIORMEASUSED) THEN
        IF(.NOT. PRIORSAMENUM .OR. .NOT. PRIORSAME) THEN
          ONAM = TRIM(OUTNAM)//'._anals'//'_'//TRIM(ANALYSIS(J))
          CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUANAL)
          AMESSAGE = ' TERMINATING: Analysis requires prior and'
          CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','no')
          CALL UTL_WRITE_MESSAGE(IUANAL,'yes','yes','no')
          IF(.NOT. PRIORSAMENUM) &
            AMESSAGE = '   models analyzed do not have the same number of prior'
          IF(.NOT. PRIORSAME) &
          AMESSAGE = '   models analyzed do not have the same prior names'
          CALL UTL_WRITE_MESSAGE(IUMSG,'no','no','yes')
          CALL UTL_WRITE_MESSAGE(IUANAL,'yes','yes','no')
          CLOSE(IUANAL)
          SKIPANALYSIS = .TRUE.
        ENDIF
      ENDIF
      PRIORMEASUSED = .FALSE.
      ! OPEN OUTPUT FILE FOR ANALYSIS
      ONAM = TRIM(OUTNAM)//'._anals'//'_'//TRIM(ANALYSIS(J))
      CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUANAL)
      WRITE(*,*)
      WRITE(*,'(A)')HYPHENS(1:80)
      WRITE(*,'(A)')HYPHENS(1:80)
      WRITE(*,10)ANALYSIS(J),TRIM(ONAM)
      WRITE(IUMSG,*)
      WRITE(IUMSG,'(A)')HYPHENS(1:80)
      WRITE(IUMSG,'(A)')HYPHENS(1:80)
      WRITE(IUMSG,10)ANALYSIS(J),TRIM(ONAM)
      WRITE(IUANAL,601)TRIM(ANALYSIS(J)),TRIM(CRITEQN(J)),TRIM(PREQN(J))
      ! ANALYZE CRITERION AND MODEL-AVERAGE PARAMETERS, AND PREDICTIONS
      CALL MMA_ANL_CRIT(IFAIL,AVGPARAML,AVGPREDL,J,KANAL, &
                                          CONVREAS,TEMP,PREQN)
      ! CLOSE ANALYSIS FILE
      CLOSE(IUANAL)
      IF(IFAIL > 0) THEN
        WRITE(IUMSG,602)TRIM(ANALYSIS(J)),TRIM(CRITEQN(J)),TRIM(PREQN(J))
        CALL UTL_STOP('EQN_EVALUATE reported failure')
      ENDIF
    ENDDO
    ! Analyses are done
    IF(IFAIL > 0) THEN
      CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    ENDIF
    IF(ALLOCATED(CRITEQN))DEALLOCATE(CRITEQN)
    IF(ALLOCATED(PREQN))DEALLOCATE(PREQN)
    CALL TYP_DEALLOC(ANHEAD)
    RETURN
  END SUBROUTINE MMA_ANL_ALL
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_ANL_CRIT(IFAIL,AVGPARAML,AVGPREDL,J,KANAL, &
                                            NMODELS,TEMP,PREQN)
    ! This subroutine evaluates the criterion defined by the
    ! analysis criterion equation and calls subroutines:
    ! MMA_ANL_MRW MMA_EVA_ORDERSTATS MMA_ANL_PREDAVG MMA_ANL_PC
    ! to
    ! DetermineWeights DetermineOrder AveragePreds AverageParams
    ! as appropriate given requests in the input file
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,            INTENT(INOUT)     :: IFAIL
    LOGICAL,            INTENT(IN)        :: AVGPARAML
    LOGICAL,            INTENT(IN)        :: AVGPREDL
    INTEGER,            INTENT(IN)        :: J
    INTEGER,            INTENT(IN)        :: KANAL
    INTEGER,            INTENT(IN)        :: NMODELS
    CHARACTER(LEN=12),  INTENT(IN)        :: TEMP
    CHARACTER(LEN=2000), DIMENSION(KANAL) :: PREQN
    ! Internal variables
    DOUBLE PRECISION                  :: ANAL(NMODELS)
    DOUBLE PRECISION                  :: DELTA(NMPTHS)
    DOUBLE PRECISION                  :: EVIDRATIO
    DOUBLE PRECISION                  :: EVIDRATIOINV
    INTEGER                           :: II
    INTEGER                           :: IMODEL
    INTEGER                           :: ITYP
    INTEGER                           :: IUMODNAMEPATH
    INTEGER                           :: LOWJ
    LOGICAL                           :: LVAL
    DOUBLE PRECISION                  :: MEASURES(NCOL-2)
    INTEGER                           :: NCOLM2
    CHARACTER(LEN=MAX_STRING_LEN)     :: ONAM = ' '
    ! Formats
    10 FORMAT(6X,'"MODEL"',6X,'"PRIOR PROB"',5X,'"CRITERION"',2X, &
              '"RANK"',3X,'"PROBABILTY"',5X,'"DELTA"',2X,'"EVIDENCE-RATIO"', &
              1X,'"ER-INVERSE as %"', &
              1X,'"PATHANDROOT"')
    20 FORMAT(/,5X,'ERROR! Analysis not undertaken because at least ', &
              /,7X,'one of the selected measures in CRITEQN is dependent ',&
              /,7X,'on evaluated models all having the same prior', &
              /,7X,'information and the models evaluated either have', &
              /,7X,'different numbers of prior information equations ', &
              /,7X,'or different names of prior information equations.',/, &
              /,5X,'To proceed with this analysis, either:', &
              /,7X,'use measures in CRITEQN that do not consider prior,', &
              /,7X,'change the models being evaluated, or', &
              /,7X,'include the same prior in all the evaluated models.')
    311 FORMAT(1X,A12,5X,A)
    312 FORMAT(/,'  A list of Model Names and Associated Paths for Analyzed ', &
                 'Models ',/,'   will be written to unit: ',I5,/, &
                 '     File: ',A)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_ANL_CRIT'
    IFAIL = 0
    ORDER = 0
    ANAL = 1D+30
    NCOLM2 = NCOL - 2
    ! DETERMINE THE CRITERION FOR EACH MODEL
    DO IMODEL = 1,NMODELS
      ! One-D array of measure just for this model
      DO II = 1,NCOLM2
        MEASURES(II) = STATSHORT(IMODEL,II)
      ENDDO
      ANAL(IMODEL) = 0
      ! Get criterion for this model
      CALL EQN_EVALUATE(IFAIL,1,NCOLM2,COLHDNAME,MEASURES,ITYP, &
                        ANAL(IMODEL),LVAL)
      IF (IFAIL.NE.0) THEN
        CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
        RETURN
      ENDIF
    ENDDO
    CALL EQN_CLN
    IF (ALLOCATED(WEIGHT)) DEALLOCATE(WEIGHT)
    ALLOCATE (WEIGHT(NMODELS))
    !SETUP MODEL WEIGHTING EQN
    CALL EQN_INI(IFAIL,1)
    IF (IFAIL.NE.0) THEN
      CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
      CALL UTL_STOP('EQN_INI reported failure')
    ENDIF
    CALL EQN_INI_INSTALL(IFAIL,1,TEMP,PREQN(J))
    IF (IFAIL.NE.0) THEN
      CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
      CALL UTL_STOP('EQN_INI_INSTALL reported failure')
    ENDIF
    ! RANK THE MODELS BASED ON THE CRITERION
    CALL MMA_EVA_ORDERSTATS(IFAIL,NMODELS,ANAL,DELTA,LOWJ)
    CALL MMA_ANL_MRW(IFAIL,NMODELS,ANAL)
    ! WRITE THE WEIGHTED ANALYZED RANKED MODELS IN ORDER OF RANK 1 IS BEST
    IF(.NOT. SKIPANALYSIS) THEN
      WRITE(IUANAL,10)
      DO IMODEL = 1,NMODELS
        EVIDRATIO = WEIGHT(ORDER(1))/WEIGHT(ORDER(IMODEL))
        EVIDRATIOINV = (1.D0/EVIDRATIO)*100.
        WRITE(IUANAL,1)TRIM(NAMEMODELSHORT(ORDER(IMODEL))), &
                       PRIMODWTSHORT(ORDER(IMODEL)),ANAL(ORDER(IMODEL)), &
                       IMODEL,WEIGHT(ORDER(IMODEL)), &
                       DELTA(ORDER(IMODEL)),EVIDRATIO,EVIDRATIOINV, &
                       TRIM(NAMEPATHSHORT(ORDER(IMODEL)))
        1 FORMAT(1X,A12,3X,1PE15.7,1X,1PE15.7,1X,I7,1X,4(1PE15.7),4X,A)
      ENDDO
      ! IF PREDICTIONS ARE AVAILABLE,
      ! CALCULATE MODEL-AVERAGED PREDICTIONS AND VARIANCES
      IF(AVGPREDL) CALL MMA_ANL_PREDAVG(IFAIL,J,NMODELS)
!      DEALLOCATE(WEIGHT)
      ! USE CRITERION FROM ANALYSES TO MODEL-AVERAGE PARAMETERS
      IF(AVGPARAML)CALL MMA_ANL_PC(IFAIL,NMODELS,ANAL,J)
    ELSE
      WRITE(IUANAL,20)
      WRITE(IUMSG,20)
    ENDIF
    CALL EQN_CLN
    ! WRITE A FILE LISTING MODEL NAMES AND THEIR ASSOCIATED PATH
    ONAM = TRIM(OUTNAM)//'._ModelNamesPaths'
    CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUMODNAMEPATH)
    IF(J .EQ. KANAL) THEN
      WRITE(*,'(A)')HYPHENS(1:80)
      WRITE(*,312)IUMODNAMEPATH,TRIM(ONAM)
      WRITE(IUMSG,'(A)')HYPHENS(1:80)
      WRITE(IUMSG,312)IUMODNAMEPATH,TRIM(ONAM)
    ENDIF
    DO IMODEL=1,NMPTHS
      IF(STATS(IMODEL,4) < 0.99D+30) &
        WRITE(IUMODNAMEPATH,311)TRIM(NAMEMODEL(IMODEL)), &
                TRIM(NAMEPATH(IMODEL))
    ENDDO
    CLOSE(IUMODNAMEPATH)
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    RETURN
  END SUBROUTINE MMA_ANL_CRIT
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_ANL_PC(IFAIL,NMODELS,ANAL,L)
    ! This subroutine writes files of parameters and variances
    ! This subroutine calculates model averages parameters and their variance
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,               INTENT(INOUT)  :: IFAIL
    INTEGER,                  INTENT(IN)  :: NMODELS
    DOUBLE PRECISION,         INTENT(IN)  :: ANAL(NMODELS)
    INTEGER,                  INTENT(IN)  :: L
    ! Local variables
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:)    :: ANALGP
    INTEGER                                      :: I
    INTEGER                                      :: IP
    INTEGER,ALLOCATABLE,DIMENSION(:)             :: IPT
    INTEGER                                      :: IUPARAMREGRESS
    INTEGER                                      :: IUPARAMS
    INTEGER                                      :: J
    INTEGER                                      :: K
    INTEGER,ALLOCATABLE, DIMENSION(:)            :: KMODELS
    INTEGER                                      :: LASTLOG
    DOUBLE PRECISION                             :: MALC
    DOUBLE PRECISION                             :: MAUC
    DOUBLE PRECISION                             :: MAV
    DOUBLE PRECISION                             :: MAVAR
    INTEGER                                      :: NCNT
    INTEGER,ALLOCATABLE, DIMENSION(:)            :: NMODNPE
    CHARACTER(LEN=MAX_STRING_LEN)                :: ONAM = ' '
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:)  :: PARNATIVE
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:)  :: PARREGRESS
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:)  :: PVNATIVE
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:)  :: PVREGRESS
    DOUBLE PRECISION                             :: SQ
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:)    :: WEIGHTSHORT
    DOUBLE PRECISION                             :: WEIGHTSHORTSUM
    ! Formats
    4 FORMAT(/,'  FOR ALL ANALYSES: ')
    5 FORMAT(/,'      NATIVE Parameter values involved in averaging ', &
             /,'         will be written to unit: ',I5,/,'         File: ',A)
    6 FORMAT(/,'      NATIVE Parameter variances involved in averaging ', &
             /,'         will be written to unit: ',I5,/,'         File: ',A)
    7 FORMAT(/,'      REGRESSION SPACE Parameter values involved in ', &
               'averaging ', &
             /,'         will be written to unit: ',I5,/,'         File: ',A)
    8 FORMAT(/,'      REGRESSION SPACE variances involved in averaging ', &
             /,'         will be written to unit: ',I5,/,'         File: ',A)
    10 FORMAT(/,1X,' Model-averaged Parameters',/,'    for Analysis: ',A,/, &
               '    will be written to unit: ',I5,/,'    File: ',A,/)
    20 FORMAT('"All values are reported in native space, even if the ', &
              'parameter was log transformed for estimation"')
    97 FORMAT(1X,'NATIVE Parameter Values Averaged for Group: ',A12)
    98 FORMAT(1X,'NATIVE Parameter Variances Averaged for Group: ',A12)
    99 FORMAT(1X,'REGRESSION Parameter Values Averaged for Group: ',A12)
    100 FORMAT(1X,'REGRESSION Parameter Variances Averaged for Group: ',A12)
    101 FORMAT(1X,'MODEL',9X,2000(3X,A12))
    102 FORMAT(1X,A12,2X,2000(1PG15.7))
    103 FORMAT(1X,' THERE ARE NO CONVERGED MODELS WITH REASONABLE PARAMETER ', &
                  'VALUES FOR THIS GROUP')
    104 FORMAT(1X,' NOTHING TO AVERAGE',/,'  EITHER 0 OR 1 MODELS ', &
               'CONVERGED WITH REASONABLE PARAMETER VALUES FOR THIS GROUP')
    110 FORMAT('"GROUP: ',A,'"   "Number of models: " "',I6,'"   "',A, &
               ' MODEL-AVERAGED PARAMETERS"')
    120 FORMAT(1X,'"PARAMETER "', &
               1X,'"Model-Averaged Lower Confidence"', &
               1X,'"Model-Averaged Value"', &
               1X,'"Model-Averaged Upper Confidence"', &
               1X,'"Model-Averaged Variance"', &
               1X,'"ESTIMATION STATE"')
    130 FORMAT(1X,A12,4(1PG29.7),'      TRANSFORMED')
    140 FORMAT(1X,A12,4(1PG29.7),'      NATIVE')
    150 FORMAT(1X,'"PARAMETER: ',A,' Cannot be averaged for group: ',A, &
        '    because it is not log transformed in all models of the group. ', &
        ' Stopped on model: ',A,' Path: ',A)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_ANL_PC'
    IFAIL = 0
    ! Open file for model-average parameters for this analysis
    ONAM = TRIM(OUTNAM)//'._params_'//TRIM(ANALYSIS(L))
    CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUPARAMS)
    WRITE(*,10)TRIM(ANALYSIS(L)),IUPARAMS,TRIM(ONAM)
    WRITE(IUMSG,10)TRIM(ANALYSIS(L)),IUPARAMS,TRIM(ONAM)
    IF(L .EQ. 1) THEN
      WRITE(*,*)
      WRITE(IUMSG,*)
      ! DURING FIRST ANALYSIS:
      ! WRITE 2 FILES LISTING FOR EACH GROUP, FOR EACH PARAMETER,
      ! 1) THE PARAMETER VALUE FOR EVERY MODEL TO BE AVERAGED
      ! 2) THE PARAMETER VARIANCE FOR EVERY MODEL TO BE AVERAGED
      IF(WRTPARAMNATIVE) THEN
        ONAM = TRIM(OUTNAM)//'._IndividParamNative'
        CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUPARAMNATIVE)
        WRITE(*,5)IUPARAMNATIVE,TRIM(ONAM)
        WRITE(IUMSG,5)IUPARAMNATIVE,TRIM(ONAM)
        ONAM = TRIM(OUTNAM)//'._IndividParVarNative'
        CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUVARNATIVE)
        WRITE(*,6)IUVARNATIVE,TRIM(ONAM)
        WRITE(IUMSG,6)IUVARNATIVE,TRIM(ONAM)
      ENDIF
      IF(WRTPARAMREGRESS) THEN
        ONAM = TRIM(OUTNAM)//'._IndividParamRegress'
        CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUPARAMREGRESS)
        WRITE(*,7)IUPARAMREGRESS,TRIM(ONAM)
        WRITE(IUMSG,7)IUPARAMREGRESS,TRIM(ONAM)
        ONAM = TRIM(OUTNAM)//'._IndividParVarRegress'
        CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUVARREGRESS)
        WRITE(*,8)IUVARREGRESS,TRIM(ONAM)
        WRITE(IUMSG,8)IUVARREGRESS,TRIM(ONAM)
      ENDIF
    ENDIF
    ALLOCATE(KMODELS(KMGP), &
             PARNATIVE(NMODELS,MAXMAXAVG), &
             PVNATIVE(NMODELS,MAXMAXAVG), &
             PARREGRESS(NMODELS,MAXMAXAVG), &
             PVREGRESS(NMODELS,MAXMAXAVG))
    KMODELS = 0
    ! CYCLE THROUGH GROUPS
    GROUP: DO I=1,KMGP
      IF(L .EQ. 1) THEN
        PARNATIVE = 0.D0
        PVNATIVE = 0.D0
        PARREGRESS = 0.D0
        PVREGRESS = 0.D0
        IF(AVGARRAY(I,1) .NE. ' ') THEN
          IF(WRTPARAMNATIVE) THEN
            WRITE(IUPARAMNATIVE,'(A)')HYPHENS(1:80)
            WRITE(IUPARAMNATIVE,97)GROUPS(I)
            IF(MAXAVG(I)>0) THEN
              WRITE(IUPARAMNATIVE,101)(AVGARRAY(I,J),J=1,MAXAVG(I))
            ELSE
              WRITE(IUPARAMNATIVE,104)
            ENDIF
            WRITE(IUVARNATIVE,'(A)')HYPHENS(1:80)
            WRITE(IUVARNATIVE,98)GROUPS(I)
            IF(MAXAVG(I)>0) THEN
              WRITE(IUVARNATIVE,101)(AVGARRAY(I,J),J=1,MAXAVG(I))
            ELSE
              WRITE(IUVARNATIVE,104)
            ENDIF
          ENDIF
          IF(WRTPARAMREGRESS) THEN
            WRITE(IUPARAMREGRESS,'(A)')HYPHENS(1:80)
            WRITE(IUPARAMREGRESS,99)GROUPS(I)
            IF(MAXAVG(I)>0) THEN
              WRITE(IUPARAMREGRESS,101)(AVGARRAY(I,J),J=1,MAXAVG(I))
            ELSE
              WRITE(IUPARAMREGRESS,104)
            ENDIF
            WRITE(IUVARREGRESS,'(A)')HYPHENS(1:80)
            WRITE(IUVARREGRESS,100)GROUPS(I)
            IF(MAXAVG(I)>0) THEN
              WRITE(IUVARREGRESS,101)(AVGARRAY(I,J),J=1,MAXAVG(I))
            ELSE
              WRITE(IUVARREGRESS,104)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      ! If it was found that there are parameters to average for this group
      IF(AVGPARAMLGP(I)) THEN
        NCNT = 0
        ! COUNT MODELS IN THIS GROUP
        DO J=1,NMODELS
          IF(NAMEPATHGPNSHORT(J) .EQ. I) THEN
            IF(NAMELSHORT(J) .EQ. 'Y') THEN
              NCNT = NCNT + 1
            ENDIF
          ENDIF
        ENDDO
        ! IF THERE ARE ONE OR MORE MODELS IN THE GROUP
        IF(NCNT > 0) THEN
          ALLOCATE(ANALGP(NCNT),IPT(NCNT),NMODNPE(NCNT),WEIGHTSHORT(NCNT))
          KMODELS(I) = NCNT
          NCNT = 0
          ! DETERMINE ACCEPTED MODELS IN THIS GROUP & assign anal value and pointer
          DO J=1,NMODELS
            IF(GROUPS(NAMEPATHGPNSHORT(J)) .EQ. GROUPS(I)) THEN
              IF(NAMELSHORT(J) .EQ. 'Y') THEN
                NCNT = NCNT + 1
                ANALGP(NCNT) = ANAL(J)
                IPT(NCNT) = J
                NMODNPE(NCNT) = STATSHORT(J,1)
              ENDIF
            ENDIF
          ENDDO
          IF(NCNT > 0) THEN
            ! IF MORE THAN ONE MODEL
            ! CALUCLATE WEIGHTS FOR ACCEPTED MODELS IN GROUP
            WEIGHTSHORT = 0.D0
            WEIGHTSHORTSUM = 0.D0
            DO J=1,NCNT
              WEIGHTSHORTSUM = WEIGHTSHORTSUM + WEIGHT(IPT(J))
            ENDDO
            DO J=1,NCNT
              WEIGHTSHORT(J) = WEIGHT(IPT(J)) / WEIGHTSHORTSUM
            ENDDO
          ELSE
            ! ONLY ONE MODEL ASSIGNWEIGHT OF ONE
            WEIGHT(1) = 1.D0
          ENDIF
          ! CALCULATE MODEL AVERAGED VALUES FOR THIS ANALYSIS
          WRITE(IUPARAMS,110)TRIM(GROUPS(I)),NCNT,TRIM(ANALYSIS(L))
          WRITE(IUPARAMS,120)
          LASTLOG = 0
          ! LOOP UP TO MAX # OF PARAMETERS TO AVERAGE FOR ANY GROUP
          DO K = 1,MAXMAXAVG
            MALC = 0.D0
            MAUC = 0.D0
            MAV = 0.D0
            MAVAR = 0.D0
            ! IF THE PARAMETER NAME IS BLANK, ALL PARAMETERS FOR THIS GROUP
            ! HAVE BEEN AVERAGED, MOVE ON TO NEXT GROUP
            IF(AVGARRAY(I,K) .EQ. ' ') EXIT
            ! CALCULATE MODEL-AVG PARAMETER VALUE
            DO J=1,NCNT
              DO IP=1,NMODNPE(J)
                IF(TRIM(AVGARRAY(I,K)) .EQ. TRIM(PNARRAYSHORT(IPT(J),IP))) THEN
                  IF(L .EQ. 1) THEN
                    PARNATIVE(J,K) = PARARRAYNATIVE(IPT(J),IP)
                    PVNATIVE(J,K) =  PVARRAYNATIVE(IPT(J),IP)
                    PARREGRESS(J,K) = PARARRAYSHORT(IPT(J),IP)
                    PVREGRESS(J,K) = PVARRAYSHORT(IPT(J),IP)
                  ENDIF
                  EXIT
                ENDIF
                IF(IP .EQ. NMODNPE(J)) THEN
                  ! TERMINATE
                  WRITE(*,*)' !!!! WARNING PARAMETER MISMATCH !!!'
                  WRITE(*,*)' NO MATCH FOR: ',TRIM(AVGARRAY(I,K))
                  WRITE(*,*)' IN MODEL: ',TRIM(NAMEMODELSHORT(IPT(J)))
                  WRITE(*,*)' PATH: ',TRIM(NAMEPATHSHORT(IPT(J)))
                  WRITE(IUMSG,*)' !!!! WARNING PARAMETER MISMATCH !!!'
                  WRITE(IUMSG,*)' NO MATCH FOR: ',TRIM(AVGARRAY(I,K))
                  WRITE(IUMSG,*)' IN MODEL: ',TRIM(NAMEMODELSHORT(IPT(J)))
                  WRITE(IUMSG,*)' PATH: ',TRIM(NAMEPATHSHORT(IPT(J)))
                  WRITE(*,*)
                  WRITE(*,*)' !!!! MMA TERMINATING !!!'
                  WRITE(*,*)
                  WRITE(IUMSG,*)
                  WRITE(IUMSG,*)' !!!! MMA TERMINATING !!!'
                  WRITE(IUMSG,*)
                  CALL UTL_STOP(' ')
                ENDIF
              ENDDO
              IF(J .EQ. 1) THEN
                LASTLOG = ILOGARRAYSHORT(IPT(J),K)
              ELSE
                IF(ILOGARRAYSHORT(IPT(J),K) .NE. LASTLOG) THEN
                  WRITE(IUPARAMS,150)TRIM(AVGARRAY(I,K)),TRIM(GROUPS(I)), &
                  TRIM(NAMEMODELSHORT(IPT(J))),TRIM(NAMEPATHSHORT(IPT(J)))
                  ! quit this averaging
                ENDIF
              ENDIF
              MAV = MAV + (PARARRAYSHORT(IPT(J),IP)*WEIGHTSHORT(J))
            ENDDO
            ! CALCULATE MODEL-AVG PARAMETER VARAIANCE
            DO J=1,NCNT
              MAVAR = MAVAR + (WEIGHTSHORT(J) &
                       * ((PVARRAYSHORT(IPT(J),IP) &
                      + ((PARARRAYSHORT(IPT(J),IP)-MAV)* &
                         (PARARRAYSHORT(IPT(J),IP)-MAV)))))
            ENDDO
            SQ = SQRT(MAVAR)
            MALC = MAV - (2.D0*SQ)
            MAUC = MAV + (2.D0*SQ)
            ! PRINT MODEL AVERAGED VALUES FOR THIS ANALYSIS FOR THIS
            ! GROUP AND PARAMETER
            IF(LASTLOG > 0) THEN
              MALC = 10.D0**MALC
              MAUC = 10.D0**MAUC
              MAVAR = (10.D0**((2.3D0*MAVAR)+(2.D0*MAV)))* &
                                             (10.D0**(2.3D0*MAVAR)-1.D0)
              MAV = 10.D0**MAV
              WRITE(IUPARAMS,130)TRIM(AVGARRAY(I,K)),MALC,MAV,MAUC,MAVAR
            ELSE
              WRITE(IUPARAMS,140)TRIM(AVGARRAY(I,K)),MALC,MAV,MAUC,MAVAR
            ENDIF
          ENDDO
          IF(L .EQ. 1) THEN
            IF(WRTPARAMNATIVE) THEN
              DO J=1,NCNT
                WRITE(IUPARAMNATIVE,102)TRIM(NAMEMODELSHORT(IPT(J))), &
                                        (PARNATIVE(J,K),K=1,MAXAVG(I))
                WRITE(IUVARNATIVE,102)TRIM(NAMEMODELSHORT(IPT(J))), &
                                        (PVNATIVE(J,K),K=1,MAXAVG(I))
              ENDDO
            ENDIF
            IF(WRTPARAMREGRESS) THEN
              DO J=1,NCNT
                WRITE(IUPARAMREGRESS,102)TRIM(NAMEMODELSHORT(IPT(J))), &
                                         (PARREGRESS(J,K),K=1,MAXAVG(I))
                WRITE(IUVARREGRESS,102)TRIM(NAMEMODELSHORT(IPT(J))), &
                                       (PVREGRESS(J,K),K=1,MAXAVG(I))
              ENDDO
            ENDIF
          ENDIF
          DEALLOCATE(ANALGP,IPT,NMODNPE,WEIGHTSHORT)
        ENDIF
        IF(L .EQ. 1 .AND. WRTPARAMNATIVE) THEN
          IF(NCNT < 1) THEN
            WRITE(IUPARAMNATIVE,103)
            WRITE(IUVARNATIVE,103)
          ENDIF
        ENDIF
        IF(L .EQ. 1 .AND. WRTPARAMREGRESS) THEN
          IF(NCNT < 1) THEN
            WRITE(IUPARAMREGRESS,103)
            WRITE(IUVARREGRESS,103)
          ENDIF
        ENDIF
      ENDIF
    ENDDO GROUP
    CLOSE(IUPARAMS)
    IF(L.EQ. 1) THEN
      IF(WRTPARAMNATIVE) THEN
        CLOSE(IUPARAMNATIVE)
        CLOSE(IUVARNATIVE)
      ENDIF
      IF(WRTPARAMREGRESS) THEN
        CLOSE(IUPARAMREGRESS)
        CLOSE(IUVARREGRESS)
      ENDIF
    ENDIF
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    IF(ALLOCATED(ANALGP))DEALLOCATE(ANALGP)
    IF(ALLOCATED(IPT))DEALLOCATE(IPT)
    IF(ALLOCATED(KMODELS))DEALLOCATE(KMODELS)
    IF(ALLOCATED(NMODNPE))DEALLOCATE(NMODNPE)
    IF(ALLOCATED(PARNATIVE))DEALLOCATE(PARNATIVE)
    IF(ALLOCATED(PARREGRESS))DEALLOCATE(PARREGRESS)
    IF(ALLOCATED(PVNATIVE))DEALLOCATE(PVNATIVE)
    IF(ALLOCATED(PVREGRESS))DEALLOCATE(PVREGRESS)
    RETURN
  END SUBROUTINE MMA_ANL_PC
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_ANL_PREDAVG(IFAIL,J,NMODELS)
    !  This subroutine model-averages the predictions
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,               INTENT(INOUT)  :: IFAIL
    INTEGER,                  INTENT(IN)  :: J
    INTEGER,                  INTENT(IN)  :: NMODELS
    ! Local variables
    INTEGER                               :: I
    INTEGER                               :: IDOPRED
    INTEGER                               :: IMODEL
    INTEGER                               :: IUPREDS
    INTEGER                               :: IUPREDSVAR
    INTEGER                               :: IUPS
    INTEGER                               :: K
    CHARACTER(LEN=MAX_STRING_LEN)         :: ONAM = ' '
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: PRDAVG
    DOUBLE PRECISION                            :: PRDMPRDAVG2
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: PVAVGCIIND
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: PVAVGCISIM
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: PVAVGCIINF
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: PVAVGPIIND
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: PVAVGPISIM
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: PVAVGPIINF
    ! Formats
    10 FORMAT(1X,' Model-averaged Predictions',/,'    for Analysis: ',A,/, &
               '    will be written to unit: ',I5,/,'    File: ',A,/)
    15 FORMAT('"PREDICTION NAME" ', &
              '"MOD-AVG PREDICTED VALUE" ', &
              '"MOD-AVG LOWER CONFIDENCE INT" ', &
              '"MOD-AVG UPPPER CONFIDENCE INT" ', &
              '"MOD-AVG STANDARD DEVIATION" ', &
              '"PLOT SYMBOL" ')
    16 FORMAT('"PREDICTION NAME" ', &
              '"MOD-AVG PREDICTED VALUE" ', &
              '"MOD-AVG LOWER PREDICTION INT" ', &
              '"MOD-AVG UPPPER PREDICTION INT" ', &
              '"MOD-AVG STANDARD DEVIATION" ', &
              '"PLOT SYMBOL" ')
    20 FORMAT('"',A,' MODEL-AVERAGED, PREDICTIONS and INDIVIDUAL ', &
              'CONFIDENCE INTERVALS" "Number of models: " "',I6,'"')
    21 FORMAT('"',A,' MODEL-AVERAGED, PREDICTIONS and SIMULTANEOUS ', &
              'CONFIDENCE INTERVALS, LIMITED" "Number of models: " "',I6,'"')
    22 FORMAT('"',A,' MODEL-AVERAGED, PREDICTIONS and SIMULTANEOUS ', &
              'CONFIDENCE INTERVALS, INFINITE" "Number of models: " "',I6,'"')
    23 FORMAT('"',A,' MODEL-AVERAGED, PREDICTIONS and INDIVIDUAL ', &
              'PREDICTION INTERVALS" "Number of models: " "',I6,'"')
    24 FORMAT('"',A,' MODEL-AVERAGED, PREDICTIONS and SIMULTANEOUS ', &
              'PREDICTION INTERVALS, LIMITED" "Number of models: " "',I6,'"')
    25 FORMAT('"',A,' MODEL-AVERAGED, PREDICTIONS and SIMULTANEOUS ', &
              'PREDICTION INTERVALS, INFINITE" "Number of models: " "',I6,'"')
    30 FORMAT(1X,4(1X,1PG25.7),6X,A20,1X,I6)
    130 FORMAT(2X,1PG25.7,6X,1PG25.7,2(1X,1PG25.7),6X,A20,1X,I6)
    205 FORMAT(/,'      Predictions involved in averaging ', &
              /,'         will be written to unit: ',I5,/,'         File: ',A)
    206 FORMAT(/,'      Prediction Variances involved in averaging ', &
              /,'         will be written to unit: ',I5,/,'         File: ',A)
    296 FORMAT(1X,'PREDICTIONS TO BE AVERAGED: ')
    297 FORMAT(1X,'INDIVIDUAL VARIANCES FOR PREDICTIONS TO BE AVERAGED: ')
    300 FORMAT(1X,'MODEL',10X,2000(1X,A20))
    310 FORMAT(1X,A12,3X,2000(1X,1PG20.8))
    !
    ALLOCATE(PRDAVG(IPRED),PVAVGCIIND(IPRED),PVAVGCISIM(IPRED), &
             PVAVGCIINF(IPRED),PVAVGPIIND(IPRED),PVAVGPISIM(IPRED), &
             PVAVGPIINF(IPRED))
    !
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_ANL_PREDAVG'
    IFAIL = 0
    ! Start preds output file
    ONAM = TRIM(OUTNAM)//'._preds_'//TRIM(ANALYSIS(J))
    CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUPS)
    WRITE(*,10)TRIM(ANALYSIS(J)),IUPS,TRIM(ONAM)
    WRITE(IUMSG,10)TRIM(ANALYSIS(J)),IUPS,TRIM(ONAM)
    IDOPRED = IPRED
    !
    ! ybars
    PRDAVG=0.D0
    ! var ybars
    PVAVGCIIND=0.D0
    PVAVGCISIM=0.D0
    PVAVGCIINF=0.D0
    PVAVGPIIND=0.D0
    PVAVGPISIM=0.D0
    PVAVGPIINF=0.D0
    ! Calculate ybar, sum of model weighted predicitons
    DO IMODEL=1,NMODELS
      DO I=1,IDOPRED
        PRDAVG(I)=PRDAVG(I)+(WEIGHT(IMODEL)*PRD(IMODEL,I))
      ENDDO
    ENDDO
    ! Calculate var ybar
    DO IMODEL=1,NMODELS
      DO I=1,IDOPRED
        PRDMPRDAVG2 = (PRD(IMODEL,I)-PRDAVG(I))**2
        PVAVGCIIND(I) = PVAVGCIIND(I) + &
                        (WEIGHT(IMODEL)*((PVARCIIND(IMODEL,I)+PRDMPRDAVG2)))
        PVAVGCISIM(I) = PVAVGCISIM(I) + &
                        (WEIGHT(IMODEL)*((PVARCISIM(IMODEL,I)+PRDMPRDAVG2)))
        PVAVGCIINF(I) = PVAVGCIINF(I) + &
                        (WEIGHT(IMODEL)*((PVARCIINF(IMODEL,I)+PRDMPRDAVG2)))
        PVAVGPIIND(I) = PVAVGPIIND(I) + &
                        (WEIGHT(IMODEL)*((PVARPIIND(IMODEL,I)+PRDMPRDAVG2)))
        PVAVGPISIM(I) = PVAVGPISIM(I) + &
                        (WEIGHT(IMODEL)*((PVARPISIM(IMODEL,I)+PRDMPRDAVG2)))
        PVAVGPIINF(I) = PVAVGPIINF(I) + &
                        (WEIGHT(IMODEL)*((PVARPIINF(IMODEL,I)+PRDMPRDAVG2)))
      ENDDO
    ENDDO
    ! WRITE INDIVIDUAL CONFIDENCE INTERVALS FOR PREDICTIONS
    WRITE(IUPS,20)TRIM(ANALYSIS(J)),NMODELS
    WRITE(IUPS,15)
    DO I=1,IPRED
      CALL MMA_ANL_W(IUPS,PRDAVG(I),PVAVGCIIND(I),PREDNAM(I),PREDPLOT(I))
    ENDDO
    ! WRITE LIMITED CONFIDENCE INTERVALS FOR PREDICTIONS
    WRITE(IUPS,21)TRIM(ANALYSIS(J)),NMODELS
    WRITE(IUPS,15)
    DO I=1,IPRED
      CALL MMA_ANL_W(IUPS,PRDAVG(I),PVAVGCISIM(I),PREDNAM(I),PREDPLOT(I))
    ENDDO
    ! WRITE INFINITE CONFIDENCE INTERVALS FOR PREDICTIONS
    WRITE(IUPS,22)TRIM(ANALYSIS(J)),NMODELS
    WRITE(IUPS,15)
    DO I=1,IPRED
      CALL MMA_ANL_W(IUPS,PRDAVG(I),PVAVGCIINF(I),PREDNAM(I),PREDPLOT(I))
    ENDDO
    ! WRITE INDIVIDUAL PREDICTION INTERVALS FOR PREDICTIONS
    WRITE(IUPS,23)TRIM(ANALYSIS(J)),NMODELS
    WRITE(IUPS,16)
    DO I=1,IPRED
      CALL MMA_ANL_W(IUPS,PRDAVG(I),PVAVGPIIND(I),PREDNAM(I),PREDPLOT(I))
    ENDDO
    ! WRITE LIMITED PREDICTION INTERVALS FOR PREDICTIONS
    WRITE(IUPS,24)TRIM(ANALYSIS(J)),NMODELS
    WRITE(IUPS,16)
    DO I=1,IPRED
      CALL MMA_ANL_W(IUPS,PRDAVG(I),PVAVGPISIM(I),PREDNAM(I),PREDPLOT(I))
    ENDDO
    ! WRITE INFINITE PREDICTION INTERVALS FOR PREDICTIONS
    WRITE(IUPS,25)TRIM(ANALYSIS(J)),NMODELS
    WRITE(IUPS,16)
    DO I=1,IPRED
      CALL MMA_ANL_W(IUPS,PRDAVG(I),PVAVGPIINF(I),PREDNAM(I),PREDPLOT(I))
    ENDDO
    CLOSE(IUPS)
    ! OPEN FILE FOR LIST OF INDIVIDUAL PREDICTIONS FOR EACH MODEL
    ! THESE MAY BE OF INTEREST TO THE USER IF THE USER HAS
    ! DOUBTS ABOUT THE AVERAGED RESULTS
    IF(WRTPREDS) THEN
      ONAM = TRIM(OUTNAM)//'._IndividPreds'
      CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUPREDS)
      WRITE(*,205)IUPREDS,TRIM(ONAM)
      WRITE(IUMSG,205)IUPREDS,TRIM(ONAM)
      ONAM = TRIM(OUTNAM)//'._IndividPredsVar'
      CALL MMA_OPEN(IFAIL,ONAM,'REPLACE','WRITE',IUPREDSVAR)
      WRITE(*,206)IUPREDSVAR,TRIM(ONAM)
      WRITE(IUMSG,206)IUPREDSVAR,TRIM(ONAM)
      WRITE(IUPREDS,296)
      WRITE(IUPREDSVAR,297)
      WRITE(IUPREDS,300)(PREDNAM(K),K=1,IPRED)
      WRITE(IUPREDSVAR,300)(PREDNAM(K),K=1,IPRED)
      ! WRITE INDIVIDUAL PREDICTIONS FOR EACH MODEL
      DO IMODEL=1,NMODELS
        WRITE(IUPREDS,310)TRIM(NAMEMODELSHORT(IMODEL)),(PRD(IMODEL,K),K=1,IPRED)
        WRITE(IUPREDSVAR,310)TRIM(NAMEMODELSHORT(IMODEL)), &
                                             (PVARCIIND(IMODEL,K),K=1,IPRED)
      ENDDO
      CLOSE(IUPREDS)
      CLOSE(IUPREDSVAR)
    ENDIF
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    IF(ALLOCATED(PRDAVG))DEALLOCATE(PRDAVG)
    IF(ALLOCATED(PVAVGCIIND))DEALLOCATE(PVAVGCIIND)
    IF(ALLOCATED(PVAVGCISIM))DEALLOCATE(PVAVGCISIM)
    IF(ALLOCATED(PVAVGCIINF))DEALLOCATE(PVAVGCIINF)
    IF(ALLOCATED(PVAVGPIIND))DEALLOCATE(PVAVGPIIND)
    IF(ALLOCATED(PVAVGPISIM))DEALLOCATE(PVAVGPISIM)
    IF(ALLOCATED(PVAVGPIINF))DEALLOCATE(PVAVGPIINF)
    RETURN
  END SUBROUTINE MMA_ANL_PREDAVG
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_ANL_W(IUW,P,PV,PN,PP)
    ! This subroutine writes one line of model-averaged predictions and their
    ! lower, upper limits, variance and plot symbol
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,            INTENT(IN) :: IUW
    DOUBLE PRECISION,   INTENT(IN) :: P
    DOUBLE PRECISION,   INTENT(IN) :: PV
    CHARACTER(LEN=20),  INTENT(IN) :: PN
    INTEGER,            INTENT(IN) :: PP
    ! Internal variables
    DOUBLE PRECISION               :: HIGH
    DOUBLE PRECISION               :: LOW
    DOUBLE PRECISION               :: SQR
    ! Formats
    30 FORMAT(1X,A20,4(1X,1PG25.7),6X,1X,I6)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_ANL_W'
    !
    SQR=SQRT(PV)
    LOW=P-(2.*SQR)
    HIGH=P+(2.*SQR)
    WRITE(IUW,30)PN,P,LOW,HIGH,SQR,PP
    RETURN
  END SUBROUTINE MMA_ANL_W
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_ANL_MRW(IFAIL,NMODELS,CRIT)
    ! This subroutine calculates model weight and rank for the specified
    ! criterion and weight equations for a given analysis
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,                             INTENT(INOUT) :: IFAIL
    INTEGER,                             INTENT(IN)    :: NMODELS
    DOUBLE PRECISION, DIMENSION(NMODELS),INTENT(IN)    :: CRIT
    ! Internal variables
    INTEGER, DIMENSION(NMODELS)          :: ACCT
    DOUBLE PRECISION, DIMENSION(6)       :: CRITVALUES
    CHARACTER(LEN=12),DIMENSION(6)       :: CRITNAMES
    DOUBLE PRECISION                     :: HIGHESTW
    INTEGER                              :: IMODEL
    INTEGER                              :: ITYP
    INTEGER                              :: JMODEL
    INTEGER                              :: LASTJ
    DOUBLE PRECISION                     :: LOWESTW
    LOGICAL                              :: LVAL
    DOUBLE PRECISION                     :: MAXW
    DOUBLE PRECISION                     :: SUMPREQN
    DATA CRITNAMES/'mincrit','maxcrit','sumcrit','avgcrit','valcrit', &
                   'priormodprob'/
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_ANL_MRW'
    IFAIL = 0
    ACCT = 0
    WEIGHT = 0
    IF(NMODELS > 1) THEN
      ! Find the MIN MAX SUM  AVG of the measure from the list of models
      CRITVALUES(1) =  1.D+30 !mincrit
      CRITVALUES(2) = -1.D+30 !maxcrit
      CRITVALUES(3) =  0.D0   !sumcrit
      CRITVALUES(4) =  0.D0   !avgcrit
      CRITVALUES(5) =  0.D0   !will be valcrit
      DO IMODEL= 1,NMODELS
          IF(CRIT(IMODEL) .LT. CRITVALUES(1)) CRITVALUES(1) = CRIT(IMODEL)
          IF(CRIT(IMODEL) .GT. CRITVALUES(2)) CRITVALUES(2) = CRIT(IMODEL)
          CRITVALUES(3) = CRITVALUES(3) + CRIT(IMODEL)
      ENDDO
      CRITVALUES(4) = CRITVALUES(3) / NMODELS
      ! Calculate Weight Equation Values
      ! and their sum for the denominator of the weight calculation
      SUMPREQN=0.D0
      DO IMODEL = 1,NMODELS
        CRITVALUES(5) = CRIT(IMODEL)
        CRITVALUES(6) = PRIMODWTSHORT(IMODEL)
        CALL EQN_EVALUATE(IFAIL,1,6,CRITNAMES,CRITVALUES,ITYP, &
                          WEIGHT(IMODEL),LVAL)
        IF (IFAIL.NE.0) THEN
          CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
          CALL UTL_STOP('Call from MMA_ANL_MRW: EQN_EVALUATE reports failure')
        ENDIF
        SUMPREQN = SUMPREQN + WEIGHT(IMODEL)
      ENDDO
      IF(SUMPREQN <= 0.D0) THEN
        IFAIL = 1
        AMESSAGE = ' SKIP THIS GROUP: [Sum of Calculated Model Weights] <= 0'
        RETURN
      ENDIF
      ! Calculate MODEL WEIGHT
      MAXW=0.0
      DO IMODEL = 1,NMODELS
        WEIGHT(IMODEL) = WEIGHT(IMODEL) / SUMPREQN
        IF(WEIGHT(IMODEL) .GT. MAXW)THEN
          MAXW = WEIGHT(IMODEL)
          ORDER(1)=IMODEL
        ENDIF
      ENDDO
      ! PUT THE MODELS IN ORDER OF WEIGHT
      ACCT(ORDER(1))=1
      HIGHESTW=WEIGHT(ORDER(1))
      DO IMODEL = 2,NMODELS
        LOWESTW = -1.D+30
        DO JMODEL = 1,NMODELS
          IF(WEIGHT(JMODEL) .LE. HIGHESTW .AND. WEIGHT(JMODEL) &
             .GT. LOWESTW .AND. ACCT(JMODEL) .EQ. 0)THEN
            ORDER(IMODEL)=JMODEL
            LOWESTW=WEIGHT(JMODEL)
            LASTJ=JMODEL
          ENDIF
        ENDDO
        HIGHESTW=WEIGHT(LASTJ)
        ACCT(LASTJ)=1
      ENDDO
    ELSE
      WEIGHT(1) = 1.D0
      ORDER(1)= 1
    ENDIF
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    RETURN
  END SUBROUTINE MMA_ANL_MRW
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_ANL_RANK(IFAIL)
    ! This subroutine determines the rank order of each column of STATS
    ! based on what COLHHD(I,3) indicates are best values. Note:
    ! for COLHD near one or zero, low is "good" as the values are
    ! the abs magnitude from one or zero respectively
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,            INTENT(INOUT) :: IFAIL
    ! Internal variables
    DOUBLE PRECISION                  :: DELTA(NMPTHS)
    DOUBLE PRECISION                  :: DENOM
    INTEGER                           :: ICOLUMN
    INTEGER                           :: IA
    INTEGER                           :: II
    INTEGER                           :: IMODEL
    INTEGER                           :: IMODELB
    INTEGER                           :: JMODEL
    INTEGER                           :: LOWJ
    DOUBLE PRECISION                  :: TEMP
    DOUBLE PRECISION                  :: VALUE(NMPTHS)
    DOUBLE PRECISION                  :: VMAX
    DOUBLE PRECISION                  :: VMIN
    ! Formats
    100 FORMAT(1X,'"ID#"      "',A12,'"',13(1X,'"',A12,'"'))
    101 FORMAT(1X,'"ID#"      "',A12,'"',11(1X,'"',A12,'"'))
    102 FORMAT(1X,'"ID#"      "',A12,'"',22(1X,'"',A12,'"'))
    200 FORMAT(I6,A20,12I15,1X,A)
    201 FORMAT(I6,A20,10I15,1X,A)
    202 FORMAT(I6,A20,21I15,1X,A)
    300 FORMAT(' "ID#"      "MODEL       " "NPE         " "NOBS        "', &
      ' "NPR         " "SWSR_CHG    " "CEV_CHG     " "MLOF_CHG    "', &
      ' "AIC_CHG     " "AICC_CHG    " "BIC_CHG     " "KIC_CHG     "', &
      ' "XTWX_CHG    " "PriPrb_CHG  " "PATHANDROOT "')
    301 FORMAT(' "ID#"      "MODEL       " "R2_OS_CHG   " ', &
      '"INT_OS_CHG  " "SLP_OS_CHG  " "R2_WS_CHG   " "INT_WS_CHG  " ', &
      '"SLP_WS_CHG  " "R2_WW_CHG   " "INT_WW_CHG  " "SLP_WW_CHG  " ', &
      '"R2_NM_CHG   " "PATHANDROOT "')
    205 FORMAT(1X,"XYZT data are not available, XYZT measures are not written")
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_ANL_RANK'
    IFAIL = 0
    RANK=NMPTHS
    RANKNORM = DBLE(NMPTHS)
    ! Write column headers in _rank file
    WRITE(IURANK,100)COLHD(NCOL-1,1),(COLHD(IA,1),IA=1,3), &
                     (COLHD(IA,1),IA=4,K1,2),COLHD(NCOL,1)
    WRITE(IURANKG,101)COLHD(NCOL-1,1), &
                     (COLHD(IA,1),IA=K1+1,K2),COLHD(NCOL,1)
    IF(XYZFILE) WRITE(IURANKXYZT,102)COLHD(NCOL-1,1), &
                     (COLHD(IA,1),IA=K3+1,K3+21),COLHD(NCOL,1)
    ! PUT THE COLUMNS IN ORDER
    COLUMNS : DO ICOLUMN=1,NCOL-2
      ORDER=0
      ! DETERMINE IF THIS COLUMN SHOULD BE ORDERED
      ! IF SO CLOSE TO ZERO OR ONE, HI-LO, LO-HI,
      VMAX = -1.D+30
      VMIN = 1.D+30
      IF(COLHD(ICOLUMN,3) .EQ. 'NONE') THEN
        GO TO 9000
      ELSEIF(COLHD(ICOLUMN,3) .EQ. 'ZERO') THEN
        DO JMODEL=1,NMPTHS
          VALUE(JMODEL)=ABS(0.0D0-STATS(JMODEL,ICOLUMN))
        ENDDO
      ELSEIF(COLHD(ICOLUMN,3) .EQ. 'ONE ') THEN
        DO JMODEL=1,NMPTHS
          VALUE(JMODEL)=ABS(1.0D0-STATS(JMODEL,ICOLUMN))
        ENDDO
      ELSE
        DO JMODEL=1,NMPTHS
          VALUE(JMODEL)=STATS(JMODEL,ICOLUMN)
        ENDDO
      ENDIF
      !NOW that appropriate values are assigned, calculate normalized values
      ! IF COLUMN IS NOT TO BE RANKED ASSIGN LOWEST RANK
      DO JMODEL=1,NMPTHS
        IF(STATS(JMODEL,ICOLUMN) .GT. 0.99D+30) THEN
          RANKNORM(JMODEL,ICOLUMN) = NMPTHS-NOTCONV-NOTREAS+1
        ELSE
          IF(VALUE(JMODEL) .GT. VMAX) THEN
            VMAX = VALUE(JMODEL)
          ENDIF
          IF(VALUE(JMODEL) .LT. VMIN) THEN
            VMIN = VALUE(JMODEL)
          ENDIF
        ENDIF
      ENDDO
      ! For those that should be ranked best when low, reverse the max and min
      IF(COLHD(ICOLUMN,3) .NE. 'HIGH') THEN
        TEMP = VMAX
        VMAX = VMIN
        VMIN = TEMP
      ENDIF
      DENOM =  VMAX - VMIN
      DO JMODEL=1,NMPTHS
        IF(STATS(JMODEL,ICOLUMN) .LT. 0.99D+30 .AND. DENOM .NE. 1.D+30) THEN
          RANKNORM(JMODEL,ICOLUMN)= &
          1.D0 + (((VMAX-VALUE(JMODEL))/DENOM) * (CONVREAS-1.D0))
        ENDIF
      ENDDO
      !NOW we have values and normalized ranks
      ! Order the values
      CALL MMA_EVA_ORDERSTATS(IFAIL,NMPTHS,VALUE,DELTA,LOWJ)
      IF(IFAIL > 0) CALL UTL_STOP('Called from MMA_ANL_RANK: '//ERRSUB)
      ! REORDER
      ! IF THIS COLUMN IS NOT TO BE RANKED, SKIP THE REST
      IF(COLHD(ICOLUMN,3) .EQ. 'NONE' .OR. DENOM .EQ. 1.D+30) THEN
        GO TO 9000
      ELSEIF(COLHD(ICOLUMN,3) .EQ. 'HIGH') THEN
        ! ORDER HIGH TO LOW
        DO IMODEL = 1,NMPTHS
          IF (IMODEL <= NOTCONV+NOTREAS) THEN
            RANK(ORDER(IMODEL),ICOLUMN) = NMPTHS-NOTCONV-NOTREAS+1
          ELSE
            IF(IMODEL > NOTCONV+NOTREAS+1) THEN
              RANK(ORDER(IMODEL),ICOLUMN) = RANK(ORDER(IMODEL-1),ICOLUMN) + 1
              IF(VALUE(ORDER(IMODEL)) .EQ. VALUE(ORDER(IMODEL-1))) &
                RANK(ORDER(IMODEL),ICOLUMN) = RANK(ORDER(IMODEL-1),ICOLUMN)
            ELSE
              RANK(ORDER(IMODEL),ICOLUMN) = 1
            ENDIF
          ENDIF
        ENDDO
      ELSE
        ! ORDER LOW TO HIGH
        DO IMODEL = NMPTHS,1,-1
          IF (IMODEL <= NOTCONV+NOTREAS) THEN
            RANK(ORDER(IMODEL),ICOLUMN) = NMPTHS-NOTCONV-NOTREAS+1
          ELSE
            IF(IMODEL < NMPTHS) THEN
              RANK(ORDER(IMODEL),ICOLUMN) = RANK(ORDER(IMODEL+1),ICOLUMN) + 1
              IF(VALUE(ORDER(IMODEL)) .EQ. VALUE(ORDER(IMODEL+1))) THEN
                RANK(ORDER(IMODEL),ICOLUMN) = RANK(ORDER(IMODEL+1),ICOLUMN)
!                DO IMODELB=IMODEL,NMPTHS
!                  RANK(ORDER(IMODELB),ICOLUMN)=RANK(ORDER(IMODELB),ICOLUMN)+1
!                ENDDO
              ENDIF
            ELSE
              RANK(ORDER(IMODEL),ICOLUMN) = 1
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      ! ORDER SHOULD BE ZERO TO INDICATE NO RANKING
      9000 IF(COLHD(ICOLUMN,3) .EQ. 'NONE') THEN
        ! ORDER HIGH TO LOW
        DO IMODEL = 1,NMPTHS
          RANK(IMODEL,ICOLUMN) = 0
          RANKNORM(IMODEL,ICOLUMN) = 0.D0
        ENDDO
      ENDIF
    ENDDO COLUMNS
    DO IMODEL = 1,NMPTHS
      WRITE(IURANK,200)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
                   (RANK(IMODEL,II),II=1,3),(RANK(IMODEL,II),II=4,K1,2), &
                   TRIM(NAMEPATH(IMODEL))
      WRITE(IURANKG,201)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
                   (RANK(IMODEL,II),II=K1+1,K2),TRIM(NAMEPATH(IMODEL))
      IF(XYZFILE)WRITE(IURANKXYZT,202)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
                   (RANK(IMODEL,II),II=K3+1,K3+21),TRIM(NAMEPATH(IMODEL))
    ENDDO
    IF(NMPR > 0) THEN
      ! Write column headers in _rank file
      WRITE(IURANK,100)COLHD(NCOL-1,1),(COLHD(IA,1),IA=1,3), &
                      (COLHD(IA,1),IA=5,K1,2),COLHD(K1,1),COLHD(NCOL,1)
      WRITE(IURANKG,101)COLHD(NCOL-1,1), &
                       (COLHD(IA,1),IA=K2+1,K3),COLHD(NCOL,1)
      DO IMODEL = 1,NMPTHS
        WRITE(IURANK,200)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
                     (RANK(IMODEL,II),II=1,3),(RANK(IMODEL,II),II=5,K1,2), &
                     RANK(IMODEL,K1),TRIM(NAMEPATH(IMODEL))
        WRITE(IURANKG,201)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
                     (RANK(IMODEL,II),II=K2+1,K3),TRIM(NAMEPATH(IMODEL))
      ENDDO
      ! Write column headers in _rank file
      WRITE(IURANK,300)
      WRITE(IURANKG,301)
      DO IMODEL = 1,NMPTHS
        WRITE(IURANK,200)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
                     (RANK(IMODEL,II),II=1,3), &
                     ((RANK(IMODEL,II+1)-RANK(IMODEL,II)),II=4,K1,2), &
                     TRIM(NAMEPATH(IMODEL))
        WRITE(IURANKG,201)IMODEL,TRIM(NAMEMODEL(IMODEL)), &
                     ((RANK(IMODEL,K2+II)-RANK(IMODEL,K1+II)),II=1,10), &
                     TRIM(NAMEPATH(IMODEL))
      ENDDO
    ENDIF
    CLOSE(IURANK)
    IF(.NOT. XYZFILE)WRITE(IURANKXYZT,205)
    CLOSE(IURANKXYZT)
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    RETURN
  END SUBROUTINE MMA_ANL_RANK
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_INI_NAMCHK(IFAIL)
    ! This subroutine compares the lower case names of this model observations
    ! to the lower case names of the first model observations and writes
    ! messages and initiates termination if they do not match
    ! Model ranking, weighting and averaging is not valid unless the
    ! same observations are used for every model
    IMPLICIT NONE
    !  Argument-list variables
    INTEGER,                INTENT(INOUT)    :: IFAIL
    !  Local variables
    INTEGER :: AOK
    INTEGER :: I
    INTEGER :: J
    INTEGER :: K
    !!!!CHARACTER(LEN=LENDNAM) :: NAMES(NTOTOBS)
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: NAMES
    ! Formats
    100 FORMAT(//,' OBSERVATIONS MUST BE THE SAME FOR ALL EVALUATED MODELS ')
    200 FORMAT(1X,' Observation name: ',A,' in alphabetically ordered names ', &
               'for first model',/,3X,'does not match ',A,' of the ', &
               'alphabetically ordered names for this model')
    299 FORMAT(/,80('!'))
    300 FORMAT(/,1X,' MISMATCHED OBSERVATION NAMES:')
    301 FORMAT(5X,A)
    302 FORMAT(1X,//,1X,'SEE LIST OF MISMATCHED NAMES IN THE #mout FILE',/)
    !
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_INI_NAMCHK'
    ALLOCATE(NAMES(NTOTOBS))
    NAMES = ' '
    !
    ! Fill the names array with the lower case observation names of all types
    DO I=1,NTOTOBS
      CALL UTL_CASE(TRIM(OBSNAME(I)),NAMES(I),-1)
    ENDDO
    ! Sort the names array in alpha order
    CALL UTL_SHELLSORT (NTOTOBS,NAMES)
    ! Compare the names to the names of the first model, terminate if mismatch
    DO I=1,NTOTOBS
      IF(NAMES(I) .NE. OBSNAMEALL(I)) THEN
        ! Write mismatch names
        WRITE(*,100)
        WRITE(IUMSG,100)
        WRITE(*,200)TRIM(NAMES(I)),TRIM(OBSNAMEALL(I))
        WRITE(IUMSG,200)TRIM(NAMES(I)),TRIM(OBSNAMEALL(I))
        IFAIL = 1
        ! Search for names that do not match and
        ! Write messages
        IF (IFAIL > 0) THEN
          WRITE(IUMSG,299)
          WRITE(IUMSG,300)
          DO J=1,NTOTOBS
            AOK = 0
            DO K=1,NTOTOBS
              IF(OBSNAME(J) .EQ. OBSNAMEALL(K)) THEN
                AOK = 1
                EXIT
              ENDIF
              IF(K .EQ. NTOTOBS .AND. AOK .EQ. 0) THEN
                WRITE(IUMSG,301)OBSNAME(J)
              ENDIF
            ENDDO
          ENDDO
          WRITE(*,302)
          AMESSAGE = ' Stopping Due To Mismatched Observation Names'
          CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
          DEALLOCATE(NAMES)
          RETURN
        ENDIF
      ENDIF
    ENDDO
    DEALLOCATE(NAMES)
    RETURN
  END SUBROUTINE MMA_INI_NAMCHK
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_CLN(IFAIL)
    ! This subroutine writes final messages and closes the last few files
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER,              INTENT(INOUT)  :: IFAIL
    ! FORMATS
    205 FORMAT(1X,"XYZT data are not available, XYZT measures are not written")
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine MMA_CLN'
    IFAIL = 0
    IF(ALLOCATED(ANALYSIS))DEALLOCATE(ANALYSIS)
    IF(ALLOCATED(AVGARRAY))DEALLOCATE(AVGARRAY)
    IF(ALLOCATED(AVGL))DEALLOCATE(AVGL)
    IF(ALLOCATED(AVGPARAMLGP))DEALLOCATE(AVGPARAMLGP)
    IF(ALLOCATED(AVGPARAMLGPI))DEALLOCATE(AVGPARAMLGPI)
    IF(ALLOCATED(COLHD))DEALLOCATE(COLHD)
    IF(ALLOCATED(COLHDNAME))DEALLOCATE(COLHDNAME)
    IF(ALLOCATED(COORDL))DEALLOCATE(COORDL)
    IF(ALLOCATED(EQNARRAY))DEALLOCATE(EQNARRAY)
    IF(ALLOCATED(EQNL))DEALLOCATE(EQNL)
    IF(ALLOCATED(GROUPS))DEALLOCATE(GROUPS)
    IF(ALLOCATED(ILOGARRAY))DEALLOCATE(ILOGARRAY)
    IF(ALLOCATED(ILOGARRAYORIG))DEALLOCATE(ILOGARRAYORIG)
    IF(ALLOCATED(ILOGARRAYSHORT))DEALLOCATE(ILOGARRAYSHORT)
    IF(ALLOCATED(IPTRNMODELS))DEALLOCATE(IPTRNMODELS)
    IF(ALLOCATED(IPLOT))DEALLOCATE(IPLOT)
    IF(ALLOCATED(ITER))DEALLOCATE(ITER)
    IF(ALLOCATED(MAXAVG))DEALLOCATE(MAXAVG)
    IF(ALLOCATED(MAXEQN))DEALLOCATE(MAXEQN)
    IF(ALLOCATED(NAMEL))DEALLOCATE(NAMEL)
    IF(ALLOCATED(NAMELSHORT))DEALLOCATE(NAMELSHORT)
    IF(ALLOCATED(NAMEMODEL))DEALLOCATE(NAMEMODEL)
    IF(ALLOCATED(NAMEMODELSHORT))DEALLOCATE(NAMEMODELSHORT)
    IF(ALLOCATED(NAMEPATH))DEALLOCATE(NAMEPATH)
    IF(ALLOCATED(NAMEPATHSHORT))DEALLOCATE(NAMEPATHSHORT)
    IF(ALLOCATED(NAMEPATHGP))DEALLOCATE(NAMEPATHGP)
    IF(ALLOCATED(NAMEPATHGPN))DEALLOCATE(NAMEPATHGPN)
    IF(ALLOCATED(NAMEPATHGPNSHORT))DEALLOCATE(NAMEPATHGPNSHORT)
    IF(ALLOCATED(OBSNAME))DEALLOCATE(OBSNAME)
    IF(ALLOCATED(OBSNAMEALL))DEALLOCATE(OBSNAMEALL)
    IF(ALLOCATED(ORDER))DEALLOCATE(ORDER)
    IF(ALLOCATED(PARARRAY))DEALLOCATE(PARARRAY)
    IF(ALLOCATED(PARARRAYORIG))DEALLOCATE(PARARRAYORIG)
    IF(ALLOCATED(PARARRAYSHORT))DEALLOCATE(PARARRAYSHORT)
    IF(ALLOCATED(PARARRAYNATIVE))DEALLOCATE(PARARRAYNATIVE)
    IF(ALLOCATED(PARAVG))DEALLOCATE(PARAVG)
    IF(ALLOCATED(PARAVGGP))DEALLOCATE(PARAVGGP)
    IF(ALLOCATED(PARAVGGPN))DEALLOCATE(PARAVGGPN)
    IF(ALLOCATED(PAREQN))DEALLOCATE(PAREQN)
    IF(ALLOCATED(PAREQNGP))DEALLOCATE(PAREQNGP)
    IF(ALLOCATED(PAREQNGPN))DEALLOCATE(PAREQNGPN)
    IF(ALLOCATED(PAREQNNAME))DEALLOCATE(PAREQNNAME)
    IF(ALLOCATED(PNARRAY))DEALLOCATE(PNARRAY)
    IF(ALLOCATED(PNARRAYORIG))DEALLOCATE(PNARRAYORIG)
    IF(ALLOCATED(PNARRAYSHORT))DEALLOCATE(PNARRAYSHORT)
    IF(ALLOCATED(PREDPLOT))DEALLOCATE(PREDPLOT)
    IF(ALLOCATED(PREDNAM))DEALLOCATE(PREDNAM)
    IF(ALLOCATED(PREDNAMLC))DEALLOCATE(PREDNAMLC)
    IF(ALLOCATED(PRD))DEALLOCATE(PRD)
    IF(ALLOCATED(PRIMODWT))DEALLOCATE(PRIMODWT)
    IF(ALLOCATED(PRIMODWTSHORT))DEALLOCATE(PRIMODWTSHORT)
    IF(ALLOCATED(PRINAME))DEALLOCATE(PRINAME)
    IF(ALLOCATED(PVARCIIND))DEALLOCATE(PVARCIIND)
    IF(ALLOCATED(PVARCISIM))DEALLOCATE(PVARCISIM)
    IF(ALLOCATED(PVARCIINF))DEALLOCATE(PVARCIINF)
    IF(ALLOCATED(PVARPIIND))DEALLOCATE(PVARPIIND)
    IF(ALLOCATED(PVARPISIM))DEALLOCATE(PVARPISIM)
    IF(ALLOCATED(PVARPIINF))DEALLOCATE(PVARPIINF)
    IF(ALLOCATED(PVARRAY))DEALLOCATE(PVARRAY)
    IF(ALLOCATED(PVARRAYORIG))DEALLOCATE(PVARRAYORIG)
    IF(ALLOCATED(PVARRAYLOG))DEALLOCATE(PVARRAYLOG)
    IF(ALLOCATED(PVARRAYLOGORIG))DEALLOCATE(PVARRAYLOGORIG)
    IF(ALLOCATED(PVARRAYSHORT))DEALLOCATE(PVARRAYSHORT)
    IF(ALLOCATED(PVARRAYNATIVE))DEALLOCATE(PVARRAYNATIVE)
    IF(ALLOCATED(RANK))DEALLOCATE(RANK)
    IF(ALLOCATED(RANKNORM))DEALLOCATE(RANKNORM)
    IF(ALLOCATED(STATS))DEALLOCATE(STATS)
    IF(ALLOCATED(STATSPERCENT))DEALLOCATE(STATSPERCENT)
    IF(ALLOCATED(STATSHORT))DEALLOCATE(STATSHORT)
    IF(ALLOCATED(TCOORD))DEALLOCATE(TCOORD)
    IF(ALLOCATED(TCOORDL))DEALLOCATE(TCOORDL)
    IF(ALLOCATED(VAL1))DEALLOCATE(VAL1)
    IF(ALLOCATED(VAL2))DEALLOCATE(VAL2)
    IF(ALLOCATED(WEIGHT))DEALLOCATE(WEIGHT)
    CALL BAS_CLN()
    CLOSE (IUMSG)
    CLOSE (IUMPTHS)
    IF(.NOT. XYZFILE)WRITE(IUOUTXYZT,205)
    CLOSE (IUOUT)
    CLOSE (IUOUTXYZT)
    IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IUMSG,'yes','yes','yes')
    RETURN
  END SUBROUTINE MMA_CLN
  !-----------------------------------------------------------------------------
  SUBROUTINE MMA_EOF(IFAIL,INNAM)
    ! This subroutine reports the path and file name of a file for which the
    !  end of file was reached when data was expected
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN)    :: INNAM
    ! Formats
    910 FORMAT(/,' END OF FILE REACHED: ',A)
    ! INITIALIZE
    AMESSAGE = ' TERMINATING DUE TO END OF FILE REACHED'
    ERRSUB=' Error in subroutine MMA_EOF'
    WRITE(*,910)INNAM
    WRITE(IUMSG,910)INNAM
    IFAIL = 1
    CALL BAS_CLN()
    RETURN
  END SUBROUTINE MMA_EOF
  !-----------------------------------------------------------------------------
END MODULE MMA_MOD
!-------------------------------------------------------------------------------
