! Last change EPP 2/4/2008 4:53:22 PM
PROGRAM SIMADJ
! Utilizes Jupiter API modules
USE GLOBAL_DATA, ONLY: AMESSAGE, LENDNAM, MAX_STRING_LEN
USE DATATYPES
USE UTILITIES
  !       MODFLOW-2000 & -2005 POSTPROCESSING PROGRAM BY E.P. POETER,
  !           CSM, IGWMC, USGS, GOLDEN, CO.
  !       Evaluates process._ext file for missing or defaulted observations and
  !       replaces them with user specified alternative simulated
  !       equivalents or a constant.
  !
! Variables
  IMPLICIT NONE
  INTEGER, ALLOCATABLE, DIMENSION(:)                   :: ASSGSIMADJ    ! if > 0, seq assigned to item in simadj list
  INTEGER, ALLOCATABLE, DIMENSION(:)                   :: ASSGOUT      ! if > 0, seq assigned to item in out list
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)    :: CHARSIMADJ    ! name of default for this observation
  CHARACTER(LEN=MAX_STRING_LEN)                        :: CHARSCR ! line from scratch file
  CHARACTER(LEN=10)                                    :: CHDATE      ! DATE
  CHARACTER(LEN=10)                                    :: CHTIME      ! TIME
  CHARACTER(LEN=10)                                    :: CHZONE      ! TIME ZONE
  INTEGER                                              :: COLDATA = 0
  INTEGER                                              :: COLNAME = 0
  INTEGER                                              :: COLSIM = 0
  INTEGER, ALLOCATABLE, DIMENSION(:)                   :: DEFAULTOBS  ! position of CHARNAM in NAMSIMADJ
  INTEGER, ALLOCATABLE, DIMENSION(:)                   :: DEFSTRING_Q ! flags for questionable default strings
  INTEGER, ALLOCATABLE, DIMENSION(:)                   :: DEFNEEDED   ! flags for items indicated to be in process._ext but are not
  INTEGER                                              :: DEFTEMP = 0
  CHARACTER (LEN=50000)                                :: DLINE = ' ' ! Character string for text storage
  INTEGER                                              :: DONELINE = 0 ! FLAG = 2 IF DONE READING INFO FROM LINE
  DOUBLE PRECISION                                     :: DUMMY = 0.D0
  CHARACTER(LEN=200)                                   :: FN          !  data file name
  CHARACTER(LEN=200)                                   :: FNOS        !  process._ext file name
  INTEGER                                              :: GLOBAL_CNT = 0
  DOUBLE PRECISION                                     :: GLOB_DEFAULT = -1.D-99
  INTEGER                                              :: IBDT(8)     ! date/time info
  INTEGER                                              :: ICOL = 1    ! column to start looking for characters
  INTEGER                                              :: IFAIL = 0   ! error flag
  INTEGER                                              :: IFOUND = 0  ! FLAG indicating whether search was succesful
  CHARACTER(LEN=200)                                   :: INNAM      ! root file name
  INTEGER                                              :: ISCR = 0    ! # items written to scratch file
  INTEGER                                              :: ISTART = 0  ! start postion of word in text string
  INTEGER                                              :: ISTAT = 0   ! file opening status
  INTEGER                                              :: ISTOP = 0   ! stop postion of word in text string
  INTEGER                                              :: IOS = 0     ! file reading status
  INTEGER                                              :: IUOUT = 0    ! unit # out
  INTEGER                                              :: IUOS = 0    ! unit # process._ext
  INTEGER                                              :: IUSIMADJ = 0  ! unit # input-sim_adjust
  INTEGER                                              :: IUOUTSCR = 0 ! unit # out_src
  INTEGER                                              :: N = 0       ! counter
  INTEGER                                              :: N2TRACK = 0 ! counter
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)    :: NAMOS       ! names in process._ext
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)    :: NAMSIMADJ     ! names in input-sim_adjust
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)    :: NAMOUT       ! names in out
  INTEGER                                              :: NB = 0      ! place saver end of last search
  INTEGER                                              :: ND =0       ! #defaults in input-sim_adjust, not written to output
  INTEGER                                              :: NDEF =0     ! #default strings needed
  INTEGER                                              :: NDUM        ! DUMMY INTEGER
  INTEGER                                              :: NN =0       ! #items in input-sim_adjust w no default,
                                                                      ! written to output or fail if not in process._ext
  INTEGER                                              :: NO =0       ! #defaults to observations in input-sim_adjust
  INTEGER                                              :: NOS =0      ! #observations in process._ext
  INTEGER                                              :: NSKIP =0    ! #of lines to skip when reading process._ext file
  INTEGER                                              :: NSQ =0      ! #questionable default strings
  INTEGER                                              :: NTOT = 0    ! # total items in input-sim_adjust = NX+ND+NN
  INTEGER                                              :: NV =0       ! # of defaults to values in input-sim_adjust
  INTEGER                                              :: NX =0       ! # extractions to be written to out
  INTEGER                                              :: NXDONE=0    ! # extractions that have been assign values
  INTEGER                                              :: NTOTDONE=0  ! # input-sim_adjust names that have been assigned values
  INTEGER                                              :: NSTART = 0  ! rememebers starting value for that search
  INTEGER, ALLOCATABLE, DIMENSION(:)                   :: OUTPOSITION  ! order in which value is printed to out
  CHARACTER(LEN=200)                                   :: OUTNAM      ! root file name
  INTEGER, ALLOCATABLE, DIMENSION(:)                   :: OV          ! 1 if observation name rather than value
  CHARACTER(LEN=1)                                     :: OVCHAR      ! temp ov
  DOUBLE PRECISION                                     :: PM_DEFAULT = -1.D-99
  DOUBLE PRECISION                                     :: RDUM = 0.D0 ! DUMMY DOUBLE
  LOGICAL                                              :: RESET=.FALSE.!determines if full loop has already been searched for item
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)          :: SEQ         ! Simulated equivalent read from process._ext
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)          :: VALOUT       ! simulated equivalent to write to out
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)          :: VALSIMADJ     ! simulated equivalent for item in input-sim_adjust list
  INTEGER, ALLOCATABLE, DIMENSION(:)                   :: XD          ! 1 if to be written to out
  CHARACTER(LEN=1)                                     :: XDCHAR      ! temp xd
  ! formats
  10 FORMAT(//,1X,'Executing SIM_ADJUST:',/)
  322 FORMAT(A)
  520 FORMAT(1X,'ERROR: Blank line encountered in instruction file "',A,'"')
  890 FORMAT(1X,'“SIMULATED EQUIVALENT” "OBSERVATION NAME”')
  900 FORMAT(G15.7,1X,A)
  901 FORMAT(//,1X,A,/,5X,' WAS EVALUATED FOR ITEMS OMITTED, ',&
      'OR ASSIGNED A DEFAULT VALUE,',/,6X,'BY THE PROCESS MODEL')
  902 FORMAT(/,I10,' OBSERVATIONS WERE ASSIGNED THE GLOBAL DEFAULT VALUE,', &
             1PG15.7,/)
  903 FORMAT(/,I10,' OBSERVATION WAS ASSIGNED THE GLOBAL DEFAULT VALUE', &
             G15.7,/)
  904 FORMAT(/,2X,A)
  905 FORMAT(2X,A)
  906 FORMAT(2X,A,/)
  907 FORMAT(2X,I10,1X,' name of first item in the sequence: ',1X,A)
  ! Get start time
  CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IBDT)
  ! ****************************************************************************
  ! ****************************** INITIALIZE **********************************
  ! ****************************************************************************
  !
  WRITE(*,10)
  IFAIL = 0
  ! Command line should include 1) InputFileName 2) RootNameForOutputFiles
  INNAM = UTL_GETARG(1) ! Get command-line arg for rootname for files
  OUTNAM = UTL_GETARG(2) ! Get command-line arg for rootname for files
  !
  !   Open the input data file
  IUSIMADJ = UTL_GETUNIT(101,150)
  FN = TRIM(INNAM)
  OPEN(UNIT=IUSIMADJ,FILE=TRIM(FN),STATUS='OLD',ACTION='READ',IOSTAT=ISTAT)
  IF (ISTAT /= 0) THEN
    WRITE(*,'(1X,A,A,A,I6)') &
    'File open failed for sim_adjust input file ',TRIM(INNAM),' --status = ',ISTAT
    CALL UTL_STOP(' ')
  ENDIF
  !
  !   Open the process._ext  file
  READ(IUSIMADJ,*)FNOS
  IUOS = UTL_GETUNIT(101,150)
  OPEN(UNIT=IUOS,FILE=TRIM(FNOS),STATUS='OLD',ACTION='READ',IOSTAT=ISTAT)
  IF (ISTAT /= 0) THEN
    WRITE(*,'(1X,A,I6)') 'File open failed for process._ext --status = ',ISTAT
    CALL UTL_STOP(' ')
  ENDIF
  !
  !   Open the fn._output-sim_adjust  file for writing
  IUOUT = UTL_GETUNIT(101,150)
  FN = TRIM(OUTNAM)//'._output-sim_adjust'
  OPEN(UNIT=IUOUT,FILE=TRIM(FN),STATUS='UNKNOWN',ACTION='WRITE')
  IUOUTSCR = UTL_GETUNIT(101,150)
  FN = TRIM(OUTNAM)//'._output-sim_adjust_scr'
  OPEN(UNIT=IUOUTSCR,STATUS='SCRATCH')
  ! Read the input-sim_adjust file
  READ(IUSIMADJ,*,IOSTAT=IOS,ERR=100,END=200) &
      NSKIP,COLSIM,COLNAME,PM_DEFAULT,GLOB_DEFAULT
  IF(COLSIM <= 0 .OR. COLNAME <= 0 .OR. NSKIP < 0)THEN
    WRITE(IUOUT,*)
    WRITE(IUOUT,*)' ERROR: COL_SIM and COL_NAM must be positive '
    WRITE(IUOUT,*)'        NSKIP must be positive or zero'
    WRITE(IUOUT,*)
    WRITE(IUOUT,*)' CORRECT INPUT in input-sim_adjust file'
    WRITE(IUOUT,*)
    WRITE(IUOUT,*)' TERMINATING'
    WRITE(IUOUT,*)
    WRITE(*,*)
    WRITE(*,*)' ERROR: COL_SIM and COL_NAM must be must be positive'
    WRITE(*,*)'        NSKIP must be positive or zero'
    WRITE(*,*)
    WRITE(*,*)' CORRECT INPUT in input-sim_adjust file'
    WRITE(*,*)
    WRITE(*,*)' TERMINATING'
    WRITE(*,*)
    CALL UTL_ENDTIME(IBDT,IUOUT)
    CLOSE(UNIT=IUSIMADJ)
    CLOSE(UNIT=IUOS)
    CLOSE(UNIT=IUOUT)
    CALL UTL_STOP(' ')
  ENDIF
  ! count the number of items in the input-sim_adjust file
  NTOT = 0
  DO
    READ(IUSIMADJ,*,IOSTAT=IOS,END=201)
    NTOT = NTOT + 1
  ENDDO
  ! rewind the process._ext file, allocate arrays, skip headers, and read each item
  201 REWIND(IUSIMADJ)
  DO N=1,2
    READ(IUSIMADJ,*)
  ENDDO
  ! Allocate arrays for simadj list
  ALLOCATE (NAMSIMADJ(NTOT),XD(NTOT),OV(NTOT),VALSIMADJ(NTOT), &
           ASSGSIMADJ(NTOT),CHARSIMADJ(NTOT),OUTPOSITION(NTOT), &
           DEFAULTOBS(NTOT),DEFSTRING_Q(NTOT),DEFNEEDED(NTOT))
  !  Initialize
  ASSGSIMADJ = 0
  CHARSIMADJ = ' '
  DEFAULTOBS = 0
  DEFSTRING_Q = 0
  DEFNEEDED = 0
  NAMSIMADJ = ' '
  OUTPOSITION = 0
  OV = -999
  VALSIMADJ = -1.D-30
  XD = -999
  ! READ EACH ITEM IN input-sim_adjust and detremine how it is used
  DO N=1,NTOT
    READ(IUSIMADJ,*,IOSTAT=IOS,ERR=100,END=200)NAMSIMADJ(N),XDCHAR,OVCHAR
    ! convert to upper case
    IF(XDCHAR == 'l') THEN
      XDCHAR = 'L'
    ELSEIF(XDCHAR == 'x') THEN
      XDCHAR = 'X'
    ENDIF
    IF(XDCHAR == 'L') THEN
      XD(N) = 1
      NX = NX + 1
      OUTPOSITION(N) = NX
    ELSEIF(XDCHAR == 'X') THEN
      ND = ND + 1
    ELSE
      AMESSAGE= &
       ' 1st symbol in input-sim_adjust must be L or X (not case sensitive)'
      CALL UTL_WRITE_MESSAGE(IUOUT,'yes','yes','yes')
      CALL UTL_STOP(' ')
    ENDIF
    IF(OVCHAR == 'o') THEN
      OVCHAR = 'O'
    ELSEIF(OVCHAR == 'v') THEN
      OVCHAR = 'V'
    ELSEIF(OVCHAR == 'n') THEN
      OVCHAR = 'N'
    ENDIF
    IF(OVCHAR == 'V') THEN
      OV(N) = 0
      NV = NV + 1
    ELSEIF(OVCHAR == 'O') THEN
      OV(N) = 1
      NO = NO + 1
    ELSEIF(OVCHAR == 'N') THEN
      OV(N) = -1
      NN = NN + 1
    ELSE
      AMESSAGE= &
     ' 2nd symbol in input-sim_adjust must be O, V or N (not case sensitive)'
      CALL UTL_WRITE_MESSAGE(IUOUT,'yes','yes','yes')
      CALL UTL_STOP(' ')
    ENDIF
  ENDDO
  ! Allocate arrays for out list
  ALLOCATE (NAMOUT(NX),VALOUT(NX),ASSGOUT(NX))
  ! Read the process._ext file
  ! Skip header lines of process._ext file as indicated in input-sim_adjust input
  IF(NSKIP > 0) THEN
    DO N=1,NSKIP
      READ(IUOS,*,IOSTAT=IOS,ERR=102,END=201)
      CYCLE
      102 AMESSAGE='Read error in process._ext'
      WRITE(*,*)' Read error process._ext line: ',N
      WRITE(IUOUT,*)' Read error process._ext line: ',N
      CALL UTL_WRITE_MESSAGE(IUOUT,'yes','yes','yes')
      CALL UTL_STOP(' ')
    ENDDO
  ENDIF
  NOS = -1
  ! count the number of items in the process._ext file
  DO
    NOS = NOS + 1
    READ(IUOS,*,IOSTAT=IOS,ERR=103,END=202)
    CYCLE
    103 AMESSAGE='Read error in process._ext'
    WRITE(*,*)' Read error process._ext line: ',NOS+NSKIP
    WRITE(IUOUT,*)' Read error process._ext line: ',NOS+NSKIP
    CALL UTL_WRITE_MESSAGE(IUOUT,'yes','yes','yes')
    CALL UTL_STOP(' ')
  ENDDO
  ! rewind the process._ext file, allocate arrays, skip headers, and read each item
  202 REWIND(IUOS)
  ALLOCATE (NAMOS(NOS),SEQ(NOS))
  ! Skip header lines of process._ext file as indicated in input-sim_adjust input
  IF(NSKIP > 0) THEN
    DO N=1,NSKIP
      READ(IUOS,*)
    ENDDO
  ENDIF
  ! read simulated equivalents and their names as printed in process._ext file
  DO N=1,NOS
    READ(IUOS,322) DLINE
    IF (DLINE==' ') THEN
      IFAIL = 1
      AMESSAGE='Read error in process._ext'
      WRITE(*,*)' Read error process._ext line: ',N+NSKIP
      WRITE(IUOUT,*)' Read error process._ext line: ',N+NSKIP
      CALL UTL_WRITE_MESSAGE(IUOUT,'yes','yes','yes')
      CALL UTL_STOP(' ')
    ENDIF
    DLINE = TRIM(DLINE)//' '
    ICOL = 0
    ISTOP = 1
    COLDATA = 0
    DONELINE = 0
    DO
      COLDATA = COLDATA+1
      ICOL = ISTOP+1
      IF(COLDATA == COLSIM) THEN
        CALL UTL_RWORD(0,3,.FALSE.,ICOL,DLINE,ISTART,ISTOP,NDUM,RDUM)
        SEQ(N) = RDUM
        DONELINE = DONELINE+1
      ELSEIF(COLDATA == COLNAME) THEN
        CALL UTL_RWORD(0,0,.FALSE.,ICOL,DLINE,ISTART,ISTOP,NDUM,RDUM)
        NAMOS(N) = trim(DLINE(ISTART:ISTOP))
        DONELINE = DONELINE+1
      ELSE
        CALL UTL_RWORD(0,0,.FALSE.,ICOL,DLINE,ISTART,ISTOP,NDUM,RDUM)
      ENDIF
      IF(DONELINE == 2) EXIT
    ENDDO
  ENDDO
  ! search for NAMSIMADJ in NAMOS and ASSIGN VALUE
  ! Likely users will order obs in process._ext and input-sim_adjust the same, so to save time:
  NB = 0            ! holds last search location in the array of NAMOS
  ! LOOP THROUGH ALL NAMES IN USER PROVIDED input-sim_adjust file
  DO N = 1,NTOT
    RESET = .FALSE.
    NSTART = NB
    ! CHECK TO SEE IF VALUE WAS PRINTED TO process._ext FILE AND IF SO ASSIGN IT
    DO N2TRACK = 1, NOS
      NB = NB + 1
      IF (NB > NOS) THEN
        IF(RESET) EXIT
        NB = 1
        RESET = .TRUE.
      ENDIF
      IF(NB == NSTART) EXIT
      !DO ASSIGNMENTS IF THIS IS A MATCH
      IF (TRIM(NAMSIMADJ(N)) == TRIM(NAMOS(NB))) THEN
        IF(SEQ(NB) == PM_DEFAULT) THEN
          EXIT
        ELSE
          VALSIMADJ(N) = SEQ(NB)
          ASSGSIMADJ(N) = 1
          NTOTDONE = NTOTDONE + 1
          IF(XD(N) == 1) THEN
            VALOUT(OUTPOSITION(N)) = SEQ(NB)
            NAMOUT(OUTPOSITION(N)) = NAMSIMADJ(N)
            ASSGOUT(OUTPOSITION(N)) = 1
            NXDONE = NXDONE + 1
          ENDIF
          EXIT
        ENDIF
      ENDIF
    ENDDO
  ENDDO
  ! LOOK FOR ITEMS NOT FOUND IN THE process._ext file
  REWIND(IUSIMADJ)
  ! input-sim_adjust again read Char or Value as appropriate based on OV 0,1, or nothing -1
  ! Read process._ext file name and header
  READ(IUSIMADJ,*,IOSTAT=IOS,ERR=100,END=200)
  READ(IUSIMADJ,*,IOSTAT=IOS,ERR=100,END=200)
  DO N=1,NTOT
    IF(ASSGSIMADJ(N) < 1) THEN
      ! use value from process._ext, no default
      IF(OV(N) < 0) THEN
        READ(IUSIMADJ,*,IOSTAT=IOS,ERR=100)
      ! read default as a number from input-sim_adjust
      ELSEIF(OV(N) == 0) THEN
        READ(IUSIMADJ,*,IOSTAT=IOS,ERR=100) &
                                         NAMSIMADJ(N),XDCHAR,OVCHAR,VALSIMADJ(N)
        CHARSIMADJ(N) = ' '
        ASSGSIMADJ(N) = 1
        NTOTDONE = NTOTDONE + 1
        IF(XD(N) == 1) THEN
          VALOUT(OUTPOSITION(N)) = VALSIMADJ(N)
          NAMOUT(OUTPOSITION(N)) = NAMSIMADJ(N)
          ASSGOUT(OUTPOSITION(N)) = 1
          NXDONE = NXDONE + 1
          IF(VALOUT(OUTPOSITION(N)) == GLOB_DEFAULT) GLOBAL_CNT = GLOBAL_CNT + 1
        ENDIF
      ! read default as a character from input-sim_adjust
      ELSE
        READ(IUSIMADJ,*,IOSTAT=IOS,ERR=100) &
                                        NAMSIMADJ(N),XDCHAR,OVCHAR,CHARSIMADJ(N)
      ENDIF
    ELSE
      READ(IUSIMADJ,*,IOSTAT=IOS,ERR=100)
    ENDIF
  ENDDO
  ! IF ALL THE NECESSARY EXTRACTED VALUES HAVE NOT BEEN FILLED, CHECK DEFAULTS
  IF(NXDONE < NX) THEN
    ! search for CHARSIMADJ in NAMOS and ASSIGN VALUE
    NB = 0            ! holds last search location in the array of NAMSIMADJ
    ! LOOP THROUGH ALL NAMES IN USER PROVIDED input-sim_adjust file
    DO N = 1,NTOT
      IF(NTOTDONE == NTOT) EXIT
      RESET = .FALSE.
      NSTART = NB
      ! IF IT HAS NOT BEEN ASSIGNED, CHECK FOR THE VALUE OF ITS DEFAULT CHARSIMADJ
      IF(ASSGSIMADJ(N) < 1) THEN
        DO N2TRACK = 1, NTOT
          IFOUND = 0
          NB = NB + 1
          IF (NB.GT.NTOT) THEN
            IF(RESET) EXIT
            NB = 1
            RESET = .TRUE.
          ENDIF
          IF(NB == NSTART) EXIT
          !DO CALCULATIONS IF THIS IS A MATCH
          IF (CHARSIMADJ(N) == NAMSIMADJ(NB)) THEN
            IFOUND = 1
            ! NOTE POSITION OF THE DEFAULT IN THE input-sim_adjust LIST
            DEFAULTOBS(N) = NB
            IF(ASSGSIMADJ(NB) > 0) THEN
              VALSIMADJ(N) = VALSIMADJ(NB)
              ASSGSIMADJ(N) = 1
              NTOTDONE = NTOTDONE + 1
              IF(XD(N) == 1) THEN
                VALOUT(OUTPOSITION(N)) = VALSIMADJ(N)
                NAMOUT(OUTPOSITION(N)) = NAMSIMADJ(N)
                ASSGOUT(OUTPOSITION(N)) = 1
                NXDONE = NXDONE + 1
                IF(VALOUT(OUTPOSITION(N)) == GLOB_DEFAULT)  &
                                           GLOBAL_CNT = GLOBAL_CNT + 1
              ENDIF
            ENDIF
            EXIT
          ENDIF
        ENDDO
        IF(IFOUND == 0) THEN
          WRITE(*,*) ' Default for ',TRIM(NAMSIMADJ(N)), &
          ' not found in input-sim_adjust'
          WRITE(*,*) '   see line ',N+2
          WRITE(IUOUTSCR,*) '" Default for ',TRIM(NAMSIMADJ(N)), &
         ' not found in input-sim_adjust"'
          WRITE(IUOUTSCR,*) '"   see line ',N+2,'"'
          ISCR = ISCR + 1
          NDEF = NDEF + 1
          DEFNEEDED(NDEF) = N+2
          VALSIMADJ(N) = GLOB_DEFAULT
          ASSGSIMADJ(N) = 1
          NTOTDONE = NTOTDONE + 1
          IF(XD(N) == 1) THEN
            VALOUT(OUTPOSITION(N)) = GLOB_DEFAULT
            NAMOUT(OUTPOSITION(N)) = NAMSIMADJ(N)
            ASSGOUT(OUTPOSITION(N)) = 1
            NXDONE = NXDONE + 1
            GLOBAL_CNT = GLOBAL_CNT + 1
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDIF
  ! If all extractions to be printed are not assigned, find secondary defaults
  IF(NXDONE < NX) THEN
    ! LOOP THROUGH ALL NAMES IN USER PROVIDED input-sim_adjust file
    DO N = 1,NTOT
      IF(NXDONE == NX) EXIT
      ! IF IT HAS NOT BEEN ASSIGNED, SEEK ADDITIONAL DEFAULTS
      IF(ASSGSIMADJ(N) < 1) THEN
        ! 1st default in the input-sim_adjust file also has not been found
        ! seek subsequent defaults
        IF(OV(N) < 0) THEN
          VALSIMADJ(N) = GLOB_DEFAULT
          ASSGSIMADJ(N) = 1
          NTOTDONE = NTOTDONE + 1
          IF(XD(N) == 1) THEN
            VALOUT(OUTPOSITION(N)) = GLOB_DEFAULT
            NAMOUT(OUTPOSITION(N)) = NAMSIMADJ(N)
            ASSGOUT(OUTPOSITION(N)) = 1
            GLOBAL_CNT = GLOBAL_CNT + 1
            NXDONE = NXDONE + 1
          ENDIF
        ELSE
          ! DEFTEMP is the line number of the next default
          DEFTEMP = DEFAULTOBS(DEFAULTOBS(N))
          DO
            IF(DEFTEMP > 0 .AND. DEFTEMP <= NTOT) THEN
              IF(ASSGSIMADJ(DEFTEMP) > 0) THEN
                VALSIMADJ(N) = VALSIMADJ(DEFTEMP)
                ASSGSIMADJ(N) = 1
                NTOTDONE = NTOTDONE + 1
                IF(XD(N) == 1) THEN
                  VALOUT(OUTPOSITION(N)) = VALSIMADJ(N)
                  NAMOUT(OUTPOSITION(N)) = NAMSIMADJ(N)
                  ASSGOUT(OUTPOSITION(N)) = 1
                  NXDONE = NXDONE + 1
                  IF(VALOUT(OUTPOSITION(N)) == GLOB_DEFAULT)  &
                                           GLOBAL_CNT = GLOBAL_CNT + 1
                ENDIF
              ENDIF
            ELSE
              ! questionable default string, global default will be assigned
              NSQ = NSQ + 1
              DEFSTRING_Q(NSQ) = N+2
              VALSIMADJ(N) = GLOB_DEFAULT
              ASSGSIMADJ(N) = 1
              NTOTDONE = NTOTDONE + 1
              IF(XD(N) == 1) THEN
                VALOUT(OUTPOSITION(N)) = GLOB_DEFAULT
                NAMOUT(OUTPOSITION(N)) = NAMSIMADJ(N)
                ASSGOUT(OUTPOSITION(N)) = 1
                NXDONE = NXDONE + 1
                GLOBAL_CNT = GLOBAL_CNT + 1
              ENDIF
              EXIT
            ENDIF
            IF(ASSGSIMADJ(N) == 0) THEN
              DEFTEMP = DEFAULTOBS(DEFTEMP)
              CYCLE
            ELSE
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDIF
  ! WRITE VALUES TO fn._output-sim_adjust file
  WRITE(IUOUT,890)
  DO N=1,NX
    IF(ASSGOUT(N) <= 0) THEN
      WRITE(*,*)' Error for the input-sim_adjust x-item # ',N
      WRITE(IUOUT,*)' Error for the input-sim_adjust x-item # ',N
      AMESSAGE= &
      ' Likely programmer error some values were not assigned'
      CALL UTL_WRITE_MESSAGE(IUOUT,'yes','yes','yes')
      AMESSAGE= &
      ' Contact Eileen Poeter, epoeter@mines.edu'
      CALL UTL_WRITE_MESSAGE(IUOUT,'yes','yes','yes')
      CALL UTL_STOP(' ')
    ENDIF
    WRITE(IUOUT,900)VALOUT(N),NAMOUT(N)
  ENDDO
  WRITE(IUOUT,901)TRIM(FNOS)
  IF(ISCR > 0) THEN
    FN = TRIM(OUTNAM)//'._output-sim_adjust_scr'
    REWIND(UNIT=IUOUTSCR)
    WRITE(IUOUT,*)
    DO N=1,ISCR*2
      READ(IUOUTSCR,*)CHARSCR
      WRITE(IUOUT,*)TRIM(CHARSCR)
    ENDDO
  ENDIF
  IF(NSQ > 0) THEN
    AMESSAGE = &
    ' Sequence of defaults may be incorrect, so global default is used'
    WRITE(*,904)TRIM(AMESSAGE)
    WRITE(IUOUT,904)TRIM(AMESSAGE)
    AMESSAGE = &
    ' Errors stem from items on the following input-sim_adjust line #s '
    WRITE(*,905)TRIM(AMESSAGE)
    WRITE(IUOUT,905)TRIM(AMESSAGE)
    DO N=1,NSQ
      WRITE(*,907)DEFSTRING_Q(N),TRIM(NAMSIMADJ(DEFSTRING_Q(N)-3))
      WRITE(IUOUT,907)DEFSTRING_Q(N),TRIM(NAMSIMADJ(DEFSTRING_Q(N)-3))
    ENDDO
    AMESSAGE=' These were written at the end of the fn._output-sim_adjust file as well '
    WRITE(*,906)TRIM(AMESSAGE)
    AMESSAGE=' '
  ENDIF
  IF(NDEF > 0) THEN
    AMESSAGE= &
    ' Defaults may be needed for following input-sim_adjust line #s'
    WRITE(*,904)TRIM(AMESSAGE)
    WRITE(IUOUT,904)TRIM(AMESSAGE)
    AMESSAGE= &
    '   because the GLOBAL DEFAULT was used'
    WRITE(*,905)TRIM(AMESSAGE)
    WRITE(IUOUT,905)TRIM(AMESSAGE)
    DO N=1,NDEF
      WRITE(*,907)DEFNEEDED(N),TRIM(NAMSIMADJ(DEFNEEDED(N)-3))
      WRITE(IUOUT,907)DEFNEEDED(N),TRIM(NAMSIMADJ(DEFNEEDED(N)-3))
    ENDDO
    AMESSAGE=' These were written at the end of the fn._output-sim_adjust file as well '
    WRITE(*,906)TRIM(AMESSAGE)
    AMESSAGE=' '
  ENDIF
  IF(GLOBAL_CNT /= 1) THEN
    WRITE(*,902)GLOBAL_CNT,GLOB_DEFAULT
    WRITE(IUOUT,902)GLOBAL_CNT,GLOB_DEFAULT
  ELSE
    WRITE(*,903)GLOBAL_CNT,GLOB_DEFAULT
    WRITE(IUOUT,903)GLOBAL_CNT,GLOB_DEFAULT
  ENDIF
  100 IF(IOS /= 0) THEN
    AMESSAGE='Read error in input-sim_adjust'
    WRITE(*,*)' Read error input-sim_adjust line: ',N
    WRITE(IUOUT,*)' Read error input-sim_adjust line: ',N
    CALL UTL_WRITE_MESSAGE(IUOUT,'yes','yes','yes')
    CALL UTL_STOP(' ')
  ENDIF
  200 IF(IOS /= 0) THEN
    AMESSAGE='End of File reached in input-sim_adjust'
    WRITE(*,*)' End of File input-sim_adjust line: ',N
    WRITE(IUOUT,*)'  End of File input-sim_adjust line: ',N
    CALL UTL_WRITE_MESSAGE(IUOUT,'yes','yes','yes')
    CALL UTL_STOP(' ')
  ENDIF
  CALL UTL_ENDTIME(IBDT,IUOUT)
  CLOSE(UNIT=IUSIMADJ)
  CLOSE(UNIT=IUOS)
  CLOSE(UNIT=IUOUT)
  CALL UTL_STOP(' ')
END PROGRAM SIMADJ

