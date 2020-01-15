! ----------------------------------------------------------------------

      module somevar

      character(len=100) :: basename,rsfnam,y1fnam,osfnam,outfnam,     &
                            dmfnam,sufnam,wtfnam,spufnam,sunfnam,word1,&
                            wtnfnam,gsfnam,senfnam,rsnfnam,basenew,    &
                            osnfnam,gswfnam,suprifnam,wtprifnam,dmpfnam
      character(len=8)   :: prognam
	CHARACTER(LEN=12)  :: runmode
      character(len=11 ) :: version
      parameter (prognam='MF2K2DX',version='Version 1.0')
      integer            :: npred,iout,iverb,ierr,i,j,nhy1
      integer            :: i_dm,i_su,i_wt,i_SPU,i_sun,i_wtn
      integer            :: i_rs,i_os,i_y1,i_sc,i_rsn,i_osn,i_dmp
      integer            :: np,nd,nh,nq,mpr,ipr,nsets,nran,nall,ndmhy1
      integer            :: npn,ndn,nhn,nqn,mprn,iprn,nsetsn,nrann,nalln
      integer            :: ifstat,nvar
      logical            :: ybase,nbase
      double precision   :: var,varn,statind,statsf,fstatsi,fstatkgtnp
      integer, allocatable    :: nipr(:),isym(:),iplot(:),nplot(:),iplotp(:),iptr(:)
      character(len=12), allocatable  :: parnam(:),parnamn(:)
      character(len=20), allocatable  :: prednam(:),obsnam(:),obsnamn(:),priornam(:)
      double precision, allocatable :: su(:,:),wt(:,:),sun(:,:),wtn(:,:)
      double precision, allocatable :: x(:,:),w(:),wq(:,:),wp(:), &
                                       wtps(:,:),prm(:,:),z(:,:), &
                                       h(:),v(:),wqy1(:),xpri(:,:), &
                                       xn(:,:),wn(:),wqn(:,:)   , &
                                       hn(:),cov(:,:),covn(:,:)

      end module somevar

! ----------------------------------------------------------------------

      program mf2k2dx

! -- MJT,Sept2005
! -- MF2K2DX is a companion program to OPR-PPR.
! -- MF2K2DX processes the contents of the MODFLOW-2000 _RS, _Y1, _OS
! -- underscore files and writes JUPITER-API Data Exchange Format (DAF)
! -- files necessary for executing OPR-PPR

      use somevar
      use utilities, only   : UTL_DX_OPEN, UTL_DX_CLOSE,utl_case,Utl_stop
      use sensitivity, only : SEN_UEV_DX_WRITE_MATRIX
      use global_data, only : max_string_len

      character(len=1)   :: yn
      integer            :: flen,idum
      character(len=max_string_len) :: outnam

! -- initialize

      i_sc =0
      iout =1
      i_rs =2
      i_os =3
      i_y1 =4
      i_rsn=7
      i_osn=8
      i_dm =9
      i_su =10
      i_wt =11
      i_SPU=12
      i_sun=13
      i_wtn=14
	i_dmp=15
      ybase = .false.
      ddum = -999.0d0
      gsfnam='****.print'
      senfnam='****.sen'
      gswfnam=' '
      outfnam='mf2k2dx._out'
      basename=' '
      ierr=0

! -- message

      WRITE(*,10) 'PROGRAM '//TRIM(PROGNAM)//' '//TRIM(version)
   10 format(/,1x,27('='),/,1x,a,/,1x,27('='),/)

! -- warning about names

      WRITE(*,15) TRIM(PROGNAM),TRIM(PROGNAM)
   15 format(1x,'WARNING:', &
           /,1x,a,' requires unique names to be defined for observations, ', &
           /,1x,'predictions, and parameters. Due to the contents of the ', &
           /,1x,'files that ',a,' reads it is not able to identify where ', &
           /,1x,'the sequence of parameters in prediction and calibration', &
           /,1x,'sensitivities is inconsistent - PLEASE CHECK THOROUGHLY.',/)

! -- prompts

      write(*,20)
   20 format(2x,'Enter the RUNMODE for OPR-PPR: ',$)
      read(*,'(a)') word1
      word1=adjustl(word1)
      call utl_case(word1,runmode,1)
      call checkmode()

! -- identify the files we will read for existing obs and prior

      write(*,40)
   40 format(/,2x,'Is there a single base name for all MF2K files',    &
             /,2x,'that correspond to existing obs and prior?: '       )
      read(*,'(a)') YN
      if(yn.eq.'Y'.or.yn.eq.'y') then
        ybase = .true.
        write(*,50)
   50   format(4x,'Enter the base name for these MF2K files: ',$)
        read(*,'(a)') basename
        basename = adjustl(basename)
        y1fnam=trim(basename)//'._y1'
        osfnam=trim(basename)//'._os'
        rsfnam=trim(basename)//'._rs'
      else
        write(*,60) '_RS file for existing observations: '
        read(*,'(a)') rsfnam
        rsfnam=adjustl(rsfnam)
        write(*,60) '_OS file for existing observations: '
        read(*,'(a)') osfnam
        osfnam=adjustl(osfnam)
        write(*,60) '_Y1 file for the predictions:       '
        read(*,'(a)') y1fnam
        y1fnam=adjustl(y1fnam)
   60   format(4x,'Enter the name of the MF2K ',a37,$)
      end if

! -- identify files we will produce and the names for them

      if(ybase)then
        dmfnam=trim(basename)//'._dm'
        dmpfnam=trim(basename)//'._dmp'
        sufnam=trim(basename)//'._su'
        suprifnam=trim(basename)//'._supri'
        wtfnam=trim(basename)//'._wt'
        wtprifnam=trim(basename)//'._wtpri'
        spufnam=trim(basename)//'._spu'
      else
        flen=index(rsfnam,'.')
        if(flen.eq.0)call utl_stop(' STOP: Error parsing RS file name')
        dmfnam=rsfnam(1:flen-1)//'._dm'
        dmpfnam=rsfnam(1:flen-1)//'._dmp'
        sufnam=rsfnam(1:flen-1)//'._su'
        suprifnam=rsfnam(1:flen-1)//'._supri'
        wtfnam=rsfnam(1:flen-1)//'._wt'
        wtprifnam=rsfnam(1:flen-1)//'._wtpri'
        flen=index(y1fnam,'.')
        if(flen.eq.0)call utl_stop(' STOP: Error parsing Y1 file name')
        spufnam=y1fnam(1:flen-1)//'._spu'
      end if

! -- identify the files we will read for potential new observations

      if(runmode.eq.'ADD') then
        write(*,70)
   70   format(/,2x,'Is there a single base name for all MF2K files',  &
               /,2x,'that correspond to possible new observations?: '  )
        read(*,'(a)') YN
        if(yn.eq.'Y'.or.yn.eq.'y') then
          nbase = .true.
          write(*,80)
   80     format(4x,'Enter the base name for these MF2K files: ',$)
          read(*,'(a)') basenew
          basenew = adjustl(basenew)
          rsnfnam=trim(basenew)//'._rs'
          osnfnam=trim(basenew)//'._os'
!          write(*,*) rsnfnam,osnfnam
        else
          write(*,90) '_RS file for new observations: '
          read(*,'(a)') rsnfnam
          rsnfnam=adjustl(rsnfnam)
          write(*,90) '_OS file for new observations: '
          read(*,'(a)') osnfnam
          osnfnam=adjustl(osnfnam)
   90     format(4x,'Enter the name of the MF2K ',a31,$)
        end if
! -- identify files we will produce and the names for them
        if(nbase)then
          sunfnam=trim(basenew)//'._sun'
          wtnfnam=trim(basenew)//'._wtn'
        else
          flen=index(rsnfnam,'.')
          if(flen.eq.0)call utl_stop('STOP: Error parsing RS file name')
          sunfnam=rsnfnam(1:flen-1)//'._sun'
          wtnfnam=rsnfnam(1:flen-1)//'._wtn'
        end if
      end if
      if(runmode.eq.'ADD-NODE') then
        write(*,95)
   95   format(/,2x,'For the grid sensitivities please: ',/)
        write(*,100) 'Grid Sensitivity File:      '
        read(*,'(a)') gsfnam
        gsfnam=adjustl(gsfnam)
        write(*,105) 'file listing nodes weights -',                   &
             ' (Hit "RETURN" to simply use uniform weights): '
        read(*,'(a)') gswfnam
        gswfnam=adjustl(gswfnam)
        write(*,100) 'Sensitivity Process File:   '
        read(*,'(a)') senfnam
        senfnam=adjustl(senfnam)
  100   format(4x,'Enter the name of the MF2K ',a28,$)
  105   format(4x,'Enter the name of the ',a28,/,4x,a56,$)
      end if
      write(*,*)

! -- open the _RS file and store the relevant portions

      call openfile(i_rs,1,rsfnam,i_sc)
      call read_rs()
      call openfile(i_rs,2,rsfnam,i_sc)
      write(*,110) trim(rsfnam)
  110 format(2x,'  Read file: ',T22,a)

! -- open the _OS file and store the relevant portions

      call openfile(i_os,1,osfnam,i_sc)
      call read_os()
      call openfile(i_os,2,osfnam,i_sc)
      write(*,110) trim(osfnam)

! -- open the _Y1 file and store the relevant portions

      call openfile(i_y1,1,y1fnam,i_sc)
      call read_y1()
      call openfile(i_y1,2,y1fnam,i_sc)
      write(*,110) trim(y1fnam)

! -- open the _RS and _OS files for new obs and store relevant portions

      if(runmode.eq.'ADD')then
        call openfile(i_rsn,1,rsnfnam,i_sc)
        call read_rsn()
        call openfile(i_rsn,2,rsnfnam,i_sc)
        write(*,110) trim(rsnfnam)
        call openfile(i_osn,1,osnfnam,i_sc)
        call read_osn()
        call openfile(i_osn,2,osnfnam,i_sc)
        write(*,110) trim(osnfnam)
      end if

! -- open the file which forms a basis for an OPR-PPR input file

      call openfile(iout,3,outfnam,i_sc)
      call wrtopr()
      call openfile(iout,2,outfnam,i_sc)
      write(*,120) trim(outfnam)
  120 format(2x,'  Written file: ',T22,a)

! -- write the model data file (with some dummy values)

      call openfile(i_dm,3,dmfnam,i_sc)
      call write_dm()
      call openfile(i_dm,2,dmfnam,i_sc)
      write(*,120) trim(dmfnam)

! -- write the dummy DMP data file (with some dummy values)

      call openfile(i_dmp,3,dmpfnam,i_sc)
      call write_dmp()
      call openfile(i_dmp,2,dmpfnam,i_sc)
      write(*,120) trim(dmpfnam)

! -- write the _SU file with sensitivities of known observations

      outnam=sufnam(1:len(trim(adjustl(sufnam)))-4)
      idum=UTL_DX_OPEN(outnam,'_su')
      call sen_uev_dx_write_matrix(idum,nd,np,np,obsnam,iptr,parnam,x,iplot)
      idum=UTL_DX_close('_su')
      write(*,120) trim(sufnam)

! -- write the _WT file with weights of known observations

      call openfile(i_wt,3,wtfnam,i_sc)
      call write_wt()
      call openfile(i_wt,2,wtfnam,i_sc)
      write(*,120) trim(wtfnam)

! -- write the _SUPRI file with sensitivities of known prior information
! -- write the _WTPRI file with weights of known prior information

      if(mpr+ipr.gt.0)then
        outnam=suprifnam(1:len(trim(adjustl(suprifnam)))-7)
        idum=UTL_DX_OPEN(outnam,'_supri')
        call sen_uev_dx_write_matrix(idum,ipr+mpr,np,np,priornam,iptr,parnam,xpri,iplotp)
        idum=UTL_DX_close('_su')
        write(*,120) trim(suprifnam)
        call openfile(i_wt,3,wtprifnam,i_sc)
        call write_wtp()
        call openfile(i_wt,2,wtprifnam,i_sc)
        write(*,120) trim(wtprifnam)
      end if

! -- write the _SPU file with sensitivities of predictions

      outnam=spufnam(1:len(trim(adjustl(spufnam)))-5)
      idum=UTL_DX_OPEN(outnam,'_spu')
      call sen_uev_dx_write_matrix(idum,npred,np,np,prednam,iptr,parnam,z,isym)
      idum=UTL_DX_close('_spu')
      write(*,120) trim(spufnam)

! -- write the _SUN and _WTN files for new observations

      if(runmode.eq.'ADD')then
        outnam=sunfnam(1:len(trim(adjustl(sunfnam)))-5)
        idum=UTL_DX_OPEN(outnam,'_sun')
        call sen_uev_dx_write_matrix(idum,ndn,np,np,obsnamn,iptr,parnam,xn,nplot)
        idum=UTL_DX_close('_sun')
        write(*,120) trim(sunfnam)
        ! weights
        call openfile(i_wtn,3,wtnfnam,i_sc)
        call write_wtn()
        call openfile(i_wtn,2,wtnfnam,i_sc)
        write(*,120) trim(wtnfnam)
      end if

! -- all done

      write(*,900) 'Program '//trim(prognam)//': Successful Execution'
  900 format(/,1x,38('='),/,1x,a,/,1x,38('='),/)
      stop

! -- problems

      stop
      end program mf2k2dx

! ----------------------------------------------------------------------

      subroutine read_rs()
! -- MJT,Sept2005
! -- read the MF2K _RS file and store entries. Modified after RESAN2K
      use utilities
      use somevar
      implicit none
! -- read the file
      read(i_rs,*,iostat=ierr) np,nd,nh,nq,mpr,ipr,nsets,nran,var
      if(ierr.ne.0) call utl_stop(' STOP: Error reading HEADER in _RS')
      nall = nh+nq+mpr+ipr
      allocate (x(np,nd),cov(np,np),parnam(np),iptr(np),stat=ierr)
      if(ierr.ne.0) call utl_stop(' STOP: Error allocating for _RS ')
      iptr=1                       !assumes all are adjustable
! -- parameter names
      read(i_rs,1350,iostat=ierr) (parnam(i),i=1,np)
 1350 format(6(a10,1x))
      if(ierr.ne.0) call utl_stop(' STOP: Error reading PARNAM in _RS')
! -- variance-covariance matrix on the parameters (not used)
      do j=1,np
        read(i_rs,1180,iostat=ierr) (cov(i,j),i=j,np)
 1180   format(16f25.0)
        DO i=j,np
          cov(j,i)=cov(i,j)
        end do
      end do
! -- sqrt of weight for observations with a diagonal weight matrix
      if (nh.gt.0) then
        allocate(w(nh),stat=ierr)
        if(ierr.ne.0) call utl_stop(' STOP: Error allocating W')
        read(i_rs,1185,iostat=ierr) (w(i),i=1,nh)
 1185   format(16f15.0)
        if(ierr.ne.0) call utl_stop(' STOP: Error reading W in _RS')
        w = w**2.0d0
      end if
! -- sqrt of weights on observations with a full weight matrix
      if (nq.gt.0) then
        allocate(wq(nq,nq),stat=ierr)
        if(ierr.ne.0) call utl_stop(' STOP: Error allocating WQ')
        do i = 1, nq
          read (i_rs,1185,iostat=ierr) (wq(i,j),j=1,nq)
          if(ierr.ne.0) call utl_stop(' STOP: Error reading WQ in _RS')
        end do
        wq = wq**2.0d0
      end if
! -- sensitivities of obs to parameters
      do j=1,nd
        read(i_rs,1185,iostat=ierr) (x(i,j),i=1,np)
        if(ierr.ne.0) call utl_stop(' STOP: Error reading X in _RS')
      end do
! -- prior information from equations
      if(ipr+mpr.gt.0) allocate(xpri(np,ipr+mpr),stat=ierr)
      if (mpr.gt.0) then
        allocate(wp(np),prm(np,mpr),stat=ierr)
        xpri=0.d0
        if(ierr.ne.0) call utl_stop(' STOP: Error allocating PRIOR')
! -- these are the actual (not sqrt) weights on prior????????????????????????????????
        do i=1,mpr
!          read(i_rs,1020,iostat=ierr) (prm(j,i),j=1,np),wp(i)
          read(i_rs,1020,iostat=ierr) (xpri(j,i),j=1,np),wp(i)
          wp(i)=wp(i)*wp(i)
 1020     format (8f15.0)
          if(ierr.ne.0) call utl_stop(' STOP: Error reading PRM in _RS')
        end do
      endif
! -- prior information with full weight matrix
      if (ipr.gt.0) then
        allocate(nipr(ipr),wtps(ipr,ipr),stat=ierr)
        if(ierr.ne.0) call utl_stop(' STOP: Error allocating PRIOR')
        read(i_rs,1420,iostat=ierr) (nipr(i),i=1,ipr)
 1420   format(16i5)
        if(ierr.ne.0) call utl_stop(' STOP: Error reading NIPR in _RS')
        do i=1,ipr
          xpri(nipr(i),i)=1.0d0
        end do
! -- these are the sqrt of the weights on prior
        do i=1,ipr
          read(i_rs,1020,iostat=ierr) (wtps(i,j),j=1,ipr)
          if(ierr.ne.0) call utl_stop('STOP: Error reading WTPS in _RS')
        end do
        wtps=wtps**2.0d0
      endif
! -- all done
      return
      end subroutine read_rs

! ----------------------------------------------------------------------

      subroutine read_os()
! -- MJT,Sept2005
! -- read the MF2K _OS file and store entries.
      use utilities
      use somevar
      implicit none
      real     :: rval
      integer  :: iobs
      ierr=0
      write(*,*) nd
      allocate(obsnam(nd),priornam(ipr+mpr),iplot(nd),iplotp(ipr+mpr),stat=ierr)
      if(ierr.ne.0) call utl_stop(' STOP: Error allocating OBSNAM')
      iplot=1
! -- read the file
      iobs=0
      do iobs=1,nd        !observations
        read(i_os,*,end=90,iostat=ierr) rval,rval,                     &
                                        iplot(iobs),obsnam(iobs)
        if(ierr.ne.0) call utl_stop(' STOP: Error reading _OS file')
      end do
      do iobs=1,ipr+mpr   !prior info
        read(i_os,*,end=90,iostat=ierr) rval,rval,                     &
                                        iplotp(iobs),priornam(iobs)
        if(ierr.ne.0) call utl_stop(' STOP: Error reading _OS file')
      end do
! -- all done
      return
  90  call utl_stop(' STOP: Error reading _OS file')
      end subroutine read_os

! ----------------------------------------------------------------------

      subroutine read_y1()
! -- MJT,Sept2005
! -- read the MF2K _Y1 file and store entries. Modified after RESAN2K
      use utilities
      use somevar
      implicit none
      ierr=0
   10 format(/,1x,a,a,a,a)
! -- read the header and check dimension
      read (i_y1,500,iostat=ierr) nvar, npred, nhy1, ifstat
  500 format(4i10)
      if(ierr.ne.0) call utl_stop(' STOP: Error reading HEADER in _Y1')
      if(nvar.ne.np) then
        write(*,10) 'NVAR ',trim(y1fnam),'NPE ',trim(rsfnam)
        call utl_stop(' STOP: Inconsistent problem dimensions')
      end if
      ndmhy1 = npred - nhy1
! -- user provided stats (not used)
      if (ifstat.gt.0) then
        read(i_y1,*,iostat=ierr) statind, statsf, fstatsi, fstatkgtnp
        if(ierr.ne.0) call utl_stop(' STOP: Error reading STATS in _Y1')
      end if
! -- allocate (much of this is loaded but not used)
      allocate(prednam(npred),isym(npred),h(npred),z(nvar,npred),      &
               stat=ierr)
      if(ierr.ne.0) call utl_stop(' STOP: Error allocating for _Y1')
! -- pred conditions for intervals on differences (produced if iycflg=1)
      read(i_y1,610,iostat=ierr) (prednam(i),i=1,npred)
  610 format (6(a12,1x))
      if(ierr.ne.0) call utl_stop(' STOP: Error reading PREDNAM in _Y1')
      read(i_y1,615,iostat=ierr) (isym(i),i=1,npred)
  615 format (16i5)
      if(ierr.ne.0) call utl_stop(' STOP: Error reading ISYM in _Y1')
      read(i_y1,550,iostat=ierr) (h(i),i=1,npred)
  550 format (6f13.0)
      if(ierr.ne.0) call utl_stop(' STOP: Error reading H in _Y1')
      if(nhy1.gt.0) then
        allocate(v(nhy1),stat=ierr)
        if(ierr.ne.0) call utl_stop(' STOP: Error allocating V in _Y1')
        read (i_y1,505,iostat=ierr) (v(i),i=1,nhy1)
  505   format (8f15.0)
        if(ierr.ne.0) call utl_stop(' STOP: Error reading V in _Y1')
      end if
      if (ndmhy1.gt.0) then
        allocate(wqy1(ndmhy1),stat=ierr)
        if(ierr.ne.0)call utl_stop('STOP: Error allocating WQY1 in _Y1')
! -- read diagonal terms of full weight matrix for flows (not used)
        read (i_y1,505,iostat=ierr) (wqy1(j),j=1,ndmhy1)
        if(ierr.ne.0) call utl_stop(' STOP: Error reading WQY1 in _Y1')
      endif
! -- read sensitivities
      do j = 1, npred
        read (i_y1,550) (z(i,j),i=1,nvar)
      end do
! -- all done
      return
      end subroutine read_y1

! ----------------------------------------------------------------------

      subroutine read_rsn()
! -- MJT,Sept2005
! -- read MF2K _RS file and store entries pertaining to new observations
! -- does not read the later portions of the file that pertain to prior
      use utilities
      use somevar
      implicit none
      ierr=0
! -- read the file
      read(i_rsn,*,iostat=ierr) npn,ndn,nhn,nqn,mprn,                  &
                                iprn,nsetsn,nrann,varn
      nalln = nhn+nqn+mprn+iprn
      if(ierr.ne.0) call utl_stop(' STOP: Error reading HEADER in _RSn')
      if(npn.ne.np) then
        write(*,10) trim(rsfnam),np,trim(rsnfnam),npn
   10   format(/,1x,'ERROR: No. of parameters read from file ',a,1x,i4,&
               /,1x,'       differs from that read from file ',a,1x,i4)
        call utl_stop()
      end if
      allocate (xn(npn,nalln), covn(npn,npn), parnamn(npn),stat=ierr)
      if(ierr.ne.0) call utl_stop(' STOP: Error allocating for _RSn ')
! -- parameter names
      read(i_rsn,1350,iostat=ierr) (parnamn(i),i=1,npn)
 1350 format(6(a10,1x))
      if(ierr.ne.0)call utl_stop(' STOP: Error reading PARNAMN in _RSn')
! -- check the parameter names jive
      call checkpar()
! -- variance-covariance matrix on the parameters (not used)
      do j=1,npn
        read(i_rsn,1180,iostat=ierr) (covn(i,j),i=j,npn)
 1180   format(16f25.0)
        DO i=j,npn
          cov(j,i)=cov(i,j)
        end do
      end do
! -- sqrt of weight for observations with a diagonal weight matrix
      if (nhn.gt.0) then
        allocate(wn(nhn),stat=ierr)
        if(ierr.ne.0) call utl_stop(' STOP: Error allocating WN')
        read(i_rsn,1185,iostat=ierr) (wn(i),i=1,nhn)
 1185   format(16f15.0)
        if(ierr.ne.0) call utl_stop(' STOP: Error reading WN in _RS')
        wn = wn**2.0d0
      end if
! -- sqrt of weights on observations with a full weight matrix
      if (nqn.gt.0) then
        allocate(wqn(nqn,nqn),stat=ierr)
        if(ierr.ne.0) call utl_stop(' STOP: Error allocating WQN')
        do i = 1, nqn
          read (i_rsn,1185,iostat=ierr) (wqn(i,j),j=1,nqn)
          if(ierr.ne.0) call utl_stop(' STOP: Error reading WQN in _RS')
        end do
        wqn = wqn**2.0d0
      end if
! -- sensitivities of obs to parameters
      do j=1,ndn
        read(i_rsn,1185,iostat=ierr) (xn(i,j),i=1,npn)
        if(ierr.ne.0) call utl_stop(' STOP: Error reading XN in _RS')
      end do
      return
      end subroutine read_rsn

! ----------------------------------------------------------------------

      subroutine read_osn()
! -- MJT,Sept2005
! -- read the MF2K _OS file and store entries for new observations
      use utilities
      use somevar
      implicit none
      real     :: rval
      integer  :: iobs
      ierr=0
      allocate(obsnamn(ndn),nplot(ndn),stat=ierr)
      if(ierr.ne.0) call utl_stop(' STOP: Error allocating OBSNAMN')
      nplot=1
! -- read the file
      iobs=0
      do
        iobs=iobs+1
        read(i_osn,*,end=90,iostat=ierr) rval,rval,                    &
                                         nplot(iobs),obsnamn(iobs)
        if(ierr.ne.0) call utl_stop(' STOP: Error reading _OS file')
      end do
   90 iobs=iobs-1
      if(iobs.ne.ndn) then
        write(*,100) trim(osnfnam),trim(rsnfnam)
  100   format(/,2x,'No. of observations read from ',a,' does not ',   &
               /,2x,'equal ND read from _RS file ',a)
        call utl_stop(' STOP: Error reading _OS file')
      end if
! -- all done
      return
      end subroutine read_osn

! ----------------------------------------------------------------------

      subroutine write_dm()
! -- MJT,Sept2005
! -- write a dummy model data file suitable for OPR-PPR execution
      use utilities
      use somevar
      implicit none
! -- formats
   10 Format(1x,'"MODEL NAME: "',A)
   20 Format(1x,'"MODEL LENGTH UNITS: "',A)
   30 Format(1x,'"MODEL MASS UNITS: "',A)
   40 Format(1x,'"MODEL TIME UNITS: "',A)
   50 Format(1x,'"NUMBER ESTIMATED PARAMETERS: "',I4)
   60 Format(1x,'"ORIGINAL NUMBER ESTIMATED PARAMETERS: "',I4)
   70 Format(1x,'"TOTAL NUMBER PARAMETERS: "',I4)
   80 Format(1x,'"NUMBER OBSERVATIONS INCLUDED: "',I4)
   90 Format(1x,'"NUMBER OBSERVATIONS PROVIDED: "',I4)
  100 Format(1x,'"NUMBER PRIOR: "',I4)
  105 Format(1x,'"REGRESSION CONVERGED: "',A)
  110 Format(1x,'"CALCULATED ERROR VARIANCE: "',f18.8)
  120 Format(1x,'"STANDARD ERROR OF THE REGRESSION: "',f18.8)
  125 Format(1x,'"MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION - DEPENDENTS (MLOFD): "',f18.8)
  130 Format(1x,'"MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION - DEPENDENTS AND PRIOR (MLOFDP): "',f18.8)
  140 Format(1x,'"AICc (MLOFD + AICc PENALTY): "',f18.8)
  150 Format(1x,'"BIC (MLOFD + BIC PENALTY): "',f18.8)
  160 Format(1x,'"HQ (MLOFD + HQ PENALTY): "',f18.8)
  170 Format(1x,'"KASHYAP (MLOFD + KASHYAP PENALTY): "',f18.8)
  180 Format(1x,'"LN DETERMINANT OF FISHER INFORMATION MATRIX: "',f18.8)
  190 Format(1x,'"RN2 DEPENDENTS: "',f18.8)
  200 Format(1x,'"RN2 DEPENDENTS AND PRIOR: "',f18.8)
  210 Format(1x,'"NUMBER OF ITERATIONS: "',I6)
  212 Format(1x,'"KASHYAP (MLOFDP + KASHYAP PENALTY wPri): "',f18.8)
  214 Format(1x,'"LN DETERMINANT OF FISHER INFORMATION MATRIX wPri: "',f18.8)
  216 Format(1x,'"SOME SENSITIVITIES BY FORWARD DIFFERENCE PERTURBATION: "',A)
  230 Format(/,1x,'#NOTE: THIS IS A DUMMY _DM FILE CONSTRUCTED BY ',   &
           /,1x,'#MF2K2DX FOR PURPOSES OF EXECUTING OPR-PPR.',   &
           /,1x,'#THE FILE WAS CONSTRUCTED FROM THE CONTENTS OF',      &
           /,1x,'#MODFLOW-2000 OUTPUT FILES: SOME VARIABLES DO NOT APPLY')
! -- write it
      if(ybase)write(i_dm,10) trim(basename)
      if(.not.ybase)write(i_dm,10) ' "MF2K2DX_OUTPUT"'
      write(i_dm,20) ' "N/A"'
      write(i_dm,30) ' "N/A"'
      write(i_dm,40) ' "N/A"'
      write(i_dm,50) np
      write(i_dm,60) np
      write(i_dm,70) np
      write(i_dm,80) nd
      write(i_dm,90) nd
      write(i_dm,100) mpr+ipr
      write(i_dm,105) ' "YES" '
      write(i_dm,110) var
      write(i_dm,120) dsqrt(var)
      write(i_dm,125) -999.0d0
      write(i_dm,130) -999.0d0
      write(i_dm,140) -999.0d0
      write(i_dm,150) -999.0d0
      write(i_dm,160) -999.0d0
      write(i_dm,170) -999.0d0
      write(i_dm,180) -999.0d0
      write(i_dm,190) -999.0d0
      write(i_dm,200) -999.0d0
      write(i_dm,210) -999
      write(i_dm,212) -999.0d0
      write(i_dm,214) -999.0d0
      write(i_dm,216) ' "YES" '
      write(i_dm,230)
! -- all done
      return
      end subroutine write_dm

! ----------------------------------------------------------------------

      subroutine write_wt()
! -- MJT,Sept2005
! -- write compressed-format weight matrix assuming matrix is square
! -- does not write the square root of the weight matrix at the bottom
      use somevar
      implicit none
      integer :: nnz,ipos,ncol,nrow,iniq,iniwp,iniwtps,one
! -- initialize
      ncol=nd
      nrow=ncol
      nnz=0
      iniq=nh
      one=1
! -- determine nnz
      if(nh.gt.0) nnz=nnz+nh
      if(nq.gt.0) then
        do i = 1, nq
          do j = 1, nq
            if(wq(i,j).ne.0.0d0) nnz=nnz+1
          end do
        end do
      end if
! -- write headers
      write(i_wt,'(I10)') one
      write(i_wt,'(a)') ' COMPRESSEDMATRIX'
      write(i_wt,'(3(i10))') nnz,nrow,ncol
! -- write the weight matrix
      if(nh.gt.0.)then
        do i=1,nh
          if(w(i).ne.0.0d0) then
            ipos = ((i-1)*ncol)+i
            write(i_wt,*) ipos,w(i)
          end if
        end do
      end if
      if(nq.gt.0)then
        do i=1,nq
          do j=1,nq
            if(wq(i,j).ne.0.0d0) then
              ipos = ((i+iniq-1)*ncol)+j+iniq
              write(i_wt,*) ipos,wq(i,j)
            end if
          end do
        end do
      end if
! -- all done
      return
      end subroutine write_wt

! ----------------------------------------------------------------------

      subroutine write_dmp()
	use somevar
	implicit none
	integer :: one
! -- MJT,Aug2007
! -- write dummy DMP file
	one=1
      write(i_dmp,*) '"NUMBER OF PREDICTION GROUPS = "',one
      write(i_dmp,*) '"NUMBER OF PARAMETERS FOR PREDICTIVE EVALUATION = "',np
! -- all done
      return
      end subroutine write_dmp

! ----------------------------------------------------------------------

      subroutine write_wtp()
! -- MJT,Sept2005
! -- write compressed-format weight matrix assuming matrix is square
! -- does not write the square root of the weight matrix at the bottom
      use somevar
      implicit none
      integer :: nnz,ipos,ncol,nrow,iniq,iniwp,iniwtps,one
! -- initialize
      ncol=mpr+ipr
      nrow=ncol
      nnz=0
      iniwp=nh+nq
      iniwtps=nh+nq+mpr
      one=1
! -- determine nnz
      if(mpr.gt.0) then
        do i=1,mpr
          if(wp(i).ne.0.0d0) nnz=nnz+1
        end do
      endif
      if (ipr.gt.0) then
        do i=1,ipr
          do j=1,ipr
            if(wtps(i,j).ne.0.0d0) nnz=nnz+1
          end do
        end do
      endif
! -- write headers
      write(i_wt,'(I10)') one
      write(i_wt,'(a)') ' COMPRESSEDMATRIX'
      write(i_wt,'(3(i10))') nnz,nrow,ncol
! -- write the weight matrix
      if(mpr.gt.0)then
        do i=1,mpr
          if(wp(i).ne.0.0d0) then
            ipos = ((i+iniwp-1)*ncol)+i+iniwp
            write(i_wt,*) ipos,wp(i)
          end if
        end do
      end if
      if(ipr.gt.0)then
        do i=1,ipr
          do j=1,ipr
            if(wtps(i,j).ne.0.0d0) then
              ipos = ((i+iniwtps-1)*ncol)+j+iniwtps
              write(i_wt,*) ipos,wtps(i,j)
            end if
          end do
        end do
      end if
! -- all done
      return
      end subroutine write_wtp

! ----------------------------------------------------------------------

      subroutine write_wtn()
! -- MJT,Sept2005
! -- write compressed-format weight matrix assuming matrix is square
! -- does not write the square root of the weight matrix at the bottom
      use utilities
      use somevar
      implicit none
      integer :: nnz,ipos,ncol,nrow,iniq,one
! -- initialize
      ncol=ndn
      iniq=nhn
      nrow=ncol
      nnz=0
      one=1
! -- determine nnz
      if(nhn.gt.0) nnz=nnz+nhn
      if(nqn.gt.0) then
        do i = 1, nqn
          do j = 1, nqn
            if(wqn(i,j).ne.0.0d0) nnz=nnz+1
          end do
        end do
      end if
! -- write headers
      write(i_wt,'(I10)') one
      write(i_wtn,'(a)') ' COMPRESSEDMATRIX'
      write(i_wtn,'(3(i10))') nnz,nrow,ncol
! -- write the weight matrix
      if(nhn.gt.0.)then
        do i=1,nhn
          if(wn(i).ne.0.0d0) then
            ipos = ((i-1)*ncol)+i
            write(i_wtn,*) ipos,wn(i)
          end if
        end do
      end if
      if(nqn.gt.0)then
        do i=1,nqn
          do j=1,nqn
            if(wqn(i,j).ne.0.0d0) then
              ipos = ((i+iniq-1)*ncol)+j+iniq
              write(i_wtn,*) ipos,wqn(i,j)
            end if
          end do
        end do
      end if
! -- all done
      return
      end subroutine write_wtn

! ----------------------------------------------------------------------

      subroutine checkpar()
      use utilities
      use somevar
      implicit none
      do j=1,np
        if(utl_samename(parnamn(j),parnam(j))) cycle
        write(*,10) trim(rsnfnam),trim(parnamn(j)),                    &
                    trim(rsfnam),trim(parnam(j))
   10   format(/,1x,'PARNAM read from ',a,' - ',a,' differs from',     &
               /,1x,'PARNAM read from ',a,' - ',a)
        call utl_stop(' STOP: Error matching PARNAMN with PARNAM')
      end do
      return
      end subroutine checkpar

! ----------------------------------------------------------------------

      subroutine checkmode()
      use utilities
      use somevar
      implicit none
      if(runmode.eq.'OPRADDNODE'.or.                                   &
         runmode.eq.'OPRADD'.or.                                       &
         runmode.eq.'OPROMIT'.or.                                      &
         runmode.eq.'PPR') then
        return
      else
        write(*,10)
   10   format(/,1x,'MODE must be equivalent to either:',              &
               /,'   OPR STATISTIC - OPROMIT, OPRADD, OPRADDNODE',     &
               /,'   PPR STATISTIC - PPR')
        call utl_stop(' STOP: Error interpretting MODE')
      end if
      return
      end subroutine checkmode

! ----------------------------------------------------------------------

      subroutine wrtopr()
      use somevar
      implicit none
      real    :: psdrat
      integer :: npperg
      iverb=5    !default verbosity level
      npperg=2   !default
      psdrat=10. !default
	!
      write(iout,10) '# Produced by '//trim(prognam)
   10 format('#',1x,20('='),/,a,/,'#',1x,20('='))
      write(iout,20) trim(adjustl(runmode)),iverb
   20 format(/,'# The following blocks must be in this sequence:',     &
          /,'#   OPTIONS KEYWORDS',                                    &
          /,'#   READ_FILES  ',                                        &
          /,'# MF2K2DX writes portions of the OPTIONS block here',     &
          /,'BEGIN OPTIONS KEYWORDS',                                  &
          /,'  mode = ',a,                                             &
          /,'  obsgroups = no',                                        &
          /,'  predroups = no',                                        &
          /,'  verbose = ',i2)
      if(runmode.eq.'PPR') write(iout,30) npperg,psdrat
   30 format('  NParPerGrp   = ',i3,                                   &
           /,'  PercentReduc = ',F6.2)
      write(iout,31)
   31 format('END OPTIONS')
	!
      write(iout,40)trim(dmfnam),trim(dmpfnam),trim(sufnam),trim(wtfnam),trim(spufnam)
   40 format(/,                                                        &
             '# MF2K2DX writes portions of the READ_FILES block here', &
             /,'BEGIN READ_FILES',                                     &
             /,'  dmfnam  = ',a,t60,'modeldata',                       &
             /,'  dmpfnam = ',a,t60,'prediction data',                 &
             /,'  sufnam  = ',a,t60,'sensitivities - existing obs',    &
             /,'  wtfnam  = ',a,t60,'weights - existing obs',          &
             /,'  spufnam = ',a,t60,'sensitivities - predictions')
      if(runmode.eq.'ADD') write(iout,41) trim(sunfnam),trim(wtnfnam)
   41 format('  sunfnam = ',a,t60,'sensitivities - potential obs',     &
             /,'  wtnfnam = ',a,t60,'weights - potential obs')
      if(mpr.gt.0) write(iout,42) trim(suprifnam),trim(wtprifnam)
   42 format('  suprifnam = ',a,t60,'sensitivities - existing prior',  &
             /,'  wtprifnam = ',a,t60,'weights - existing prior')
      write(iout,43)
   43 format('END READ_FILES')
	!
      if(runmode.eq.'OPRADDNODE') then
          if(gswfnam.eq.' ') gswfnam ='NONE_OR_UNKNOWN'
          write(iout,44) trim(gsfnam),trim(gswfnam)
   44     format(/,                                                    &
             '# MF2K2DX writes portions of the ADD_NODE_DATA block here', &
             /,'BEGIN ADD_NODE_DATA',                                  &
             /,'  gridsensfile = ',a,t60,'grid sensitivities file',    &
             /,'  gridwtsfile =  ',a,t60,'weights for grid sensitivities',    &
             /,'  parfile =      ',t60,'modflow2000_b file'       ,    &
             /,'  fileformat =   binary',t60,'output file format (ascii or binary)' ,    &
             /,'  ngridrow   =   -999                           ' ,    &
             /,'  ngridcol   =   -999                           ' ,    &
             /,'  ngridlay   =   -999                           ' ,    &
             /,'  ntimes     =   -999                           ' ,    &
		   /,'  # maxnodecalcs = 1  (DEBUG OPTION)            ' ,    &
		   /,'END ADD_NODE_DATA',/)
	end if
	!
      write(iout,60)
   60 format(/,'# Additional BLOCKS should be added here as required')
      return
      end subroutine wrtopr

! ----------------------------------------------------------------------

      subroutine openfile(ifile,intend,fnam,iout)
!    call is "call openfile(ifile,intend,fnam,iout)"
!    fnam = file name
!    ifile = unit number
!    iout = unit number to report messages/errors/progress
!    intend = 1: open existing ascii
!    intend = 2: close existing file
!    intend = 3: open unknown ascii
!    intend = 4: open existing binary
      character*(*) fnam
      logical testfl,lop
      integer intend,ifile,ierr,maxunit,n
      maxunit = 100
      ierr=0
      inquire(file=fnam,exist=testfl)
      if(.not.testfl) then
        if (intend.eq.1.or.intend.eq.2.or.intend.eq.4) then
          write(*,10) trim(adjustl(fnam))
          stop
        end if
      endif
! -- intend = 1: open existing ascii
      if (intend.eq.1) then
        open(ifile,file=fnam,status='old',iostat=ierr)
        if (ierr.ne.0) write(iout,20) 'opening',trim(adjustl(fnam))
        if(iout.ne.0) write(iout,30) 'Opened',trim(adjustl(fnam))
      endif
! -- intend = 2: close existing file
      if (intend.eq.2) then
        close(ifile,iostat=ierr)
        if (ierr.ne.0) write(iout,20) 'closing',trim(adjustl(fnam))
        if(iout.ne.0) write(iout,30) 'Closed',trim(adjustl(fnam))
      endif
! -- intend = 3: open unknown ascii
      if (intend.eq.3) then
        open(ifile,file=fnam,status='unknown',iostat=ierr)
        if (ierr.ne.0) write(iout,20) 'opening',trim(adjustl(fnam))
        if(iout.ne.0) write(iout,30) 'Opened',trim(adjustl(fnam))
      endif
! -- intend = 4: open unknown binary
      if (intend.eq.4) then
        open(ifile,file=fnam,status='unknown',form='binary',iostat=ierr)
        if (ierr.ne.0) write(iout,20) 'opening',trim(adjustl(fnam))
        if(iout.ne.0) write(iout,30) 'Opened',trim(adjustl(fnam))
      endif
! -- if error close open files w/ unit no. up to maxunit and stop
      if (ierr.ne.0) then
        do n=1,maxunit
          inquire(unit=n,opened=lop)
          if (lop) close(n)
        end do
        stop
      end if
! -- return if all okay
      return
10    format(/,1x,' Error: file ',a,' does not exist')
20    format(/,1x,' Error: problem ',a,' file ',a)
30    format(1x,'Successfully ',a,' file ',a)
      end subroutine openfile
