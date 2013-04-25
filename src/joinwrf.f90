!
!##################################################################
!##################################################################
!######                                                      ######
!######                   PROGRAM JOINWRF                    ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!
PROGRAM joinwrf
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  This program joins WRF history files in patches into one large piece.
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Yunheng Wang (04/25/2007)
!
!  MODIFICATION HISTORY:
!  William.Gustafson@pnl.gov, 16-Feb-2009: Added nocolons option
!
!  04/16/2012 (Y. Wang)
!  Improved attadj option to read extra column or extra row of patches.

!-----------------------------------------------------------------------
!
  IMPLICIT NONE

  INTEGER, PARAMETER :: nmaxvars   = 300
  INTEGER, PARAMETER :: nmaxwrffil = 100
  INTEGER, PARAMETER :: nmaxprocs  = 4000

!-----------------------------------------------------------------------
!
! NAMLIST variables
!
!-----------------------------------------------------------------------

  CHARACTER(LEN=256) :: dir_extd            ! directory of external data
  INTEGER            :: io_form
  CHARACTER(LEN=19)  :: start_time_str,end_time_str
  CHARACTER(LEN=11)  :: history_interval
  INTEGER            :: grid_id
  INTEGER            :: filename_convention
  LOGICAL            :: nocolons
  INTEGER            :: magnitude_processor

  NAMELIST /wrfdfile/ dir_extd,io_form,grid_id,filename_convention,     &
                      nocolons,magnitude_processor,                     &
                      start_time_str,history_interval,end_time_str

  INTEGER            :: proc_sw
  INTEGER            :: nproc_x, nproc_y
  INTEGER            :: nproc_xin, nproc_yin
  NAMELIST /patches/ proc_sw, nproc_x, nproc_y,nproc_xin, nproc_yin

  CHARACTER(LEN=256) :: outdirname
  CHARACTER(LEN=5)   :: outfiletail
  INTEGER            :: nvarout
  CHARACTER(LEN=20)  :: varlist(NMAXVARS)
  LOGICAL            :: attadj
  LOGICAL            :: jointime
  INTEGER            :: istride, jstride
  NAMELIST /output/ outdirname,outfiletail,jointime,nvarout,varlist,    &
                    attadj,istride,jstride

  INTEGER :: debug
  NAMELIST /debugging/ debug

!-----------------------------------------------------------------------
!
! Misc. local variables
!
!-----------------------------------------------------------------------

  INTEGER :: strlen,istatus
  INTEGER :: i,j,n
  INTEGER :: nprocs(nmaxprocs)

  CHARACTER(LEN=256) :: filenames(NMAXWRFFIL)
  CHARACTER(LEN=1) :: colon
  INTEGER :: nfiles
  INTEGER :: abstimes, abstimei, abstimee
  INTEGER :: ids,ide,jds,jde,idss,idse,jdss,jdse

  CHARACTER(LEN=1) :: ach
  INTEGER :: year,month,day,hour,minute,second

  INTEGER :: ips, ipe, jps, jpe, ipss, ipse, jpss, jpse
  INTEGER :: nx,ny
  INTEGER :: nguess

  LOGICAL :: paddingx, paddingy

  CHARACTER(LEN=256) :: tmpstr

!-----------------------------------------------------------------------
!
! External functions
!
!-----------------------------------------------------------------------

  CHARACTER(LEN=20) :: upcase
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Begining of executable code below
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!

  WRITE(6,'(10(/5x,a),/)')                                  &
      '###################################################################',&
      '###################################################################',&
      '####                                                           ####',&
      '####                Welcome to JOINWRF                         ####',&
      '####                                                           ####',&
      '####   A program that reads in patches of WRF history files    ####',&
      '####          and join them into one large piece.              ####',&
      '####                                                           ####',&
      '###################################################################',&
      '###################################################################'
!
!-----------------------------------------------------------------------
!
!  Read in namelist &wrfdfile
!
!-----------------------------------------------------------------------
!
  dir_extd = './'

  start_time_str        = '0000-00-00_00:00:00'
  history_interval      = '00_00:00:00'
  end_time_str          = '0000-00-00_00:00:00'

  io_form               = 7
  grid_id               = 1
  filename_convention   = 0
  nocolons              = .false.
  magnitude_processor   = 4

  READ(5,wrfdfile,ERR=999)
  WRITE(6,'(2x,a)') 'Namelist wrfdfile read in successfully.'

  strlen = LEN_TRIM(dir_extd)
  IF(strlen > 0) THEN
    IF(dir_extd(strlen:strlen) /= '/') THEN
      dir_extd(strlen+1:strlen+1) = '/'
      strlen = strlen + 1
    END IF
  ELSE
    dir_extd = './'
  END IF

  IF (io_form /= 7 ) THEN
    WRITE(6,'(1x,a)') 'ERROR: Only netCDF format is supported at present.'
    istatus = 1
    GOTO 100
  END IF

  WRITE(6,'(5x,3a)')     'dir_extd = ''', TRIM(dir_extd),''','
  WRITE(6,'(5x,a,i3,a)') 'io_form  = ', io_form,','
  WRITE(6,'(5x,a,i3,a)') 'grid_id  = ', grid_id,','
  WRITE(6,'(5x,a,i3,a)') 'filename_convention  = ', filename_convention,','
  WRITE(6,'(5x,a,L3,a)') 'ncolons  = ', nocolons,','
  WRITE(6,'(5x,a,i3,a)') 'magnitude_processor  = ', magnitude_processor,','
  WRITE(6,'(5x,3a)')     'start_time_str   = ''', start_time_str,''','
  WRITE(6,'(5x,a,8x,2a)')'history_interval = ''', history_interval,''','
  WRITE(6,'(5x,3a)')     'end_time_str     = ''', end_time_str,''','
!
!-----------------------------------------------------------------------
!
!  Read in namelist &patches
!
!-----------------------------------------------------------------------
!
  proc_sw = 0
  nproc_x = 1
  nproc_y = 1
  nproc_xin = 0
  nproc_yin = 0

  READ(5,patches,ERR=999)
  WRITE(6,'(/,2x,a)') 'Namelist arpsgrid read in successfully.'
  WRITE(6,'(5(5x,a,i4,a,/))') 'proc_sw   = ', proc_sw,',',              &
                              'nproc_x   = ', nproc_x,',',              &
                              'nproc_y   = ', nproc_y,',',              &
                              'nproc_xin = ', nproc_xin,',',            &
                              'nproc_yin = ', nproc_yin,','

!
!-----------------------------------------------------------------------
!
!  Read in namelist &output and &debugging
!
!-----------------------------------------------------------------------
!
  outdirname = './'
  outfiletail= ''
  nvarout    = 0
  varlist(:) = ' '
  attadj     = .FALSE.
  istride    = 1
  jstride    = 1

  READ(5,output,ERR=999)
  WRITE(6,'(/,2x,a)') 'Namelist output was successfully read.'

  strlen = LEN_TRIM(outdirname)
  IF(strlen > 0) THEN
    IF(outdirname(strlen:strlen) /= '/') THEN
      outdirname(strlen+1:strlen+1) = '/'
      strlen = strlen + 1
    END IF
  ELSE
    outdirname = './'
  END IF

  IF (nvarout > 0) THEN
    DO n = 1,nvarout
      varlist(n) = upcase(varlist(n))
    END DO

    nvarout = nvarout+1
    varlist(nvarout) = 'Times'
  END IF

  WRITE(6,'(5x,3a)' )    'outdirname = ''', TRIM(outdirname),''','
  WRITE(6,'(5x,3a)' )    'outfiltail = ''', TRIM(outfiletail),''','
  WRITE(6,'(5x,a,I3,a)') 'nvarout    = ', nvarout,','
  DO n = 1,nvarout
    WRITE(6,'(7x,a,I3,3a)') 'varlist(',n,') = ''', TRIM(varlist(n)),''','
  END DO
  WRITE(6,'(5x,a,L,a)')  'attadj    = ', attadj,','
  WRITE(6,'(5x,a,L,a)')  'jointime  = ', jointime,','
  WRITE(6,'(5x,a,I3,a)') 'istride   = ', istride,','
  WRITE(6,'(5x,a,I3,a)') 'jstride   = ', jstride,','

  debug = 0
  READ(5,debugging,ERR=999)
  WRITE(6,'(/,2x,a)'   ) 'Namelist debugging was successfully read.'
  WRITE(6,'(5x,a,i3,a,/)') 'debug = ', debug,','

  istatus = 0

!-----------------------------------------------------------------------
!
! Prepare for reading WRF files
!
!-----------------------------------------------------------------------
  colon = ":"
  IF( nocolons ) colon = "_"


  READ(end_time_str,    '(I4.4,5(a,I2.2))')      &
                  year,ach,month,ach,day,ach,hour,ach,minute,ach,second
  IF (year < 1969) THEN
    abstimee = day*24*3600+hour*3600+minute*60+second
  ELSE
    CALL ctim2abss(year,month,day,hour,minute,second,abstimee)
  END IF

  READ(history_interval,'(I2.2,3(a,I2.2))')      &
                                     day,ach,hour,ach,minute,ach,second
  abstimei = day*24*3600+hour*3600+minute*60+second

  READ(start_time_str,  '(I4.4,5(a,I2.2))')      &
                  year,ach,month,ach,day,ach,hour,ach,minute,ach,second
  IF (year < 1969) THEN
    abstimes = day*24*3600+hour*3600+minute*60+second
  ELSE
    CALL ctim2abss(year,month,day,hour,minute,second,abstimes)
  END IF

  IF ( nproc_xin < 1 .OR. nproc_yin < 1) THEN
    IF (jointime .AND. nproc_x*nproc_y == 1) THEN
      nproc_xin = 1
    ELSE

      CALL get_wrf_filename(filename_convention,dir_extd,grid_id,       &
                            year,month,day,hour,minute,second,          &
                            colon,magnitude_processor,proc_sw,.true.,   &
                            tmpstr,istatus)

      IF (istatus /= 0) GOTO 100

      CALL get_wrf_patch_indices(TRIM(tmpstr),io_form,                   &
                           ips,ipe,ipss,ipse,jps,jpe,jpss,jpse,nx,ny,   &
                           istatus)

      IF (istatus /= 0) GOTO 100

      IF (nproc_xin < 1) THEN
        nguess = NINT(1.0*nx/(ipse-ipss+1))

        WRITE(6,'(1x,a,/)') '*****************************'
        WRITE(6,'(1x,a,/,10x,a,I4,a,/,10x,a,/)')                            &
        'WARNING: Number of processors for WRF data in X direction was not specified ',&
        'The program has guessed that it should be nproc_xin = ',nguess,'.',&
        'Please check to make sure it is the right number!!!'
        nproc_xin = nguess
      END IF

      IF (nproc_yin < 1 .AND. attadj) THEN
        nguess = NINT(1.0*ny/(jpse-jpss+1))

        WRITE(6,'(1x,a,/)') '*****************************'
        WRITE(6,'(1x,a,/,10x,a,I4,a,/,10x,a,/)')                            &
        'WARNING: Number of processors for WRF data in Y direction was not specified ',&
        'The program has guessed that it should be nproc_yin = ',nguess,'.',&
        'Please check to make sure it is the right number!!!'
        nproc_yin = nguess
      END IF

    END IF
  END IF

!  IF ( nproc_xin < proc_sw+nproc_x ) THEN
!    WRITE(6,'(1x,a,/)') '*****************************'
!    WRITE(6,'(1x,a,I4,a,/,8x,a,I4,a,/,3(8x,a,/))')                      &
!      'ERROR: Either parameter nproc_x = ',nproc_x,' is too large ',    &
!      'or  parameter  nproc_xin = ', nproc_xin,' is too small,',        &
!      'because nproc_xin < proc_sw+nproc_x, number of patches in X direction.', &
!      'If you do not know the exact value of nproc_xin, you can specify 0',     &
!      'to let the program guess for you automatically.'
!    STOP
!  END IF

  paddingx = .FALSE.
  paddingy = .FALSE.

  IF (attadj) THEN
    IF (MOD(proc_sw,nproc_xin)+nproc_x < nproc_xin) THEN
      nproc_x = nproc_x + 1    ! Read extra column of patches
      paddingx = .TRUE.
    END IF

    IF (proc_sw/nproc_xin + nproc_y  < nproc_yin) THEN
      nproc_y = nproc_y + 1    ! read extra row of patches
      paddingy = .TRUE.
    END IF

  END IF

  n = 0
  DO j = 0,nproc_y-1
    DO i = 0,nproc_x-1
      n = n+1
      nprocs(n) = proc_sw + j*nproc_xin + i  ! for merging purpose
    END DO
  END DO

!-----------------------------------------------------------------------
!
! Check file and get dimensions
!
!-----------------------------------------------------------------------

  filenames(:) = ' '
  CALL check_files_dimensions(NMAXWRFFIL,grid_id,filename_convention,   &
          colon,io_form,magnitude_processor,                            &
          jointime,nprocs,nproc_x,nproc_y,abstimes,abstimei,abstimee,   &
          dir_extd,filenames,nfiles,paddingx,paddingy,                  &
          istride,jstride,                                              &
          ids,ide,idss,idse,jds,jde,jdss,jdse,istatus)

  IF (istatus /= 0) GO TO 100

!-----------------------------------------------------------------------
!
! Diagnostic outputs
!
!-----------------------------------------------------------------------

  WRITE(6,'(/,1x,a)') '*****************************'

  WRITE(6,'(1x,2(2(a,I4),a,3x,2(a,I4),a,/,24x))')                     &
      'The joined  indices  is: stag - ids = ',ids, ', ide = ',ide,';', &
                                      'jds = ',jds, ', jde = ',jde,'.', &
                             'unstag - idss= ',idss,', idse= ',idse,';',&
                                      'jdss= ',jdss,', jdse= ',jdse,'.'
  WRITE(6,'(1x,a,2(I4,a),/)') 'With istride = ',istride,', jstride = ',jstride,'.'
  IF (MOD(ids-ids,istride) /= 0) THEN
    WRITE(*,'(1x,a)') 'WARNING: istride may not be a good value.'
  END IF

  IF (MOD(ids-ids,istride) /= 0) THEN
    WRITE(*,'(1x,a)') 'WARNING: jstride may not be a good value.'
  END IF

  IF (attadj) THEN
    WRITE(6,'(1x,2(2(a,I4),a,3x,2(a,I4),a,/,24x))')                     &
      'The joined subdomain is: stag - ids = ',1, ', ide = ',(ide-ids)/istride+1,';', &
                                      'jds = ',1, ', jde = ',(jde-jds)/jstride+1,'.', &
                             'unstag - idss= ',1,', idse= ',(idse-idss)/istride+1,';',&
                                      'jdss= ',1,', jdse= ',(jdse-jdss)/jstride+1,'.'

    IF (ide /= idse+1 .OR. jde /= jdse+1) THEN
      WRITE(*,'(1x,a)') 'ERROR: Wrong staggered dimension size. Please check istride or jstride.'
      istatus = -1
      GOTO 100
    END IF

  END IF

  WRITE(*,'(/,1x,a,/)') 'Patches to be joined are:'
  DO j = nproc_y-1,0,-1
    WRITE(*,'(5x,I3,a)',ADVANCE='NO') j+1,' |'
    DO i = 0, nproc_x-1
      WRITE(*,'(I5)',ADVANCE='NO') proc_sw + j*nproc_xin + i
    END DO
    WRITE(*,*)
  END DO

  WRITE(*,'(5x,a)',ADVANCE='NO') '     '
  DO i = 0, nproc_x-1
    WRITE(*,'(a)',ADVANCE='NO') ' ----'
  END DO
  WRITE(*,*)

  WRITE(*,'(5x,a)',ADVANCE='NO') '  j/i'
  DO i = 0, nproc_x-1
    WRITE(*,'(1x,I3,a)',ADVANCE='NO') i+1, ' '
  END DO
  WRITE(*,'(1x,a,/)') '  '

!-----------------------------------------------------------------------
!
! Join files
!
!-----------------------------------------------------------------------

  IF (nvarout == 0) nvarout = nmaxvars

  IF (io_form == 7) THEN
    CALL joinwrfncdf(filenames,nfiles,attadj,jointime,filename_convention, &
                  magnitude_processor,nprocs,n,                         &
                  ids,ide,idss,idse,jds,jde,jdss,jdse,istride,jstride,  &
                  outdirname,outfiletail,nvarout,varlist,debug,istatus)
    IF (istatus /= 0) GOTO 100
  END IF

  GO TO 100

!-----------------------------------------------------------------------
!
! Just before termination
!
!-----------------------------------------------------------------------

  999  WRITE(6,'(1x, a,a)') 'Error reading NAMELIST file. Job stopped.'
  STOP

  100    CONTINUE
  IF (istatus == 0) THEN
    WRITE(6,'(/,4x,a,/)') '==== Program JOINWRF terminated normally ===='
  ELSE
    WRITE(6,'(/,4x,a,I3,a/)') '**** Program JOINWRF terminated with error = ',istatus,' ****'
  END IF

  STOP
END PROGRAM joinwrf
