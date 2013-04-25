!
!##################################################################
!##################################################################
!######                                                      ######
!######           SUBROUTINE check_files_dimensions          ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!
SUBROUTINE check_files_dimensions(MAXWRFFIL,grid_id,fileconv,           &
                colon,io_form,magnitude_processor,                      &
                jointime,nprocs,nproc_x,nproc_y,abstimes,abstimei,      &
                abstimee,dir_extd,extdname,nextdfil,                    &
                paddingx,paddingy,istride,jstride,                      &
                ids,ide,idss,idse,jds,jde,jdss,jdse,istatus)
!
!-----------------------------------------------------------------------
!
! PURPOSE: Check the existence of WRF files to be read and return the
!          valid file number, file names and the domain grid indices.
!
!-----------------------------------------------------------------------
!
!  AUTHOR:
!  Yunheng Wang (04/26/2007)
!
!  MODIFICATION HISTORY:
!  William.Gustafson@pnl.gov, 16-Feb-2009: Added nocolons option
!
!  04/16/2012 (Y. Wang)
!  Improved attadj option to read extra column or extra row of patches.
!
!-----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,            INTENT(IN)  :: MAXWRFFIL
  INTEGER,            INTENT(IN)  :: grid_id
  CHARACTER(LEN=1),   INTENT(IN)  :: colon
  INTEGER,            INTENT(IN)  :: magnitude_processor
  INTEGER,            INTENT(IN)  :: io_form
  INTEGER,            INTENT(IN)  :: fileconv
  LOGICAL,            INTENT(IN)  :: jointime
  INTEGER,            INTENT(IN)  :: abstimes, abstimei, abstimee
  INTEGER,            INTENT(IN)  :: nproc_x,nproc_y
  INTEGER,            INTENT(IN)  :: nprocs(nproc_x*nproc_y)
  CHARACTER(LEN=256), INTENT(IN)  :: dir_extd
  CHARACTER(LEN=256), INTENT(OUT) :: extdname(MAXWRFFIL)
  INTEGER,            INTENT(OUT) :: nextdfil
  LOGICAL,            INTENT(IN)  :: paddingx, paddingy
  INTEGER,            INTENT(IN)  :: istride, jstride
  INTEGER,            INTENT(OUT) :: ids, ide, jds, jde
  INTEGER,            INTENT(OUT) :: idss,idse,jdss,jdse
  INTEGER,            INTENT(OUT) :: istatus

!-----------------------------------------------------------------------
!
! Misc. local variables
!
!-----------------------------------------------------------------------

  INTEGER :: year, month, day, hour, minute, second
  INTEGER :: ifile, npx, npy, n
  INTEGER :: ips, ipe, jps, jpe, ipss, ipse, jpss, jpse
  INTEGER :: ipssv, ipesv, jpssv, jpesv
  INTEGER :: imod, jmod
  INTEGER :: nx,ny

  CHARACTER(LEN=256) :: tmpstr
  INTEGER :: abss

  LOGICAL :: fexist
  LOGICAL :: dset = .FALSE., in_a_row = .FALSE.

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Begining of executable code ....
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  ids  = 99999999
  ide  = 0
  idss = 99999999
  idse = 0

  jds  = 99999999
  jde  = 0
  jdss = 99999999
  jdse = 0

  nextdfil    = 0
  extdname(:) = ' '
  istatus     = 0

  WRITE(6,'(1x,a,/,1x,a,/)')                             &
              '============================','WRF files to be read are:'

  ifile = abstimes
  DO WHILE (ifile <= abstimee)

    IF (ifile < 30*24*3600) THEN
      year = 1
      month = 1
      day = ifile/(24*3600)
      abss = MOD(ifile,24*3600)
      hour = abss / 3600
      abss = MOD( abss, 3600 )
      minute = abss / 60
      second = MOD( abss, 60 )
    ELSE
      CALL abss2ctim(ifile,year,month,day,hour,minute,second)
    END IF

    nextdfil = nextdfil + 1

    CALL get_wrf_filename(fileconv,dir_extd,grid_id,                    &
                          year,month,day,hour,minute,second,            &
                          colon,magnitude_processor,1,.false.,          &
                          extdname(nextdfil),istatus)

    ipssv = 0
    ipesv = 0
    jpssv = 0
    jpesv = 0

    n = 0
    DO npy = 1,nproc_y
      in_a_row = .FALSE.
      DO npx = 1,nproc_x
        IF (npx > 1) in_a_row = .TRUE.

        n = n+1

        IF (jointime .AND. nproc_x*nproc_y == 1) THEN
          CALL get_wrf_filename(fileconv,dir_extd,grid_id,              &
                            year,month,day,hour,minute,second,          &
                            colon,magnitude_processor,nprocs(n),.false.,&
                            tmpstr,istatus)
        ELSE
          CALL get_wrf_filename(fileconv,dir_extd,grid_id,              &
                            year,month,day,hour,minute,second,          &
                            colon,magnitude_processor,nprocs(n),.true., &
                            tmpstr,istatus)
        END IF

        INQUIRE(FILE=TRIM(tmpstr), EXIST = fexist )
        IF(.NOT. fexist) THEN
          WRITE(6,'(1x,3a)') 'ERROR: The WRF file ',                    &
                         TRIM(tmpstr),' does not exist.'
          istatus = -1
          RETURN
        ELSE
          CALL get_wrf_patch_indices(TRIM(tmpstr),io_form,              &
                         ips,ipe,ipss,ipse,jps,jpe,jpss,jpse,nx,ny,istatus)
!          ## JH 27.06.2012
!          WRITE(6,'(8x,2(a,I5))') 'nx= ',nx,', ny= ',ny
!          WRITE(6,'(8x,2(a,I4))') 'npx= ',npx,', npy= ',npy
!          WRITE(6,'(8x,4(a,I5))') 'ips:',ips,' ipe:',ipe,' ipss:',ipss,' ipse:',ipse
!          WRITE(6,'(8x,4(a,I5))') 'jps:',jps,' jpe:',jpe,' jpss:',jpss,' jpse:',jpse
!          ##
          IF (istatus /= 0) EXIT

          IF (.NOT. dset) THEN
            IF (npx == 1) THEN
              ids  = ips
              idss = ipss
            END IF

            IF (npx == nproc_x) THEN
              IF (paddingx) THEN
                imod = MOD(ips-ids,istride)
                IF (istride == 1 .OR. imod == 0) THEN
                  ide  = ips
                ELSE
                  ide = istride-imod + ips
                END IF
                idse = ipss-1
              ELSE
                ide  = ipe
                idse = ipse
              END IF
            END IF

            IF (npy == 1) THEN
              jds  = jps
              jdss = jpss
            END IF

            IF (npy == nproc_y) THEN
              IF (paddingy) THEN
                jmod = MOD(jps-jds,jstride)
                IF (jstride == 1 .OR. jmod == 0) THEN
                  jde  = jps
                ELSE
                  jde = jstride -jmod + jps
                END IF
                jdse = jpss-1
              ELSE
                jde  = jpe
                jdse = jpse
              END IF
            END IF

          END IF

          IF ( n > 1) THEN
            IF (in_a_row) THEN
              IF (jps /= jpssv .OR. jpe /= jpesv .OR. ips /= ipesv+1) THEN
                WRITE(6,'(/,1x,a,I4,2a,/,8x,2(a,I2),a,/,8x,a,/,8x,a,/)')  &
                  'ERROR: Patch ',n,' for file ',TRIM(tmpstr),            &
                  'at relative patch (',npx,',',npy,                      &
                  ') is not aligned in a row with its former patch.',     &
                  'Please check parameter nproc_xin. Either it was specified with a wrong number', &
                  'or the program has made a bad guess about it.'
                istatus = -2
                RETURN
              END IF
            ELSE
              IF (jps /= jpesv+1) THEN
                WRITE(6,'(/,1x,a,I4,2a,/,8x,2(a,I2),a,/,8x,a,/,8x,a,/)')  &
                  'ERROR: Patch ',n,' for file ',TRIM(tmpstr),            &
                  'at relative patch (',npx,',',npy,                      &
                  ') is not aligned in column with its former patch.',    &
                  'Please check parameter nproc_xin. Either it was specified with a wrong number', &
                  'or the program has made a bad guess about it.'
                istatus = -3
                RETURN
              END IF
            END IF
          END IF

          ipssv = ips
          ipesv = ipe
          jpssv = jps
          jpesv = jpe

          WRITE(6,'(3x,a,I2.2,a,I4,a,/,5x,a)')                            &
             'WRF file ',nextdfil,': patch - ',n,' =', TRIM(tmpstr)
!          ## JH 27.06.2012
!          WRITE(6,'(8x,2(a,I5))') 'nx= ',nx,', ny= ',ny
!          WRITE(6,'(8x,2(a,I5))') 'npx= ',npx,', npy= ',npy
!          WRITE(6,'(8x,4(a,I5))') 'ips:',ips,' ipe:',ipe,' ipss:',ipss,' ipse:',ipse
!          WRITE(6,'(8x,4(a,I5))') 'jps:',jps,' jpe:',jpe,' jpss:',jpss,' jpse:',jpse
!          ##
        END IF
      END DO
    END DO

    ifile = ifile + abstimei
    dset = .TRUE.

    WRITE(*,*)
  END DO


!-----------------------------------------------------------------------
!
! Validate nextdfil before return
!
!-----------------------------------------------------------------------

  IF(nextdfil < 1) THEN
    WRITE(6,'(a)') 'No input WRF file was valid. Please check the input file.'
    istatus = -3
    RETURN
  END IF

  IF (ide < ids .OR. jde < jds) THEN
    WRITE(6,'(1x,2(a,I4),/36x,2(a,I4),a)')                              &
    'ERROR: Domain indices are invalid: ids = ',ids,', ide = ',ide,     &
    '; jds = ',jds,', jde = ',jde,'.'
    istatus = -3
    RETURN
  END IF

  RETURN
END SUBROUTINE check_files_dimensions
!
!##################################################################
!##################################################################
!######                                                      ######
!######           SUBROUTINE joinwrfncdf                     ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!
SUBROUTINE  joinwrfncdf(filenames,nfile,attadj,jointime,fileconv,       &
                  magnitude_processor,procs,npatch,                     &
                  ids,ide,idss,idse,jds,jde,jdss,jdse,istride,jstride,  &
                  outdirname,filetail,nvarout,varlists,debug,istatus)
!
!-----------------------------------------------------------------------
!
! PURPOSE:
!
!    Join WRF files in netCDF patches into one large piece.
!
!-----------------------------------------------------------------------
!
! Author: Yunheng Wang (04/27/2007)
!
! MODIFICATIONS:
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE
  INTEGER, INTENT(IN)            :: nfile
  LOGICAL, INTENT(IN)            :: attadj
  LOGICAL, INTENT(IN)            :: jointime
  INTEGER, INTENT(IN)            :: fileconv, magnitude_processor
  INTEGER, INTENT(IN)            :: npatch
  INTEGER, INTENT(IN)            :: procs(npatch)
  INTEGER, INTENT(IN)            :: ids,ide,idss,idse,jds,jde,jdss,jdse
  INTEGER, INTENT(IN)            :: istride, jstride
  INTEGER, INTENT(INOUT)         :: nvarout
  INTEGER, INTENT(IN)            :: debug
  INTEGER, INTENT(OUT)           :: istatus

  CHARACTER(LEN=*),  INTENT(IN)  :: filenames(nfile)
  CHARACTER(LEN=*),  INTENT(IN)  :: outdirname
  CHARACTER(LEN=5),  INTENT(IN)  :: filetail
  CHARACTER(LEN=20), INTENT(IN)  :: varlists(nvarout)

!
!-----------------------------------------------------------------------
!
! Including files
!
!-----------------------------------------------------------------------

  INCLUDE 'netcdf.inc'

!-----------------------------------------------------------------------
!
! Misc. local variables
!
!-----------------------------------------------------------------------

  INTEGER :: nf, nvar, n
  INTEGER :: strlen
  LOGICAL :: ispatch(NF_MAX_VARS)

  CHARACTER(LEN=256) :: infilename, outfilename
  INTEGER :: finid, foutid

  INTEGER :: idsout, ideout, jdsout, jdeout
  INTEGER :: idssout, idseout, jdssout, jdseout

  !LOGICAL :: paddingx, paddingy
  !INTEGER :: nxdim, nydim
  REAL    :: newctrlat, newctrlon, lat1, lat2,lon1,lon2
  INTEGER :: newlatidx, newlonidx, oldlatidx1, oldlonidx1, oldlatidx2, oldlonidx2
  LOGICAL :: latavg, lonavg
  INTEGER :: latset, lonset

  !
  ! Dimension variables
  !
  CHARACTER(LEN=32), PARAMETER :: xdimname  = 'west_east_stag'
  CHARACTER(LEN=32), PARAMETER :: ydimname  = 'south_north_stag'
  CHARACTER(LEN=32), PARAMETER :: xsdimname = 'west_east'
  CHARACTER(LEN=32), PARAMETER :: ysdimname = 'south_north'
  CHARACTER(LEN=32) :: diminnames(NF_MAX_DIMS)
  CHARACTER(LEN=32) :: dimname

  INTEGER :: nxid, nyid, nxlg, nylg, nxsid, nysid, nxslg, nyslg
  INTEGER :: unlimdimid, unlimdimlen, unlimodimlen, odimid
  INTEGER :: ndims, dimid, dimlen, narrsize

  INTEGER :: dimina(NF_MAX_DIMS)         ! Dimension size in original file
  INTEGER :: dimouta(NF_MAX_DIMS)        ! Dimension size in joined files

  !
  ! Attribute variables
  !
  CHARACTER(LEN=32), PARAMETER :: attnm_ips  = 'WEST-EAST_PATCH_START_STAG'
  CHARACTER(LEN=32), PARAMETER :: attnm_ipe  = 'WEST-EAST_PATCH_END_STAG'
  CHARACTER(LEN=32), PARAMETER :: attnm_ipss = 'WEST-EAST_PATCH_START_UNSTAG'
  CHARACTER(LEN=32), PARAMETER :: attnm_ipse = 'WEST-EAST_PATCH_END_UNSTAG'
  CHARACTER(LEN=32), PARAMETER :: attnm_jps  = 'SOUTH-NORTH_PATCH_START_STAG'
  CHARACTER(LEN=32), PARAMETER :: attnm_jpe  = 'SOUTH-NORTH_PATCH_END_STAG'
  CHARACTER(LEN=32), PARAMETER :: attnm_jpss = 'SOUTH-NORTH_PATCH_START_UNSTAG'
  CHARACTER(LEN=32), PARAMETER :: attnm_jpse = 'SOUTH-NORTH_PATCH_END_UNSTAG'
  CHARACTER(LEN=32) :: attname
  INTEGER :: ipsid,  ipeid,  jpsid,  jpeid
  INTEGER :: ipssid, ipseid, jpssid, jpseid
  INTEGER :: ips, ipe, ipss, ipse
  INTEGER :: jps, jpe, jpss, jpse
  INTEGER :: attnum, ngatts
  INTEGER :: istart, jstart, iend, jend, imod, jmod

  CHARACTER(LEN=32), PARAMETER :: attnm_anx = 'WEST-EAST_GRID_DIMENSION'
  CHARACTER(LEN=32), PARAMETER :: attnm_any = 'SOUTH-NORTH_GRID_DIMENSION'
  INTEGER :: anxid, anyid

  CHARACTER(LEN=32), PARAMETER :: attnm_adx = 'DX'
  CHARACTER(LEN=32), PARAMETER :: attnm_ady = 'DY'
  INTEGER :: adxid, adyid
  REAL    :: adx, ady

  !
  ! Dataset varaibles
  !
  INTEGER, PARAMETER :: MAX_RANK = 4    ! Assume the max rank is 5
  CHARACTER(LEN=32) :: varname
  INTEGER :: varid, nvars, ovarid
  INTEGER :: vartype, varndims, varnatts
  INTEGER :: vardimids(MAX_RANK)
  INTEGER :: startidx(MAX_RANK), countidx(MAX_RANK), strideidx(MAX_RANK)
  INTEGER :: outstart(MAX_RANK), outcount(MAX_RANK)
  INTEGER :: outcount3d(3), outcount4d(4)
  INTEGER :: vardim, vdimid

  INTEGER :: varidlists(NF_MAX_VARS), varoutidlists(NF_MAX_VARS)

  INTEGER, ALLOCATABLE :: varari(:)
  REAL,    ALLOCATABLE :: vararr(:)
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: varai2d
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: varai3d
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:) :: varai4d
  REAL, ALLOCATABLE, DIMENSION(:,:) :: varar2d
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: varar3d
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: varar4d
  CHARACTER(LEN=256)   :: tmpstr,fmtstr

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code below
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  varidlists(:) = 0
  nxlg  = ide-ids+1
  nylg  = jde-jds+1
  nxslg = idse-idss+1
  nyslg = jdse-jdss+1

  startidx(:)  = 1
  unlimdimlen  = 1

  istatus = 0
  files_loop: DO nf = 1,nfile

!-----------------------------------------------------------------------
!
! First, Create the merged file based on attributes from the first patch
!
!-----------------------------------------------------------------------

    IF (.NOT. jointime .OR. nf == 1) THEN
      strlen = LEN_TRIM(filenames(nf))
      n = INDEX(filenames(nf),'/',.TRUE.)

      WRITE(outfilename,'(3a)') TRIM(outdirname),                         &
                                filenames(nf)(n+1:strlen),filetail

      IF (jointime .AND. npatch == 1) THEN
        WRITE(infilename, '(a)')       TRIM(filenames(nf))
      ELSE
        WRITE(fmtstr,'(a,2(I1,a))') '(a,a,I',magnitude_processor,'.',magnitude_processor,')'
        WRITE(infilename, FMT=fmtstr) TRIM(filenames(nf)),'_',procs(1)
      END IF

      IF (debug > 0) WRITE(6,'(1x,2a)') 'Opening file - ',TRIM(infilename)
      istatus = nf_open(infilename,NF_NOWRITE,finid)   ! Open file
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      IF (debug > 0) WRITE(6,'(1x,2a)') 'Creating file - ',TRIM(outfilename)
!      istatus = nf_create(TRIM(outfilename),NF_CLOBBER,foutid)                     ! CDF 1
      istatus = NF_CREATE(TRIM(outfilename),IOR(NF_CLOBBER,NF_64BIT_OFFSET),foutid) ! CDF 2
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      !
      ! Set dimensions
      !
      IF (fileconv < 2) THEN
        istatus = nf_inq_dimid(finid,xdimname,nxid)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        istatus = nf_inq_dimid(finid,ydimname,nyid)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      ELSE               ! NMM core
        nxid = -1
        nyid = -1
      END IF

      istatus = nf_inq_dimid(finid,xsdimname,nxsid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_inq_dimid(finid,ysdimname,nysid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_inq_unlimdim(finid,unlimdimid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_inq_ndims(finid,ndims)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      IF (debug > 0) WRITE(6,'(5x,a,I2)') 'Copying dimensions - ',ndims
      DO dimid = 1,ndims
        istatus = nf_inq_dim(finid,dimid,dimname,dimlen)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)

        diminnames(dimid) = dimname
        dimina(dimid)  = dimlen             ! Save dimension id and len
        dimouta(dimid) = dimlen             ! Output dimension id and len
        IF (dimid == nxid) THEN
          dimlen = nxlg
          dimlen = (dimlen-1)/istride+1
          dimouta(dimid) = dimlen
        ELSE IF (dimid == nxsid) THEN
          dimlen = nxslg
          dimlen = (dimlen-1)/istride+1
          dimouta(dimid) = dimlen
        ELSE IF (dimid == nyid) THEN
          dimlen = nylg
          dimlen = (dimlen-1)/jstride+1
          dimouta(dimid) = dimlen
        ELSE IF (dimid == nysid) THEN
          dimlen = nyslg
          dimlen = (dimlen-1)/jstride+1
          dimouta(dimid) = dimlen
        ELSE IF (dimid == unlimdimid) THEN
          dimlen = NF_UNLIMITED
        END IF

        IF (debug > 0) WRITE(6,'(9x,2a)') 'Dimension name - ',TRIM(dimname)
        istatus = nf_def_dim(foutid,dimname,dimlen,odimid)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      END DO

      !
      ! Set Global attributes
      !
      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_ips),ipsid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_ipe),ipeid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_ipss),ipssid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_ipse),ipseid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_jps),jpsid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_jpe),jpeid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_jpss),jpssid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_jpse),jpseid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_anx),anxid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_any),anyid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_adx),adxid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_inq_attid(finid,NF_GLOBAL,TRIM(attnm_ady),adyid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_get_att_real(finid,NF_GLOBAL,TRIM(attnm_adx),adx)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_get_att_real(finid,NF_GLOBAL,TRIM(attnm_ady),ady)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_inq_natts(finid,ngatts)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      IF (debug > 0) WRITE(6,'(5x,a,I2)') 'Copying global attributes - ',ngatts

      IF (attadj) THEN
        idsout  = 1
        ideout  = ide  - ids  + 1
        idssout = 1
        idseout = idse - idss + 1

        jdsout  = 1
        jdeout  = jde  - jds  + 1
        jdssout = 1
        jdseout = jdse - jdss + 1

        newlatidx = (jdeout-jdsout)/2    ! Added to modify CEN_LAT & CEN_LON
        newlonidx = (ideout-idsout)/2    ! 0 base

        oldlatidx1 = newlatidx*jstride+jds  ! old domain index
        oldlonidx1 = newlonidx*istride+ids  ! old domain index

        latavg = .FALSE.
        lonavg = .FALSE.
        latset = 0
        lonset = 0

        IF (MOD((jdeout-jdsout),2) /= 0) latavg = .TRUE.
        IF (MOD((ideout-idsout),2) /= 0) lonavg = .TRUE.

        oldlatidx2 = -99999999
        oldlonidx2 = -99999999
        IF (.NOT. latavg .AND. .NOT. lonavg) THEN
          oldlatidx2 = oldlatidx1-jstride
          oldlonidx2 = oldlonidx1-istride
        END IF

        IF (debug > 0) WRITE(6,'(9x,a,/,9x,a,2L,2(a,2I4))')             &
                    'It is required to adjust global attribute.',       &
                    'latavg,lonavg = ',latavg,lonavg,                   &
                    ', lonidx,latidx = ',newlonidx+1,newlatidx+1,       &
                    ', lonidx,latidx (in original domain) = ',oldlonidx1,oldlatidx1

      ELSE
        idsout  = ids
        ideout  = ide
        idssout = idss
        idseout = idse

        jdsout  = jds
        jdeout  = jde
        jdssout = jdss
        jdseout = jdse
      END IF

      ideout  = (ideout-idsout)/istride + idsout
      idseout = (idseout-idssout)/istride + idssout
      jdeout  = (jdeout-jdsout)/jstride + jdsout
      jdseout = (jdseout-jdssout)/jstride + jdssout

      DO attnum = 1,ngatts

        istatus = nf_inq_attname(finid,NF_GLOBAL,attnum,attname)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)

        IF (debug > 0) WRITE(6,'(9x,2a)') 'Attribute name - ',TRIM(attname)

        IF (attnum == ipsid) THEN
          istatus = NF_PUT_ATT_INT(foutid,NF_GLOBAL,TRIM(attnm_ips),NF_INT,1,idsout)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSE IF (attnum == ipeid) THEN
          istatus = NF_PUT_ATT_INT(foutid,NF_GLOBAL,TRIM(attnm_ipe),NF_INT,1,ideout)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSE IF (attnum == jpsid) THEN
          istatus = NF_PUT_ATT_INT(foutid,NF_GLOBAL,TRIM(attnm_jps),NF_INT,1,jdsout)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSE IF (attnum == jpeid) THEN
          istatus = NF_PUT_ATT_INT(foutid,NF_GLOBAL,TRIM(attnm_jpe),NF_INT,1,jdeout)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSE IF (attnum == ipssid) THEN
          istatus = NF_PUT_ATT_INT(foutid,NF_GLOBAL,TRIM(attnm_ipss),NF_INT,1,idssout)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSE IF (attnum == ipseid) THEN
          istatus = NF_PUT_ATT_INT(foutid,NF_GLOBAL,TRIM(attnm_ipse),NF_INT,1,idseout)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSE IF (attnum == jpssid) THEN
          istatus = NF_PUT_ATT_INT(foutid,NF_GLOBAL,TRIM(attnm_jpss),NF_INT,1,jdssout)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSE IF (attnum == jpseid) THEN
          istatus = NF_PUT_ATT_INT(foutid,NF_GLOBAL,TRIM(attnm_jpse),NF_INT,1,jdseout)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSE IF (attnum == anxid) THEN      ! Adjust nx
          IF (istride > 1 .OR. attadj) THEN
            istatus = NF_PUT_ATT_INT(foutid,NF_GLOBAL,TRIM(attnm_anx),NF_INT,1,(ideout-idsout)+1)
            IF (istatus /= NF_NOERR) CALL handle_err(istatus)
          ELSE
            istatus = nf_copy_att(finid,NF_GLOBAL,attname,foutid,NF_GLOBAL)
            IF (istatus /= NF_NOERR) CALL handle_err(istatus)
          END IF
        ELSE IF (attnum == anyid) THEN      ! Adjust ny
          IF (jstride > 1 .OR. attadj) THEN
            istatus = NF_PUT_ATT_INT(foutid,NF_GLOBAL,TRIM(attnm_any),NF_INT,1,(jdeout-jdsout)+1)
            IF (istatus /= NF_NOERR) CALL handle_err(istatus)
          ELSE
            istatus = nf_copy_att(finid,NF_GLOBAL,attname,foutid,NF_GLOBAL)
            IF (istatus /= NF_NOERR) CALL handle_err(istatus)
          END IF
        ELSE IF (attnum == adxid) THEN      ! adjust dx
          istatus = NF_PUT_ATT_REAL(foutid,NF_GLOBAL,TRIM(attnm_adx),NF_REAL,1,adx*istride)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSE IF (attnum == adyid) THEN      ! adjust dy
          istatus = NF_PUT_ATT_REAL(foutid,NF_GLOBAL,TRIM(attnm_ady),NF_REAL,1,ady*jstride)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSE
          istatus = nf_copy_att(finid,NF_GLOBAL,attname,foutid,NF_GLOBAL)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        END IF

      END DO

      !
      ! Define variables
      !
      istatus = nf_inq_nvars(finid,nvars)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      IF (nvarout >= nvars) THEN
        nvarout = nvars
        DO n = 1,nvars
          varidlists(n) = n
        END DO
      ELSE
        nvar = nvarout         ! suppose to process this number
        nvarout = 0            ! actually got
        DO n = 1,nvar
          istatus = nf_inq_varid(finid,TRIM(varlists(n)),ovarid)
          IF (istatus /= NF_NOERR) THEN
            WRITE(6,'(1x,3a)') 'WARNING: Variable ',TRIM(varlists(n)),' not found. Skipped.'
            CYCLE
          END IF
          nvarout = nvarout + 1
          varidlists(nvarout) = ovarid
        END DO
      END IF

      IF (debug > 0) WRITE(6,'(5x,a,I4)') 'Defining variables - ',nvarout

      nvarout_loop: DO n = 1,nvarout
        varid = varidlists(n)
        istatus = nf_inq_var(finid,varid,varname,vartype,varndims,vardimids,varnatts)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)

        IF (debug > 0) WRITE(6,'(9x,2a)') 'Variables - ',TRIM(varname)

        ! Dimensions should be in the same order
        istatus = nf_def_var(foutid,varname,vartype,varndims,vardimids,ovarid)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)

        varoutidlists(n) = ovarid

        DO attnum = 1,varnatts          ! Copy variable attributes
         istatus = nf_inq_attname(finid,varid,attnum,attname)
         IF (istatus /= NF_NOERR) CALL handle_err(istatus)

         istatus = nf_copy_att(finid,varid,attname,foutid,ovarid)
         IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        END DO

      END DO nvarout_loop

      istatus = nf_enddef(foutid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      IF(debug > 0) WRITE(6,'(1x,a)') 'Merged file have been defined.'

      istatus = nf_close(finid)                              ! Close file
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

    END IF         ! File created.

    IF (.NOT. jointime) unlimdimlen = 1
    unlimodimlen = 0

!-----------------------------------------------------------------------
!
! Loop over each variable and allocate it for each patch, in the end write to merged file
!
!-----------------------------------------------------------------------

    var_loop: DO nvar = 1, nvarout
     WRITE(6,'(a,I2,a,I2,a)') ' ### var_loop ',nvar,' from ',nvarout,' ###'

     varid = varidlists(nvar)
     vardimids(:) = 0

     ispatch(:) = .FALSE.
     patch_loop: DO n = 1, npatch
      WRITE(6,'(a,I4,a,I4,a)') ' ### patch_loop ',n,' from ',npatch,' ###'
      IF (jointime .AND. npatch == 1) THEN
        WRITE(infilename, '(a)')       TRIM(filenames(nf))
      ELSE
        WRITE(fmtstr,'(a,2(I1,a))') '(a,a,I',magnitude_processor,'.',magnitude_processor,')'
        WRITE(infilename, FMT=fmtstr) TRIM(filenames(nf)),'_',procs(n)
      END IF

      IF (debug > 0) WRITE(6,'(1x,2a)') 'Opening file - ',TRIM(infilename)

      istatus = nf_open(TRIM(infilename),NF_NOWRITE,finid)   ! Open file
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      !
      ! Get patch indice
      !
      istatus = nf_get_att_int(finid,NF_GLOBAL,TRIM(attnm_ips),ips)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_get_att_int(finid,NF_GLOBAL,TRIM(attnm_ipe),ipe)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_get_att_int(finid,NF_GLOBAL,TRIM(attnm_ipss),ipss)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_get_att_int(finid,NF_GLOBAL,TRIM(attnm_ipse),ipse)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_get_att_int(finid,NF_GLOBAL,TRIM(attnm_jps),jps)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_get_att_int(finid,NF_GLOBAL,TRIM(attnm_jpe),jpe)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_get_att_int(finid,NF_GLOBAL,TRIM(attnm_jpss),jpss)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      istatus = nf_get_att_int(finid,NF_GLOBAL,TRIM(attnm_jpse),jpse)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

!-----------------------------------------------------------------------
!
! Get and save dimension size for this patch
!
!-----------------------------------------------------------------------

      istatus = nf_inq_ndims(finid,ndims)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      dimina(:) = 0
      DO dimid = 1,ndims
        istatus = nf_inq_dim(finid,dimid,dimname,dimlen)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)

        diminnames(dimid) = dimname
        dimina(dimid)     = dimlen             ! Save dimension id and len
      END DO

!-----------------------------------------------------------------------
!
!    Handle dimensions
!
!-----------------------------------------------------------------------

     istatus = nf_inq_var(finid,varid,varname,vartype,varndims,vardimids,varnatts)
     IF (istatus /= NF_NOERR) CALL handle_err(istatus)

     startidx(:)  = 1  ! read from nc (nf_get_vars), where to start
     countidx(:)  = 1  ! read from nc (nf_get_vars), how far to read (vector) 
     strideidx(:) = 1  ! read from nc (nf_get_vars)
     outstart(:)  = 1  ! write to nc, vector length of subdomain
     outcount(:)  = 1  ! write to nc, dim of field to write to output
     narrsize = 1      ! for allocation of 2d variables

     istart = 1
     iend   = 1
     jstart = 1
     jend   = 1
     vardims_loop: DO vardim = 1, varndims
       vdimid = vardimids(vardim)
       countidx(vardim) = dimina(vdimid)
       outcount(vardim) = dimina(vdimid)
       IF ( vdimid == nxid) THEN
         imod = MOD((ips-ids),istride)
         IF (imod == 0) THEN
           startidx(vardim) = 1
         ELSE
           startidx(vardim) = istride - imod + 1
         END IF
         istart = ips+startidx(vardim)-1    ! start index relative to global domain
         iend   = MIN(ipe,ide)
         IF (iend < istart) THEN
            countidx(vardim)  = 0
         ELSE
            countidx(vardim)  = (iend-istart)/istride + 1
         END IF
         strideidx(vardim) = istride

         outstart(vardim) = (istart-ids)/istride + 1
         outcount(vardim) = ide
         ispatch(nvar) = .TRUE.
       ELSE IF ( vdimid == nyid) THEN
         jmod = MOD((jps-jds),jstride)
         IF (jmod == 0) THEN
           startidx(vardim) = 1
         ELSE
           startidx(vardim) = jstride - jmod + 1
         END IF
         jstart = jps+startidx(vardim)-1
         jend   = MIN(jpe,jde)
         IF (jend < jstart) THEN
           countidx(vardim)  = 0
         ELSE
           countidx(vardim)  = (jend-jstart)/jstride + 1
         END IF
         strideidx(vardim) = jstride

         outstart(vardim) = (jstart - jds)/jstride + 1
         outcount(vardim) = jde
         ispatch(nvar) = .TRUE.
       ELSE IF ( vdimid == nxsid) THEN
         imod = MOD((ipss-idss),istride)
         IF (imod == 0) THEN
           startidx(vardim) = 1
         ELSE
           startidx(vardim) = istride - imod + 1
         END IF
         istart = ipss+startidx(vardim)-1
         iend   = MIN(ipse,idse)
         IF (iend < istart) THEN
           countidx(vardim)  = 0
         ELSE
           countidx(vardim)  = (iend-istart)/istride + 1
         END IF
         strideidx(vardim) = istride

         outstart(vardim) = (istart - idss)/istride + 1
         outcount(vardim) = idse
         ispatch(nvar) = .TRUE.
       ELSE IF ( vdimid == nysid) THEN
         jmod = MOD((jpss-jdss),jstride)
         IF (jmod == 0) THEN
           startidx(vardim) = 1
         ELSE
           startidx(vardim) = jstride - jmod + 1
         END IF
         jstart = jpss+startidx(vardim)-1
         jend   = MIN(jpse,jdse)
         IF (jend < jstart) THEN
           countidx(vardim)  = 0
         ELSE
           countidx(vardim)  = (jend-jstart)/jstride + 1
         END IF
         strideidx(vardim) = jstride

         outstart(vardim) = (jstart - jdss)/jstride + 1
         outcount(vardim) = jdse
         ispatch(nvar) = .TRUE.
       ELSE IF (vdimid == unlimdimid) THEN
         outstart(vardim) = unlimdimlen
         IF (unlimodimlen <= 0) THEN
           unlimodimlen = countidx(vardim)
         ELSE
           IF ( unlimodimlen /= countidx(vardim)) THEN
             WRITE(6,'(1x,a,/)') 'ERROR: Inconsisten size for UNLIMITED dimension.'
             istatus = -1
             RETURN
           END IF
         END IF
       ELSE
         outstart(vardim) = 1
       END IF

       IF (countidx(vardim) <= 0) THEN
         IF (debug > 0) THEN
           WRITE(6,'(9x,2a)') 'Processing variables - ',TRIM(varname)
           WRITE(6,'(12x,a,i0,3a)')                                  &
             'Path ',n,' skipped because dimension "',               &
             TRIM(diminnames(vdimid)),'" has zero length.'
          END IF
          CYCLE var_loop
       END IF
       narrsize = countidx(vardim)*narrsize
     END DO vardims_loop

     IF ( n > 1 .AND. (.NOT. ispatch(nvar)) ) THEN
       IF (debug > 2) WRITE(6,'(9x,3a)') 'Variable ',TRIM(varname),' skipped.'
       CYCLE
     ELSE
       IF (debug > 1) THEN
         WRITE(6,'(9x,3a,I2)')                                       &
         'Processing variables - ',TRIM(varname),' with rank = ',varndims

         DO vardim = 1,varndims
           vdimid = vardimids(vardim)
           WRITE(6,'(12x,a,4(a,I4))') diminnames(vdimid),            &
           ', outstart = ',outstart(vardim),', size = ', countidx(vardim), &
           ' <-- start = ',startidx(vardim),', stride = ',strideidx(vardim)
         END DO
       END IF
     END IF

     ovarid = varoutidlists(nvar)

!     IF (debug > 0) WRITE(6,'(9x,3a,I4)')                          &
!          'Copying variables - ',TRIM(varname),' from patch: ',n

      IF ( varndims == 3 ) THEN
        outcount3d = outcount(1:3)
      ELSEIF ( varndims == 4 ) THEN
        outcount4d = outcount(1:4)
      END IF

      SELECT CASE (vartype)

!-----------------------------------------------------------------------
!
!     integers
!
!-----------------------------------------------------------------------

      CASE (NF_INT)
      IF (debug> 1 ) WRITE(6,*) '### CASE integers'

!     ALLOCATE space for one variable and all patches
        IF (n == 1 .and. varndims == 2) THEN
          IF (ALLOCATED(varari)) DEALLOCATE(varari, STAT = istatus)
          ALLOCATE(varari(narrsize), STAT = istatus)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSEIF ( n == 1 .and. varndims == 3 ) THEN
          IF (ALLOCATED(varai3d)) DEALLOCATE(varai3d,STAT = istatus)
          ALLOCATE(varai3d(outcount3d(1),outcount3d(2),outcount3d(3)),STAT = istatus)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSEIF ( n == 1 .and. varndims == 4 ) THEN
          IF (ALLOCATED(varai4d)) DEALLOCATE(varai4d,STAT = istatus)
          ALLOCATE(varai4d(outcount4d(1),outcount4d(2),outcount4d(3),    &
             outcount4d(4)),STAT = istatus)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        END IF

!     READ data from patch
        IF ( varndims == 2 ) THEN
          WRITE(6,*) 'READ 2D INTEGER'
          istatus = NF_GET_VARS_INT(finid,varid,startidx,countidx,strideidx,varari)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSEIF ( varndims == 3 ) THEN
          istatus = NF_GET_VARS_INT(finid,varid,startidx,countidx,strideidx,   &
            varai3d(outstart(1):outstart(1)+countidx(1)-1,                     &
            outstart(2):outstart(2)+countidx(2)-1,1:outcount3d(3)))
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSEIF ( varndims == 4 ) THEN
          istatus = NF_GET_VARS_INT(finid,varid,startidx,countidx,strideidx,   &
            varai4d(outstart(1):outstart(1)+countidx(1)-1,                     &
            outstart(2):outstart(2)+countidx(2)-1,                             &
            outstart(3):outstart(3)+countidx(3)-1,                             &
            1:outcount4d(4)))
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        END IF

!     WRITE allocated data to final file
        IF( varndims == 2 ) THEN
          istatus = nf_put_vara_INT(foutid,ovarid,outstart,countidx,varari)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
          DEALLOCATE(varari)
        END IF
        IF (n == npatch .and. varndims == 3 ) THEN
          istatus = nf_put_vara_INT(foutid,ovarid,(/1,1,1/),outcount3d,varai3d)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
          DEALLOCATE(varai3d)
        ELSEIF (n == npatch .and. varndims == 4 ) THEN
          istatus = nf_put_vara_INT(foutid,ovarid,(/1,1,1,1/),outcount4d,varai4d)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
          DEALLOCATE(varai4d)
        ENDIF
        !istatus = nf_put_vara_INT(foutid,ovarid,(/1,1,1/),(/ide-1,jde-1,countidx(3)/),varai)
        !istatus = nf_put_var_INT(foutid,ovarid,varai)
        !istatus = nf_put_vara_INT(foutid,ovarid,outstart,countidx,varari)

!-----------------------------------------------------------------------
!
!     reals
!
!-----------------------------------------------------------------------

      CASE (NF_FLOAT)
      IF (debug> 1 ) WRITE(6,*) '### CASE reals'

!     ALLOCATE space for one variable and all patches
        IF (n == 1 .and. varndims == 2) THEN
          IF (ALLOCATED(vararr)) DEALLOCATE(vararr, STAT = istatus)
          ALLOCATE(vararr(narrsize), STAT = istatus)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSEIF ( n == 1 .and. varndims == 3 ) THEN
          IF (ALLOCATED(varar3d)) DEALLOCATE(varar3d,STAT = istatus)
          ALLOCATE(varar3d(outcount3d(1),outcount3d(2),outcount3d(3)),STAT = istatus)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSEIF ( n == 1 .and. varndims == 4 ) THEN
          IF (ALLOCATED(varar4d)) DEALLOCATE(varar4d,STAT = istatus)
          ALLOCATE(varar4d(outcount4d(1),outcount4d(2),outcount4d(3),    &
             outcount4d(4)),STAT = istatus)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        END IF

!     READ data from patch
        IF ( varndims == 2 ) THEN
          istatus = NF_GET_VARS_REAL(finid,varid,startidx,countidx,strideidx,vararr)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSEIF ( varndims == 3 ) THEN
          istatus = NF_GET_VARS_REAL(finid,varid,startidx,countidx,strideidx,   &
            varar3d(outstart(1):outstart(1)+countidx(1)-1,                     &
            outstart(2):outstart(2)+countidx(2)-1,1:outcount3d(3)))
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        ELSEIF ( varndims == 4 ) THEN
          istatus = NF_GET_VARS_REAL(finid,varid,startidx,countidx,strideidx,  &
            varar4d(outstart(1):outstart(1)+countidx(1)-1,                     &
            outstart(2):outstart(2)+countidx(2)-1,                             &
            outstart(3):outstart(3)+countidx(3)-1,                             &
            1:outcount4d(4)))
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        END IF

!     WRITE allocated data to final file
        IF( varndims == 2 ) THEN
          istatus = nf_put_vara_REAL(foutid,ovarid,outstart,countidx,vararr)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
          DEALLOCATE(vararr)
        END IF
        IF (n == npatch .and. varndims == 3 ) THEN
          istatus = nf_put_vara_REAL(foutid,ovarid,(/1,1,1/),outcount3d,varar3d)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
          DEALLOCATE(varar3d)
        ELSEIF (n == npatch .and. varndims == 4 ) THEN
          istatus = nf_put_vara_REAL(foutid,ovarid,(/1,1,1,1/),outcount4d,varar4d)
          IF (istatus /= NF_NOERR) CALL handle_err(istatus)
          DEALLOCATE(varar4d)
        ENDIF

!-----------------------------------------------------------------------
!
!     Character string
!
!-----------------------------------------------------------------------

      CASE (NF_CHAR)
      IF (debug> 1 ) WRITE(6,*) '### CASE strings'

      istatus = NF_GET_VARS_TEXT(finid,varid,startidx,countidx,strideidx,tmpstr)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      istatus = nf_put_vara_TEXT(foutid,ovarid,outstart,countidx,TRIM(tmpstr))
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)

      CASE DEFAULT
        WRITE(6,'(1x,a,I2)') 'ERROR: unsupported variable type = ',vartype
        istatus = -4
        RETURN
      END SELECT

!-----------------------------------------------------------------------
!     END SELECT
!-----------------------------------------------------------------------

      IF (debug> 1 ) WRITE(6,*) '### Close file'
       istatus = nf_close(finid)                              ! Close file
       IF (istatus /= NF_NOERR) CALL handle_err(istatus)
     END DO patch_loop
    END DO var_loop

    unlimdimlen = unlimdimlen + unlimodimlen                 ! Add # of time levels
                                                             ! in output file
!-----------------------------------------------------------------------
!
! Close the output file if applicable
!
!-----------------------------------------------------------------------

    IF (.NOT. jointime .OR. nf == nfile) THEN

      IF (attadj) THEN

        IF ( .NOT. lonavg .AND. .NOT. latavg .AND. latset == 0 .AND. lonset == 0) THEN ! at grid point
          newctrlat = 0.5*(lat1+lat2)
          newctrlon = 0.5*(lon1+lon2)
          latset = latset+1
          lonset = lonset+1
        END IF

        IF (latset /= 1 .OR. lonset /= 1) THEN
          WRITE(*,'(1x,2(a,i0),8x,a)')                                  &
            'ERROR: latset = ',latset,', lonset = ',lonset,             &
            'Program aborting ...     Please report.'
          STOP
        END IF

        WRITE(6,'(/,9x,a,2F8.2,/)')                                     &
           'Changing CEN_LAT & CEN_LON to ',newctrlat,newctrlon

        istatus = NF_REDEF( foutid )
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)

        istatus = NF_PUT_ATT_REAL(foutid,NF_GLOBAL,'CEN_LAT',NF_REAL,1,newctrlat)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        istatus = NF_PUT_ATT_REAL(foutid,NF_GLOBAL,'MOAD_CEN_LAT',NF_REAL,1,newctrlat)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)
        istatus = NF_PUT_ATT_REAL(foutid,NF_GLOBAL,'CEN_LON',NF_REAL,1,newctrlon)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)

        istatus = NF_ENDDEF(foutid)
        IF (istatus /= NF_NOERR) CALL handle_err(istatus)
      END IF

      istatus = nf_close(foutid)
      IF (istatus /= NF_NOERR) CALL handle_err(istatus)
    END IF

  END DO files_loop

  RETURN
END SUBROUTINE joinwrfncdf

!
!##################################################################
!##################################################################
!######                                                      ######
!######       SUBROUTINE get_wrf_patch_indices               ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!
SUBROUTINE get_wrf_patch_indices(filename,io_form,ips,ipe,ipss,ipse,    &
                                 jps,jpe,jpss,jpse,nx,ny,istatus)

!-----------------------------------------------------------------------
!
!  PURPOSE:
!    Get the size of data patch stored in the WRF data file
!
!-----------------------------------------------------------------------
!
!  AUTHOR:
!  Yunheng Wang (04/26/2007)
!
!  MODIFICATION HISTORY:
!
!-----------------------------------------------------------------------

  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN)  :: filename
  INTEGER,          INTENT(IN)  :: io_form
  INTEGER,          INTENT(OUT) :: ips, ipe, jps, jpe
  INTEGER,          INTENT(OUT) :: ipss,ipse,jpss,jpse
  INTEGER,          INTENT(OUT) :: nx,ny
  INTEGER,          INTENT(OUT) :: istatus

  INCLUDE 'netcdf.inc'

!------------------------------------------------------------------
!
!  Misc. local variables
!
!------------------------------------------------------------------

  INTEGER           :: ncid
  CHARACTER(LEN=80) :: errmsg

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  istatus = 0
  IF (io_form == 7) THEN
    istatus = NF_OPEN(TRIM(filename),NF_NOWRITE,ncid)
    IF(istatus /= NF_NOERR)  THEN
       print*,'ERROR with file: ',trim(filename)
       GO TO 999
    END IF

    istatus = NF_GET_ATT_INT(ncid,NF_GLOBAL,'WEST-EAST_PATCH_START_STAG',ips)
    IF(istatus /= NF_NOERR)  GO TO 999

    istatus = NF_GET_ATT_INT(ncid,NF_GLOBAL,'WEST-EAST_PATCH_END_STAG',ipe)
    IF(istatus /= NF_NOERR)  GO TO 999

    istatus = NF_GET_ATT_INT(ncid,NF_GLOBAL,'WEST-EAST_PATCH_START_UNSTAG',ipss)
    IF(istatus /= NF_NOERR)  GO TO 999

    istatus = NF_GET_ATT_INT(ncid,NF_GLOBAL,'WEST-EAST_PATCH_END_UNSTAG',ipse)
    IF(istatus /= NF_NOERR)  GO TO 999

    istatus = NF_GET_ATT_INT(ncid,NF_GLOBAL,'SOUTH-NORTH_PATCH_START_STAG',jps)
    IF(istatus /= NF_NOERR)  GO TO 999

    istatus = NF_GET_ATT_INT(ncid,NF_GLOBAL,'SOUTH-NORTH_PATCH_END_STAG',jpe)
    IF(istatus /= NF_NOERR)  GO TO 999

    istatus = NF_GET_ATT_INT(ncid,NF_GLOBAL,'SOUTH-NORTH_PATCH_START_UNSTAG',jpss)
    IF(istatus /= NF_NOERR)  GO TO 999

    istatus = NF_GET_ATT_INT(ncid,NF_GLOBAL,'SOUTH-NORTH_PATCH_END_UNSTAG',jpse)
    IF(istatus /= NF_NOERR)  GO TO 999

    istatus = NF_GET_ATT_INT(ncid,NF_GLOBAL,'WEST-EAST_GRID_DIMENSION',nx)
    IF(istatus /= NF_NOERR)  GO TO 999

    istatus = NF_GET_ATT_INT(ncid,NF_GLOBAL,'SOUTH-NORTH_GRID_DIMENSION',ny)
    IF(istatus /= NF_NOERR)  GO TO 999

    istatus = NF_CLOSE(ncid)
    IF(istatus /= NF_NOERR)  GO TO 999
  ELSE
    istatus   = -1
    ips = 0
    ipe = 0
    ipse= 0
    jps = 0
    jpe = 0
    jpse= 0
    WRITE(6,'(1x,a,/)')       &
      'WARNING: Only support netCDF file at present for patch indices.'
  END IF

  RETURN

  999 CONTINUE
  errmsg = NF_STRERROR(istatus)
  WRITE(6,'(1x,2a)') 'NetCDF error: ',errmsg
  istatus = -1

  RETURN
END SUBROUTINE get_wrf_patch_indices
!
! get WRF/METGRID data file name
!
SUBROUTINE get_wrf_filename(filename_convention,dirname,grid_id,        &
                            year,month,day,hour,minute,second,          &
                            colon,magnitude_processor,proc,splitfile,   &
                            filename,istatus)

!#######################################################################

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: filename_convention
  INTEGER, INTENT(IN) :: grid_id
  INTEGER, INTENT(IN) :: year,month,day,hour,minute,second
  INTEGER, INTENT(IN) :: magnitude_processor
  INTEGER, INTENT(IN) :: proc
  LOGICAL, INTENT(IN) :: splitfile

  CHARACTER(LEN=*), INTENT(IN)  :: dirname
  CHARACTER(LEN=1), INTENT(IN)  :: colon
  CHARACTER(LEN=*), INTENT(OUT) :: filename
  INTEGER,          INTENT(OUT) :: istatus

!-----------------------------------------------------------------------

  CHARACTER(LEN=3 ) :: faffix
  CHARACTER(LEN=80) :: fheader, ftrailer
  CHARACTER(LEN=80) :: fmtstr

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  istatus = 0

  SELECT CASE (filename_convention)
  CASE (0,3)
    WRITE(fheader,'(a)') 'wrfout_d'
    WRITE(faffix, '(a)') '   '
  CASE (1)
    WRITE(fheader,'(a)') 'met_em.d'
    WRITE(faffix, '(a)') '.nc'
  CASE (2)
    WRITE(fheader,'(a)') 'met_nmm.d'
    WRITE(faffix, '(a)') '.nc'
  CASE DEFAULT
    WRITE(6,'(1x,a,I2)') 'ERROR: Unsupported file name convention - ',filename_convention
    istatus = -1
    RETURN
  END SELECT

  IF (splitfile) THEN
    WRITE(fmtstr,'(a,2(I1,a))') '(2a,I',magnitude_processor,'.',magnitude_processor,')'
    WRITE(ftrailer,FMT=TRIM(fmtstr)) TRIM(faffix),'_',proc
  ELSE
    WRITE(ftrailer,FMT='(a)')        TRIM(faffix)
  END IF

  WRITE(filename,FMT='(a,a,I2.2,a,I4.4,5(a,I2.2),a)')                   &
      TRIM(dirname),TRIM(fheader),grid_id,'_',                          &
      year,'-',month,'-',day,'_',hour,colon,minute,colon,second,        &
      TRIM(ftrailer)

  RETURN
END SUBROUTINE get_wrf_filename

!#######################################################################
!
! Handle netCDF error
!
!#######################################################################

SUBROUTINE handle_err(istat)

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: istat
  INCLUDE 'netcdf.inc'

  IF (istat /= NF_NOERR) THEN
    PRINT *, TRIM(nf_strerror(istat))
    STOP 'NetCDF error!'
  END IF

  RETURN
END SUBROUTINE handle_err

!
! Convert a character string to upper case
!
FUNCTION upcase(string) RESULT(upper)

!#######################################################################

  IMPLICIT NONE

  INTEGER, PARAMETER :: lenstr = 20

  CHARACTER(LEN=lenstr), INTENT(IN) :: string
  CHARACTER(LEN=lenstr)             :: upper

  INTEGER :: j

  DO j = 1,lenstr
    IF(string(j:j) >= "a" .AND. string(j:j) <= "z") THEN
      upper(j:j) = ACHAR(IACHAR(string(j:j)) - 32)
    ELSE
      upper(j:j) = string(j:j)
    END IF
  END DO
END FUNCTION upcase
