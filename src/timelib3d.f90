!
!##################################################################
!##################################################################
!######                                                      ######
!######                SUBROUTINE JULDAY                     ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE julday( year, month, day, jday )
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Compute Julian day from year, month, and day
!
!  Start from 1 (Jan. 1) to 365, or 366 for leap year (Dec. 31)
!
!  The rule is that a year will be a leap year if
!
!    the year can be divided by 400, or
!    the year can by divided by 4, but not by 100
!
!  Form this rule year 1972, 1984, 1996, and 2000 are leap years,
!  but 1700, 1800 and 1900 are not.
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Yuhe Liu
!  07/29/93
!
!  MODIFICATIONS:
!
!  05/06/1998 (Yuhe Liu)
!  Corrected the leap year calculation.
!
!-----------------------------------------------------------------------
!
!  INPUT:
!
!    year       Reference calendar year
!    month      Reference monthe of the year
!    day        Reference day of the month
!
!    OUTPUT:
!
!    jday       Julian day, start from 1 -- Jan. 1 to 365 -- Dec. 31
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE

  INTEGER :: year, month, day, jday
  INTEGER :: lpyear, lp

  INTEGER :: mndys(12)     ! Day numbers for each month
  DATA mndys/0,31,59,90,120,151,181,212,243,273,304,334/
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  IF ( MOD(year,400) == 0 .OR.                                          &
         (MOD(year,4) == 0 .AND. MOD(year,100) /= 0 ) ) THEN
    lpyear = 1
  ELSE
    lpyear = 0
  END IF

  lp = 0
  IF ( month > 2 ) lp = lpyear

  jday = mndys(month) + day + lp

  RETURN
END SUBROUTINE julday
!
!##################################################################
!##################################################################
!######                                                      ######
!######                SUBROUTINE CALDAY                     ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE calday( jday, year, month, day )
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Computes the month and day from Julian day.
!
!  Start from Jan. 1 -- day 1 to Dec. 31 -- day 365, or 366 for leap.
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Yuhe Liu
!  07/29/93
!
!  MODIFICATIONS:
!
!  05/06/1998 (Yuhe Liu)
!  Corrected the leap year calculation.
!
!-----------------------------------------------------------------------
!
!  INPUT:
!
!    year       Calendar year
!    jday       Julian day, start from 1 -- Jan. 1 to 365 -- Dec. 31
!
!    OUTPUT:
!
!    month      Monthe of the year
!    day        Day of the month
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE

  INTEGER :: jday
  INTEGER :: year, month, day

  INTEGER :: i, lp

  INTEGER :: mndys(12)     ! Day numbers for each month
  DATA mndys/0,31,59,90,120,151,181,212,243,273,304,334/
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  IF ( MOD(year,400) == 0 .OR.                                          &
         (MOD(year,4) == 0 .AND. MOD(year,100) /= 0) ) THEN
    lp = 1
  ELSE
    lp = 0
  END IF

  DO i = 1, 2
    IF ( jday > mndys(i) ) THEN
      month = i
    END IF
  END DO

  DO i = 3, 12
    IF ( jday > mndys(i)+lp ) THEN
      month = i
    END IF
  END DO

  day = jday - mndys(month)
  IF ( month > 2 ) day = day - lp
!
  RETURN
END SUBROUTINE calday
!
!##################################################################
!##################################################################
!######                                                      ######
!######                SUBROUTINE CTIM2ABSS                  ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE ctim2abss( iyr,imon,idy,ihr,imin,isec, abstsec )
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Convert the calendar date and time to absolute time in second
!  beginning from 00:00:00 UTC, Jan. 1, 1960
!
!-----------------------------------------------------------------------
!
!  AUTHOR:
!
!-----------------------------------------------------------------------
!
!  INPUT:
!
!    iyr        Calendar year
!    imon       Monthe of the year
!    idy        Day of the month
!
!    ihr        Hours in the day
!    imin       Minute of the hour
!    isec       Seconds of the minute
!
!  OUTPUT:
!
!    abstsec     Time in seconds referred to 00:00:00, Jan. 1, 1960
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE

  INTEGER :: iyr, imon, idy, ihr, imin, isec
  INTEGER :: abstsec

  INTEGER :: lp,jdy
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  lp = (iyr + 3 - 1960) / 4         ! Number of leap days since 1960

  CALL julday(iyr, imon, idy, jdy)

  abstsec = (iyr-1960) * 31536000                                       &
          + (jdy-1)    * 86400                                          &
          + ihr        * 3600                                           &
          + imin       * 60                                             &
          + isec

  abstsec = abstsec + 86400 * lp

  RETURN
END SUBROUTINE ctim2abss
!
!##################################################################
!##################################################################
!######                                                      ######
!######                SUBROUTINE ABSS2CTIM                  ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE abss2ctim( abstsec, iyr, imon, idy, ihr, imin, isec )
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Convert the absolute time(sec) starting from 00:00:00 UTC, Jan.
!  1, 1960, to calendar day time.
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Yuhe Liu
!  05/24/94
!
!-----------------------------------------------------------------------
!
!  INPUT:
!
!    abstsec     Time in seconds starting from 00:00:00, Jan. 1, 1960
!
!  OUTPUT:
!
!    iyr        Calendar year
!    jdy        Julian day
!
!    ihr        Hours in the day
!    imin       Minutes of the hour
!    isec       Seconds of the minute
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE
!
  INTEGER :: abstsec
  INTEGER :: iyr, imon, idy, ihr, imin, isec

  INTEGER :: i,j,k,lp,jdy
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  isec = MOD( abstsec, 60 )
  imin = MOD( abstsec/60, 60)
  ihr  = MOD( abstsec/3600, 24)

  jdy  = abstsec/86400 + 1

  i = ( jdy - 1 ) / 1461
  j = MOD( jdy - 1, 1461) + 1
  k = ( j - 2 ) / 365

  iyr = i * 4 + k + 1960
  lp = i + ( k + 3 ) / 4

  jdy = jdy - ( i * 4 + k ) * 365 - lp

  CALL calday( jdy, iyr, imon, idy )

  RETURN
END SUBROUTINE abss2ctim
