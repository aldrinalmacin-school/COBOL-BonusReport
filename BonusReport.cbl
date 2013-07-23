      *NAME: ALDRIN JEROME ALMACIN
      *DATE: JULY 21, 2013
      *PURPOSE: TO CREATE A PROPERLY FORMATTED BONUS REPORT PROGRAM.
       IDENTIFICATION DIVISION.
         PROGRAM-ID. BONUS-REPORT.
      *****************************************************************
       ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT PAYROLL-MASTER
             ASSIGN TO "CH0901.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.
             
           SELECT PAYROLL-OUT
             ASSIGN TO "CH0901.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.
      *****************************************************************
       DATA DIVISION.
         FILE SECTION.
         FD  PAYROLL-MASTER.
         01  PAYROLL-MASTER-IN.
            05  EMPLOYEE-NO-IN          PIC X(5).
            05  EMPLOYEE-NAME-IN        PIC X(20).
            05  TERRITORY-NO-IN         PIC X(2).
            05  OFFICE-NO-IN            PIC X(2).
            05  ANNUAL-SALARY-IN        PIC 9(6).
            05                          PIC X(29).
            05  DATE-HIRED-IN.
                10 DATE-MO-IN           PIC X(2).
                10 DATE-DAY-IN          PIC X(2).
                10 DATE-YR-IN           PIC X(4).
         FD  PAYROLL-OUT.
         01  PAYROLL-BONUS-RECORD       PIC X(80).
         WORKING-STORAGE SECTION.
         01  WORK-AREAS.
             05  WA-ARE-THERE-MORE-RECORDS PIC X(3)   VALUE 'YES'.
                 88  WA-MORE-RECORDS                  VALUE 'YES'.
                 88  WA-NO-MORE-RECORDS               VALUE 'NO '.
         01  PAGE-HEADING.
             05                         PIC X(40)  VALUE SPACES.
             05                         PIC X(13)  
                 VALUE 'BONUS  REPORT'.
             05                         PIC X(7)   VALUE SPACES.
             05                         PIC X(5)   VALUE 'PAGE '.
             05  PH-HEAD-PAGE           PIC 99.
             05                         PIC X(3)   VALUE SPACES.
             05  PH-HEAD-DATE.
                 10  PH-HEAD-MO         PIC 99.
                 10                     PIC X      VALUE '/'.
                 10  PH-HEAD-DAY        PIC 99.
                 10                     PIC X      VALUE '/'.
                 10  PH-HEAD-YR         PIC 9(4).
         01  TERRITORY-HEADING.
             05                         PIC X(10)  VALUE SPACES.
             05                         PIC X(13)  
                 VALUE 'TERRITORY -- '.
             05  TH-TERRITORY-NO        PIC XX.
             05                         PIC X(55)  VALUE SPACES.
         01  OFFICE-HEADING.
             05                         PIC X(20)  VALUE SPACES.
             05                         PIC X(10)  VALUE 'OFFICE -- '.
             05  OH-OFFICE-NO           PIC XX.
             05                         PIC X(48)  VALUE SPACES.
         01  EMPLOYEE-HEADING.
             05                         PIC X(10)  VALUE SPACES.
             05                         PIC X(13)  
                 VALUE 'EMPLOYEE NAME'.
             05                         PIC X(7)   VALUE SPACES.
             05                         PIC X(5)   VALUE 'BONUS'.
             05                         PIC X(45)  VALUE SPACES.
             05  OH-OFFICE-NO           PIC XX.
             05                         PIC X(48)  VALUE SPACES.
         01  BONUS-INFO-LINE.
             05  BIL-EMPLOYEE-NAME      PIC X(20).
             05  BIL-BONUS-AMOUNT       PIC $ZZ,ZZZ.99.
      *****************************************************************
       PROCEDURE DIVISION.