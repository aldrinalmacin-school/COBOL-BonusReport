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
             ORGANIZATION IS LINE SEQUENTIAL
             
           SELECT PAYROLL-OUT
             ASSIGN TO "CH0901.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
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
            05                         PIC X(29).
            05  DATE-HIRED-IN.
                10 DATE-MO-IN           PIC X(2).
                10 DATE-DAY-IN          PIC X(2).
                10 DATE-YR-IN           PIC X(4).
         FD  PAYROLL-OUT.
         WORKING-STORAGE SECTION.
         01  ARE-THERE-MORE-RECORDS     PIC X(3)   VALUE 'YES'.
             05  MORE-RECORDS                      VALUE 'YES'.
             05  NO-MORE-RECORDS                   VALUE 'NO '.
         01  HL-HEADING1.
             05                         PIC X(40)  VALUE SPACES.
             05                         PIC X(13)  VALUE 'BONUS  REPORT'
             05                         PIC X(5)   VALUE 'PAGE '
             05  HEAD-PAGE              PIC ZZ9.
      *****************************************************************
       PROCEDURE DIVISION.