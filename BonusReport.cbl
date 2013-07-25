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
             ASSIGN TO "OUTPUT.RPT"
             ORGANIZATION IS LINE SEQUENTIAL.
      *****************************************************************
       DATA DIVISION.
         FILE SECTION.
         FD  PAYROLL-MASTER
             RECORD CONTAINS 82 CHARACTERS.
         01  PAYROLL-MASTER-IN.
            05  EMPNO-IN                PIC X(5).
            05  EMPNAME-IN              PIC X(20).
            05  TERR-NO-IN              PIC X(2).
            05  OFFICE-NO-IN            PIC X(2).
            05  ANNUAL-SALARY-IN        PIC 9(6).
            05                          PIC X(29).
            05  DATE-HIRED-IN.
                10 HIRE-DATE-MO-IN      PIC 9(2).
                10 HIRE-DATE-DAY-IN     PIC 9(2).
                10 HIRE-DATE-YR-IN      PIC 9(4).
            05                          PIC X(10).
         FD  PAYROLL-OUT
             RECORD CONTAINS 80 CHARACTERS.
         01  PAYROLL-BONUS-RECORD       PIC X(80).
         WORKING-STORAGE SECTION.
         01  WORK-AREAS.
             05  WA-ARE-THERE-MORE-RECORDS PIC X(3)  VALUE 'YES'.
                 88  WA-MORE-RECORDS                 VALUE 'YES'.
                 88  WA-NO-MORE-RECORDS              VALUE 'NO '.
             05  WA-FIRST-RECORD        PIC X(3)     VALUE 'YES'.
             05  WA-RECORD-CTR          PIC 9(4)     VALUE ZEROS.
             05  WA-PAGE-CTR            PIC 999      VALUE ZEROS.
             05  WA-OLD-OFFICE-NO       PIC XX       VALUE ZEROS.
             05  WA-OLD-TERR-NO         PIC XX       VALUE ZEROS.
             05  WA-BONUS-AMT           PIC 99999V99 VALUE ZEROS.
             05  WA-TODAY-DATE.
                 10  WA-IN-YR           PIC 9(4).
                 10  WA-IN-MO           PIC 99.
                 10  WA-IN-DAY          PIC 99.
         01  WORK-CONSTANTS.
             05  WC-BONUS-PERC          PIC V99      VALUE .10.
             05  WC-YEAR-AFTER         PIC 9(4)     VALUE 1994.
         01  PAGE-HEADING.
             05                         PIC X(40)    VALUE SPACES.
             05                         PIC X(13)  
                 VALUE 'BONUS  REPORT'.
             05                         PIC X(7)     VALUE SPACES.
             05                         PIC X(5)     VALUE 'PAGE '.
             05  PH-HEAD-PAGE           PIC 99.
             05                         PIC X(3)     VALUE SPACES.
             05  PH-HEAD-DATE.
                 10  PH-HEAD-MO         PIC 99.
                 10                     PIC X        VALUE '/'.
                 10  PH-HEAD-DAY        PIC 99.
                 10                     PIC X        VALUE '/'.
                 10  PH-HEAD-YR         PIC 9(4).
         01  TERR-HEADING.
             05                         PIC X(10)    VALUE SPACES.
             05                         PIC X(13)  
                 VALUE 'TERRITORY -- '.
             05  TH-TERR-NO             PIC XX.
             05                         PIC X(55)    VALUE SPACES.
         01  OFFICE-HEADING.
             05                         PIC X(20)    VALUE SPACES.
             05                         PIC X(10)    VALUE 'OFFICE -- '.
             05  OH-OFFICE-NO           PIC XX.
             05                         PIC X(48)    VALUE SPACES.
         01  EMP-HEADING.
             05                         PIC X(10)    VALUE SPACES.
             05                         PIC X(13)  
                 VALUE 'EMPLOYEE NAME'.
             05                         PIC X(7)     VALUE SPACES.
             05                         PIC X(5)     VALUE 'BONUS'.
             05                         PIC X(45)    VALUE SPACES.
             05  EH-EMPNO               PIC XX.
             05                         PIC X(38)    VALUE SPACES.
         01  BONUS-INFO-LINE.
             05                         PIC X(10)    VALUE SPACES.
             05  BIL-EMPNAME            PIC X(20).
             05  BIL-BONUS-AMOUNT       PIC $ZZ,ZZZ.ZZ.
             05                         PIC X(40)    VALUE SPACES.
      *****************************************************************
       PROCEDURE DIVISION.
       100-MAIN-MOD.
         PERFORM 800-OPEN-FILES-MOD
         PERFORM 200-GET-DATE
         PERFORM UNTIL WA-NO-MORE-RECORDS
           READ PAYROLL-MASTER
             AT END
               MOVE 'NO ' TO WA-ARE-THERE-MORE-RECORDS
             NOT AT END
               ADD 1 TO WA-RECORD-CTR
               PERFORM 300-CALC-MOD
         END-PERFORM
         PERFORM 900-CLOSE-FILES-MOD
         STOP RUN.
      *****************************************************************
       200-GET-DATE.
         DISPLAY "PLEASE ENTER TODAY'S DATE(YYYYMMDD): "
         ACCEPT WA-TODAY-DATE
               
         MOVE WA-IN-MO  TO PH-HEAD-MO
         MOVE WA-IN-DAY TO PH-HEAD-DAY
         MOVE WA-IN-YR  TO PH-HEAD-YR.
      *****************************************************************
       300-CALC-MOD.
         EVALUATE TRUE
           WHEN WA-FIRST-RECORD = 'YES'
             MOVE OFFICE-NO-IN TO WA-OLD-OFFICE-NO
             MOVE TERR-NO-IN TO WA-OLD-TERR-NO
             PERFORM 400-HEADING-MOD
             MOVE 'NO ' TO WA-FIRST-RECORD
           WHEN TERR-NO-IN NOT = WA-OLD-TERR-NO
             DISPLAY "BREAK TERR"
             PERFORM 500-TERR-BREAK
           WHEN OFFICE-NO-IN NOT = WA-OLD-OFFICE-NO
             DISPLAY "BREAK OFFICE"
             PERFORM 600-OFFICE-BREAK
         END-EVALUATE
         
         MOVE EMPNAME-IN   TO BIL-EMPNAME
         
         IF HIRE-DATE-YR-IN < WC-YEAR-AFTER
           DISPLAY HIRE-DATE-YR-IN
           COMPUTE WA-BONUS-AMT = ANNUAL-SALARY-IN * WC-BONUS-PERC
           ADD WA-BONUS-AMT TO ANNUAL-SALARY-IN
             ON SIZE ERROR
               DISPLAY 'TOTAL TERRITORY SALARY FIELD SHORT FOR RECORD '
                  'NUMBER,  ', WA-RECORD-CTR
               STOP RUN
             NOT ON SIZE ERROR
               CONTINUE
           END-ADD
           MOVE WA-BONUS-AMT TO BIL-BONUS-AMOUNT
         ELSE
           MOVE ZERO TO BIL-BONUS-AMOUNT
         END-IF
                 
         WRITE PAYROLL-BONUS-RECORD FROM BONUS-INFO-LINE
           AFTER ADVANCING 2 LINES.
      *****************************************************************
       400-HEADING-MOD.
         ADD 1 TO WA-PAGE-CTR
         MOVE WA-PAGE-CTR       TO PH-HEAD-PAGE
         MOVE WA-OLD-TERR-NO    TO TH-TERR-NO
         MOVE WA-OLD-OFFICE-NO  TO OH-OFFICE-NO
         WRITE PAYROLL-BONUS-RECORD FROM PAGE-HEADING
             AFTER ADVANCING PAGE
         WRITE PAYROLL-BONUS-RECORD FROM TERR-HEADING
             AFTER ADVANCING 2 LINES
         WRITE PAYROLL-BONUS-RECORD FROM OFFICE-HEADING
             AFTER ADVANCING 2 LINES
         WRITE PAYROLL-BONUS-RECORD FROM EMP-HEADING
             AFTER ADVANCING 2 LINES.
      *****************************************************************
       500-TERR-BREAK.
         MOVE TERR-NO-IN TO WA-OLD-TERR-NO
         PERFORM 600-OFFICE-BREAK.
      *****************************************************************
       600-OFFICE-BREAK.
         MOVE OFFICE-NO-IN TO WA-OLD-OFFICE-NO
         IF TERR-NO-IN = WA-OLD-TERR-NO
            PERFORM 400-HEADING-MOD
         END-IF.
      *****************************************************************
       800-OPEN-FILES-MOD.
         OPEN    INPUT  PAYROLL-MASTER
                 OUTPUT PAYROLL-OUT.
      *****************************************************************
       900-CLOSE-FILES-MOD.
         CLOSE   PAYROLL-MASTER
                 PAYROLL-OUT.