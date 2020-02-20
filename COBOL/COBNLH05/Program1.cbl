             IDENTIFICATION DIVISION.
       PROGRAM-ID.     CBLNLH05.
       DATE-WRITTEN.   1/22/2020.
       AUTHOR.         NICK HOUSER.
       DATE-COMPILED.
      *******************************************
      *  THIS PROGRAM READS A FILE AND CREATES  *
      *      A REPORT OF POP CASES SOLD BY      *
      *    TEAM. IT CREATES AN ERROR REPORT     *
      *            ON INVALID INPUT.            *
      *******************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT POP-SALES-REPORT
               ASSIGN TO 'C:\SCHOOL\COBOL\CBLPOPSL.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRINTOUT
               ASSIGN TO 'C:\SCHOOL\COBOL\CBLPOPSL.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.

           SELECT ERROROUT
               ASSIGN TO 'C:\SCHOOL\COBOL\CBLPOPER.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.
      
       DATA DIVISION.
       FILE SECTION.

       FD POP-SALES-REPORT
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-POPSALES
           RECORD CONTAINS 71 CHARACTERS.
      *DECLARING INPUT VARIABLES
       01 I-POPSALES.
           05 I-LNAME                  PIC X(15).
           05 I-FNAME                  PIC X(15).
           05 I-ADDRESS                PIC X(15).
           05 I-CITY                   PIC X(10).
           05 I-STATE                  PIC XX.
               88 VAL-STATE        VALUE 'IA' 'IL' 'MI' 'MO' 'NE' 'WI'.
               88 VAL-5CENTS       VALUE 'IA' 'NE' 'WI'.
           05 I-ZIP5                   PIC 9(5).
           05 I-ZIP4                   PIC 9(4).
           05 I-POP-TYPE               PIC 99.
               88 VAL-POP-TYPE     VALUE 1 THRU 6.
           05 I-NUM-CASES              PIC 99.
           05 I-TEAM                   PIC X.
               88 VAL-TEAM         VALUE 'A' THRU 'E'.

       FD PRINTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRINTLINE
           LINAGE IS 60 WITH FOOTING AT 54.

       01 PRINTLINE                PIC X(132).

       FD ERROROUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS ERRORLINE
           LINAGE IS 60 WITH FOOTING AT 54.

       01 ERRORLINE            PIC X(132).

       WORKING-STORAGE SECTION.
       01 WORK-AREA.
           05  EOF             PIC X(5)        VALUE 'FALSE'.
           05  ERROR-SWITCH    PIC X(4).

      *CALCULATED VARIABLES
           05  C-DEPOSIT-AMT   PIC 9(4)V99.
           05  C-TOTAL-SALES   PIC 9(5)V99.
           05  C-PCTR-D        PIC 99          VALUE 0.
           05  C-PCTR-E        PIC 99          VALUE 0.

      *GRAND-TOTAL-ACCUMULATORS
           05  GT-POP1-CASES   PIC 9(6)        VALUE 0.
           05  GT-POP2-CASES   PIC 9(6)        VALUE 0.
           05  GT-POP3-CASES   PIC 9(6)        VALUE 0.
           05  GT-POP4-CASES   PIC 9(6)        VALUE 0.
           05  GT-POP5-CASES   PIC 9(6)        VALUE 0.
           05  GT-POP6-CASES   PIC 9(6)        VALUE 0.
           05  GT-TEAMA-AMT    PIC 9(9)V99     VALUE 0.
           05  GT-TEAMB-AMT    PIC 9(9)V99     VALUE 0.
           05  GT-TEAMC-AMT    PIC 9(9)V99     VALUE 0.
           05  GT-TEAMD-AMT    PIC 9(9)V99     VALUE 0.
           05  GT-TEAME-AMT    PIC 9(9)V99     VALUE 0.
           05  ERR-CTR         PIC 9(4)        VALUE 0.

       01 CURRENT-DATE-AND-TIME.
           05  I-DATE.
               10  I-YY        PIC 9(4).
               10  I-MM        PIC 99.
               10  I-DD        PIC 99.
           05  I-TIME          PIC X(11).

       01 BLANK-LINE.
           05 FILLER           PIC X(132).

       01 COMPANY-TITLE.
           05  FILLER          PIC X(6)        VALUE 'DATE:'.
           05  O-MM            PIC 99.
           05  FILLER          PIC X           VALUE '/'.
           05  O-DD            PIC 99.
           05  FILLER          PIC X           VALUE '/'.
           05  O-YY            PIC 9(4).
           05  FILLER          PIC X(36)       VALUE ' '.
           05  FILLER          PIC X(72)                         
                                   VALUE "ALBIA SOCCER CLUB FUNDRAISER".
           05  FILLER          PIC X(6)        VALUE 'PAGE:'.
           05  O-PCTR          PIC Z9.

       01 REPORT-TITLE-LINE1.
           05  FILLER          PIC X(58)       VALUE 'COBNLH05'.
           05  FILLER          PIC X(74)       VALUE 'HOUSER DIVISION'.
       01 REPORT-TITLE-LINE2.
           05  FILLER          PIC X(60)       VALUE ' '.
           05  O-REPORT        PIC X(6).
           05  FILLER          PIC X(66)       VALUE 'REPORT'.

       01  DETAIL-COLUMN-HEADING.
           05  FILLER          PIC X(3)        VALUE ' '.
           05  FILLER          PIC X(17)       VALUE 'LAST NAME'.
           05  FILLER          PIC X(17)       VALUE 'FIRST NAME'.
           05  FILLER          PIC X(12)       VALUE 'CITY'.
           05  FILLER          PIC X(6)        VALUE 'STATE'.
           05  FILLER          PIC X(12)       VALUE 'ZIP CODE'.
           05  FILLER          PIC X(21)       VALUE 'POP TYPE'.
           05  FILLER          PIC X(14)       VALUE 'QUANTITY'.
           05  FILLER          PIC X(17)       VALUE 'DEPOSIT AMT'.
           05  FILLER          PIC X(13)       VALUE 'TOTAL SALES'.

       01 DETAIL-LINE.
           05  FILLER          PIC XXX         VALUE ' '.
           05  O-LNAME         PIC X(15).
           05  FILLER          PIC XX          VALUE ' '.
           05  O-FNAME         PIC X(15).
           05  FILLER          PIC XX          VALUE ' '.
           05  O-CITY          PIC X(10).
           05  FILLER          PIC X(3)        VALUE ' '.
           05  O-STATE         PIC XX.
           05  FILLER          PIC XXX         VALUE ' '.
           05  O-ZIP5          PIC 9(5).
           05  FILLER          PIC X           VALUE '-'.
           05  O-ZIP4          PIC 9(4).
           05  FILLER          PIC XX          VALUE ' '.
           05  O-POP-TYPE      PIC X(16).
           05  FILLER          PIC X(8)        VALUE ' '.
           05  O-NUM-CASES     PIC Z9.
           05  FILLER          PIC X(11)       VALUE ' '.
           05  O-DEPOSIT-AMT   PIC $$$$.99.
           05  FILLER          PIC X(9)        VALUE ' '.
           05  O-TOTAL-SALES   PIC $$,$$$.99.
           05  FILLER          PIC XXX         VALUE ' '.

       01 TOTAL-DETAIL-LINE.
           05  O-TOTALS        PIC X(132).

       01 TOTAL-CASES-LINE.
           05  FILLER          PIC XXX         VALUE ' '.
           05  O-GT-POP1       PIC X(16).
           05  FILLER          PIC X           VALUE ' '.
           05  O-GT-POP1-CASES PIC ZZZ,ZZ9.
           05  FILLER          PIC X(6)         VALUE ' '.
           05  O-GT-POP2       PIC X(16).
           05  FILLER          PIC X           VALUE ' '.
           05  O-GT-POP2-CASES PIC ZZZ,ZZ9.
           05  FILLER          PIC X(6)         VALUE ' '.
           05  O-GT-POP3       PIC X(16).
           05  FILLER          PIC X           VALUE ' '.
           05  O-GT-POP3-CASES PIC ZZZ,ZZ9.
           05  FILLER          PIC X(45)       VALUE ' '.

       01 TOTAL-TEAMS-LINE.
           05  FILLER          PIC XXX         VALUE ' '.
           05  O-TEAM1         PIC XX.
           05  O-GT-TEAM-AMT  PIC $$$$,$$$,$$$.99.
           05  FILLER          PIC X(112)      VALUE ' '.


       01 ERROR-COLUMN-HEADING.
           05 FILLER           PIC X(72)       VALUE 'ERROR RECORD'.
           05 FILLER           PIC X(60)                               
                                           VALUE 'ERROR DESCRIPTION'.

       01 ERROR-DESC-LINE.
           05 O-POPSALES      PIC X(72).
           05 O-ERR-DESC       PIC X(60).
           
       01 TOTAL-ERROR-LINE.
           05  FILLER          PIC X(13)       VALUE 'TOTAL ERRORS'.
           05  O-ERR-CTR       PIC Z,ZZ9.
           05  FILLER          PIC X(114)      VALUE ' '.


       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL EOF = 'TRUE'.
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           OPEN INPUT POP-SALES-REPORT.
           OPEN OUTPUT PRINTOUT.
           OPEN OUTPUT ERROROUT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YY TO O-YY.
           MOVE I-DD TO O-DD.
           MOVE I-MM TO O-MM.
           PERFORM 9900-HEADINGS-DETAIL.
           PERFORM 9910-HEADINGS-ERROR.
           PERFORM 9000-READ.


       2000-MAINLINE.
           PERFORM 2100-VALIDATION THRU 2100-X.
           IF ERROR-SWITCH = 'PASS'
               PERFORM 2200-CALCS
               PERFORM 2300-OUTPUT
           ELSE
               PERFORM 2400-ERROR-ROUTINE.
           PERFORM 9000-READ.

       2100-VALIDATION.
           MOVE 'FAIL' TO ERROR-SWITCH.
           IF I-LNAME = " "
               MOVE "LAST NAME IS REQUIRED." TO O-ERR-DESC
               GO TO 2100-X.
           IF I-FNAME = " "
               MOVE "FIRST NAME IS REQUIRED." TO O-ERR-DESC
               GO TO 2100-X.
           IF I-ADDRESS = " "
               MOVE "ADDRESS IS REQUIRED." TO O-ERR-DESC
               GO TO 2100-X.
           IF I-CITY = " "
               MOVE "CITY IS REQUIRED." TO O-ERR-DESC
               GO TO 2100-X.
           IF NOT VAL-STATE
               MOVE 'VALID STATES ARE IA, IL, MI, MO, NE AND WI.'      
                   TO O-ERR-DESC
               GO TO 2100-X.
           IF I-ZIP5 NOT NUMERIC OR I-ZIP4 NOT NUMERIC
               MOVE "ZIP CODE MUST BE NUMERIC." TO O-ERR-DESC
               GO TO 2100-X.
           IF I-POP-TYPE NOT NUMERIC
               MOVE 'POP TYPE MUST BE NUMERIC.' TO O-ERR-DESC
               GO TO 2100-X.
           IF NOT VAL-POP-TYPE
               MOVE "POP TYPE MUST BE 1 THROUGH 6." TO O-ERR-DESC
               GO TO 2100-X.
           IF I-NUM-CASES NOT NUMERIC
               MOVE "NUMBER OF CASE'S MUST BE NUMERIC." TO O-ERR-DESC
               GO TO 2100-X.
           IF I-NUM-CASES = 0
               MOVE 'CASES ORDERED MUST BE A MINIMUM OF 1.'            
                   TO O-ERR-DESC
               GO TO 2100-X.
           IF NOT VAL-TEAM
               MOVE 'TEAM MUST BE A THROUGH E.' TO O-ERR-DESC
               GO TO 2100-X.
           MOVE 'PASS' TO ERROR-SWITCH.
       
       2100-X.
           EXIT.

       2200-CALCS.
	       MOVE 0 TO C-DEPOSIT-AMT.
           IF VAL-5CENTS
               COMPUTE C-DEPOSIT-AMT = .05 * 24 * I-NUM-CASES.
           IF I-STATE = 'MI'
               COMPUTE C-DEPOSIT-AMT = .1 * 24 * I-NUM-CASES.
           COMPUTE C-TOTAL-SALES = 18.71 * I-NUM-CASES + C-DEPOSIT-AMT.
           EVALUATE I-POP-TYPE
               WHEN 01
                   MOVE 'COKE' TO O-POP-TYPE
                   ADD I-NUM-CASES TO GT-POP1-CASES
               WHEN 02
                   MOVE 'DIET COKE' TO O-POP-TYPE
                   ADD I-NUM-CASES TO GT-POP2-CASES
               WHEN 03
                   MOVE 'MELLO YELLO' TO O-POP-TYPE
                   ADD I-NUM-CASES TO GT-POP3-CASES
               WHEN 04
                   MOVE 'CHERRY COKE' TO O-POP-TYPE
                   ADD I-NUM-CASES TO GT-POP4-CASES
               WHEN 05
                   MOVE 'DIET CHERRY COKE' TO O-POP-TYPE
                   ADD I-NUM-CASES TO GT-POP5-CASES
               WHEN 06
                   MOVE 'SPRITE' TO O-POP-TYPE
                   ADD I-NUM-CASES TO GT-POP6-CASES.

           EVALUATE I-TEAM
               WHEN 'A'
                   ADD C-TOTAL-SALES TO GT-TEAMA-AMT
               WHEN 'B'
                   ADD C-TOTAL-SALES TO GT-TEAMB-AMT
               WHEN 'C'
                   ADD C-TOTAL-SALES TO GT-TEAMC-AMT
               WHEN 'D'
                   ADD C-TOTAL-SALES TO GT-TEAMD-AMT
               WHEN 'E'
                   ADD C-TOTAL-SALES TO GT-TEAME-AMT.

       2300-OUTPUT.
           MOVE I-LNAME TO O-LNAME.
           MOVE I-FNAME TO O-FNAME.
           MOVE I-CITY TO O-CITY.
           MOVE I-STATE TO O-STATE.
           MOVE I-ZIP5 TO O-ZIP5.
           MOVE I-ZIP4 TO O-ZIP4.
           MOVE I-NUM-CASES TO O-NUM-CASES.
           MOVE C-DEPOSIT-AMT TO O-DEPOSIT-AMT.
           MOVE C-TOTAL-SALES TO O-TOTAL-SALES.
           WRITE PRINTLINE FROM DETAIL-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM 9900-HEADINGS-DETAIL.

       2400-ERROR-ROUTINE.
           ADD 1 TO ERR-CTR.
           MOVE I-POPSALES TO O-POPSALES.
           WRITE ERRORLINE FROM ERROR-DESC-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM 9910-HEADINGS-ERROR.

       3000-CLOSING.
           PERFORM 3100-GRAND-TOTALS.
           CLOSE POP-SALES-REPORT.
           CLOSE PRINTOUT.
           CLOSE ERROROUT.

       3100-GRAND-TOTALS.
      *ERROR GRAND TOTALS
           MOVE ERR-CTR TO O-ERR-CTR.
           WRITE ERRORLINE FROM TOTAL-ERROR-LINE
               AFTER ADVANCING 3 LINES.
      *POP CASES GRAND TOTALS
           PERFORM 9900-HEADINGS-DETAIL.
           MOVE "GRAND TOTALS:" TO O-TOTALS.
           WRITE PRINTLINE FROM TOTAL-DETAIL-LINE
               AFTER ADVANCING 3 LINES.
           MOVE "COKE" TO O-GT-POP1.
           MOVE "DIET COKE" TO O-GT-POP2.
           MOVE "MELLO YELLOW" TO O-GT-POP3.
           MOVE GT-POP1-CASES TO O-GT-POP1-CASES.
           MOVE GT-POP2-CASES TO O-GT-POP2-CASES.
           MOVE GT-POP3-CASES TO O-GT-POP3-CASES.
           WRITE PRINTLINE FROM TOTAL-CASES-LINE
               AFTER ADVANCING 2 LINES.
           MOVE "CHERRY COKE" TO O-GT-POP1.
           MOVE "DIET CHERRY COKE" TO O-GT-POP2.
           MOVE "SPRITE" TO O-GT-POP3.
           MOVE GT-POP4-CASES TO O-GT-POP1-CASES.
           MOVE GT-POP5-CASES TO O-GT-POP2-CASES.
           MOVE GT-POP6-CASES TO O-GT-POP3-CASES.
           WRITE PRINTLINE FROM TOTAL-CASES-LINE
               AFTER ADVANCING 2 LINES.
      *TEAM AMOUNT GRAND TOTALS
           MOVE 'TEAM TOTALS:' TO O-TOTALS.
           WRITE PRINTLINE FROM TOTAL-DETAIL-LINE
               AFTER ADVANCING 3 LINES.
           MOVE 'A' TO O-TEAM1.
           MOVE GT-TEAMA-AMT TO O-GT-TEAM-AMT.
           WRITE PRINTLINE FROM TOTAL-TEAMS-LINE
               AFTER ADVANCING 2 LINES.
           MOVE 'B' TO O-TEAM1.
           MOVE GT-TEAMB-AMT TO O-GT-TEAM-AMT.
           WRITE PRINTLINE FROM TOTAL-TEAMS-LINE
               AFTER ADVANCING 2 LINES.
           MOVE 'C' TO O-TEAM1.
           MOVE GT-TEAMC-AMT TO O-GT-TEAM-AMT.
           WRITE PRINTLINE FROM TOTAL-TEAMS-LINE
               AFTER ADVANCING 2 LINES.
           MOVE 'D' TO O-TEAM1.
           MOVE GT-TEAMD-AMT TO O-GT-TEAM-AMT.
           WRITE PRINTLINE FROM TOTAL-TEAMS-LINE
               AFTER ADVANCING 2 LINES.
           MOVE 'E' TO O-TEAM1.
           MOVE GT-TEAME-AMT TO O-GT-TEAM-AMT.
           WRITE PRINTLINE FROM TOTAL-TEAMS-LINE
               AFTER ADVANCING 2 LINES.

       9000-READ.
           READ POP-SALES-REPORT
               AT END
                   MOVE 'TRUE' TO EOF.

       9900-HEADINGS-DETAIL.
           ADD 1 TO C-PCTR-D.
           MOVE C-PCTR-D TO O-PCTR.
           MOVE "SALES" TO O-REPORT.
           WRITE PRINTLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE.
           WRITE PRINTLINE FROM REPORT-TITLE-LINE1
               AFTER ADVANCING 1 LINES.
           WRITE PRINTLINE FROM REPORT-TITLE-LINE2
               AFTER ADVANCING 1 LINES.
           WRITE PRINTLINE FROM DETAIL-COLUMN-HEADING
               AFTER ADVANCING 2 LINE.

       9910-HEADINGS-ERROR.
           ADD 1 TO C-PCTR-E.
           MOVE C-PCTR-E TO O-PCTR.
           MOVE "ERROR" TO O-REPORT.
           WRITE ERRORLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE.
           WRITE ERRORLINE FROM REPORT-TITLE-LINE1
               AFTER ADVANCING 1 LINES.
           WRITE ERRORLINE FROM REPORT-TITLE-LINE2
               AFTER ADVANCING 1 LINES.
           WRITE ERRORLINE FROM ERROR-COLUMN-HEADING
               AFTER ADVANCING 2 LINE.

