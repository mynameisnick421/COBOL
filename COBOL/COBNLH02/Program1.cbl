       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBNLH02.
       DATE-WRITTEN.   12/10/19.
       AUTHOR.         NICK HOUSER.
       DATE-COMPILED.
      *******************************************
      *  THIS PROGRAM READS A FILE AND CREATES  *
      *         A REPORT OF BOATS SOLD          *
      *  WITH MAJOR CONTROL BREAK ON BOAT TYPE  *
      *******************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BOAT-REPORT
               ASSIGN TO 'C:\SCHOOL\COBOL\CBLBOAT1.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRINTOUT
               ASSIGN TO 'C:\SCHOOL\COBOL\BOATRPT1.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.
      
       DATA DIVISION.
       FILE SECTION.

       FD  BOAT-REPORT
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-BOATS
           RECORD CONTAINS 42 CHARACTERS.
      *DECLARING INPUT VARIABLES
       01 I-BOATS.
           05 I-LAST-NAME              PIC X(15).
           05 I-STATE                  PIC XX.
           05 I-BOAT-COST              PIC 9(6)V99.
           05 I-PURCHASE-DATE.
               10 I-PURCHASE-YYYY      PIC 9(4).
               10 I-PURCHASE-MM        PIC 99.
               10 I-PURCHASE-DD        PIC 99.
           05 I-BOAT-TYPE              PIC X.
           05 I-ACCESSORY-PACKAGE      PIC 9.
           05 I-PREP-DELIVER-COST      PIC 9(5)V99.

       FD PRINTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRINTLINE
           LINAGE IS 60 WITH FOOTING AT 53.

       01 PRINTLINE                PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  EOF                 PIC X(5)        VALUE 'FALSE'.
           05  H-BOAT-TYPE-CHAR    PIC X.
      *CALCULATED VARIABLES
           05  C-TOTAL-COST        PIC 9(7)V99.
           05  C-PCTR              PIC 9(3)        VALUE 0.
           05  E-BOAT-TYPE         PIC X(13).
      *MAJOR ACCUMULATORS
           05  M-NUM-SOLD          PIC 9(4)        VALUE 0.
           05  M-TOTAL-COST        PIC 9(9)V99     VALUE 0.  
      *GRAND-TOTAL-ACCUMULATORS
           05  GT-NUM-SOLD         PIC 9(5)        VALUE 0.
           05  GT-TOTAL-COST       PIC 9(11)V99    VALUE 0.

       01  CURRENT-DATE-AND-TIME.
           05  I-DATE.
               10  I-YY        PIC 9(4).
               10  I-MM        PIC 99.
               10  I-DD        PIC 99.
           05  I-TIME          PIC X(11).

       01 BLANK-LINE.
           05 FILLER           PIC X(132).

       01  COMPANY-TITLE.
           05  FILLER          PIC X(6)        VALUE 'DATE:'.
           05  O-MM            PIC 99.
           05  FILLER          PIC X           VALUE '/'.
           05  O-DD            PIC 99.
           05  FILLER          PIC X           VALUE '/'.
           05  O-YY            PIC 9(4).
           05  FILLER          PIC X(40)       VALUE ' '.
           05  FILLER          PIC X(19)                         
                                   VALUE "HOUSER'S BOATS INC.".
           05  FILLER          PIC X(49)       VALUE ' '.
           05  FILLER          PIC X(6)        VALUE 'PAGE:'.
           05  O-PCTR          PIC Z9.

       01  COLUMN-HEADING1.
           05  FILLER          PIC X(44)       VALUE 'CUSTOMER'.
           05  FILLER          PIC X(13)       VALUE 'BOAT'.
           05  FILLER          PIC X(19)       VALUE 'PURCHASE'.
           05  FILLER          PIC X(30)       VALUE 'ACCESSORY'.
           05  FILLER          PIC X(21)       VALUE 'PREP'.
           05  FILLER          PIC X(5)        VALUE 'TOTAL'.

       01  COLUMN-HEADING2.
           05  FILLER          PIC X(23)       VALUE 'LAST NAME'.
           05  FILLER          PIC X(21)       VALUE 'STATE'.
           05  FILLER          PIC X(13)       VALUE 'COST'.
           05  FILLER          PIC X(19)       VALUE 'DATE'.
           05  FILLER          PIC X(30)       VALUE 'PACKAGE'.
           05  FILLER          PIC X(22)       VALUE 'COST'.
           05  FILLER          PIC X(4)        VALUE 'COST'.

       01 MAJOR-HEADING.
           05  FILLER          PIC X(11)       VALUE 'BOAT TYPE:'.
           05  O-MT-BOAT-TYPE  PIC X(13).
           05  FILLER          PIC X(108)      VALUE ' '.

       01  DETAIL-LINE.
           05  O-LAST-NAME     PIC X(16).
           05  FILLER          PIC X(8)        VALUE ' '.
           05  O-STATE         PIC XX.
           05  FILLER          PIC X(12)       VALUE ' '.
           05  O-BOAT-COST     PIC ZZZ,ZZZ.99.
           05  FILLER          PIC X(9)        VALUE ' '.
           05  O-PURCHASE-MM   PIC 99.
           05  FILLER          PIC X           VALUE '/'.
           05  O-PURCHASE-DD   PIC 99.
           05  FILLER          PIC X           VALUE '/'.
           05  O-PURCHASE-YY   PIC 99.
           05  FILLER          PIC X(11)       VALUE ' '.
           05  O-ACCES-PACKAGE PIC X(15).
           05  FILLER          PIC X(9)        VALUE ' '.
           05  O-PREP-COST     PIC ZZZ,ZZZ.99.
           05  FILLER          PIC X(10)       VALUE ' '.
           05  O-TOTAL-COST    PIC Z,ZZZ,ZZZ.99.

       01 MAJOR-BREAK.
           05  FILLER          PIC X(23)       VALUE ' '.
           05  FILLER          PIC X(14)       VALUE 'SUBTOTALS FOR'.
           05  O-MB-BOAT-TYPE  PIC X(13).
           05  FILLER          PIC X(10)       VALUE ' '.
           05  FILLER          PIC X(14)       VALUE 'NUMBER SOLD:'.
           05  O-M-NUM-SOLD    PIC Z,ZZ9.
           05  FILLER          PIC X(38)       VALUE ' '.
           05  O-M-TOTAL-COST  PIC $$$$,$$$,$$$.99.

       01  TOTAL-LINE.
           05  FILLER          PIC X(23)       VALUE ' '.
           05  FILLER          PIC X(37)       VALUE 'GRAND TOTALS'.
           05  FILLER          PIC X(13)       VALUE 'NUMBER SOLD:'.
           05  O-GT-NUM-SOLD   PIC ZZ,ZZ9.
           05  FILLER          PIC X(35)       VALUE ' '.
           05  O-GT-TOTAL-COST PIC $$$,$$$,$$$,$$$.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL EOF = 'TRUE'.
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           OPEN INPUT BOAT-REPORT.
           OPEN OUTPUT PRINTOUT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YY TO O-YY.
           MOVE I-DD TO O-DD.
           MOVE I-MM TO O-MM.
           PERFORM 9000-READ.
           MOVE I-BOAT-TYPE TO H-BOAT-TYPE-CHAR.
           PERFORM 9100-HEADINGS.

       2000-MAINLINE.
           IF H-BOAT-TYPE-CHAR NOT = I-BOAT-TYPE
               PERFORM 9300-MAJOR-BREAK
               PERFORM 9200-MAJOR-TITLE-LINE.
           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.

       2100-CALCS.
           IF I-ACCESSORY-PACKAGE = 1
               MOVE "ELECTRONICS" TO O-ACCES-PACKAGE
           ELSE
               IF I-ACCESSORY-PACKAGE = 2
                   MOVE "SKI PACKAGE" TO O-ACCES-PACKAGE
               ELSE
                   MOVE "FISHING PACKAGE" TO O-ACCES-PACKAGE.

           ADD I-BOAT-COST TO I-PREP-DELIVER-COST GIVING C-TOTAL-COST.
      *ADDING TO MAJOR VARIABLES
           ADD 1 TO M-NUM-SOLD.
           ADD C-TOTAL-COST TO M-TOTAL-COST.
              
       2200-OUTPUT.
           MOVE I-LAST-NAME TO O-LAST-NAME.
           MOVE I-STATE TO O-STATE.
           MOVE I-BOAT-COST TO O-BOAT-COST.
           MOVE I-PURCHASE-YYYY TO O-PURCHASE-YY.
           MOVE I-PURCHASE-MM TO O-PURCHASE-MM.
           MOVE I-PURCHASE-DD TO O-PURCHASE-DD.
           MOVE I-PREP-DELIVER-COST TO O-PREP-COST.
           MOVE C-TOTAL-COST TO O-TOTAL-COST.
           WRITE PRINTLINE FROM DETAIL-LINE
               AFTER ADVANCING 1 LINES
                   AT EOP
                       PERFORM 9100-HEADINGS.

       3000-CLOSING.
           PERFORM 3100-GRAND-TOTALS.
           CLOSE BOAT-REPORT.
           CLOSE PRINTOUT.

       3100-GRAND-TOTALS.
           PERFORM 9300-MAJOR-BREAK
           MOVE GT-NUM-SOLD TO O-GT-NUM-SOLD.
           MOVE GT-TOTAL-COST TO O-GT-TOTAL-COST.
           WRITE PRINTLINE FROM TOTAL-LINE
               AFTER ADVANCING 3 LINES.

       9000-READ.
           READ BOAT-REPORT
               AT END
                   MOVE 'TRUE' TO EOF.

       9100-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           WRITE PRINTLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE.
           WRITE PRINTLINE FROM COLUMN-HEADING1
               AFTER ADVANCING 2 LINES.
           WRITE PRINTLINE FROM COLUMN-HEADING2
               AFTER ADVANCING 1 LINE.
           PERFORM 9200-MAJOR-TITLE-LINE.

       9200-MAJOR-TITLE-LINE.
           EVALUATE I-BOAT-TYPE
               WHEN "B"
                   MOVE "BASS BOAT" TO E-BOAT-TYPE
               WHEN "P"
                   MOVE "PONTOON" TO E-BOAT-TYPE
               WHEN "S"
                   MOVE "SKI BOAT" TO E-BOAT-TYPE
               WHEN "J"
                   MOVE "JOHN BOAT" TO E-BOAT-TYPE
               WHEN "C"
                   MOVE "CANOE" TO E-BOAT-TYPE
               WHEN "R"
                   MOVE "CABIN CRUISER" TO E-BOAT-TYPE.
           MOVE E-BOAT-TYPE TO O-MT-BOAT-TYPE.
           WRITE PRINTLINE FROM MAJOR-HEADING
               AFTER ADVANCING 2 LINES.
           WRITE PRINTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE
                   AT EOP
                       PERFORM 9100-HEADINGS.

       9300-MAJOR-BREAK.
           MOVE M-NUM-SOLD TO O-M-NUM-SOLD.
           MOVE M-TOTAL-COST TO O-M-TOTAL-COST.
           MOVE E-BOAT-TYPE TO O-MB-BOAT-TYPE.
           WRITE PRINTLINE FROM MAJOR-BREAK
               AFTER ADVANCING 2 LINES.
      *ADDING TO GT VARIABLES
           ADD M-NUM-SOLD TO GT-NUM-SOLD.
           ADD M-TOTAL-COST TO GT-TOTAL-COST.
      *RESET MAJOR VARIABLES
           MOVE 0 TO M-NUM-SOLD.
           MOVE 0 TO M-TOTAL-COST.
           MOVE I-BOAT-TYPE TO H-BOAT-TYPE-CHAR.