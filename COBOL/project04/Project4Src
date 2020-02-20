             IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBNLH04.
       DATE-WRITTEN.   1/9/2020.
       AUTHOR.         NICK HOUSER.
       DATE-COMPILED.
      *******************************************
      *  THIS PROGRAM READS A FILE AND CREATES  *
      *      AN OZARK CONDO BILLING REPORT      *
      *******************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OZARK-REPORT
               ASSIGN TO 'C:\SCHOOL\COBOL\OZARK.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRINTOUT
               ASSIGN TO 'C:\SCHOOL\COBOL\VACATION.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.
      
       DATA DIVISION.
       FILE SECTION.

       FD OZARK-REPORT
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-OZARK
           RECORD CONTAINS 31 CHARACTERS.
      *DECLARING INPUT VARIABLES
       01 I-OZARK.
           05 I-GUEST                  PIC X(20).
           05 I-CONDO                  PIC XX.
           05 I-BEDROOMS               PIC 9.
           05 I-NIGHTS                 PIC 99.
           05 I-PETS                   PIC X.
           05 I-HOTTUB                 PIC X.
           05 I-DOCKSLIP               PIC 99V99.

       FD PRINTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRINTLINE
           LINAGE IS 60 WITH FOOTING AT 54.

       01 PRINTLINE                PIC X(132).

       WORKING-STORAGE SECTION.
       01 WORK-AREA.
           05  EOF             PIC X(5)        VALUE 'FALSE'.
      *EVALUATED VARIABLES
           05  C-CLEANFEE      PIC 999.
           05  C-PETDEAL       PIC 9(5)V99.
           05  C-HTUBDEAL      PIC 9(5)V99.
           05  C-NIGHTFEE      PIC 9(5)V99.
           05  C-HALFNIGHT     PIC 9(3).
      *CALCULATED VARIABLES
           05  C-CONDOFEE      PIC 9(6)V99.
           05  C-DOCKFEE       PIC 9(4)V99.
           05  C-SUBTOTAL      PIC 9(6)V99.
           05  C-TOTALDEAL     PIC S9(6)V99.
           05  C-TOTALDUE      PIC 9(6)V99.
           05  C-PCTR          PIC 99          VALUE 0.
      *GRAND-TOTAL-ACCUMULATORS
           05  GT-SUBTOTAL     PIC 9(8)V99     VALUE 0.
           05  GT-TOTALDEAL    PIC S9(7)V99    VALUE 0.
           05  GT-TOTALDUE     PIC 9(8)V99     VALUE 0.
           05  GT-RENTALCTR    PIC 999         VALUE 0.
           05  GT-FREENIGHT    PIC 99V9        VALUE 0.
           05  GT-FREECLEAN    PIC 99          VALUE 0.
           05  GT-PETFEE       PIC 9(5)V99     VALUE 0.
           05  GT-HTUBFEE      PIC 9(5)V99     VALUE 0.

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
           05  FILLER          PIC X(42)       VALUE ' '.
           05  FILLER          PIC X(66)                         
                                       VALUE "PMG MANAGEMENT".
           05  FILLER          PIC X(6)        VALUE 'PAGE:'.
           05  O-PCTR          PIC Z9.

       01 REPORT-TITLE-LINE.
           05  FILLER          PIC X(50)       VALUE 'COBNLH04'.
           05  FILLER          PIC X(21)                                
                                       VALUE 'LAKE OF THE OZARKS -'.
           05  O-MONTH         PIC X(61).

       01 COLUMN-HEADING1.
           05  FILLER          PIC X(38)       VALUE ' '.
           05  FILLER          PIC X(10)       VALUE 'STAY'.
           05  FILLER          PIC X(12)       VALUE 'NIGHT'.
           05  FILLER          PIC X(6)        VALUE 'CONDO'.
           05  FILLER          PIC X(10)       VALUE 'CLEANING'.
           05  FILLER          PIC X(32)       VALUE 'DOCK SLIP'.
           05  FILLER          PIC X(14)       VALUE 'DEAL'.
           05  FILLER          PIC X(10)       VALUE 'AMOUNT'.

       01 COLUMN-HEADING2.
           05  FILLER          PIC X(17)       VALUE 'CONDOMINIUM'.
           05  FILLER          PIC X(20)       VALUE 'GUEST NAME'.
           05  FILLER          PIC X(13)       VALUE 'NIGHTS'.
           05  FILLER          PIC X(12)       VALUE 'FEE'.
           05  FILLER          PIC X(9)        VALUE 'FEE'.
           05  FILLER          PIC X(11)       VALUE 'FEE'.
           05  FILLER          PIC X(8)        VALUE 'FEE'.
           05  FILLER          PIC X(16)       VALUE 'SUBTOTAL'.
           05  FILLER          PIC X(19)       VALUE 'AMOUNT'.
           05  FILLER          PIC X(7)        VALUE 'DUE'.

       01 DETAIL-LINE.
           05  O-CONDO         PIC X(15).
           05  FILLER          PIC XX          VALUE ' '.
           05  O-GUEST         PIC X(20).
           05  FILLER          PIC X(2)        VALUE ' '.
           05  O-NIGHTS        PIC Z9.
           05  FILLER          PIC X(3)        VALUE ' '.
           05  O-NIGHTFEE      PIC $$,$$$.99.
           05  FILLER          PIC X           VALUE ' '.
           05  O-CONDOFEE      PIC $$$$,$$$.99.
           05  FILLER          PIC XX          VALUE ' '.
           05  O-CLEANFEE      PIC $$$$.99.
           05  FILLER          PIC XX          VALUE ' '.
           05  O-DOCKFEE       PIC $$,$$$.99.
           05  FILLER          PIC XX          VALUE ' '.
           05  O-SUBTOTAL      PIC $$$$,$$$.99.
           05  FILLER          PIC X(4)        VALUE ' '.
           05  O-TOTALDEAL     PIC $$$,$$$.99+.
           05  FILLER          PIC X(4)        VALUE ' '.
           05  O-TOTALDUE      PIC $$$$,$$$.99.
           05  PRICEFLAG       PIC X(4).

       01 TOTAL-LINE1.
           05  FILLER          PIC X(84)       VALUE 'GRAND TOTALS:'.
           05  O-GT-SUBTOTAL   PIC $$$,$$$,$$$.99.
           05  FILLER          PIC X           VALUE ' '.
           05  O-GT-TOTALDEAL  PIC $$,$$$,$$$.99+.
           05  FILLER          PIC X           VALUE ' '.
           05  O-GT-TOTALDUE   PIC $$$,$$$,$$$.99.
           05  FILLER          PIC X(4)        VALUE ' '.

        01 TOTAL-LINE2.
           05  FILLER          PIC X(12)       VALUE ' '.
           05  FILLER          PIC X(19)
                                       VALUE 'NUMBER OF RENTALS:'.
           05  O-GT-RENTALCTR  PIC ZZ9.
           05  FILLER          PIC X(6)        VALUE ' '.
           05  FILLER          PIC X(13)       VALUE 'FREE NIGHTS:'.
           05  O-GT-FREENIGHT  PIC ZZ.9.
           05  FILLER          PIC XXX         VALUE ' '.
           05  FILLER          PIC X(15)       VALUE 'FREE CLEANING:'.
           05  O-GT-FREECLEAN  PIC Z9.
           05  FILLER          PIC X(55)       VALUE ' '.

       01 TOTAL-LINE3.
           05  FILLER          PIC X(14)       VALUE ' '.
           05  FILLER          PIC X(10)       VALUE 'PET FEES:'.
           05  O-GT-PETFEE     PIC $$$,$$$.99.
           05  FILLER          PIC X(19)       VALUE ' '.
           05  FILLER          PIC X(14)       VALUE 'HOT TUB FEES:'.
           05  O-GT-HTUBFEE    PIC $$$,$$$.99.
           05  FILLER          PIC X(55)       VALUE ' '.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL EOF = 'TRUE'.
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           OPEN INPUT OZARK-REPORT.
           OPEN OUTPUT PRINTOUT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YY TO O-YY.
           MOVE I-DD TO O-DD.
           MOVE I-MM TO O-MM.
           PERFORM 1100-GET-MONTH
           PERFORM 9900-HEADINGS.
           PERFORM 9000-READ.

       1100-GET-MONTH.
           EVALUATE I-MM
               WHEN 01
                   MOVE 'JANUARY' TO O-MONTH
               WHEN 02
                   MOVE 'FEBUARY' TO O-MONTH
               WHEN 03
                   MOVE 'MARCH' TO O-MONTH
               WHEN 04
                   MOVE 'APRIL' TO O-MONTH
               WHEN 05
                   MOVE 'MAY' TO O-MONTH
               WHEN 06
                   MOVE 'JUNE' TO O-MONTH
               WHEN 07
                   MOVE 'JULY' TO O-MONTH
               WHEN 08
                   MOVE 'AUGUST' TO O-MONTH
               WHEN 09
                   MOVE 'SEPTEMBER' TO O-MONTH
               WHEN 10
                   MOVE 'OCTOBER' TO O-MONTH
               WHEN 11
                   MOVE 'NOVEMBER' TO O-MONTH
               WHEN 12
                   MOVE 'DECEMBER' TO O-MONTH.

       2000-MAINLINE.
           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.

       2100-CALCS.
       MOVE '    ' TO PRICEFLAG.  
	   MOVE 0 TO C-TOTALDEAL.
	   MOVE 0 TO C-PETDEAL.
	   MOVE 0 TO C-HTUBDEAL.
	   EVALUATE I-CONDO
           WHEN 'HB'
               MOVE 'HORSESHOE BEND' TO O-CONDO
               MULTIPLY I-BEDROOMS BY 99.5 GIVING C-NIGHTFEE ROUNDED
               MULTIPLY C-NIGHTFEE BY I-NIGHTS GIVING C-CONDOFEE ROUNDED
               MOVE 100 TO C-CLEANFEE
               IF I-PETS = 'Y'
                       MULTIPLY C-CONDOFEE BY .1 GIVING C-PETDEAL 
                       ROUNDED
                       ADD C-PETDEAL TO C-TOTALDEAL
               END-IF 
               IF I-HOTTUB = 'Y'
                       MULTIPLY C-CONDOFEE BY .05 GIVING C-HTUBDEAL 
                       ROUNDED
                       ADD C-HTUBDEAL TO C-TOTALDEAL
               END-IF
           WHEN 'OB'
               MOVE 'OSAGE BEACH' TO O-CONDO
               MULTIPLY I-BEDROOMS BY 188 GIVING C-NIGHTFEE ROUNDED
               MULTIPLY C-NIGHTFEE BY I-NIGHTS GIVING C-CONDOFEE ROUNDED
               MOVE 150 TO C-CLEANFEE
               IF I-PETS = 'Y'
                       MULTIPLY C-CONDOFEE BY .1 GIVING C-PETDEAL 
                       ROUNDED
                       ADD C-PETDEAL TO C-TOTALDEAL
               END-IF
               IF I-NIGHTS > 6 
                       SUBTRACT C-NIGHTFEE FROM C-TOTALDEAL 
                       ADD 1 TO GT-FREENIGHT
               END-IF
           WHEN 'PP'
               MOVE 'PISTOL POINT' TO O-CONDO
               MULTIPLY I-BEDROOMS BY 50 GIVING C-NIGHTFEE ROUNDED
               MULTIPLY C-NIGHTFEE BY I-NIGHTS GIVING C-CONDOFEE ROUNDED
               MOVE 75 TO C-CLEANFEE
           WHEN 'RB'
               MOVE 'REGATTA BAY' TO O-CONDO
               MULTIPLY I-BEDROOMS BY 62.10 GIVING C-NIGHTFEE ROUNDED
               MULTIPLY C-NIGHTFEE BY I-NIGHTS GIVING C-CONDOFEE ROUNDED
               MOVE 75 TO C-CLEANFEE
               IF I-NIGHTS > 5
                   SUBTRACT 75 FROM C-TOTALDEAL 
                   ADD 1 TO GT-FREECLEAN
               END-IF
           WHEN 'SB'
               MOVE 'SHAWNEE BEND' TO O-CONDO
               MULTIPLY I-BEDROOMS BY 100 GIVING C-NIGHTFEE ROUNDED
               MULTIPLY C-NIGHTFEE BY I-NIGHTS GIVING C-CONDOFEE ROUNDED
               MOVE 150 TO C-CLEANFEE
               IF I-PETS = 'Y'
                       MULTIPLY C-CONDOFEE BY .1 GIVING C-PETDEAL 
                       ROUNDED
                       ADD C-PETDEAL TO C-TOTALDEAL
               END-IF
           WHEN 'L '
               MOVE 'LEDGES' TO O-CONDO
               MULTIPLY I-BEDROOMS BY 76.35 GIVING C-NIGHTFEE ROUNDED
               MULTIPLY C-NIGHTFEE BY I-NIGHTS GIVING C-CONDOFEE ROUNDED
               MOVE 0 TO C-CLEANFEE
               IF I-HOTTUB = 'Y'
                       MULTIPLY C-CONDOFEE BY .075 GIVING C-HTUBDEAL 
                       ROUNDED
                       ADD C-HTUBDEAL TO C-TOTALDEAL
               END-IF
           WHEN 'HT'
               MOVE 'HARBOR TOWN' TO O-CONDO
               MULTIPLY I-BEDROOMS BY 50 GIVING C-NIGHTFEE ROUNDED
               MULTIPLY C-NIGHTFEE BY I-NIGHTS GIVING C-CONDOFEE ROUNDED
               MOVE 100 TO C-CLEANFEE
               IF I-NIGHTS > 2
                       DIVIDE C-NIGHTFEE BY 2 GIVING C-HALFNIGHT
                       SUBTRACT C-HALFNIGHT FROM C-TOTALDEAL
                       ADD .5 TO GT-FREENIGHT
               END-IF
           WHEN 'CP'
               MOVE 'COMPASSE POINTE' TO O-CONDO
               MULTIPLY I-BEDROOMS BY 125 GIVING C-NIGHTFEE ROUNDED
               MULTIPLY C-NIGHTFEE BY I-NIGHTS GIVING C-CONDOFEE ROUNDED
               MOVE 0 TO C-CLEANFEE
               IF I-NIGHTS > 4
                       SUBTRACT C-NIGHTFEE FROM C-TOTALDEAL
                       ADD 1 TO GT-FREENIGHT
               END-IF
           END-EVALUATE
      *CALCULATIONS USING EVALUATED VARIABLES
           MULTIPLY I-DOCKSLIP BY I-NIGHTS GIVING C-DOCKFEE ROUNDED.
           ADD C-CONDOFEE C-CLEANFEE C-DOCKFEE GIVING C-SUBTOTAL.
           ADD C-SUBTOTAL C-TOTALDEAL GIVING C-TOTALDUE.
      *ADDING TO GT ACCUMULATORS
           ADD C-SUBTOTAL TO GT-SUBTOTAL.
           ADD C-TOTALDEAL TO GT-TOTALDEAL.
           ADD C-TOTALDUE TO GT-TOTALDUE.
           ADD C-PETDEAL TO GT-PETFEE.
           ADD C-HTUBDEAL TO GT-HTUBFEE.
           ADD 1 TO GT-RENTALCTR.
           
       2200-OUTPUT.
           MOVE I-GUEST TO O-GUEST.
           MOVE I-NIGHTS TO O-NIGHTS.
           MOVE C-NIGHTFEE TO O-NIGHTFEE.
           MOVE C-CONDOFEE TO O-CONDOFEE.
           MOVE C-CLEANFEE TO O-CLEANFEE.
           MOVE C-DOCKFEE TO O-DOCKFEE.
           MOVE C-SUBTOTAL TO O-SUBTOTAL.
           MOVE C-TOTALDEAL TO O-TOTALDEAL.
           MOVE C-TOTALDUE TO O-TOTALDUE.
           IF C-TOTALDUE > 750
               MOVE '****' TO PRICEFLAG.
           WRITE PRINTLINE FROM DETAIL-LINE
               AFTER ADVANCING 1 LINES
                   AT EOP
                       PERFORM 9900-HEADINGS.

       3000-CLOSING.
           PERFORM 3100-GRAND-TOTALS.
           CLOSE OZARK-REPORT.
           CLOSE PRINTOUT.

       3100-GRAND-TOTALS.
           MOVE GT-SUBTOTAL TO O-GT-SUBTOTAL.
           MOVE GT-TOTALDEAL TO O-GT-TOTALDEAL,
           MOVE GT-TOTALDUE TO O-GT-TOTALDUE.
           MOVE GT-RENTALCTR TO O-GT-RENTALCTR.
           MOVE GT-FREECLEAN TO O-GT-FREECLEAN.
           MOVE GT-FREENIGHT TO O-GT-FREENIGHT.
           MOVE GT-PETFEE TO O-GT-PETFEE.
           MOVE GT-HTUBFEE TO O-GT-HTUBFEE.
           WRITE PRINTLINE FROM TOTAL-LINE1
               AFTER ADVANCING 3 LINES.
           WRITE PRINTLINE FROM TOTAL-LINE2
               AFTER ADVANCING 2 LINES.
           WRITE PRINTLINE FROM TOTAL-LINE3
               AFTER ADVANCING 1 LINE.

       9000-READ.
           READ OZARK-REPORT
               AT END
                   MOVE 'TRUE' TO EOF.

       9900-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           WRITE PRINTLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE.
           WRITE PRINTLINE FROM REPORT-TITLE-LINE
               AFTER ADVANCING 1 LINES.
           WRITE PRINTLINE FROM COLUMN-HEADING1
               AFTER ADVANCING 2 LINES.
           WRITE PRINTLINE FROM COLUMN-HEADING2
               AFTER ADVANCING 1 LINE.
           WRITE PRINTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINES.
