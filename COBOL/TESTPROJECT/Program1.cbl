       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBNLH03.
       DATE-WRITTEN.   1/2/2020.
       AUTHOR.         NICK HOUSER.
       DATE-COMPILED.
      *******************************************
      *  THIS PROGRAM READS A FILE AND CREATES  *
      *         A REPORT OF BOATS SOLD          *
      *  WITH MAJOR CONTROL BREAK ON BOAT TYPE  *
      *     AND MINOR CONTROL BREAK ON STATE    *
      *******************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       01 WORK-AREA.
           05  FIRST-NAME      PIC X(15)   VALUE 'BEATRICE'.
           05  NICKNAME2        PIC X(10)   VALUE 'ETHEL' . 
           05  X               PIC X.
      
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "WILMA" TO NICKNAME2.
           IF FIRST-NAME = 'PATRICE'
               DISPLAY "YOU SHOULD HAVE BEEN NAMED PATRICK"
               ACCEPT X
           ELSE 
               IF NICKNAME2 = 'ETHEL'
                   DISPLAY "DON'T LOOK!!!!"
                   DISPLAY "IT'S THE STREAK."
                   ACCEPT X
               ELSE
                   DISPLAY "JUST WHO ARE YOU???"
                   ACCEPT X.
       