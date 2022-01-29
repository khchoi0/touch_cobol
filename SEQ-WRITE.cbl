       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQ-WRITE.
       AUTHOR. KA HOU, CHOI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT 
              STUDENT-FILE ASSIGN TO "STUDENTS-INPUT.DAT"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS STUDENT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01  STUDENT-DETAILS.
           02 STUDENT-ID PIC 9(7).
           02 STUDENT-NAME.
              03 SURNAME PIC X(8).
              03 INITIALS PIC XX.
           02 DATE-OF-BIRTH.
              03 Y-O-BIRTH PIC 9(4).
              03 M-O-BIRTH PIC 9(2).
              03 D-O-BIRTH PIC 9(2).
           02 COURSE-CODE PIC X(4).
           02 GENDER PIC X.
       WORKING-STORAGE SECTION. 
       01  STUDENT-FILE-STATUS PIC 9(2).
           88 STUDENT-FILE-ALREADY-OPEN VALUE 41.
       01  END-INPUT PIC 9 VALUE 1.

       PROCEDURE DIVISION.
       MAIN.
           IF NOT STUDENT-FILE-ALREADY-OPEN THEN
              OPEN OUTPUT STUDENT-FILE
           END-IF.
           
           IF END-INPUT = 1 THEN
              DISPLAY "ENTER STUDENT DETAILS USING TEMPLATE BELOW. "
              "ENTER NO DATA TO END."
           END-IF.
           
           GO TO GET-STUDENT-DETAILS DEPENDING ON END-INPUT.

           CLOSE STUDENT-FILE
           STOP RUN.

       GET-STUDENT-DETAILS.
           DISPLAY "ENTER - STUDID, SURNAME, INITIALS, YOB, MOB, DOB, "
           "COURSE, GENDER"
           DISPLAY "NNNNNNNSSSSSSSSIIYYYYMMDDCCCCG"
           ACCEPT STUDENT-DETAILS.
           WRITE STUDENT-DETAILS
           IF STUDENT-DETAILS = SPACES THEN
              MOVE 2 TO END-INPUT
           END-IF.
           GO TO GET-STUDENT-DETAILS DEPENDING ON END-INPUT.
           GO TO MAIN.

       END PROGRAM SEQ-WRITE.
