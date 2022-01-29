       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQ-READ.
       AUTHOR. KA HOU, CHOI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "STUDENTS.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS STUDENT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01  STUDENT-DETAILS.
           88 END-OF-STUDENT-FILE VALUE 2.
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

       PROCEDURE DIVISION.
       MAIN.
           IF NOT STUDENT-FILE-ALREADY-OPEN THEN
              OPEN INPUT STUDENT-FILE
           END-IF.

           READ STUDENT-FILE
              AT END SET END-OF-STUDENT-FILE TO TRUE
           END-READ

           PERFORM UNTIL END-OF-STUDENT-FILE
              DISPLAY STUDENT-ID SPACE STUDENT-NAME SPACE COURSE-CODE 
              SPACE Y-O-BIRTH
              READ STUDENT-FILE
                 AT END SET END-OF-STUDENT-FILE TO TRUE
              END-READ
           END-PERFORM
           CLOSE STUDENT-FILE
           STOP RUN.

       END PROGRAM SEQ-READ.
