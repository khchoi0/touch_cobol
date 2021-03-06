       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MALE-SORT.
       AUTHOR. KA HOU, CHOI.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT 
              STUDENT-FILE ASSIGN TO "STUDENTS.DAT"
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT 
              MALE-STUDENT-FILE ASSIGN TO "MALESTUDS.DAT"
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT WORK-FILE ASSIGN TO "WORK.TMP".

       DATA DIVISION. 
       FILE SECTION. 
       FD  STUDENT-FILE.
       01  STUDENT-REC PIC X(30).
           88 END-OF-FILE VALUE HIGH-VALUES.

       FD MALE-STUDENT-FILE.
       01  MALE-STUDENT-REC PIC X(30).

       SD  WORK-FILE.
       01  WORK-REC.
           02 FILLER PIC 9(7).
           02 W-STUDENT-NAME PIC X(10).
           02 FILLER PIC X(12).
           02 W-GENDER PIC X.
              88 MALE-STUDENT VALUE "M".

       PROCEDURE DIVISION.
       MAIN.
           SORT 
              WORK-FILE ON ASCENDING KEY W-STUDENT-NAME 
              INPUT PROCEDURE IS GET-MALE-STUDENTS
              GIVING MALE-STUDENT-FILE.
           STOP RUN.

       GET-MALE-STUDENTS.
           OPEN  INPUT STUDENT-FILE 
           READ STUDENT-FILE 
              AT END SET END-OF-FILE TO TRUE
           END-READ 
           PERFORM UNTIL END-OF-FILE 
              MOVE STUDENT-REC TO WORK-REC
              IF MALE-STUDENT
                 RELEASE WORK-REC 
              END-IF 
              READ STUDENT-FILE 
                 AT END SET END-OF-FILE TO TRUE
              END-READ 
           END-PERFORM
           CLOSE STUDENT-FILE.
