       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MERGE-FILE.
       AUTHOR. KA HOU, CHOI.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT 
              STUDENT-FILE ASSIGN TO "STUDENTS.DAT"
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT 
              INSERTIONS-FILE ASSIGN TO "TRANSINS.DAT"
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT 
              NEW-STUDENT-FILE ASSIGN TO "STUDENT-MERGE-FILE.NEW"
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT 
              WORK-FILE ASSIGN TO "WORK.TMP".

       DATA DIVISION. 
       FILE SECTION. 
       FD  STUDENT-FILE.
       01  STUDENT-REC PIC X(30).

       FD  INSERTIONS-FILE.
       01  INSERTION-REC PIC X(30).

       FD NEW-STUDENT-FILE.
       01  NEW-STUDENT-REC PIC X(30).

       SD  WORK-FILE.
       01  WORK-REC.
           02 W-STUDENT-ID PIC X(7).
           02 FILLER PIC X(23).

       PROCEDURE DIVISION.
       MAIN.
           MERGE WORK-FILE 
              ON ASCENDING KEY W-STUDENT-ID 
              USING INSERTIONS-FILE, STUDENT-FILE 
              GIVING NEW-STUDENT-FILE.
           STOP RUN.

       END PROGRAM MERGE-FILE.
       