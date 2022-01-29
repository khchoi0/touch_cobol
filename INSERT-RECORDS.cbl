       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERT-RECORDS.
       AUTHOR. KA HOU, CHOI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT
              STUDENT-RECORDS ASSIGN "./datasets/STUDENTS.DAT"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS STUDENT-FILE-STATUS.

           SELECT
              TRANS-RECORDS ASSIGN "./datasets/TRANSINS.DAT"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS TRANS-FILE-STATUS.

           SELECT NEW-STUDENT-RECORDS ASSIGN "./datasets/STUDENTS.NEW"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS NEW-STUDENT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-RECORDS.
       01  STUDENT-RECORD.
           02 STUDENT-ID PIC X(7).
           02 FILLER PIC X(23).

       FD TRANS-RECORDS.
       01 TRANS-RECORD.
           02 TRANS-STUDENT-ID PIC X(7).
           02 FILLER PIC X(23).

       FD NEW-STUDENT-RECORDS.
       01  NEW-STUDENT-RECORD PIC X(30).
       
       WORKING-STORAGE SECTION.
       01  STUDENT-FILE-STATUS PIC 9(2).
           88 STUDENT-FILE-ALREADY-OPEN VALUE 41.
       01  TRANS-FILE-STATUS PIC 9(2).
           88 TRANS-FILE-ALREADY-OPEN VALUE 41.
       01  NEW-STUDENT-FILE-STATUS PIC 9(2).
           88 NEW-STUDENT-FILE-ALREADY-OPEN VALUE 41.
       01  END-READING-STUDENT-FILE PIC 9 VALUE 1.
       01  END-READING-TRANS-FILE PIC 9 VALUE 1.

       PROCEDURE DIVISION.
       MAIN.
           IF NOT STUDENT-FILE-ALREADY-OPEN THEN
              OPEN INPUT STUDENT-RECORDS
           END-IF
           IF NOT TRANS-FILE-ALREADY-OPEN THEN
              OPEN INPUT TRANS-RECORDS 
           END-IF
           IF NOT NEW-STUDENT-FILE-ALREADY-OPEN THEN 
              OPEN OUTPUT NEW-STUDENT-RECORDS 
           END-IF
           
           IF END-READING-STUDENT-FILE = 1 THEN 
              READ STUDENT-RECORDS
                 AT END
                    MOVE HIGH-VALUES TO STUDENT-RECORD
                    MOVE 9999999 TO STUDENT-ID
                    MOVE 2 TO END-READING-STUDENT-FILE 
              END-READ
           END-IF
           
           IF END-READING-TRANS-FILE = 1 THEN 
              READ TRANS-RECORDS 
                 AT END
                    MOVE HIGH-VALUES TO TRANS-RECORD
                    MOVE 9999999 TO TRANS-STUDENT-ID
                    MOVE 2 TO END-READING-TRANS-FILE
              END-READ
           END-IF

           IF 
              END-READING-STUDENT-FILE = 1 OR 
              END-READING-TRANS-FILE = 1 
           THEN 
             GO TO READ-FILE 
           END-IF

           CLOSE STUDENT-RECORDS 
           CLOSE TRANS-RECORDS
           CLOSE NEW-STUDENT-RECORDS 
           STOP RUN.

       READ-FILE.
           IF 
              STUDENT-ID = TRANS-STUDENT-ID
           THEN 
              DISPLAY "ERROR: " TRANS-STUDENT-ID " ALREADY EXISTS " 
              "IN FILE"
              READ TRANS-RECORDS 
                 AT END
                    MOVE HIGH-VALUES TO TRANS-RECORD
                    MOVE 2 TO END-READING-TRANS-FILE
              END-READ
           END-IF

           IF 
              STUDENT-ID < TRANS-STUDENT-ID
           THEN 
              WRITE NEW-STUDENT-RECORD FROM STUDENT-RECORD
              DISPLAY "S < T: " NEW-STUDENT-RECORD 
              READ STUDENT-RECORDS
                 AT END
                    MOVE HIGH-VALUES TO STUDENT-RECORD
                    MOVE 9999999 TO STUDENT-ID
                    MOVE 2 TO END-READING-STUDENT-FILE 
                    DISPLAY "END READING STUDENT"
              END-READ
           END-IF

           IF 
              STUDENT-ID > TRANS-STUDENT-ID
           THEN 
              WRITE NEW-STUDENT-RECORD FROM TRANS-RECORD
              DISPLAY "S > T: " NEW-STUDENT-RECORD 
              READ TRANS-RECORDS 
                 AT END
                    MOVE HIGH-VALUES TO TRANS-RECORD
                    MOVE 9999999 TO TRANS-STUDENT-ID
                    MOVE 2 TO END-READING-TRANS-FILE
                    DISPLAY "END RAEDING TRANS"
              END-READ
           END-IF
           
           IF 
              END-READING-STUDENT-FILE = 1 OR 
              END-READING-TRANS-FILE = 1 
           THEN 
             GO TO READ-FILE 
           END-IF

           DISPLAY STUDENT-RECORD 
           DISPLAY TRANS-RECORD 
           
           DISPLAY "END READ FILE"
           GO TO MAIN.

       END PROGRAM INSERT-RECORDS.
