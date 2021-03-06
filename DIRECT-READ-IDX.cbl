       IDENTIFICATION DIVISION. 
       PROGRAM-ID. DIRECT-READ-IDX.
       AUTHOR. KA HOU, CHOI.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT 
              VIDEO-FILE ASSIGN TO "IDXVIDEO"
              ORGANIZATION IS INDEXED
              ACCESS MODE DYNAMIC
              RECORD KEY IS VIDEO-CODE
              ALTERNATE RECORD KEY IS VIDEO-TITLE
                 WITH DUPLICATES
              FILE STATUS IS VIDEO-STATUS.

       DATA DIVISION. 
       FILE SECTION. 
       FD  VIDEO-FILE.
       01  VIDEO-REC.
           02 VIDEO-CODE PIC 9(5).
           02 VIDEO-TITLE PIC X(40).
           02 SUPPLIER-CODE PIC 99.

       WORKING-STORAGE SECTION. 
       01  VIDEO-STATUS PIC X(2).
           88 RECORD-FOUND VALUE "00".
       01  REQUIRED-KEY PIC 9.
           88 VIDEO-CODE-KEY VALUE 1.
           88 VIDEO-TITLE-KEY VALUE 2.
       01  PRINT-VIDEO-RECORD.
           02 PRINT-VIDEO-CODE PIC 9(5).
           02 PRINT-VIDEO-TITLE PIC BBBBX(40).
           02 PRINT-SUPPLIER-CODE PIC BBBB99.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT VIDEO-FILE.

           DISPLAY "CHOOSE KEY VIDEO-CODE = 1, VIDEO-TITLE = 2 --> " 
              WITH NO ADVANCING.
           ACCEPT REQUIRED-KEY.

           IF VIDEO-CODE-KEY THEN 
              DISPLAY "ENTER VIDEO CODE (5 DIGITS) --> " 
                 WITH NO ADVANCING 
              ACCEPT VIDEO-CODE
              READ VIDEO-FILE
                 KEY IS VIDEO-CODE
                 INVALID KEY DISPLAY "VIDEO STATUS :- ", VIDEO-STATUS
              END-READ
           END-IF.

           IF VIDEO-TITLE-KEY THEN 
              DISPLAY "ENTER VIDEO TITEL (40 CHARS) --> " 
                 WITH NO ADVANCING 
              ACCEPT VIDEO-TITLE
              READ VIDEO-FILE 
                 KEY IS VIDEO-TITLE 
                 INVALID KEY DISPLAY "VIDEO STATUS :- ", VIDEO-STATUS 
              END-READ
           END-IF.

           IF RECORD-FOUND THEN 
              MOVE VIDEO-CODE TO PRINT-VIDEO-CODE
              MOVE VIDEO-TITLE TO PRINT-VIDEO-TITLE
              MOVE SUPPLIER-CODE TO PRINT-SUPPLIER-CODE
              DISPLAY PRINT-VIDEO-RECORD 
           END-IF.

           CLOSE VIDEO-FILE.
           STOP RUN.

       END PROGRAM DIRECT-READ-IDX.
                 