       IDENTIFICATION DIVISION.
       PROGRAM-ID. CENTRAL.
       AUTHOR. KA HOU, CHOI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT
              MASTER-FILE ASSIGN TO "master.txt"
              ORGANIZATION   IS LINE SEQUENTIAL
              ACCESS MODE    IS SEQUENTIAL
              FILE STATUS    IS MASTER-FILE-STATUS.

           SELECT
              T71-ONE-FILE ASSIGN TO "trans711.txt"
              ORGANIZATION   IS LINE SEQUENTIAL
              ACCESS MODE    IS SEQUENTIAL
              FILE STATUS    IS T71-ONE-FILE-STATUS.

           SELECT
              T71-THREE-FILE ASSIGN TO "trans713.txt"
              ORGANIZATION   IS LINE SEQUENTIAL
              ACCESS MODE    IS SEQUENTIAL
              FILE STATUS    IS T71-THREE-FILE-STATUS.

           SELECT
              SORTED-T71-ONE-FILE ASSIGN TO "transSorted711.txt"
              ORGANIZATION   IS LINE SEQUENTIAL
              ACCESS MODE    IS SEQUENTIAL
              FILE STATUS    IS SORTED-T71-ONE-FILE-STATUS.

           SELECT
              SORTED-T71-THREE-FILE ASSIGN TO "transSorted713.txt"
              ORGANIZATION   IS LINE SEQUENTIAL
              ACCESS MODE    IS SEQUENTIAL
              FILE STATUS    IS SORTED-T71-THREE-FILE-STATUS.

           SELECT
              SORTED-TRANS-FILE ASSIGN TO "transSorted.txt"
              ORGANIZATION   IS LINE SEQUENTIAL
              ACCESS MODE    IS SEQUENTIAL
              FILE STATUS    IS SORTED-TRANS-FILE-STATUS.

           SELECT
              UPDATED-MASTER-FILE ASSIGN TO "updatedMaster.txt"
              ORGANIZATION   IS LINE SEQUENTIAL
              ACCESS MODE    IS SEQUENTIAL
              FILE STATUS    IS UPDATED-MASTER-FILE-STATUS.

           SELECT
              NEGATIVE-REPORT-FILE ASSIGN TO "negReport.txt"
              ORGANIZATION   IS LINE SEQUENTIAL
              ACCESS MODE    IS SEQUENTIAL
              FILE STATUS    IS NEGATIVE-REPORT-FILE-STATUS.

           SELECT WORK-FILE ASSIGN TO "WORK.TMP".

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       01  MASTER-RECORD.
           02 MSTR-ACCT-HOLDER-NAME            PIC X(20).
           02 MSTR-ACCT-INFO.
              03 MSTR-ACCT-NUMBER              PIC 9(16).
              03 MSTR-ACCT-PASSWORD            PIC 9(6).
           02 MSTR-ACCT-BALANCE                PIC S9(13)V9(2)
                                               SIGN LEADING SEPARATE.

       FD  T71-ONE-FILE.
       01  T71-ONE-RECORD.
           02 ONE-ACCT-NUMBER                  PIC 9(16).
           02 ONE-OPERATION                    PIC A.
           02 ONE-AMOUNT                       PIC 9(5)V9(2).
           02 ONE-TIMESTAMP                    PIC 9(5).

       FD  T71-THREE-FILE.
       01  T71-THREE-RECORD.
           02 THREE-ACCT-NUMBER                PIC 9(16).
           02 THREE-OPERATION                  PIC A.
           02 THREE-AMOUNT                     PIC 9(5)V9(2).
           02 THREE-TIMESTAMP                  PIC 9(5).

       FD  SORTED-T71-ONE-FILE.
       01  SORTED-T71-ONE-RECORD.
           02 SORTED-ONE-ACCT-NUMBER           PIC 9(16).
           02 SORTED-ONE-OPERATION             PIC A.
           02 SORTED-ONE-AMOUNT                PIC 9(5)V9(2).
           02 SORTED-ONE-TIMESTAMP             PIC 9(5).

       FD  SORTED-T71-THREE-FILE.
       01  SORTED-T71-THREE-RECORD.
           02 SORTED-THREE-ACCT-NUMBER         PIC 9(16).
           02 SORTED-THREE-OPERATION           PIC A.
           02 SORTED-THREE-AMOUNT              PIC 9(5)V9(2).
           02 SORTED-THREE-TIMESTAMP           PIC 9(5).

       FD  SORTED-TRANS-FILE.
       01  SORTED-TRANS-RECORD.
           02 SORTED-TRANS-ACCT-NUMBER         PIC 9(16).
           02 SORTED-TRANS-OPERATION           PIC A.
           02 SORTED-TRANS-AMOUNT              PIC 9(5)V9(2).
           02 SORTED-TRANS-TIMESTAMP           PIC 9(5).

       FD  UPDATED-MASTER-FILE.
       01  UPDATED-MASTER-RECORD.
           02 UPDATED-ACCT-HOLDER-NAME         PIC X(20).
           02 UPDATED-ACCT-INFO.
              03 UPDATED-ACCT-NUMBER           PIC 9(16).
              03 UPDATED-ACCT-PASSWORD         PIC 9(6).
           02 UPDATED-ACCT-BALANCE             PIC S9(13)V9(2)
                                               SIGN LEADING SEPARATE.

       FD  NEGATIVE-REPORT-FILE.
       01  REPORT-RECORD.
           02 PREFIX-NAME                      PIC X(6).
           02 REPORT-ACCT-HOLDER-NAME          PIC X(20).
           02 PREFIX-ACCT-NUMBER               PIC X(17).
           02 REPORT-ACCT-NUMBER               PIC 9(16).
           02 PREFIX-BALANCE                   PIC X(10).
           02 REPORT-BALANCE                   PIC S9(13)V9(2)
                                               SIGN LEADING SEPARATE.

       SD  WORK-FILE.
       01  WORK-RECORD.
           02 WORK-ACCT-NUMBER                 PIC 9(16).
           02 FILLER                           PIC X(8).
           02 WORK-TIMESTAMP                   PIC 9(5).

       WORKING-STORAGE SECTION.
       01  MASTER-FILE-STATUS                  PIC 99.
           88 MASTER-FILE-EOF-REACHED          VALUE 10.
           88 MASTER-FILE-NOT-FOUND            VALUE 35.
       01  T71-ONE-FILE-STATUS                 PIC 99.
           88 T71-ONE-FILE-NOT-FOUND           VALUE 35.
       01  T71-THREE-FILE-STATUS               PIC 99.
           88 T71-THREE-FILE-NOT-FOUND         VALUE 35.
       01  SORTED-T71-ONE-FILE-STATUS          PIC 99.
           88 SORTED-ONE-FILE-EOF-REACHED      VALUE 10.
           88 SORTED-ONE-FILE-ALREADY-OPEN     VALUE 41.
       01  SORTED-T71-THREE-FILE-STATUS        PIC 99.
           88 SORTED-THREE-FILE-EOF-REACHED    VALUE 10.
           88 SORTED-THREE-FILE-ALREADY-OPEN   VALUE 41.
       01  SORTED-TRANS-FILE-STATUS            PIC 99.
           88 SORTED-TRANS-FILE-ALREADY-OPEN   VALUE 41.
       01  UPDATED-MASTER-FILE-STATUS          PIC 99.
           88 UPDATED-FILE-ALREADY-OPEN        VALUE 41.
       01  NEGATIVE-REPORT-FILE-STATUS         PIC 99.
           88 REPORT-FILE-ALREADY-OPEN         VALUE 41.

       01  MASTER-ITERATION-STATUS             PIC 9 VALUE 1.
           88 ITERATION-INITIALIZED            VALUE 0.
           88 NEXT-ACCT                        VALUE 0 THRU 1.
           88 SEARCHING-FOR-ACCT-TRANSAC       VALUE 2.

       01  BARRED-ACCT-BUFFER.
           02 PREFIX-NAME                      PIC X(6)
              VALUE "Name: ".
           02 BARRED-HOLDER-NAME               PIC X(20).
           02 PREFIX-NUMBER                    PIC X(17)
              VALUE " Account Number: ".
           02 BARRED-NUMBER                    PIC 9(16).
           02 PREFIX-BALANCE                   PIC X(10)
              VALUE " Balance: ".
           02 BARRED-BALANCE                   PIC S9(13)V9(2)
                                               SIGN LEADING SEPARATE.

       PROCEDURE DIVISION.
       CHECK-IF-ALL-INPUT-FILES-EXIST.
           OPEN INPUT MASTER-FILE, T71-ONE-FILE, T71-THREE-FILE.
           IF
              MASTER-FILE-NOT-FOUND OR
              T71-ONE-FILE-NOT-FOUND OR
              T71-THREE-FILE-NOT-FOUND
           THEN
              DISPLAY "[ABORT]: NECESSARY FILES NOT FOUND"
              STOP RUN
           END-IF.

           GO TO SORT-TRANS-FILES.

       SORT-TRANS-FILES.
           DISPLAY SPACES
           DISPLAY "=========================================="
           SORT WORK-FILE
              ON ASCENDING KEY WORK-ACCT-NUMBER
              ON ASCENDING KEY WORK-TIMESTAMP
              USING T71-ONE-FILE
              GIVING SORTED-T71-ONE-FILE.
           DISPLAY "SORTED: T71-ONE-FILE".

           SORT WORK-FILE
              ON ASCENDING KEY WORK-ACCT-NUMBER
              ON ASCENDING KEY WORK-TIMESTAMP
              USING T71-THREE-FILE
              GIVING SORTED-T71-THREE-FILE.
           DISPLAY "SORTED: T71-THREE-FILE".

           DISPLAY "=========================================="
           OPEN INPUT SORTED-T71-ONE-FILE, SORTED-T71-THREE-FILE.
           OPEN OUTPUT SORTED-TRANS-FILE.

           READ SORTED-T71-ONE-FILE
              AT END
                 MOVE HIGH-VALUES TO SORTED-T71-ONE-RECORD
                 SET SORTED-ONE-FILE-EOF-REACHED TO TRUE
           END-READ.

           READ SORTED-T71-THREE-FILE
              AT END
                 MOVE HIGH-VALUES TO SORTED-T71-THREE-RECORD
                 SET SORTED-THREE-FILE-EOF-REACHED TO TRUE
           END-READ.

           IF
              NOT SORTED-ONE-FILE-EOF-REACHED OR
              NOT SORTED-THREE-FILE-EOF-REACHED
           THEN
              GO TO MERGE-TRANS-FILES
           END-IF.

           DISPLAY "TRANS FILES BOTH EMPTY"
           DISPLAY SPACES
           CLOSE SORTED-T71-ONE-FILE, SORTED-T71-THREE-FILE.
           CLOSE SORTED-TRANS-FILE.
           GO TO UPDATE-MASTER-FILE.

       MERGE-TRANS-FILES.
           IF
              SORTED-ONE-ACCT-NUMBER = SORTED-THREE-ACCT-NUMBER AND
              SORTED-ONE-TIMESTAMP < SORTED-THREE-TIMESTAMP
           THEN
              WRITE SORTED-TRANS-RECORD FROM SORTED-T71-ONE-RECORD
              DISPLAY "[ TS] ONE < THREE: " SORTED-TRANS-RECORD " (711)"
              READ SORTED-T71-ONE-FILE
                 AT END
                    MOVE HIGH-VALUES TO SORTED-T71-ONE-RECORD
                    SET SORTED-ONE-FILE-EOF-REACHED TO TRUE
                    DISPLAY "              EOF: SORTED-T71-ONE-FILE"
              END-READ
           END-IF.

           IF
              SORTED-ONE-ACCT-NUMBER = SORTED-THREE-ACCT-NUMBER AND
              SORTED-ONE-TIMESTAMP > SORTED-THREE-TIMESTAMP
           THEN
              WRITE SORTED-TRANS-RECORD FROM SORTED-T71-THREE-RECORD
              DISPLAY "[ TS] ONE > THREE: " SORTED-TRANS-RECORD " (713)"
              READ SORTED-T71-THREE-FILE
                 AT END
                    MOVE HIGH-VALUES TO SORTED-T71-THREE-RECORD
                    SET SORTED-THREE-FILE-EOF-REACHED TO TRUE
                    DISPLAY "              EOF: SORTED-T71-THREE-FILE"
              END-READ
           END-IF.

           IF SORTED-ONE-ACCT-NUMBER < SORTED-THREE-ACCT-NUMBER
           THEN
              WRITE SORTED-TRANS-RECORD FROM SORTED-T71-ONE-RECORD
              DISPLAY "[NUM] ONE < THREE: " SORTED-TRANS-RECORD " (711)"
              READ SORTED-T71-ONE-FILE
                 AT END
                    MOVE HIGH-VALUES TO SORTED-T71-ONE-RECORD
                    SET SORTED-ONE-FILE-EOF-REACHED TO TRUE
                    DISPLAY "              EOF: SORTED-T71-ONE-FILE"
              END-READ
           END-IF.

           IF SORTED-ONE-ACCT-NUMBER > SORTED-THREE-ACCT-NUMBER
           THEN
              WRITE SORTED-TRANS-RECORD FROM SORTED-T71-THREE-RECORD
              DISPLAY "[NUM] ONE > THREE: " SORTED-TRANS-RECORD " (713)"
              READ SORTED-T71-THREE-FILE
                 AT END
                    MOVE HIGH-VALUES TO SORTED-T71-THREE-RECORD
                    SET SORTED-THREE-FILE-EOF-REACHED TO TRUE
                    DISPLAY "              EOF: SORTED-T71-THREE-FILE"
              END-READ
           END-IF.

           IF
              NOT SORTED-ONE-FILE-EOF-REACHED OR
              NOT SORTED-THREE-FILE-EOF-REACHED
           THEN
             GO TO MERGE-TRANS-FILES
           END-IF.

           DISPLAY "           MERGED: [TWO] TRANSAC FILES"
           CLOSE SORTED-T71-ONE-FILE, SORTED-T71-THREE-FILE.
           CLOSE SORTED-TRANS-FILE.
           GO TO UPDATE-MASTER-FILE.

       UPDATE-MASTER-FILE.
           IF NEXT-ACCT
           THEN
              IF ITERATION-INITIALIZED
              THEN
                 WRITE UPDATED-MASTER-RECORD FROM MASTER-RECORD
                 DISPLAY "WRITTEN BALANCE: " UPDATED-ACCT-BALANCE
                 DISPLAY SPACES
                 CLOSE SORTED-TRANS-FILE
                 OPEN INPUT SORTED-TRANS-FILE
              END-IF

              IF NOT ITERATION-INITIALIZED
              THEN
                 OPEN INPUT MASTER-FILE, SORTED-TRANS-FILE
                 OPEN OUTPUT UPDATED-MASTER-FILE
                 SET ITERATION-INITIALIZED TO TRUE
              END-IF

              READ MASTER-FILE
                 AT END
                    SET MASTER-FILE-EOF-REACHED TO TRUE
                    DISPLAY "EOF: MASTER-FILE"
                    DISPLAY "=========================================="
                    CLOSE MASTER-FILE, UPDATED-MASTER-FILE
                    CLOSE SORTED-TRANS-FILE
                    GO TO GENERATE-NEGATIVE-REPORT
              END-READ
              SET SEARCHING-FOR-ACCT-TRANSAC TO TRUE
              DISPLAY "=========================================="
              DISPLAY SPACES
              DISPLAY MASTER-RECORD
              DISPLAY SPACES
              DISPLAY "TRANSACTION RECORD SEARCHING: "
           END-IF.

           READ SORTED-TRANS-FILE
              AT END
                 SET NEXT-ACCT TO TRUE
                 DISPLAY "> EOF: SORTED-TRANSAC-FILE......<"
                         "~ [END SEARCHING]"
                 DISPLAY SPACES
                 GO TO UPDATE-MASTER-FILE
           END-READ.

           IF
              SORTED-TRANS-ACCT-NUMBER > MSTR-ACCT-NUMBER
           THEN
              SET NEXT-ACCT TO TRUE
              DISPLAY "> " SORTED-TRANS-RECORD  " <~ [END SEARCHING]"
              DISPLAY SPACES
              GO TO UPDATE-MASTER-FILE
           END-IF.
           DISPLAY "> " SORTED-TRANS-RECORD  " <".

           IF SORTED-TRANS-ACCT-NUMBER = MSTR-ACCT-NUMBER
           THEN
              DISPLAY "~~~~~~~~~~> ORIGINAL BALANCE: " MSTR-ACCT-BALANCE
              DISPLAY "~~~~~~~~~~>      TRANSACTION: "
                      SORTED-TRANS-OPERATION "        "
                      SORTED-TRANS-AMOUNT

              IF SORTED-TRANS-OPERATION = "D"
              THEN
                 ADD SORTED-TRANS-AMOUNT TO MSTR-ACCT-BALANCE
                    GIVING MSTR-ACCT-BALANCE
              END-IF
              IF SORTED-TRANS-OPERATION = "W"
              THEN
                 SUBTRACT SORTED-TRANS-AMOUNT FROM MSTR-ACCT-BALANCE
                    GIVING MSTR-ACCT-BALANCE
              END-IF
              DISPLAY "~~~~~~~~~~>  UPDATED BALANCE: " MSTR-ACCT-BALANCE
              DISPLAY SPACES
           END-IF.

           GO TO UPDATE-MASTER-FILE.

       GENERATE-NEGATIVE-REPORT.
           IF
              NOT UPDATED-FILE-ALREADY-OPEN OR
              NOT REPORT-FILE-ALREADY-OPEN
           THEN
              OPEN INPUT UPDATED-MASTER-FILE
              OPEN OUTPUT NEGATIVE-REPORT-FILE
           END-IF.

           READ UPDATED-MASTER-FILE
              AT END
                 DISPLAY "=========================================="
                 DISPLAY SPACES
                 CLOSE UPDATED-MASTER-FILE, NEGATIVE-REPORT-FILE
                 STOP RUN
           END-READ.

           IF UPDATED-ACCT-BALANCE IS NEGATIVE
           THEN
              MOVE UPDATED-ACCT-HOLDER-NAME TO BARRED-HOLDER-NAME
              MOVE UPDATED-ACCT-NUMBER      TO BARRED-NUMBER
              MOVE UPDATED-ACCT-BALANCE     TO BARRED-BALANCE
              WRITE REPORT-RECORD FROM BARRED-ACCT-BUFFER
              DISPLAY REPORT-RECORD
           END-IF.

           GO TO GENERATE-NEGATIVE-REPORT.

       END PROGRAM CENTRAL.
