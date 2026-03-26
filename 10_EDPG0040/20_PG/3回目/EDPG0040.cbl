      ******************************************************************
      * Author:WATANABE KAZUMA
      * Date:20260227
      * Purpose:納期回答データを読み込み、
      *         購買担当者CD・部品CD・ベンダーCDをサマリキーとし
      *         サマリキーごとに納期回答数量をサマリし、
      *         納入予定データを出力する
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION                                         DIVISION.
       PROGRAM-ID. EDPG0040.
       ENVIRONMENT                                            DIVISION.
       INPUT-OUTPUT                                           SECTION.
       FILE-CONTROL.
           SELECT U01-FILE ASSIGN TO 'C:\COBOL\EDPG0040\U01.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT U21-FILE ASSIGN TO 'C:\COBOL\EDPG0040\U21.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA                                                   DIVISION.
       FILE                                                   SECTION.
      *>   納期回答データ
       FD  U01-FILE.
       01  U01-REC.
         03  U01-KOBAI-TANTO-CD                               PIC X(05).
         03  U01-BUHIN-CD                                     PIC X(10).
         03  U01-VENDOR-CD                                    PIC X(08).
         03  U01-NKKAITO-INF.
           05  U01-SUPPLY-DAY                                 PIC 9(02).
           05  U01-NKKAITO-SU                                 PIC 9(07).
         03  U01-ORDER-NO                                     PIC X(07).

      *>   納入予定データ
       FD  U21-FILE.
       01  U21-REC.
         03  U21-KOBAI-TANTO-CD                               PIC X(05).
         03  U21-BUHIN-CD                                     PIC X(10).
         03  U21-VENDOR-CD                                    PIC X(08).
         03  U21-VENDOR-NAME                                  PIC X(30).
         03  U21-NKKAITO-INF.
           05  U21-NKKAITO-TBL OCCURS 6 TIMES.
             07  U21-NKKAITO-SU                               PIC 9(07).
         03  U21-ORDER-NO                                     PIC X(07).

       WORKING-STORAGE                                        SECTION.
       01  KEY-NEW.
         03  KEY-NEW-KOBAI-TANTO-CD                           PIC X(05).
         03  KEY-NEW-BUHIN-CD                                 PIC X(10).
         03  KEY-NEW-VENDOR-CD                                PIC X(08).
       01  KEY-OLD.
         03  KEY-OLD-KOBAI-TANTO-CD                           PIC X(05).
         03  KEY-OLD-BUHIN-CD                                 PIC X(10).
         03  KEY-OLD-VENDOR-CD                                PIC X(08).
       01  CNT-U01                                            PIC 9(07).
       01  CNT-U21                                            PIC 9(07).
      *>   PG開始・終了時日付データ
       01  WK-SYS-DATE.
         03  WK-SYS-DATE-YYYY                                 PIC 9(04).
         03  WK-SYS-DATE-MM                                   PIC 9(02).
         03  WK-SYS-DATE-DD                                   PIC 9(02).
         03  WK-SYS-DATE-HH                                   PIC 9(02).
         03  WK-SYS-DATE-HM                                   PIC 9(02).
         03  WK-SYS-DATE-SS                                   PIC 9(02).

       PROCEDURE                                              DIVISION.
       S000-RROC                                              SECTION.
      *>   初期処理
           PERFORM S100-INIT.
      *>   主処理
           PERFORM S200-MAIN
             UNTIL KEY-NEW = HIGH-VALUE.
      *>   終了処理
           PERFORM S300-FINL.
           STOP RUN.

      *>   初期処理
       S100-INIT                                              SECTION.
           MOVE     FUNCTION CURRENT-DATE TO WK-SYS-DATE.
           DISPLAY '*******   EDPG00040  START = '
                    WK-SYS-DATE-YYYY
                   '/'
                    WK-SYS-DATE-MM
                   '/'
                    WK-SYS-DATE-DD
                   '  '
                    WK-SYS-DATE-HH
                   ':'
                    WK-SYS-DATE-HM
                   ':'
                    WK-SYS-DATE-SS
                   '   *******'.

           OPEN INPUT  U01-FILE
                OUTPUT U21-FILE.

           PERFORM     S110-READ-U01.

      *    納期回答データREAD処理
       S110-READ-U01                                          SECTION.
           READ U01-FILE
             AT END
               MOVE HIGH-VALUE            TO KEY-NEW
             NOT AT END
               ADD  1                     TO CNT-U01
               MOVE U01-KOBAI-TANTO-CD    TO KEY-NEW-KOBAI-TANTO-CD
               MOVE U01-BUHIN-CD          TO KEY-NEW-BUHIN-CD
               MOVE U01-VENDOR-CD         TO KEY-NEW-VENDOR-CD
           END-READ.

      *>   主処理
       S200-MAIN                                              SECTION.
           INITIALIZE U21-REC.
           MOVE       U01-KOBAI-TANTO-CD  TO U21-KOBAI-TANTO-CD.
           MOVE       U01-BUHIN-CD        TO U21-BUHIN-CD.
           MOVE       U01-VENDOR-CD       TO U21-VENDOR-CD.
           MOVE       U01-ORDER-NO        TO U21-ORDER-NO.

           MOVE KEY-NEW-KOBAI-TANTO-CD    TO KEY-OLD-KOBAI-TANTO-CD.
           MOVE KEY-NEW-BUHIN-CD          TO KEY-OLD-BUHIN-CD.
           MOVE KEY-NEW-VENDOR-CD         TO KEY-OLD-VENDOR-CD.

           PERFORM UNTIL KEY-NEW NOT = KEY-OLD
             ADD      U01-NKKAITO-SU TO U21-NKKAITO-SU(U01-SUPPLY-DAY)
             PERFORM  S110-READ-U01
           END-PERFORM.

           WRITE   U21-REC.
           ADD     1                      TO CNT-U21.

      *>   終了処理
       S300-FINL                                              SECTION.
           CLOSE U01-FILE
                 U21-FILE.

           DISPLAY '   (U01) READ  = ' CNT-U01.
           DISPLAY '   (U21) WRITE = ' CNT-U21.

           MOVE     FUNCTION CURRENT-DATE TO WK-SYS-DATE.
           DISPLAY '*******   EDPG00040  END   = '
                    WK-SYS-DATE-YYYY
                   '/'
                    WK-SYS-DATE-MM
                   '/'
                    WK-SYS-DATE-DD
                   '  '
                    WK-SYS-DATE-HH
                   ':'
                    WK-SYS-DATE-HM
                   ':'
                    WK-SYS-DATE-SS
                   '   *******'.
