       PROCESS DLL,NODYNAM,EXPORTALL,TEST(NOSEP),NOCICS
      *+---------------------------------------------------------------+
      *| Product: IBM Rational Developer for z Systems                 |
      *| Component: IBM z/OS Automated Unit Testing Framework (zUnit)  |
      *|   for Enterprise COBOL and PL/I                               |
      *| Program: Enterprise COBOL zUnit Test Case                     |
      *| Date Generated: 02/07/2019 06:32                              |
      *| ID: 7605af2a-9ca7-4bd9-bf89-b2628ade22f6                      |
      *+---------------------------------------------------------------+
      *+---------------------------------------------------------------+
      *| TLGIPOL0                                                      |
      *|     This program is immediately called by the IBM             |
      *|     z/OS Automated Unit Testing Framework (zUnit)             |
      *|     Test Runner to allow for initialization of the            |
      *|     Test Case. Upon return from this program, the             |
      *|     Test Runner will attempt to call the ADDTESTS             |
      *|     program.                                                  |
      *|                                                               |
      *| @param TEST-CASE-PTR (input),                                 |
      *|     A pointer-by-value to an area maintained by the           |
      *|     zUnit Test Runner that identifies the Test Case           |
      *|     and associated resources.                                 |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TLGIPOL0'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CBLTESTC-ID PIC X(36)
            VALUE '7605af2a-9ca7-4bd9-bf89-b2628ade22f6'.
       1 CBLTESTC-NAME PIC X(8)
            VALUE 'TLGIPOL0'.
       1 CBLTESTC-ID-LEN PIC S9(9) COMP-5.
       1 CBLTESTC-NAME-LEN PIC S9(9) COMP-5.
       1 CBLTESTC-SETUP    FUNCTION-POINTER.
       1 CBLTESTC-TEARDOWN FUNCTION-POINTER.
       1 CBLTESTC-ADDTESTS FUNCTION-POINTER.
       LINKAGE SECTION.
       1 TEST-CASE-PTR POINTER.
       PROCEDURE DIVISION USING BY VALUE TEST-CASE-PTR.
      *    this program does not require editing.
           MOVE LENGTH OF CBLTESTC-ID TO CBLTESTC-ID-LEN
           MOVE LENGTH OF CBLTESTC-NAME TO CBLTESTC-NAME-LEN
           SET CBLTESTC-ADDTESTS TO ENTRY 'ADDTESTS'
           SET CBLTESTC-SETUP    TO ENTRY 'SETUP'
           SET CBLTESTC-TEARDOWN TO ENTRY 'TEARDOWN'
           CALL 'AZUTCINI' USING
               BY VALUE     TEST-CASE-PTR
               BY REFERENCE CBLTESTC-ID
               BY VALUE     CBLTESTC-ID-LEN
               BY REFERENCE CBLTESTC-NAME
               BY VALUE     CBLTESTC-NAME-LEN
               BY VALUE     CBLTESTC-ADDTESTS
               BY VALUE     CBLTESTC-SETUP
               BY VALUE     CBLTESTC-TEARDOWN
           .
        END PROGRAM 'TLGIPOL0'.
      *+---------------------------------------------------------------+
      *| ADDTESTS                                                      |
      *|     This program is called by the zUnit Test Runner           |
      *|     to allow for adding Tests to the Test Case. Upon          |
      *|     return from this program, the Test Runner will            |
      *|     call the added Tests, surrounding each with calls         |
      *|     to the SETUP and TEARDOWN programs.                       |
      *|                                                               |
      *| @param TEST-CASE-PTR (input),                                 |
      *|     A pointer-by-value to an area maintained by the           |
      *|     zUnit Test Runner that identifies the Test Case           |
      *|     and associated resources.                                 |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ADDTESTS'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 TEST-ENTRY     FUNCTION-POINTER.
       1 TEST-NAME      PIC X(254).
       1 TEST-NAME-LEN  PIC S9(9) COMP-5.
       LINKAGE SECTION.
       1 TEST-CASE-PTR POINTER.
       PROCEDURE DIVISION USING BY VALUE TEST-CASE-PTR.
      *    add tests to the test case.
           SET TEST-ENTRY TO ENTRY 'TEST2'
           MOVE 'TEST2' TO TEST-NAME
           MOVE 5 TO TEST-NAME-LEN
           CALL 'AZUTCADD' USING
               BY VALUE     TEST-CASE-PTR
               BY VALUE     TEST-ENTRY
               BY REFERENCE TEST-NAME
               BY VALUE     TEST-NAME-LEN
           .
       END PROGRAM 'ADDTESTS'.
      *+---------------------------------------------------------------+
      *| SETUP                                                         |
      *|     This program is invoked by the zUnit Test Runner          |
      *|     prior to each Test to allow for allocation of             |
      *|     resources (e.g., memory, connections) that are            |
      *|     required to create the Test Fixture.                      |
      *|                                                               |
      *| @param TEST-CASE-PTR (input),                                 |
      *|     A pointer-by-value to an area maintained by the           |
      *|     zUnit Test Runner that identifies the Test Case           |
      *|     and associated resources.                                 |
      *|                                                               |
      *| @param TEST-FIXTURE-PTR (output),                             |
      *|     A pointer-by-reference in which to store the address      |
      *|     of a user-defined structure that represents the Test      |
      *|     Fixture. References to all allocated resources should     |
      *|     be maintained in this structure so that they may be       |
      *|     accessed in the respective Test program, and released     |
      *|     in the TEARDOWN program.                                  |
      *|                                                               |
      *| @param TEST-NAME-PTR (input),                                 |
      *|     A pointer-by-value to an area containing the name         |
      *|     of the Test for which a Test Fixture should be            |
      *|     allocated.                                                |
      *|                                                               |
      *| @param TEST-NAME-LEN (input),                                 |
      *|     A integer-by-value that specifies the length in           |
      *|     bytes of the value contained in parameter                 |
      *|     TEST-NAME-PTR.                                            |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'SETUP'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CEEGTST-SIZE PIC S9(9) COMP-5.
       1 CEEGTST-HEAP PIC S9(9) COMP-5.
       1 PARM-SIZE    PIC S9(9) COMP-5.
       1 CURRENT-TEST-FIXTURE-PTR POINTER.
       1 CURRENT-TEST-FIXTURE-PTR-VALUE REDEFINES
           CURRENT-TEST-FIXTURE-PTR PIC S9(9) COMP-5.
       1 NEXT-TEST-FIXTURE-PTR POINTER.
       1 NEXT-TEST-FIXTURE-PTR-VALUE REDEFINES
           NEXT-TEST-FIXTURE-PTR  PIC S9(9) COMP-5.
       1 FAIL-MESSAGE-TXT PIC X(254).
       1 FAIL-MESSAGE-LEN PIC S9(9) COMP-5.
       1 AZ-TEST-INPUT-DATA-VALUE.
          3 ZUT00000000.
            5 PIC X(4) DISPLAY VALUE 'SSP1'.
          3 ZUT00000001.
            5 PIC X(4) DISPLAY VALUE '0024'.
          3 ZUT00000002.
            5 PIC X(1) DISPLAY VALUE
            ''''.
          3 ZUT00000003.
            5 PIC X(2) VALUE X'0E02'.
          3 ZUT00000004.
            5 PIC X(8) DISPLAY VALUE 'LGIPOL01'.
          3 ZUT00000005.
            5 PIC X(6) DISPLAY VALUE '01IMOT'.
       1 DFHEIBLK IS EXTERNAL.
         2 EIBTIME PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBDATE PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBTRNID PICTURE X(4).
         2 EIBTASKN PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBTRMID PICTURE X(4).
         2 DFHEIGDI PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBCPOSN PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBCALEN PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBAID PICTURE X(1).
         2 EIBFN PICTURE X(2).
          2 EIBFN-AZ REDEFINES EIBFN.
          3 PIC X(2) DISPLAY.
         2 EIBRCODE PICTURE X(6).
         2 EIBDS PICTURE X(8).
         2 EIBREQID PICTURE X(8).
         2 EIBRSRCE PICTURE X(8).
         2 EIBSYNC PICTURE X.
         2 EIBFREE PICTURE X.
         2 EIBRECV PICTURE X.
         2 EIBSEND PICTURE X.
         2 EIBATT PICTURE X.
         2 EIBEOC PICTURE X.
         2 EIBFMH PICTURE X.
         2 EIBCOMPL PICTURE X(1).
         2 EIBSIG PICTURE X(1).
         2 EIBCONF PICTURE X(1).
         2 EIBERR PICTURE X(1).
         2 EIBERRCD PICTURE X(4).
         2 EIBSYNRB PICTURE X.
         2 EIBNODAT PICTURE X.
         2 EIBRESP PICTURE S9(8) USAGE COMPUTATIONAL.
         2 EIBRESP2 PICTURE S9(8) USAGE COMPUTATIONAL.
         2 EIBRLDBK PICTURE X(1).
       1 AZ-TEST-NAME-FOR-SUB   PIC X(254) EXTERNAL.
       1 AZ-TEST-NAME-LEN  PIC S9(9) COMP-5 EXTERNAL.
       1 AZ-TEST-CASE-PTR  POINTER EXTERNAL.
       1 AZ-TEST-CASE-PTR-VALUE REDEFINES
           AZ-TEST-CASE-PTR  PIC 9(9) COMP-5.
       LINKAGE SECTION.
       1 DFHCOMMAREA.
         3 CA-REQUEST-ID PIC X(6).
         3 CA-RETURN-CODE PIC 9(2).
         3 CA-CUSTOMER-NUM PIC 9(10).
         3 CA-REQUEST-SPECIFIC PIC X(32482).
         3 CA-CUSTOMER-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-FIRST-NAME PIC X(10).
         5 CA-LAST-NAME PIC X(20).
         5 CA-DOB PIC X(10).
         5 CA-HOUSE-NAME PIC X(20).
         5 CA-HOUSE-NUM PIC X(4).
         5 CA-POSTCODE PIC X(8).
         5 CA-NUM-POLICIES PIC 9(3).
         5 CA-PHONE-MOBILE PIC X(20).
         5 CA-PHONE-HOME PIC X(20).
         5 CA-EMAIL-ADDRESS PIC X(100).
         5 CA-POLICY-DATA PIC X(32267).
         3 CA-CUSTSECR-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-CUSTSECR-PASS PIC X(32).
         5 CA-CUSTSECR-COUNT PIC X(4).
         5 CA-CUSTSECR-STATE PIC X.
         5 CA-CUSTSECR-DATA PIC X(32445).
         3 CA-POLICY-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-POLICY-NUM PIC 9(10).
         5 CA-POLICY-COMMON.
         7 CA-ISSUE-DATE PIC X(10).
         7 CA-EXPIRY-DATE PIC X(10).
         7 CA-LASTCHANGED PIC X(26).
         7 CA-BROKERID PIC 9(10).
         7 CA-BROKERSREF PIC X(10).
         7 CA-PAYMENT PIC 9(6).
         5 CA-POLICY-SPECIFIC PIC X(32400).
         5 CA-ENDOWMENT REDEFINES CA-POLICY-SPECIFIC.
         7 CA-E-WITH-PROFITS PIC X.
         7 CA-E-EQUITIES PIC X.
         7 CA-E-MANAGED-FUND PIC X.
         7 CA-E-FUND-NAME PIC X(10).
         7 CA-E-TERM PIC 99.
         7 CA-E-SUM-ASSURED PIC 9(6).
         7 CA-E-LIFE-ASSURED PIC X(31).
         7 CA-E-PADDING-DATA PIC X(32348).
         5 CA-HOUSE REDEFINES CA-POLICY-SPECIFIC.
         7 CA-H-PROPERTY-TYPE PIC X(15).
         7 CA-H-BEDROOMS PIC 9(3).
         7 CA-H-VALUE PIC 9(8).
         7 CA-H-HOUSE-NAME PIC X(20).
         7 CA-H-HOUSE-NUMBER PIC X(4).
         7 CA-H-POSTCODE PIC X(8).
         7 CA-H-FILLER PIC X(32342).
         5 CA-MOTOR REDEFINES CA-POLICY-SPECIFIC.
         7 CA-M-MAKE PIC X(15).
         7 CA-M-MODEL PIC X(15).
         7 CA-M-VALUE PIC 9(6).
         7 CA-M-REGNUMBER PIC X(7).
         7 CA-M-COLOUR PIC X(8).
         7 CA-M-CC PIC 9(4).
         7 CA-M-MANUFACTURED PIC X(10).
         7 CA-M-PREMIUM PIC 9(6).
         7 CA-M-ACCIDENTS PIC 9(6).
         7 CA-M-FILLER PIC X(32323).
         5 CA-COMMERCIAL REDEFINES CA-POLICY-SPECIFIC.
         7 CA-B-ADDRESS PIC X(255).
         7 CA-B-POSTCODE PIC X(8).
         7 CA-B-LATITUDE PIC X(11).
         7 CA-B-LONGITUDE PIC X(11).
         7 CA-B-CUSTOMER PIC X(255).
         7 CA-B-PROPTYPE PIC X(255).
         7 CA-B-FIREPERIL PIC 9(4).
         7 CA-B-FIREPREMIUM PIC 9(8).
         7 CA-B-CRIMEPERIL PIC 9(4).
         7 CA-B-CRIMEPREMIUM PIC 9(8).
         7 CA-B-FLOODPERIL PIC 9(4).
         7 CA-B-FLOODPREMIUM PIC 9(8).
         7 CA-B-WEATHERPERIL PIC 9(4).
         7 CA-B-WEATHERPREMIUM PIC 9(8).
         7 CA-B-STATUS PIC 9(4).
         7 CA-B-REJECTREASON PIC X(255).
         7 CA-B-FILLER PIC X(31298).
         5 CA-CLAIM REDEFINES CA-POLICY-SPECIFIC.
         7 CA-C-NUM PIC 9(10).
         7 CA-C-DATE PIC X(10).
         7 CA-C-PAID PIC 9(8).
         7 CA-C-VALUE PIC 9(8).
         7 CA-C-CAUSE PIC X(255).
         7 CA-C-OBSERVATIONS PIC X(255).
         7 CA-C-FILLER PIC X(31854).
       1 TEST-CASE-PTR    POINTER.
       1 TEST-CASE-PTR-VALUE REDEFINES
           TEST-CASE-PTR  PIC 9(9) COMP-5.
       1 TEST-FIXTURE-PTR POINTER.
       1 TEST-NAME-PTR    POINTER.
       1 TEST-NAME-LEN    PIC S9(9) COMP-5.
       1 TEST-NAME        PIC X(254).
       PROCEDURE DIVISION USING BY VALUE TEST-CASE-PTR
                          BY REFERENCE TEST-FIXTURE-PTR
                          BY VALUE TEST-NAME-PTR
                          BY VALUE TEST-NAME-LEN.
           SET ADDRESS OF TEST-NAME TO TEST-NAME-PTR
           MOVE TEST-NAME(1:TEST-NAME-LEN) TO
              AZ-TEST-NAME-FOR-SUB(1:TEST-NAME-LEN)
           MOVE TEST-NAME-LEN TO AZ-TEST-NAME-LEN
           EVALUATE TEST-NAME(1:TEST-NAME-LEN)
              WHEN 'TEST2'
                DISPLAY 'SETUP (' TEST-NAME(1:TEST-NAME-LEN) ')'
      *       setup for test TEST2
                PERFORM ALLOCATE-PARM
      *       initialize parameter
                PERFORM INITIALIZE-PARM
      *       set input data to parameter
                MOVE 94853 TO EIBTIME OF DFHEIBLK
                MOVE 119037 TO EIBDATE OF DFHEIBLK
                MOVE ZUT00000000 TO EIBTRNID OF DFHEIBLK
                MOVE 394 TO EIBTASKN OF DFHEIBLK
                MOVE ZUT00000001 TO EIBTRMID OF DFHEIBLK
                MOVE 0 TO DFHEIGDI OF DFHEIBLK
                MOVE 1848 TO EIBCPOSN OF DFHEIBLK
                MOVE 32500 TO EIBCALEN OF DFHEIBLK
                MOVE ZUT00000002 TO EIBAID OF DFHEIBLK
                MOVE ZUT00000003 TO EIBFN-AZ OF DFHEIBLK
                MOVE ZUT00000004 TO EIBRSRCE OF DFHEIBLK
                MOVE 0 TO EIBRESP OF DFHEIBLK
                MOVE 0 TO EIBRESP2 OF DFHEIBLK
                MOVE ZUT00000005 TO CA-REQUEST-ID OF DFHCOMMAREA
                MOVE 2 TO CA-CUSTOMER-NUM OF DFHCOMMAREA
                MOVE 1 TO CA-POLICY-NUM OF CA-POLICY-REQUEST OF
           DFHCOMMAREA
                MOVE TEST-CASE-PTR-VALUE TO AZ-TEST-CASE-PTR-VALUE
           END-EVALUATE
           GOBACK
           .
        ALLOCATE-PARM.
      *    allocate an instance of parameter structure
           INITIALIZE CEEGTST-HEAP CEEGTST-SIZE
      *    get a parameter size
           INITIALIZE PARM-SIZE
      *    LENGTH OF DFHCOMMAREA: 32500
           IF 32500 > PARM-SIZE
             MOVE 32500 TO PARM-SIZE
           END-IF
           ADD PARM-SIZE TO CEEGTST-SIZE
      *    get heap storage
           CALL 'CEEGTST' USING CEEGTST-HEAP CEEGTST-SIZE
                TEST-FIXTURE-PTR OMITTED
           SET NEXT-TEST-FIXTURE-PTR TO TEST-FIXTURE-PTR
      *    set address of a parameter
           SET ADDRESS OF DFHCOMMAREA TO NEXT-TEST-FIXTURE-PTR
           EXIT.
        INITIALIZE-PARM.
           INITIALIZE DFHEIBLK
           INITIALIZE DFHCOMMAREA
           EXIT.
       END PROGRAM 'SETUP'.
      *+---------------------------------------------------------------+
      *| TEARDOWN                                                      |
      *|     This program is invoked by the zUnit Test Runner          |
      *|     after each Test to allow for releasing resources          |
      *|     (e.g., memory, connection) which were allocated           |
      *|     during creation of the Test Fixture in the SETUP          |
      *|     program.                                                  |
      *|                                                               |
      *| @param TEST-CASE-PTR (input),                                 |
      *|     A pointer-by-value to an area maintained by the           |
      *|     zUnit Test Runner that identifies the Test Case           |
      *|     and associated resources.                                 |
      *|                                                               |
      *| @param TEST-FIXTURE-PTR (input),                              |
      *|     A pointer-by-value to a user-defined structure,           |
      *|     established previously in the SETUP program, that         |
      *|     represents the Test Fixture.                              |
      *|                                                               |
      *| @param TEST-NAME-PTR (input),                                 |
      *|     A pointer-by-value to an area containing the name         |
      *|     of the Test for which a Test Fixture should be            |
      *|     allocated.                                                |
      *|                                                               |
      *| @param TEST-NAME-LEN (input),                                 |
      *|     A integer-by-value that specifies the length in           |
      *|     bytes of the value contained in parameter                 |
      *|     TEST-NAME-PTR.                                            |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEARDOWN'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 FAIL-MESSAGE-TXT PIC X(254).
       1 FAIL-MESSAGE-LEN PIC S9(9) COMP-5.
       LINKAGE SECTION.
       1 TEST-CASE-PTR    POINTER.
       1 TEST-FIXTURE-PTR POINTER.
       1 TEST-NAME-PTR    POINTER.
       1 TEST-NAME-LEN    PIC S9(9) COMP-5.
       1 TEST-NAME        PIC X(254).
       PROCEDURE DIVISION USING BY VALUE TEST-CASE-PTR
                          BY VALUE TEST-FIXTURE-PTR
                          BY VALUE TEST-NAME-PTR
                          BY VALUE TEST-NAME-LEN.
           SET ADDRESS OF TEST-NAME TO TEST-NAME-PTR
           EVALUATE TEST-NAME(1:TEST-NAME-LEN)
              WHEN 'TEST2'
      *         free test fixture for 'TEST2'
                CALL 'CEEFRST' USING TEST-FIXTURE-PTR OMITTED
                DISPLAY 'TEARDOWN (' TEST-NAME(1:TEST-NAME-LEN) ')'
           END-EVALUATE
           .
       END PROGRAM 'TEARDOWN'.
      *+---------------------------------------------------------------+
      *| TEST2                                                         |
      *|     A Test (supply more detail).                              |
      *|                                                               |
      *| @param TEST-CASE-PTR (input),                                 |
      *|     A pointer-by-value to an area maintained by the           |
      *|     zUnit Test Runner that identifies the Test Case           |
      *|     and associated resources.                                 |
      *|                                                               |
      *| @param TEST-FIXTURE-PTR (input),                              |
      *|     A pointer-by-value to a user-defined structure,           |
      *|     established previously in the SETUP program, that         |
      *|     represents the Test Fixture.                              |
      *|                                                               |
      *| @param TEST-NAME-PTR (input),                                 |
      *|     A pointer-by-value to an area containing the name         |
      *|     of the Test for which a Test Fixture should be            |
      *|     allocated.                                                |
      *|                                                               |
      *| @param TEST-NAME-LEN (input),                                 |
      *|     A integer-by-value that specifies the length in           |
      *|     bytes of the value contained in parameter                 |
      *|     TEST-NAME-PTR.                                            |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST2'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 FAIL-MESSAGE-TXT PIC X(254).
       1 FAIL-MESSAGE-LEN PIC S9(9) COMP-5.
       1 PARM-SIZE        PIC S9(9) COMP-5.
       1 AZ-COMPARE EXTERNAL.
         3 AZ-COMPARE-ITEM-NAME-PTR POINTER.
         3 AZ-COMPARE-ITEM-NAME-LEN PIC S9(9) COMP-5.
         3 AZ-COMPARE-ITEM-VALUE-PTR POINTER.
         3 AZ-COMPARE-ITEM-VALUE-LEN PIC S9(9) COMP-5.
         3 AZ-COMPARE-ITEM-EXP-VALUE-PTR POINTER.
         3 AZ-COMPARE-ITEM-EXP-VALUE-LEN PIC S9(9) COMP-5.
       1 DFHEIBLK IS EXTERNAL.
         2 EIBTIME PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBDATE PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBTRNID PICTURE X(4).
         2 EIBTASKN PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBTRMID PICTURE X(4).
         2 DFHEIGDI PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBCPOSN PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBCALEN PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBAID PICTURE X(1).
         2 EIBFN PICTURE X(2).
         2 EIBRCODE PICTURE X(6).
         2 EIBDS PICTURE X(8).
         2 EIBREQID PICTURE X(8).
         2 EIBRSRCE PICTURE X(8).
         2 EIBSYNC PICTURE X.
         2 EIBFREE PICTURE X.
         2 EIBRECV PICTURE X.
         2 EIBSEND PICTURE X.
         2 EIBATT PICTURE X.
         2 EIBEOC PICTURE X.
         2 EIBFMH PICTURE X.
         2 EIBCOMPL PICTURE X(1).
         2 EIBSIG PICTURE X(1).
         2 EIBCONF PICTURE X(1).
         2 EIBERR PICTURE X(1).
         2 EIBERRCD PICTURE X(4).
         2 EIBSYNRB PICTURE X.
         2 EIBNODAT PICTURE X.
         2 EIBRESP PICTURE S9(8) USAGE COMPUTATIONAL.
         2 EIBRESP2 PICTURE S9(8) USAGE COMPUTATIONAL.
         2 EIBRLDBK PICTURE X(1).
       LOCAL-STORAGE SECTION.
       1 AZ-CONVERT.
         3 AZ-CONVERT-HEXIN  PIC X(1).
         3 AZ-CONVERT-HEXVAL PIC X(2).
         3 AZ-HEXSTR PIC X(16) VALUE "0123456789ABCDEF".
         3 AZ-DEC  PIC S9(4) COMP VALUE 0.
         3 FILLER REDEFINES AZ-DEC.
           5 FILLER PIC X.
           5 AZ-DECBYTE PIC X.
         3 AZ-I PIC S9(8) COMP.
         3 AZ-J PIC S9(8) COMP.
         3 AZ-Q PIC S9(8) COMP.
         3 AZ-R PIC S9(8) COMP.
         3 AZ-Q1 PIC S9(8) COMP.
         3 AZ-R1 PIC S9(8) COMP.
       LINKAGE SECTION.
       1 DFHCOMMAREA.
         3 CA-REQUEST-ID PIC X(6).
         3 CA-RETURN-CODE PIC 9(2).
         3 CA-CUSTOMER-NUM PIC 9(10).
         3 CA-REQUEST-SPECIFIC PIC X(32482).
         3 CA-CUSTOMER-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-FIRST-NAME PIC X(10).
         5 CA-LAST-NAME PIC X(20).
         5 CA-DOB PIC X(10).
         5 CA-HOUSE-NAME PIC X(20).
         5 CA-HOUSE-NUM PIC X(4).
         5 CA-POSTCODE PIC X(8).
         5 CA-NUM-POLICIES PIC 9(3).
         5 CA-PHONE-MOBILE PIC X(20).
         5 CA-PHONE-HOME PIC X(20).
         5 CA-EMAIL-ADDRESS PIC X(100).
         5 CA-POLICY-DATA PIC X(32267).
         3 CA-CUSTSECR-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-CUSTSECR-PASS PIC X(32).
         5 CA-CUSTSECR-COUNT PIC X(4).
         5 CA-CUSTSECR-STATE PIC X.
         5 CA-CUSTSECR-DATA PIC X(32445).
         3 CA-POLICY-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-POLICY-NUM PIC 9(10).
         5 CA-POLICY-COMMON.
         7 CA-ISSUE-DATE PIC X(10).
         7 CA-EXPIRY-DATE PIC X(10).
         7 CA-LASTCHANGED PIC X(26).
         7 CA-BROKERID PIC 9(10).
         7 CA-BROKERSREF PIC X(10).
         7 CA-PAYMENT PIC 9(6).
         5 CA-POLICY-SPECIFIC PIC X(32400).
         5 CA-ENDOWMENT REDEFINES CA-POLICY-SPECIFIC.
         7 CA-E-WITH-PROFITS PIC X.
         7 CA-E-EQUITIES PIC X.
         7 CA-E-MANAGED-FUND PIC X.
         7 CA-E-FUND-NAME PIC X(10).
         7 CA-E-TERM PIC 99.
         7 CA-E-SUM-ASSURED PIC 9(6).
         7 CA-E-LIFE-ASSURED PIC X(31).
         7 CA-E-PADDING-DATA PIC X(32348).
         5 CA-HOUSE REDEFINES CA-POLICY-SPECIFIC.
         7 CA-H-PROPERTY-TYPE PIC X(15).
         7 CA-H-BEDROOMS PIC 9(3).
         7 CA-H-VALUE PIC 9(8).
         7 CA-H-HOUSE-NAME PIC X(20).
         7 CA-H-HOUSE-NUMBER PIC X(4).
         7 CA-H-POSTCODE PIC X(8).
         7 CA-H-FILLER PIC X(32342).
         5 CA-MOTOR REDEFINES CA-POLICY-SPECIFIC.
         7 CA-M-MAKE PIC X(15).
         7 CA-M-MODEL PIC X(15).
         7 CA-M-VALUE PIC 9(6).
         7 CA-M-REGNUMBER PIC X(7).
         7 CA-M-COLOUR PIC X(8).
         7 CA-M-CC PIC 9(4).
         7 CA-M-MANUFACTURED PIC X(10).
         7 CA-M-PREMIUM PIC 9(6).
         7 CA-M-ACCIDENTS PIC 9(6).
         7 CA-M-FILLER PIC X(32323).
         5 CA-COMMERCIAL REDEFINES CA-POLICY-SPECIFIC.
         7 CA-B-ADDRESS PIC X(255).
         7 CA-B-POSTCODE PIC X(8).
         7 CA-B-LATITUDE PIC X(11).
         7 CA-B-LONGITUDE PIC X(11).
         7 CA-B-CUSTOMER PIC X(255).
         7 CA-B-PROPTYPE PIC X(255).
         7 CA-B-FIREPERIL PIC 9(4).
         7 CA-B-FIREPREMIUM PIC 9(8).
         7 CA-B-CRIMEPERIL PIC 9(4).
         7 CA-B-CRIMEPREMIUM PIC 9(8).
         7 CA-B-FLOODPERIL PIC 9(4).
         7 CA-B-FLOODPREMIUM PIC 9(8).
         7 CA-B-WEATHERPERIL PIC 9(4).
         7 CA-B-WEATHERPREMIUM PIC 9(8).
         7 CA-B-STATUS PIC 9(4).
         7 CA-B-REJECTREASON PIC X(255).
         7 CA-B-FILLER PIC X(31298).
         5 CA-CLAIM REDEFINES CA-POLICY-SPECIFIC.
         7 CA-C-NUM PIC 9(10).
         7 CA-C-DATE PIC X(10).
         7 CA-C-PAID PIC 9(8).
         7 CA-C-VALUE PIC 9(8).
         7 CA-C-CAUSE PIC X(255).
         7 CA-C-OBSERVATIONS PIC X(255).
         7 CA-C-FILLER PIC X(31854).
       1 TEST-CASE-PTR    POINTER.
       1 TEST-FIXTURE-PTR POINTER.
       1 TEST-FIXTURE-PTR-VALUE REDEFINES
           TEST-FIXTURE-PTR  PIC S9(9) COMP-5.
       1 TEST-NAME-PTR    POINTER.
       1 TEST-NAME-LEN    PIC S9(9) COMP-5.
       1 TEST-NAME        PIC X(254).
       PROCEDURE DIVISION USING BY VALUE TEST-CASE-PTR
                          BY VALUE TEST-FIXTURE-PTR
                          BY VALUE TEST-NAME-PTR
                          BY VALUE TEST-NAME-LEN.
       MAIN SECTION.
           SET ADDRESS OF TEST-NAME TO TEST-NAME-PTR
      *    display test name on entry
           DISPLAY TEST-NAME(1:TEST-NAME-LEN) ' Started...'
      *    establish addressability to test fixture
      *    set address of a parameter
           SET ADDRESS OF DFHCOMMAREA TO TEST-FIXTURE-PTR
      *    call test program
           DISPLAY 'CALL LGIPOL01'
           CALL 'LGIPOL01'
           USING DFHEIBLK DFHCOMMAREA
           .
      *    display test name on exit
           DISPLAY TEST-NAME(1:TEST-NAME-LEN)
           ' Successful.'.
           GOBACK.
       CONVERT.
           MOVE AZ-CONVERT-HEXIN TO AZ-DECBYTE
           DIVIDE AZ-DEC BY 16 GIVING AZ-Q REMAINDER AZ-R
           COMPUTE AZ-Q1 = AZ-Q + 1
           COMPUTE AZ-R1 = AZ-R + 1
           MOVE AZ-HEXSTR(AZ-Q1:1) TO AZ-CONVERT-HEXVAL(1:1)
           MOVE AZ-HEXSTR(AZ-R1:1) TO AZ-CONVERT-HEXVAL(2:1)
           EXIT.
       THROW-ASSERTION.
      *    throw an assertion exception (ends test)
           CALL 'AZUASTFC' USING BY VALUE TEST-CASE-PTR
                   BY REFERENCE FAIL-MESSAGE-TXT
                   BY VALUE FAIL-MESSAGE-LEN
                   BY VALUE AZ-COMPARE-ITEM-NAME-PTR
                   BY VALUE AZ-COMPARE-ITEM-NAME-LEN
                   BY VALUE AZ-COMPARE-ITEM-VALUE-PTR
                   BY VALUE AZ-COMPARE-ITEM-VALUE-LEN
                   BY VALUE AZ-COMPARE-ITEM-EXP-VALUE-PTR
                   BY VALUE AZ-COMPARE-ITEM-EXP-VALUE-LEN
           EXIT.
       END PROGRAM 'TEST2'.