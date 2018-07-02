 IDENTIFICATION DIVISION.                                         09/29/00000100
 PROGRAM-ID.    EE305.                                            EE305 00000200
 AUTHOR.        GARY VANDERHOFF.                                     LV200000300
 DATE-WRITTEN.  MAY 2004.                                            CL200000400
 DATE-COMPILED.                                                      CL200000500
******************************************************************   CL200000600
*                                                                *   CL200000700
*  PROGRAM FUNCTION/TASK:                                        *   CL200000800
*                                                                *   CL200000900
*    DAILY MTV ELIGIBILITY EXTRACT                               *   CL200001000
*                                                                *   CL200001100
*    MATCH/MERGE ANY UNLOADED FILE SORTED IN PERSON(PID) SEQUENCE*   CL200001200
*    SEQUENCE TO THE EEODSMB2 FORMAT.                            *   CL200001300
*                                                                *   CL200001400
*    THIS PGM READS IN FILE OF ELIGIBLE MEMBERS CREATED          *   CL200001500
*    IN PGM EE300 AND RETRIEVES DETAIL FOR EACH RECORD.          *   CL200001600
*    PREVIOUSLY UNLOADED FROM MTV DB2 TABLES.                    *   CL200001700
*    THE PROGRAM EMULATES ALL PREVIOUS DB2 SQL CURSORS/SELECTS.  *   CL200001800
*                                                                *   CL200001900
*    THE PERSON AND MEMBER TABLES HAVE PREVIOUSLY BEEN           *   CL200002000
*    MATCHED TOGETHER IN PROGRAM EEZ301 AND THEN                 *   CL200002100
*    MERGED WITH THE OUTPUT FROM EE300 IN PROGRAM EEZ304.        *   CL200002200
*                                                                *   CL200002300
*    THE ADDRESS AND ADDR ASSOCIATION TABLES HAVE BEEN           *   CL200002400
*    MATCHED TOGETHER IN PROGRAM EEZ303.                         *   CL200002500
*                                                                *   CL200002600
*    THIS IS A COBOL III PROGRAM, COMPILE USING S390             *   CL200002700
*                                                                *   CL200002800
*  INPUTS:                                                       *   CL200002900
*                                                                *   CL200003000
*    MTVMBR      -> ALL MTV MEMBERSHIP ACTIVE WITHIN THE         *   CL200003100
*                   PAST 730 DAYS                                *   CL200003200
*                   (CREATED IN PGM EE300, UPDATED IN EEZ304)    *   CL200003300
*                   (EEODSMB1 TO EEODSMB2)                       *   CL200003400
*                                                                *   CL200003500
*    MTV TABLES  -> MEMBER, PERSON,                              *   CL200003600
*                   METAVANCE_ADR_ASSN,METAVANCE_ADDRESS,        *   CL200003700
*                   METAVANCE_PHONE,METAVANCE_ELEC_ADR           *   CL200003800
*                                                                *   CL200003900
*    EXTRACTED   -> SME012 - MEMBER  INCLUDED IN (EEODSMB2)      *   CL200004000
*                   SME039 - PERSON  INCLUDED IN (EEODSMB2)      *   CL200004100
*                   SRC030 - METAVANCE_ADDRESS   (EEODSADD)      *   CL200004200
*                   SRC031 - METAVANCE_ADR_ASSN  (EEODSADD)      *   CL200004300
*                   SRC033H- METAVANCE_PHONE - HOME              *   CL200004400
*                   SRC033W- METAVANCE_PHONE - WORK              *   CL200004500
*                   SRC033P- METAVANCE_PHONE - PRIMARY           *   CL200004600
*                   SRC034 - METAVANCE_ELEC_ADR                  *   CL200004700
*                                                                *   CL200004800
*             NOTE: ANY TABLE CHANGES NEED TO BE MADE TO EZT     *   CL200004900
*                   PROGRAMS EEZ301,EEZ303 AND EEZ304            *   CL200005000
*                   AND TO ALL DCLGEN NAMES REPLACING ALL COMP   *   CL200005100
*                   NULL INDICATORS WITH PIC X(01).              *   CL200005200
*                                                                *   CL200005300
*  OUTPUTS:                                                      *   CL200005400
*                                                                *   CL200005500
*    EEODSMB2    -> ALL MTV MEMBERSHIP ACTIVE WITHIN THE         *   CL200005600
*                   PAST 730 DAYS,                               *   CL200005700
*                   EXPANDED TO INCLUDE PERSONAL INFO            *   CL200005800
*                                                                *   CL200005900
*    MTVERR      -> ERROR RECORDS WHERE MEMBER IS NOT            *   CL200006000
*                   FOUND DURING TABLE LOOKUP PROCESS            *   CL200006100
*                                                                *   CL200006200
*    EEMBRCNT    -> RECORD COUNT FOR THE NUMBER OF RECORDS       *   CL200006300
*                   ON THE OUTPUT FILE 'EEODSMB2'                *   CL200006400
*                                                                *   CL200006500
*  JOB STREAM: JOB NAME  PROC NAME  WHEN RUN (D,WK,M/E,REQ)      *   CL200006600
*              --------  ---------  ----------------------       *   CL200006700
*                                                                *   CL200006800
*               EE0300    EEP307    DAILY                        *   CL200006900
*                                                                *   CL200007000
******************************************************************   CL200007100
*                                                                *   CL200007200
* HISTORY OF REVISION:                                           *   CL200007300
*                                                                *   CL200007400
* TURNOVER  NAME, PWR/PNR NUMBER(S)                              *   CL200007500
*   DATE    DESCRIPTION OF CHANGE(S)                             *   CL200007600
* --------  -----------------------------------------------------*   CL200007700
* 09/29/14  VIVEK SINGH (VXS0066)                                *   CL200007800
*           CA#4525890- INCREASED THE THRESHOLD LIMIT OF MEMBERS *   CL200007900
*           WITH BLANK ADDRESSES FROM 200 TO 500                 *   CL200008000
*           FIND  'VXS0066 09/29/14 CA#4525890'                  *   CL200008100
*                                                                *   CL200008200
* 07/27/14  VIVEK SINGH (VXS0066)                                *   CL200008300
*           CA#4380298- WRITING ERROR FILE WITH THE MEMBERS WITH *   CL200008400
*           BLANK ADDRESSES AND TO ABEND THE JOB IF MEMBER COUNT *   CL200008500
*           INCREASES THE THRESHOLD LIMIT OF 200                 *   CL200008600
*           FIND  'VXS0066 07/27/14 CA#4380298'                  *   CL200008700
*                                                                *   CL200008800
* 12/15/10  RUPESH THAKKAR(RXT0239)                              *   CL200008900
*           CO# 439981 - DATA WHICH IS NOT REQUIRED IN EE305 IS  *   CL200009000
*           COMMENTED.                                           *   CL200009100
*           FIND  'RXT0239 12/15/10 CO# 439981'                  *   CL200009200
*                                                                *   CL200009300
* 10/06/09  MANU CHOUBEY(MXC0156)                                *   CL200009400
*           CA#1326215 N-81624                                   *   CL200009500
*           CHANGES TO READ THE END RECORD OF THE FILE           *   CL200009600
*           EEODSADD                                             *   CL200009700
*           FIND 'MXC0156 10/06/09 CA# 1326215'                  *   CL200009800
*                                                                *   CL200009900
* 05/07/07  GAURAV JAJU (GXJ0129)                                *   CL200010000
*           BOP - MSU EE0307 REMEDY # 1617722                    *   CL200010100
*           CHANGES TO IMPROVE PERFORMANCE                       *   CL200010200
*           ADDED THE FUNCTIONALITY OF A_ELPERS TABLE            *   CL200010300
*           FIND 'GXJ0129 05/07/07 CHANGES'                      *   CL200010400
*                                                                *   CL200010500
*                                                                *   CL200010600
* 07/01/05  SYNTEL PWR # 64991                                   *   CL200010700
*           MTV REWRITE PROJECT                                  *   CL200010800
*           FIND '* SYNTEL CHANGES BEGIN   -  MTV'               *   CL200010900
*                                                                *   CL200011000
* 12/10/04  SYNTEL                                               *   CL200011100
*           CHANGES RELATED TO CHAIN OF TRUST PROJECT            *   CL200011200
*           FIND '*SYNTEL START CHAIN OF TRUST'                  *   CL200011300
*                                                                *   CL200011400
* 02/24/05  SAT5320  RLS 5.0                                     *   CL200011500
*           CREATE A NEW OUTPUT FILE TO WRITE MBR-OUTPUT-CNT     *   CL200011600
*                                                                *   CL200011700
* 05/26/04  GARY VANDERHOFF    PWR 54158                         *   CL200011800
*           THIS PROGRAM HAS BEEN RE-WRITTEN.                    *   CL200011900
*           ALL OF THE DB2 TABLES HAVE BEEN REMOVED              *   CL200012000
*           AND REPLACED WITH AN UNLOADED TABLE FILES            *   CL200012100
*           FOR RUN TIME PURPOSES.                               *   CL200012200
*           THE PERSON AND MEMBER TABLES HAVE PREVIOUSLY BEEN    *   CL200012300
*           MATCHED AND THEN MERGED WITH THE OUTPUT FROM EE300.  *   CL200012400
*           THE ADDRESS AND ADDR ASSOCIATION TABLES HAVE BEEN    *   CL200012500
*           MATCHED TOGETHER IN PROGRAM EEZ303.                  *   CL200012600
*                                                                *   CL200012700
******************************************************************   CL200012800
*END DMGR                                                            CL200012900
                                                                     CL200013000
 ENVIRONMENT DIVISION.                                               CL200013100
 CONFIGURATION SECTION.                                              CL200013200
 INPUT-OUTPUT SECTION.                                               CL200013300
 FILE-CONTROL.                                                       CL200013400
     SELECT MTVMBR-FILE          ASSIGN TO MTVMBRIN.                 CL200013500
                                                                     CL200013600
     SELECT EEODSMB2-FILE        ASSIGN TO MTVPEOUT.                 CL200013700
                                                                     CL200013800
     SELECT MTVERR-FILE          ASSIGN TO MTVERROR.                 CL200013900
     SELECT EEODSADD-FILE        ASSIGN TO EEODSADD.                 CL200014000
     SELECT SRC033H-FILE         ASSIGN TO SRC033H.                  CL200014100
     SELECT SRC033W-FILE         ASSIGN TO SRC033W.                  CL200014200
     SELECT SRC033P-FILE         ASSIGN TO SRC033P.                  CL200014300
     SELECT SRC034-FILE          ASSIGN TO SRC034.                   CL200014400
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200014500
     SELECT AELPER-FILE          ASSIGN TO AELPER.                   CL200014600
*GXJ0129 05/07/07 CHANGES END                                        CL200014700
* SAT5320 RLS-5.0 CHANGES BEGIN                                      CL200014800
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200014900
*    SELECT MBRCNT-FILE          ASSIGN TO EEMBRCNT.                 CL200015000
*GXJ0129 05/07/07 CHANGES END                                        CL200015100
* SAT5320 RLS-5.0 CHANGES END                                        CL200015200
                                                                     CL200015300
*SYNTEL START CHAIN OF TRUST                                         CL200015400
     SELECT MTVCOT-IN1           ASSIGN TO MTVCOTI1.                 CL200015500
     SELECT MTVCOT-IN2           ASSIGN TO MTVCOTI2.                 CL200015600
     SELECT MTVCOT-OUT           ASSIGN TO MTVCOTO.                  CL200015700
*SYNTEL END CHAIN OF TRUST                                           CL200015800
                                                                     CL200015900
 DATA DIVISION.                                                      CL200016000
 FILE  SECTION.                                                      CL200016100
                                                                     CL200016200
 FD  MTVMBR-FILE                                                     CL200016300
     LABEL RECORDS ARE STANDARD                                      CL200016400
     BLOCK CONTAINS 0 RECORDS                                        CL200016500
     RECORDING MODE IS F                                             CL200016600
     DATA RECORD IS MTVMBR-REC-IN.                                   CL200016700
                                                                     CL200016800
*01  MTVMBR-REC-IN               PIC X(3100).                        CL200016900
**** SYNTEL CHANGES BEGIN   -  MTV REWRITE PROJECT **********        CL200017000
*01  MTVMBR-REC-IN               PIC X(3300).                        CL200017100
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200017200
*01  MTVMBR-REC-IN               PIC X(1402).                        CL200017300
 01  MTVMBR-REC-IN               PIC X(1500).                        CL200017400
*GXJ0129 05/07/07 CHANGES END                                        CL200017500
**** SYNTEL CHANGES ENDS    -  MTV REWRITE PROJECT **********        CL200017600
                                                                     CL200017700
 FD  EEODSMB2-FILE                                                   CL200017800
     LABEL RECORDS ARE STANDARD                                      CL200017900
     BLOCK CONTAINS 0 RECORDS                                        CL200018000
     RECORDING MODE IS F                                             CL200018100
     DATA RECORD IS EEODSMB2-REC-OUT.                                CL200018200
                                                                     CL200018300
**** SYNTEL CHANGES BEGIN   -  MTV REWRITE PROJECT **********        CL200018400
*01  EEODSMB2-REC-OUT            PIC X(3100).                        CL200018500
*01  EEODSMB2-REC-OUT            PIC X(3300).                        CL200018600
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200018700
*01  EEODSMB2-REC-OUT            PIC X(1402).                        CL200018800
 01  EEODSMB2-REC-OUT            PIC X(1500).                        CL200018900
*GXJ0129 05/07/07 CHANGES END                                        CL200019000
**** SYNTEL CHANGES ENDS    -  MTV REWRITE PROJECT **********        CL200019100
                                                                     CL200019200
 FD  MTVERR-FILE                                                     CL200019300
     LABEL RECORDS ARE STANDARD                                      CL200019400
     BLOCK CONTAINS 0 RECORDS                                        CL200019500
     RECORDING MODE IS F                                             CL200019600
     DATA RECORD IS MTVERR-REC.                                      CL200019700
                                                                     CL200019800
*VXS0066 07/27/14 CA#4380298 BEG                                     CL200019900
*01  MTVERR-REC                  PIC X(750).                         CL200020000
 01  MTVERR-REC                  PIC X(1500).                        CL200020100
*VXS0066 07/27/14 CA#4380298 END                                     CL200020200
                                                                     CL200020300
* SAT5320 RLS-5.0 CHANGES BEGIN                                      CL200020400
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200020500
*                                                                    CL200020600
*FD  MBRCNT-FILE                                                     CL200020700
*    LABEL RECORDS ARE STANDARD                                      CL200020800
*    BLOCK CONTAINS 0 RECORDS                                        CL200020900
*    RECORDING MODE IS F                                             CL200021000
*    DATA RECORD IS MBRCNT-REC.                                      CL200021100
*                                                                    CL200021200
*01  MBRCNT-REC                  PIC X(80).                          CL200021300
*                                                                    CL200021400
*GXJ0129 05/07/07 CHANGES END                                        CL200021500
* SAT5320 RLS-5.0 CHANGES END                                        CL200021600
                                                                     CL200021700
 FD  EEODSADD-FILE                                                   CL200021800
     RECORDING MODE IS F.                                            CL200021900
 01  EEODSADD-REC.                                                   CL200022000
     05 EEODSADD-KEY.                                                CL200022100
        10 EEODSADD-WHO-IDENTIFIER  PIC X(16).                       CL200022200
        10 EEODSADD-PRIMARY-FLAG    PIC X(01).                       CL200022300
        10 EEODSADD-END-DATE        PIC X(10).                       CL200022400
*RXT0239 12/15/10 CO# 439981  START                                  CL200022500
*       10 EEODSADD-LAST-MAINT-TS   PIC X(26).                       CL200022600
*RXT0239 12/15/10 CO# 439981  END                                    CL200022700
        10 EEODSADD-EFFECTIVE-DATE  PIC X(10).                       CL200022800
     05 FILLER                      PIC X(395).                      CL200022900
                                                                     CL200023000
 FD  SRC033H-FILE                                                    CL200023100
     RECORDING MODE IS F.                                            CL200023200
 01  SRC033H-REC                    PIC X(166).                      CL200023300
                                                                     CL200023400
 FD  SRC033W-FILE                                                    CL200023500
     RECORDING MODE IS F.                                            CL200023600
 01  SRC033W-REC                    PIC X(166).                      CL200023700
                                                                     CL200023800
 FD  SRC033P-FILE                                                    CL200023900
     RECORDING MODE IS F.                                            CL200024000
 01  SRC033P-REC                    PIC X(166).                      CL200024100
                                                                     CL200024200
 FD  SRC034-FILE                                                     CL200024300
     RECORDING MODE IS F.                                            CL200024400
 01  SRC034-REC                     PIC X(195).                      CL200024500
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200024600
 FD  AELPER-FILE                                                     CL200024700
     RECORDING MODE IS F.                                            CL200024800
 01  AELPER-REC                     PIC X(131).                      CL200024900
*NEED TO CHANGE THE REC LENGTH WHEN DCLS CHANGES                     CL200025000
*GXJ0129 05/07/07 CHANGES END                                        CL200025100
                                                                     CL200025200
*SYNTEL START CHAIN OF TRUST                                         CL200025300
 FD  MTVCOT-IN1                                                      CL200025400
     LABEL RECORDS ARE STANDARD                                      CL200025500
     BLOCK CONTAINS 0 RECORDS                                        CL200025600
     RECORDING MODE IS F                                             CL200025700
     DATA RECORD IS MTVCOT-IN1-REC.                                  CL200025800
                                                                     CL200025900
 01  MTVCOT-IN1-REC.                                                 CL200026000
     05 MTVCOTI1-PROGRAM-NAME        PIC X(08).                      CL200026100
     05 MTVCOTI1-PROCESS-DATE        PIC X(10).                      CL200026200
     05 MTVCOTI1-INPUT-CNT           PIC 9(09).                      CL200026300
     05 MTVCOTI1-ERROR-CNT           PIC 9(09).                      CL200026400
     05 MTVCOTI1-OUTPUT-CNT          PIC 9(09).                      CL200026500
     05 FILLER                       PIC X(55).                      CL200026600
                                                                     CL200026700
 FD  MTVCOT-IN2                                                      CL200026800
     LABEL RECORDS ARE STANDARD                                      CL200026900
     BLOCK CONTAINS 0 RECORDS                                        CL200027000
     RECORDING MODE IS F                                             CL200027100
     DATA RECORD IS MTVCOT-IN2-REC.                                  CL200027200
                                                                     CL200027300
 01  MTVCOT-IN2-REC.                                                 CL200027400
     05 MTVCOTI2-PROGRAM-NAME        PIC X(08).                      CL200027500
     05 MTVCOTI2-PROCESS-DATE        PIC X(10).                      CL200027600
     05 MTVCOTI2-INPUT-CNT           PIC 9(09).                      CL200027700
     05 MTVCOTI2-ERROR-CNT           PIC 9(09).                      CL200027800
     05 MTVCOTI2-OUTPUT-CNT          PIC 9(09).                      CL200027900
     05 FILLER                       PIC X(55).                      CL200028000
                                                                     CL200028100
 FD  MTVCOT-OUT                                                      CL200028200
     LABEL RECORDS ARE STANDARD                                      CL200028300
     BLOCK CONTAINS 0 RECORDS                                        CL200028400
     RECORDING MODE IS F                                             CL200028500
     DATA RECORD IS MTVCOT-OUT-REC.                                  CL200028600
                                                                     CL200028700
 01  MTVCOT-OUT-REC                 PIC X(100).                      CL200028800
                                                                     CL200028900
*SYNTEL END CHAIN OF TRUST                                           CL200029000
                                                                     CL200029100
 EJECT                                                               CL200029200
 WORKING-STORAGE SECTION.                                            CL200029300
                                                                     CL200029400
 01  WS-BEGIN                       PIC X(44)                        CL200029500
     VALUE '*** WORKING STORAGE FOR EE305 STARTS HERE***'.           CL200029600
                                                                     CL200029700
 01  WS-SWITCHES.                                                    CL200029800
     05  END-OF-ELIG-SW             PIC X(1) VALUE 'N'.              CL200029900
         88  END-OF-ELIG                     VALUE 'Y'.              CL200030000
     05  WS-EEODSPER-REC-FOUND-SW   PIC X(1) VALUE 'N'.              CL200030100
         88  WS-EEODSPER-REC-FOUND           VALUE 'Y'.              CL200030200
         88  WS-EEODSPER-REC-NOT-FOUND       VALUE 'N'.              CL200030300
     05  WS-EEODSADD-REC-FOUND-SW   PIC X(1) VALUE 'N'.              CL200030400
         88  WS-EEODSADD-REC-FOUND           VALUE 'Y'.              CL200030500
         88  WS-EEODSADD-REC-NOT-FOUND       VALUE 'N'.              CL200030600
     05  WS-SRC033H-REC-FOUND-SW    PIC X(1) VALUE 'N'.              CL200030700
         88  WS-SRC033H-REC-FOUND            VALUE 'Y'.              CL200030800
         88  WS-SRC033H-REC-NOT-FOUND        VALUE 'N'.              CL200030900
     05  WS-SRC033W-REC-FOUND-SW    PIC X(1) VALUE 'N'.              CL200031000
         88  WS-SRC033W-REC-FOUND            VALUE 'Y'.              CL200031100
         88  WS-SRC033W-REC-NOT-FOUND        VALUE 'N'.              CL200031200
     05  WS-SRC033P-REC-FOUND-SW    PIC X(1) VALUE 'N'.              CL200031300
         88  WS-SRC033P-REC-FOUND            VALUE 'Y'.              CL200031400
         88  WS-SRC033P-REC-NOT-FOUND        VALUE 'N'.              CL200031500
     05  WS-SRC034-REC-FOUND-SW     PIC X(1) VALUE 'N'.              CL200031600
         88  WS-SRC034-REC-FOUND             VALUE 'Y'.              CL200031700
         88  WS-SRC034-REC-NOT-FOUND         VALUE 'N'.              CL200031800
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200031900
     05  WS-AELPER-REC-FOUND-SW     PIC X(1) VALUE 'N'.              CL200032000
         88  WS-AELPER-REC-FOUND             VALUE 'Y'.              CL200032100
         88  WS-AELPER-REC-NOT-FOUND         VALUE 'N'.              CL200032200
*GXJ0129 05/07/07 CHANGES END                                        CL200032300
     05  WS-EEODSADD-END-SW         PIC X(1) VALUE 'N'.              CL200032400
         88  WS-EEODSADD-END                 VALUE 'Y'.              CL200032500
         88  WS-EEODSADD-NOT-END             VALUE 'N'.              CL200032600
     05  WS-SRC033H-END-SW          PIC X(1) VALUE 'N'.              CL200032700
         88  WS-SRC033H-END                  VALUE 'Y'.              CL200032800
         88  WS-SRC033H-NOT-END              VALUE 'N'.              CL200032900
     05  WS-SRC033W-END-SW          PIC X(1) VALUE 'N'.              CL200033000
         88  WS-SRC033W-END                  VALUE 'Y'.              CL200033100
         88  WS-SRC033W-NOT-END              VALUE 'N'.              CL200033200
     05  WS-SRC033P-END-SW          PIC X(1) VALUE 'N'.              CL200033300
         88  WS-SRC033P-END                  VALUE 'Y'.              CL200033400
         88  WS-SRC033P-NOT-END              VALUE 'N'.              CL200033500
     05  WS-SRC034-END-SW           PIC X(1) VALUE 'N'.              CL200033600
         88  WS-SRC034-END                   VALUE 'Y'.              CL200033700
         88  WS-SRC034-NOT-END               VALUE 'N'.              CL200033800
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200033900
     05  WS-AELPER-END-SW           PIC X(1) VALUE 'N'.              CL200034000
         88  WS-AELPER-END                   VALUE 'Y'.              CL200034100
         88  WS-AELPER-NOT-END               VALUE 'N'.              CL200034200
*GXJ0129 05/07/07 CHANGES END                                        CL200034300
     05  WS-EEODSADD-EOF-SW         PIC X(1) VALUE 'N'.              CL200034400
         88  WS-EEODSADD-EOF                 VALUE 'Y'.              CL200034500
         88  WS-EEODSADD-NOT-EOF             VALUE 'N'.              CL200034600
     05  WS-SRC033H-EOF-SW          PIC X(1) VALUE 'N'.              CL200034700
         88  WS-SRC033H-EOF                  VALUE 'Y'.              CL200034800
         88  WS-SRC033H-NOT-EOF              VALUE 'N'.              CL200034900
     05  WS-SRC033W-EOF-SW          PIC X(1) VALUE 'N'.              CL200035000
         88  WS-SRC033W-EOF                  VALUE 'Y'.              CL200035100
         88  WS-SRC033W-NOT-EOF              VALUE 'N'.              CL200035200
     05  WS-SRC033P-EOF-SW          PIC X(1) VALUE 'N'.              CL200035300
         88  WS-SRC033P-EOF                  VALUE 'Y'.              CL200035400
         88  WS-SRC033P-NOT-EOF              VALUE 'N'.              CL200035500
     05  WS-SRC034-EOF-SW           PIC X(1) VALUE 'N'.              CL200035600
         88  WS-SRC034-EOF                   VALUE 'Y'.              CL200035700
         88  WS-SRC034-NOT-EOF               VALUE 'N'.              CL200035800
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200035900
     05  WS-AELPER-EOF-SW           PIC X(1) VALUE 'N'.              CL200036000
         88  WS-AELPER-EOF                   VALUE 'Y'.              CL200036100
         88  WS-AELPER-NOT-EOF               VALUE 'N'.              CL200036200
*GXJ0129 05/07/07 CHANGES END                                        CL200036300
     05  WS-MTVCOT1-EOF-SW          PIC X(1) VALUE 'N'.              CL200036400
         88  WS-MTVCOT1-EOF                  VALUE 'Y'.              CL200036500
         88  WS-MTVCOT1-NOT-EOF              VALUE 'N'.              CL200036600
     05  WS-MTVCOT2-EOF-SW          PIC X(1) VALUE 'N'.              CL200036700
         88  WS-MTVCOT2-EOF                  VALUE 'Y'.              CL200036800
         88  WS-MTVCOT2-NOT-EOF              VALUE 'N'.              CL200036900
                                                                     CL200037000
 01  WS-MISC-FIELDS.                                                 CL200037100
     05 WS-PARAGRAPH-NAME          PIC X(30)  VALUE SPACES.          CL200037200
*                                  BELOW IS LOW VALUE HEX X'00'      CL200037300
    05 WS-LOW-VALUES               PIC X(01)  VALUE ' '.             CL200037400
                                                                     CL200037500
 01  WS-CURRENT-DATE               PIC X(21).                        CL200037600
 01  WS-CURR-DATE.                                                   CL200037700
     05  WS-CURR-CCYY              PIC 9(4).                         CL200037800
     05  WS-CURR-DASH1             PIC X(1)  VALUE '-'.              CL200037900
     05  WS-CURR-MM                PIC 9(2).                         CL200038000
     05  WS-CURR-DASH2             PIC X(1)  VALUE '-'.              CL200038100
     05  WS-CURR-DD                PIC 9(2).                         CL200038200
 01  WS-TIME.                                                        CL200038300
     05  WS-TIME-HH                PIC 9(2).                         CL200038400
     05  WS-TIME-COLEN1            PIC X(1)  VALUE ':'.              CL200038500
     05  WS-TIME-MM                PIC 9(2).                         CL200038600
     05  WS-TIME-COLEN2            PIC X(1)  VALUE ':'.              CL200038700
     05  WS-TIME-SS                PIC 9(2).                         CL200038800
                                                                     CL200038900
 01  WS-MBR-EFF-DT                 PIC X(10).                        CL200039000
 01  WS-MBR-END-DT                 PIC X(10).                        CL200039100
 01  WS-PREV-PERSON-ID             PIC X(16) VALUE SPACES.           CL200039200
                                                                     CL200039300
 01  WS-COUNTS.                                                      CL200039400
     05  MBR-READ-CNT              PIC 9(9)  VALUE ZEROES.           CL200039500
     05  MBR-OUTPUT-CNT            PIC 9(9)  VALUE ZEROES.           CL200039600
     05  MBR-ERROR-CNT             PIC 9(9)  VALUE ZEROES.           CL200039700
     05  EEODSADD-CNT              PIC 9(9)  VALUE ZEROES.           CL200039800
     05  SRC033H-CNT               PIC 9(9)  VALUE ZEROES.           CL200039900
     05  SRC033W-CNT               PIC 9(9)  VALUE ZEROES.           CL200040000
     05  SRC033P-CNT               PIC 9(9)  VALUE ZEROES.           CL200040100
     05  SRC034-CNT                PIC 9(9)  VALUE ZEROES.           CL200040200
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200040300
     05  AELPER-CNT                PIC 9(9)  VALUE ZEROES.           CL200040400
*GXJ0129 05/07/07 CHANGES END                                        CL200040500
* SAT5320 RLS-5.0 CHANGES BEGIN                                      CL200040600
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200040700
                                                                     CL200040800
*01  EE305CNT-REC.                                                   CL200040900
*    05  PGM-NAME                  PIC X(6)  VALUE 'EE305 '.         CL200041000
*    05  MBR-TRANS-CNT             PIC 9(9)  VALUE ZEROES.           CL200041100
*    05  FILLER                    PIC X(65) VALUE SPACES.           CL200041200
                                                                     CL200041300
*GXJ0129 05/07/07 CHANGES END                                        CL200041400
* SAT5320 RLS-5.0 CHANGES END                                        CL200041500
                                                                     CL200041600
******LUDE EEODSMB1                                                  CL200041700
 ++INCLUDE EEODSMB2                                                  CL200041800
                                                                     CL200041900
 ++INCLUDE EEODSADD                                                  CL200042000
* RXT0239 12/15/10 CO# 439981  START                                 CL200042100
*01  WS-EEODSADD-PRIM              PIC X(777) VALUE SPACES.          CL200042200
*01  WS-EEODSADD-CURR              PIC X(777) VALUE SPACES.          CL200042300
*01  WS-EEODSADD-NORM              PIC X(777) VALUE SPACES.          CL200042400
 01  WS-EEODSADD-PRIM              PIC X(432) VALUE SPACES.          CL200042500
 01  WS-EEODSADD-CURR              PIC X(432) VALUE SPACES.          CL200042600
 01  WS-EEODSADD-NORM              PIC X(432) VALUE SPACES.          CL200042700
* RXT0239 12/15/10 CO# 439981  END                                   CL200042800
                                                                     CL200042900
 ++INCLUDE EESRC033                                                  CL200043000
 ++INCLUDE EESRC034                                                  CL200043100
 01  WS-SRC033H-REC                PIC X(166) VALUE SPACES.          CL200043200
 01  WS-SRC033W-REC                PIC X(166) VALUE SPACES.          CL200043300
 01  WS-SRC033P-REC                PIC X(166) VALUE SPACES.          CL200043400
                                                                     CL200043500
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200043600
 ++INCLUDE EEAELPER                                                  CL200043700
 01  WS-AELPER-REC                PIC X(131) VALUE SPACES.           CL200043800
*NEED TO CHANGE REC LENGTH WHEN DCLA LENGTH CHANGES                  CL200043900
*GXJ0129 05/07/07 CHANGES END                                        CL200044000
                                                                     CL200044100
*SYNTEL START CHAIN OF TRUST                                         CL200044200
 ++INCLUDE EEMTVCOT                                                  CL200044300
*SYNTEL END CHAIN OF TRUST                                           CL200044400
                                                                     CL200044500
 01  WS-END                        PIC X(43) VALUE                   CL200044600
     '*** WORKING STORAGE FOR EE305 ENDS HERE ***'.                  CL200044700
                                                                     CL200044800
 LINKAGE SECTION.                                                    CL200044900
 EJECT                                                               CL200045000
                                                                     CL200045100
*************************************************************        CL200045200
 PROCEDURE DIVISION.                                                 CL200045300
*************************************************************        CL200045400
                                                                     CL200045500
     PERFORM 1000-INITIALIZATION                                     CL200045600
        THRU 1000-INITIALIZATION-EXIT                                CL200045700
                                                                     CL200045800
     PERFORM 2000-GET-PERSON                                         CL200045900
        THRU 2000-GET-PERSON-EXIT                                    CL200046000
        UNTIL END-OF-ELIG                                            CL200046100
                                                                     CL200046200
     PERFORM 9000-TERMINATION                                        CL200046300
        THRU 9000-TERMINATION-EXIT                                   CL200046400
                                                                     CL200046500
     GOBACK.                                                         CL200046600
                                                                     CL200046700
                                                                     CL200046800
*****************************************************                CL200046900
*                                                   *                CL200047000
*  INITIALIZATION PROCESS                           *                CL200047100
*                                                   *                CL200047200
*****************************************************                CL200047300
                                                                     CL200047400
 1000-INITIALIZATION.                                                CL200047500
                                                                     CL200047600
     DISPLAY '******* START EE305 *******'                           CL200047700
                                                                     CL200047800
     MOVE FUNCTION CURRENT-DATE    TO WS-CURRENT-DATE                CL200047900
     MOVE WS-CURRENT-DATE(1:4)     TO WS-CURR-CCYY                   CL200048000
     MOVE WS-CURRENT-DATE(5:2)     TO WS-CURR-MM                     CL200048100
     MOVE WS-CURRENT-DATE(7:2)     TO WS-CURR-DD                     CL200048200
     MOVE WS-CURRENT-DATE(9:2)     TO WS-TIME-HH                     CL200048300
     MOVE WS-CURRENT-DATE(11:2)    TO WS-TIME-MM                     CL200048400
     MOVE WS-CURRENT-DATE(13:2)    TO WS-TIME-SS                     CL200048500
     DISPLAY '*** CURRENT DATE: '     WS-CURR-DATE ' ' WS-TIME       CL200048600
                                                                     CL200048700
     OPEN INPUT MTVMBR-FILE                                          CL200048800
         OUTPUT EEODSMB2-FILE                                        CL200048900
         OUTPUT MTVERR-FILE                                          CL200049000
* SAT5320 RLS 5.0 BEGIN                                              CL200049100
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200049200
*        OUTPUT MBRCNT-FILE                                          CL200049300
*GXJ0129 05/07/07 CHANGES END                                        CL200049400
* SAT5320 RLS 5.0 END                                                CL200049500
                                                                     CL200049600
     OPEN INPUT EEODSADD-FILE.                                       CL200049700
     OPEN INPUT SRC033H-FILE.                                        CL200049800
     OPEN INPUT SRC033W-FILE.                                        CL200049900
     OPEN INPUT SRC033P-FILE.                                        CL200050000
     OPEN INPUT SRC034-FILE.                                         CL200050100
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200050200
     OPEN INPUT AELPER-FILE.                                         CL200050300
*GXJ0129 05/07/07 CHANGES END                                        CL200050400
                                                                     CL200050500
     PERFORM 1500-READ-ELIG-INPUT                                    CL200050600
        THRU 1500-READ-ELIG-INPUT-EXIT.                              CL200050700
                                                                     CL200050800
     READ EEODSADD-FILE                                              CL200050900
       AT END                                                        CL200051000
          SET WS-EEODSADD-EOF      TO TRUE                           CL200051100
      NOT AT END                                                     CL200051200
        ADD 1                      TO EEODSADD-CNT                   CL200051300
     END-READ                                                        CL200051400
                                                                     CL200051500
     READ SRC033H-FILE           INTO DCLA-METAVANCE-PHONE           CL200051600
       AT END                                                        CL200051700
          SET WS-SRC033H-EOF       TO TRUE                           CL200051800
      NOT AT END                                                     CL200051900
        ADD 1                      TO SRC033H-CNT                    CL200052000
        MOVE DCLA-METAVANCE-PHONE  TO WS-SRC033H-REC                 CL200052100
     END-READ                                                        CL200052200
                                                                     CL200052300
     READ SRC033W-FILE           INTO DCLA-METAVANCE-PHONE           CL200052400
       AT END                                                        CL200052500
          SET WS-SRC033W-EOF       TO TRUE                           CL200052600
      NOT AT END                                                     CL200052700
        ADD 1                      TO SRC033W-CNT                    CL200052800
        MOVE DCLA-METAVANCE-PHONE  TO WS-SRC033W-REC                 CL200052900
     END-READ                                                        CL200053000
                                                                     CL200053100
     READ SRC033P-FILE           INTO DCLA-METAVANCE-PHONE           CL200053200
       AT END                                                        CL200053300
          SET WS-SRC033P-EOF       TO TRUE                           CL200053400
      NOT AT END                                                     CL200053500
        ADD 1                      TO SRC033P-CNT                    CL200053600
        MOVE DCLA-METAVANCE-PHONE  TO WS-SRC033P-REC                 CL200053700
     END-READ                                                        CL200053800
                                                                     CL200053900
     READ SRC034-FILE            INTO DCLA-METAVANCE-ELEC-ADR        CL200054000
       AT END                                                        CL200054100
          SET WS-SRC034-EOF        TO TRUE                           CL200054200
      NOT AT END                                                     CL200054300
        ADD 1                      TO SRC034-CNT                     CL200054400
     END-READ                                                        CL200054500
                                                                     CL200054600
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200054700
     READ AELPER-FILE            INTO DCLA-AELPER                    CL200054800
       AT END                                                        CL200054900
          SET WS-AELPER-EOF        TO TRUE                           CL200055000
      NOT AT END                                                     CL200055100
        ADD 1                      TO AELPER-CNT                     CL200055200
        MOVE DCLA-AELPER           TO WS-AELPER-REC                  CL200055300
     END-READ                                                        CL200055400
*GXJ0129 05/07/07 CHANGES END                                        CL200055500
     .                                                               CL200055600
                                                                     CL200055700
 1000-INITIALIZATION-EXIT.                                           CL200055800
     EXIT.                                                           CL200055900
                                                                     CL200056000
******************************************************************   CL200056100
*        READ ELIGIBILITY FILE INPUT                             *   CL200056200
******************************************************************   CL200056300
                                                                     CL200056400
 1500-READ-ELIG-INPUT.                                               CL200056500
                                                                     CL200056600
     READ MTVMBR-FILE INTO MTV-MBR-CVG2                              CL200056700
       AT END                                                        CL200056800
         SET END-OF-ELIG TO TRUE                                     CL200056900
       NOT AT END                                                    CL200057000
         ADD 1 TO MBR-READ-CNT                                       CL200057100
     END-READ.                                                       CL200057200
                                                                     CL200057300
 1500-READ-ELIG-INPUT-EXIT.                                          CL200057400
     EXIT.                                                           CL200057500
                                                                     CL200057600
*************************************************************        CL200057700
*                                                           *        CL200057800
*        EXTRACT MEMBER INFORMATION                         *        CL200057900
*                                                           *        CL200058000
*  THIS PARAGRAPH USES CONTRACT-ID, MEMBER-ID TO SELECT     *        CL200058100
*  DETAIL FOR MEMBERS READ FROM THE INPUT FILE CREATED IN   *        CL200058200
*  PGM EE300 AND UPDATED WITH PERSON/MEMBER DATA IN EEZ304. *        CL200058300
*************************************************************        CL200058400
                                                                     CL200058500
 2000-GET-PERSON.                                                    CL200058600
                                                                     CL200058700
     MOVE MVELG-EFFECTIVE-DATE     OF MTV-MBR-CVG2                   CL200058800
                                   TO WS-MBR-EFF-DT                  CL200058900
     MOVE MVELG-END-DATE           OF MTV-MBR-CVG2                   CL200059000
                                   TO WS-MBR-END-DT                  CL200059100
                                                                     CL200059200
**** SYNTEL CHANGES BEGIN   -  MTV REWRITE PROJECT **********        CL200059300
*    INSPECT MVMBR-INFORMATION REPLACING ALL WS-LOW-VALUES BY ' '    CL200059400
**** SYNTEL CHANGES ENDS    -  MTV REWRITE PROJECT **********        CL200059500
                                                                     CL200059600
     PERFORM 2500-GET-ADDRESS                                        CL200059700
                                                                     CL200059800
     PERFORM 2600-PROCESS-PHONE-HOME                                 CL200059900
        THRU 2600-PROCESS-PHONE-HOME-EXIT.                           CL200060000
                                                                     CL200060100
     PERFORM 2640-PROCESS-PHONE-WORK                                 CL200060200
        THRU 2640-PROCESS-PHONE-WORK-EXIT.                           CL200060300
                                                                     CL200060400
     PERFORM 2650-PROCESS-PHONE-PRIM                                 CL200060500
        THRU 2650-PROCESS-PHONE-PRIM-EXIT.                           CL200060600
                                                                     CL200060700
     PERFORM 2700-GET-EMAIL                                          CL200060800
        THRU 2700-GET-EMAIL-EXIT.                                    CL200060900
                                                                     CL200061000
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200061100
     PERFORM 2750-PROCESS-AELPER                                     CL200061200
        THRU 2750-PROCESS-AELPER-EXIT.                               CL200061300
*GXJ0129 05/07/07 CHANGES END                                        CL200061400
                                                                     CL200061500
     PERFORM 3000-OUTPUT-EEODSMB2                                    CL200061600
        THRU 3000-OUTPUT-EEODSMB2-EXIT                               CL200061700
                                                                     CL200061800
     MOVE MVMBR-PERSON-ID          TO WS-PREV-PERSON-ID              CL200061900
                                                                     CL200062000
     PERFORM 1500-READ-ELIG-INPUT                                    CL200062100
        THRU 1500-READ-ELIG-INPUT-EXIT.                              CL200062200
                                                                     CL200062300
 2000-GET-PERSON-EXIT.                                               CL200062400
     EXIT.                                                           CL200062500
                                                                     CL200062600
                                                                     CL200062700
*****************************************************                CL200062800
*                                                   *                CL200062900
*  THIS PARAGRAPH HANDLES PROCESSING FOR RETRIEVING *                CL200063000
*  ADDRESS INFORMATION BASED ON MBR DATA            *                CL200063100
*  CURSOR PROCESS MUST BE USED BECAUSE OF DATE RANGE*                CL200063200
*  BUT WE WILL ONLY RETRIEVE 1 ROW - THE LATEST     *                CL200063300
*  APPROPRIATE ADDRESS.                             *                CL200063400
*  1ST LOOK FOR CURRENT PRIMARY; THEN LOOK FOR      *                CL200063500
*  CURRENT; THEN LOOK FOR ANY.                      *                CL200063600
*                                                   *                CL200063700
*  THIS PROCESS DOES THE SAME AS PREVIOUS DB2/SQL   *                CL200063800
*                                                   *                CL200063900
*****************************************************                CL200064000
                                                                     CL200064100
 2500-GET-ADDRESS.                                                   CL200064200
                                                                     CL200064300
     PERFORM 4020-GET-EEODSADD                                       CL200064400
                                                                     CL200064500
     IF WS-EEODSADD-PRIM            > SPACES                         CL200064600
        SET WS-EEODSADD-REC-FOUND  TO TRUE                           CL200064700
        MOVE WS-EEODSADD-PRIM      TO EEODSADD-ADDRESS               CL200064800
     ELSE                                                            CL200064900
     IF WS-EEODSADD-CURR            > SPACES                         CL200065000
        SET WS-EEODSADD-REC-FOUND  TO TRUE                           CL200065100
        MOVE WS-EEODSADD-CURR      TO EEODSADD-ADDRESS               CL200065200
     ELSE                                                            CL200065300
     IF WS-EEODSADD-NORM            > SPACES                         CL200065400
        SET WS-EEODSADD-REC-FOUND  TO TRUE                           CL200065500
        MOVE WS-EEODSADD-NORM      TO EEODSADD-ADDRESS               CL200065600
     END-IF                                                          CL200065700
     END-IF                                                          CL200065800
     END-IF                                                          CL200065900
                                                                     CL200066000
     IF WS-EEODSADD-REC-FOUND                                        CL200066100
        PERFORM 2510-MOVE-ADDRESS                                    CL200066200
     ELSE                                                            CL200066300
        INITIALIZE DCLA-METAVANCE-ADDRESS                            CL200066400
        INITIALIZE DCLA-METAVANCE-ADR-ASSN                           CL200066500
        INITIALIZE MTV-METAVANCE-ADDRESS                             CL200066600
        INITIALIZE MTV-METAVANCE-ADR-ASSN                            CL200066700
     END-IF                                                          CL200066800
     .                                                               CL200066900
 2500-GET-ADDRESS-EXIT.                                              CL200067000
     EXIT.                                                           CL200067100
                                                                     CL200067200
 2510-MOVE-ADDRESS.                                                  CL200067300
                                                                     CL200067400
*---MOVE ADR ASSN FIELDS                                             CL200067500
**** SYNTEL CHANGES BEGIN   -  MTV REWRITE PROJECT **********        CL200067600
*    MOVE WHO-TYPE-IND           OF DCLA-METAVANCE-ADR-ASSN          CL200067700
*          TO MVMAA-WHO-TYPE-IND                                     CL200067800
*    MOVE WHO-IDENTIFIER         OF DCLA-METAVANCE-ADR-ASSN          CL200067900
*          TO MVMAA-WHO-IDENTIFIER                                   CL200068000
*    IF ATTENTION-NAME-NI       NOT = '?'                            CL200068100
*    AND ATTENTION-NAME-LEN       > 0 AND < 56                       CL200068200
*       MOVE ATTENTION-NAME-TEXT OF DCLA-METAVANCE-ADR-ASSN          CL200068300
*           (1:ATTENTION-NAME-LEN)                                   CL200068400
*          TO MVMAA-ATTENTION-NAME                                   CL200068500
*    END-IF                                                          CL200068600
**** SYNTEL CHANGES ENDS    -  MTV REWRITE PROJECT **********        CL200068700
     MOVE EFFECTIVE-DATE         OF DCLA-METAVANCE-ADR-ASSN          CL200068800
           TO MVMAA-EFFECTIVE-DATE                                   CL200068900
**** SYNTEL CHANGES BEGIN   -  MTV REWRITE PROJECT **********        CL200069000
*    MOVE END-DATE               OF DCLA-METAVANCE-ADR-ASSN          CL200069100
*          TO MVMAA-END-DATE                                         CL200069200
*    IF MAIL-INDICATOR-NI       NOT = '?'                            CL200069300
*       MOVE MAIL-INDICATOR      OF DCLA-METAVANCE-ADR-ASSN          CL200069400
*          TO MVMAA-MAIL-INDICATOR                                   CL200069500
*    END-IF                                                          CL200069600
*    MOVE PRIMARY-FLAG           OF DCLA-METAVANCE-ADR-ASSN          CL200069700
*          TO MVMAA-PRIMARY-FLAG                                     CL200069800
*    MOVE VOID-FLAG              OF DCLA-METAVANCE-ADR-ASSN          CL200069900
*          TO MVMAA-VOID-FLAG                                        CL200070000
*    MOVE OPERATOR-ID            OF DCLA-METAVANCE-ADR-ASSN          CL200070100
*          TO MVMAA-OPERATOR-ID                                      CL200070200
*    MOVE LAST-MAINT-TS          OF DCLA-METAVANCE-ADR-ASSN          CL200070300
*          TO MVMAA-LAST-MAINT-TS                                    CL200070400
*    IF   IDENTIFIER            OF DCLA-METAVANCE-ADR-ASSN NUMERIC   CL200070500
*       MOVE IDENTIFIER          OF DCLA-METAVANCE-ADR-ASSN          CL200070600
*          TO MVMAA-IDENTIFIER                                       CL200070700
*    ELSE                                                            CL200070800
*       MOVE ZEROS                                                   CL200070900
*          TO MVMAA-IDENTIFIER                                       CL200071000
*    END-IF                                                          CL200071100
*    MOVE ADDRESS-TYPE           OF DCLA-METAVANCE-ADR-ASSN          CL200071200
*          TO MVMAA-ADDRESS-TYPE                                     CL200071300
*    MOVE ADDRESS-CODE           OF DCLA-METAVANCE-ADR-ASSN          CL200071400
*          TO MVMAA-ADDRESS-CODE                                     CL200071500
*    IF   ADDRESS-IDENTIFIER    OF DCLA-METAVANCE-ADR-ASSN NUMERIC   CL200071600
*       MOVE ADDRESS-IDENTIFIER  OF DCLA-METAVANCE-ADR-ASSN          CL200071700
*          TO MVMAA-ADDRESS-IDENTIFIER                               CL200071800
*    ELSE                                                            CL200071900
*       MOVE ADDRESS-IDENTIFIER  OF DCLA-METAVANCE-ADR-ASSN          CL200072000
*          MOVE ADDRESS-1-TEXT      OF DCLA-METAVANCE-ADDRESS        CL200072100
*    END    (1:ADDRESS-1-LEN)                                        CL200072200
*    IF       TO MVMAD-ADDRESS-1                                     CL200072300
*    ANDEND-IF                                                       CL200072400
*      MOVE OPERATING-HOURS-TEXT OF DCLA-METAVANCE-ADR-ASSN          CL200072500
*          (1:OPERATING-HOURS-LEN)                                   CL200072600
*          TO MVMAA-OPERATING-HOURS                                  CL200072700
*    END-IF                                                          CL200072800
*    MOVE VERSION-NUMBER         OF DCLA-METAVANCE-ADR-ASSN          CL200072900
*          TO MVMAA-VERSION-NUMBER                                   CL200073000
*    IF MVMAA-VERSION-NUMBER    NOT NUMERIC                          CL200073100
*       MOVE ZEROS                                                   CL200073200
*          TO MVMAA-VERSION-NUMBER                                   CL200073300
*    END-IF                                                          CL200073400
*    IF RECON-DATE-NI OF DCLA-METAVANCE-ADR-ASSN NOT = '?'           CL200073500
*       MOVE RECON-DATE          OF DCLA-METAVANCE-ADR-ASSN          CL200073600
*          TO MVMAA-RECON-DATE                                       CL200073700
*    END-IF                                                          CL200073800
*    MOVE AUDIT-FLAG             OF DCLA-METAVANCE-ADR-ASSN          CL200073900
*          TO MVMAA-AUDIT-FLAG                                       CL200074000
*    MOVE CREATION-TS            OF DCLA-METAVANCE-ADR-ASSN          CL200074100
*          TO MVMAA-CREATION-TS                                      CL200074200
**** SYNTEL CHANGES ENDS    -  MTV REWRITE PROJECT **********        CL200074300
                                                                     CL200074400
*---MOVE ADDRESS FIELDS                                              CL200074500
     IF UC-ADDRESS-1-LEN          > 0 AND < 56                       CL200074600
        MOVE UC-ADDRESS-1-TEXT  OF DCLA-METAVANCE-ADDRESS            CL200074700
          (1:UC-ADDRESS-1-LEN)                                       CL200074800
           TO MVMAD-UC-ADDRESS-1                                     CL200074900
     END-IF                                                          CL200075000
     IF ADDRESS-2-NI            NOT = '?'                            CL200075100
     AND ADDRESS-2-LEN            > 0 AND < 56                       CL200075200
        MOVE ADDRESS-2-TEXT      OF DCLA-METAVANCE-ADDRESS           CL200075300
            (1:ADDRESS-2-LEN)                                        CL200075400
           TO MVMAD-ADDRESS-2                                        CL200075500
     END-IF                                                          CL200075600
     IF UC-CITY-LEN               > 0 AND < 56                       CL200075700
        MOVE UC-CITY-TEXT        OF DCLA-METAVANCE-ADDRESS           CL200075800
          (1:UC-CITY-LEN)                                            CL200075900
           TO MVMAD-UC-CITY                                          CL200076000
     END-IF                                                          CL200076100
     MOVE STATE-CODE             OF DCLA-METAVANCE-ADDRESS           CL200076200
           TO MVMAD-STATE-CODE                                       CL200076300
     MOVE ZIP                    OF DCLA-METAVANCE-ADDRESS           CL200076400
           TO MVMAD-ZIP                                              CL200076500
     MOVE COUNTY-CODE            OF DCLA-METAVANCE-ADDRESS           CL200076600
           TO MVMAD-COUNTY-CODE                                      CL200076700
     MOVE COUNTRY-CODE           OF DCLA-METAVANCE-ADDRESS           CL200076800
           TO MVMAD-COUNTRY-CODE                                     CL200076900
**** SYNTEL CHANGES BEGIN   -  MTV REWRITE PROJECT **********        CL200077000
*    MOVE WHO-TYPE-IND           OF DCLA-METAVANCE-ADDRESS           CL200077100
*          TO MVMAD-WHO-TYPE-IND                                     CL200077200
*    MOVE WHO-IDENTIFIER         OF DCLA-METAVANCE-ADDRESS           CL200077300
*          TO MVMAD-WHO-IDENTIFIER                                   CL200077400
**** SYNTEL CHANGES ENDS    -  MTV REWRITE PROJECT **********        CL200077500
     IF ADDRESS-1-LEN             > 0 AND < 56                       CL200077600
        MOVE ADDRESS-1-TEXT      OF DCLA-METAVANCE-ADDRESS           CL200077700
         (1:ADDRESS-1-LEN)                                           CL200077800
           TO MVMAD-ADDRESS-1                                        CL200077900
     END-IF                                                          CL200078000
     IF ADDRESS-3-NI            NOT = '?'                            CL200078100
     AND ADDRESS-3-LEN            > 0 AND < 56                       CL200078200
        MOVE ADDRESS-3-TEXT      OF DCLA-METAVANCE-ADDRESS           CL200078300
            (1:ADDRESS-3-LEN)                                        CL200078400
           TO MVMAD-ADDRESS-3                                        CL200078500
     END-IF                                                          CL200078600
     IF CITY-LEN  > 0 AND < 56                                       CL200078700
        MOVE CITY-TEXT           OF DCLA-METAVANCE-ADDRESS           CL200078800
         (1:CITY-LEN)                                                CL200078900
           TO MVMAD-CITY                                             CL200079000
     END-IF                                                          CL200079100
**** SYNTEL CHANGES BEGIN   -  MTV REWRITE PROJECT **********        CL200079200
*    IF GEO-CODE-NI             NOT = '?'                            CL200079300
*    AND GEO-CODE-LEN             > 0 AND < 56                       CL200079400
*       MOVE GEO-CODE-TEXT       OF DCLA-METAVANCE-ADDRESS           CL200079500
*           (1:GEO-CODE-LEN)                                         CL200079600
*          TO MVMAD-GEO-CODE                                         CL200079700
*    END-IF                                                          CL200079800
*    MOVE EFF-PROCESS-DATE       OF DCLA-METAVANCE-ADDRESS           CL200079900
*          TO MVMAD-EFF-PROCESS-DATE                                 CL200080000
*    MOVE VOID-FLAG              OF DCLA-METAVANCE-ADDRESS           CL200080100
*          TO MVMAD-VOID-FLAG                                        CL200080200
*    MOVE OPERATOR-ID            OF DCLA-METAVANCE-ADDRESS           CL200080300
*          TO MVMAD-OPERATOR-ID                                      CL200080400
*    MOVE LAST-MAINT-TS          OF DCLA-METAVANCE-ADDRESS           CL200080500
*          TO MVMAD-LAST-MAINT-TS                                    CL200080600
*    IF   IDENTIFIER             OF DCLA-METAVANCE-ADDRESS NUMERIC   CL200080700
*       MOVE IDENTIFIER          OF DCLA-METAVANCE-ADDRESS           CL200080800
*          TO MVMAD-IDENTIFIER                                       CL200080900
*    ELSE                                                            CL200081000
*       MOVE ZEROS                                                   CL200081100
*          TO MVMAD-IDENTIFIER                                       CL200081200
*    END-IF                                                          CL200081300
*    MOVE VERSION-NUMBER         OF DCLA-METAVANCE-ADDRESS           CL200081400
*          TO MVMAD-VERSION-NUMBER                                   CL200081500
*    IF   MVMAD-VERSION-NUMBER  NOT NUMERIC                          CL200081600
*       MOVE ZEROS                                                   CL200081700
*          TO MVMAD-VERSION-NUMBER                                   CL200081800
*    END-IF                                                          CL200081900
*    MOVE AUDIT-FLAG             OF DCLA-METAVANCE-ADDRESS           CL200082000
*          TO MVMAD-AUDIT-FLAG                                       CL200082100
*    MOVE CREATION-TS            OF DCLA-METAVANCE-ADDRESS           CL200082200
*          TO MVMAD-CREATION-TS                                      CL200082300
*    IF RECON-DATE-NI OF DCLA-METAVANCE-ADDRESS NOT = '?'            CL200082400
*       MOVE RECON-DATE          OF DCLA-METAVANCE-ADDRESS           CL200082500
*          TO MVMAD-RECON-DATE                                       CL200082600
*    END-IF                                                          CL200082700
**** SYNTEL CHANGES ENDS    -  MTV REWRITE PROJECT **********        CL200082800
     .                                                               CL200082900
 2510-EXIT.                                                          CL200083000
     EXIT.                                                           CL200083100
                                                                     CL200083200
*****************************************************                CL200083300
*                                                   *                CL200083400
*  THIS HANDLES PROCESSING FOR RETRIEVING HOME PHONE*                CL200083500
*  FOR PERSON.  CURSOR PROCESSING REQUIRED BECAUSE  *                CL200083600
*  PERSON CAN HAVE MORE THAN 1 HOME PHONE. WE WILL  *                CL200083700
*  ONLY WRITE OUT 1 HOME PHONE- THE 1ST RETURNED.   *                CL200083800
*                                                   *                CL200083900
*  THIS PROCESS DOES THE SAME AS PREVIOUS DB2/SQL   *                CL200084000
*                                                   *                CL200084100
*****************************************************                CL200084200
                                                                     CL200084300
 2600-PROCESS-PHONE-HOME.                                            CL200084400
                                                                     CL200084500
     PERFORM 4030-GET-SRC033H                                        CL200084600
                                                                     CL200084700
     IF WS-SRC033H-REC-FOUND                                         CL200084800
        PERFORM 2610-MOVE-PHONE-HOME                                 CL200084900
     ELSE                                                            CL200085000
        INITIALIZE MTV-METAVANCE-HOME-PHONE                          CL200085100
     END-IF.                                                         CL200085200
                                                                     CL200085300
 2600-PROCESS-PHONE-HOME-EXIT.                                       CL200085400
     EXIT.                                                           CL200085500
                                                                     CL200085600
 2610-MOVE-PHONE-HOME.                                               CL200085700
*---MOVE FIELDS TO OUTPUT;DCLGEN FIELDS WILL BE USED AGAIN           CL200085800
     IF PHONE-NUMBER             OF DCLA-METAVANCE-PHONE NUMERIC     CL200085900
       MOVE PHONE-NUMBER         OF DCLA-METAVANCE-PHONE             CL200086000
        TO MVPHH-PHONE-NUMBER                                        CL200086100
     ELSE                                                            CL200086200
        MOVE ZEROS                                                   CL200086300
        TO MVPHH-PHONE-NUMBER                                        CL200086400
     END-IF                                                          CL200086500
**** SYNTEL CHANGES BEGIN   -  MTV REWRITE PROJECT **********        CL200086600
*    IF EXTENSION-NI          NOT = '?'                              CL200086700
*       MOVE EXTENSION           OF DCLA-METAVANCE-PHONE             CL200086800
*       TO MVPHH-EXTENSION                                           CL200086900
*    END-IF                                                          CL200087000
*    MOVE PHONE-TYPE             OF DCLA-METAVANCE-PHONE             CL200087100
*       TO MVPHH-PHONE-TYPE                                          CL200087200
*    MOVE PHONE-CODE             OF DCLA-METAVANCE-PHONE             CL200087300
*       TO MVPHH-PHONE-CODE                                          CL200087400
*    MOVE WHO-TYPE-INDICATOR     OF DCLA-METAVANCE-PHONE             CL200087500
*       TO MVPHH-WHO-TYPE-INDICATOR                                  CL200087600
*    MOVE WHO-IDENTIFIER         OF DCLA-METAVANCE-PHONE             CL200087700
*       TO MVPHH-WHO-IDENTIFIER                                      CL200087800
*    IF COUNTRY-ID-NI         NOT = '?'                              CL200087900
*       MOVE COUNTRY-ID          OF DCLA-METAVANCE-PHONE             CL200088000
*       TO MVPHH-COUNTRY-ID                                          CL200088100
*    END-IF                                                          CL200088200
*    IF PAGER-PIN-ID-NI       NOT = '?'                              CL200088300
*       MOVE PAGER-PIN-ID        OF DCLA-METAVANCE-PHONE             CL200088400
*       TO MVPHH-PAGER-PIN-ID                                        CL200088500
*    END-IF                                                          CL200088600
*    MOVE OPERATOR-ID            OF DCLA-METAVANCE-PHONE             CL200088700
*       TO MVPHH-OPERATOR-ID                                         CL200088800
*    MOVE LAST-MAINT-TS          OF DCLA-METAVANCE-PHONE             CL200088900
*       TO MVPHH-LAST-MAINT-TS                                       CL200089000
*    IF      IDENTIFIER          OF DCLA-METAVANCE-PHONE NUMERIC     CL200089100
*       MOVE IDENTIFIER          OF DCLA-METAVANCE-PHONE             CL200089200
*       TO MVPHH-IDENTIFIER                                          CL200089300
*    ELSE                                                            CL200089400
*       MOVE ZEROS                                                   CL200089500
*       TO MVPHH-IDENTIFIER                                          CL200089600
*    END-IF                                                          CL200089700
*    MOVE PRIMARY-FLAG           OF DCLA-METAVANCE-PHONE             CL200089800
*       TO MVPHH-PRIMARY-FLAG                                        CL200089900
*    MOVE EFFECTIVE-DATE         OF DCLA-METAVANCE-PHONE             CL200090000
*       TO MVPHH-EFFECTIVE-DATE                                      CL200090100
*    MOVE END-DATE               OF DCLA-METAVANCE-PHONE             CL200090200
*       TO MVPHH-END-DATE                                            CL200090300
*    IF RECON-DATE-NI            OF DCLA-METAVANCE-PHONE NOT = '?'   CL200090400
*       MOVE RECON-DATE          OF DCLA-METAVANCE-PHONE             CL200090500
*       TO MVPHH-RECON-DATE                                          CL200090600
*    END-IF                                                          CL200090700
*    MOVE AUDIT-FLAG             OF DCLA-METAVANCE-PHONE             CL200090800
*       TO MVPHH-AUDIT-FLAG                                          CL200090900
*    MOVE VERSION-NUMBER         OF DCLA-METAVANCE-PHONE             CL200091000
*       TO MVPHH-VERSION-NUMBER                                      CL200091100
*    MOVE CREATION-TS            OF DCLA-METAVANCE-PHONE             CL200091200
*       TO MVPHH-CREATION-TS                                         CL200091300
**** SYNTEL CHANGES ENDS    -  MTV REWRITE PROJECT **********        CL200091400
     .                                                               CL200091500
                                                                     CL200091600
 2600-EXIT.                                                          CL200091700
     EXIT.                                                           CL200091800
                                                                     CL200091900
                                                                     CL200092000
*****************************************************                CL200092100
*  THIS HANDLES PROCESSING FOR RETRIEVING WORK PHONE*                CL200092200
*  FOR PERSON.  CURSOR PROCESSING REQUIRED BECAUSE  *                CL200092300
*  PERSON CAN HAVE MORE THAN 1 WORK PHONE. WE WILL  *                CL200092400
*  ONLY WRITE OUT 1 WORK PHONE- THE 1ST RETURNED.   *                CL200092500
*                                                   *                CL200092600
*  THIS PROCESS DOES THE SAME AS PREVIOUS DB2/SQL   *                CL200092700
*                                                   *                CL200092800
*****************************************************                CL200092900
                                                                     CL200093000
 2640-PROCESS-PHONE-WORK.                                            CL200093100
                                                                     CL200093200
     PERFORM 4040-GET-SRC033W                                        CL200093300
                                                                     CL200093400
     IF WS-SRC033W-REC-FOUND                                         CL200093500
        PERFORM 2645-MOVE-PHONE-WORK                                 CL200093600
     ELSE                                                            CL200093700
        INITIALIZE MTV-METAVANCE-WORK-PHONE                          CL200093800
     END-IF                                                          CL200093900
     .                                                               CL200094000
 2640-PROCESS-PHONE-WORK-EXIT.                                       CL200094100
     EXIT.                                                           CL200094200
                                                                     CL200094300
 2645-MOVE-PHONE-WORK.                                               CL200094400
*---MOVE FIELDS TO OUTPUT;DCLGEN FIELDS WILL BE USED AGAIN           CL200094500
     IF PHONE-NUMBER             OF DCLA-METAVANCE-PHONE NUMERIC     CL200094600
       MOVE PHONE-NUMBER         OF DCLA-METAVANCE-PHONE             CL200094700
        TO MVPHW-PHONE-NUMBER                                        CL200094800
     ELSE                                                            CL200094900
        MOVE ZEROS                                                   CL200095000
        TO MVPHW-PHONE-NUMBER                                        CL200095100
     END-IF                                                          CL200095200
     IF EXTENSION-NI          NOT = '?'                              CL200095300
        MOVE EXTENSION           OF DCLA-METAVANCE-PHONE             CL200095400
        TO MVPHW-EXTENSION                                           CL200095500
     END-IF                                                          CL200095600
**** SYNTEL CHANGES BEGIN   -  MTV REWRITE PROJECT **********        CL200095700
*    MOVE PHONE-TYPE             OF DCLA-METAVANCE-PHONE             CL200095800
*       TO MVPHW-PHONE-TYPE                                          CL200095900
*    MOVE PHONE-CODE             OF DCLA-METAVANCE-PHONE             CL200096000
*       TO MVPHW-PHONE-CODE                                          CL200096100
                                                                     CL200096200
*    MOVE WHO-TYPE-INDICATOR     OF DCLA-METAVANCE-PHONE             CL200096300
*       TO MVPHW-WHO-TYPE-INDICATOR                                  CL200096400
*    MOVE WHO-IDENTIFIER         OF DCLA-METAVANCE-PHONE             CL200096500
*       TO MVPHW-WHO-IDENTIFIER                                      CL200096600
*    IF COUNTRY-ID-NI         NOT = '?'                              CL200096700
*       MOVE COUNTRY-ID          OF DCLA-METAVANCE-PHONE             CL200096800
*       TO MVPHW-COUNTRY-ID                                          CL200096900
*    END-IF                                                          CL200097000
*    IF PAGER-PIN-ID-NI       NOT = '?'                              CL200097100
*       MOVE PAGER-PIN-ID        OF DCLA-METAVANCE-PHONE             CL200097200
*       TO MVPHW-PAGER-PIN-ID                                        CL200097300
*    END-IF                                                          CL200097400
*    MOVE OPERATOR-ID            OF DCLA-METAVANCE-PHONE             CL200097500
*       TO MVPHW-OPERATOR-ID                                         CL200097600
*    MOVE LAST-MAINT-TS          OF DCLA-METAVANCE-PHONE             CL200097700
*       TO MVPHW-LAST-MAINT-TS                                       CL200097800
*    IF      IDENTIFIER          OF DCLA-METAVANCE-PHONE NUMERIC     CL200097900
*       MOVE IDENTIFIER          OF DCLA-METAVANCE-PHONE             CL200098000
*       TO MVPHW-IDENTIFIER                                          CL200098100
*    ELSE                                                            CL200098200
*       MOVE ZEROS                                                   CL200098300
*       TO MVPHW-IDENTIFIER                                          CL200098400
*    END-IF                                                          CL200098500
*    MOVE PRIMARY-FLAG           OF DCLA-METAVANCE-PHONE             CL200098600
*       TO MVPHW-PRIMARY-FLAG                                        CL200098700
*    MOVE EFFECTIVE-DATE         OF DCLA-METAVANCE-PHONE             CL200098800
*       TO MVPHW-EFFECTIVE-DATE                                      CL200098900
*    MOVE END-DATE               OF DCLA-METAVANCE-PHONE             CL200099000
*       TO MVPHW-END-DATE                                            CL200099100
*    IF RECON-DATE-NI            OF DCLA-METAVANCE-PHONE NOT = '?'   CL200099200
*       MOVE RECON-DATE          OF DCLA-METAVANCE-PHONE             CL200099300
*       TO MVPHW-RECON-DATE                                          CL200099400
*    END-IF                                                          CL200099500
*    MOVE AUDIT-FLAG             OF DCLA-METAVANCE-PHONE             CL200099600
*       TO MVPHW-AUDIT-FLAG                                          CL200099700
*    MOVE VERSION-NUMBER         OF DCLA-METAVANCE-PHONE             CL200099800
*       TO MVPHW-VERSION-NUMBER                                      CL200099900
*    MOVE CREATION-TS            OF DCLA-METAVANCE-PHONE             CL200100000
*       TO MVPHW-CREATION-TS                                         CL200100100
**** SYNTEL CHANGES ENDS    -  MTV REWRITE PROJECT **********        CL200100200
     .                                                               CL200100300
 2645-EXIT.                                                          CL200100400
     EXIT.                                                           CL200100500
                                                                     CL200100600
                                                                     CL200100700
 2650-PROCESS-PHONE-PRIM.                                            CL200100800
                                                                     CL200100900
     PERFORM 4050-GET-SRC033P                                        CL200101000
                                                                     CL200101100
     IF WS-SRC033P-REC-FOUND                                         CL200101200
        PERFORM 2655-MOVE-PHONE-PRIM                                 CL200101300
     ELSE                                                            CL200101400
        INITIALIZE MTV-METAVANCE-PHONE-PRIM                          CL200101500
     END-IF                                                          CL200101600
     .                                                               CL200101700
 2650-PROCESS-PHONE-PRIM-EXIT.                                       CL200101800
     EXIT.                                                           CL200101900
                                                                     CL200102000
 2655-MOVE-PHONE-PRIM.                                               CL200102100
*---MOVE PRIM PHONE FIELDS                                           CL200102200
                                                                     CL200102300
     IF PRIMARY-FLAG             OF DCLA-METAVANCE-PHONE = 'Y'       CL200102400
        IF PHONE-NUMBER          OF DCLA-METAVANCE-PHONE NUMERIC     CL200102500
           MOVE PHONE-NUMBER     OF DCLA-METAVANCE-PHONE             CL200102600
           TO MVMSU-PHONE                                            CL200102700
        ELSE                                                         CL200102800
           MOVE ZEROS            TO MVMSU-PHONE                      CL200102900
        END-IF                                                       CL200103000
        IF EXTENSION-NI       NOT = '?'                              CL200103100
           MOVE EXTENSION        OF DCLA-METAVANCE-PHONE             CL200103200
        TO MVMSU-EXTENSION                                           CL200103300
        END-IF                                                       CL200103400
        MOVE PHONE-TYPE          OF DCLA-METAVANCE-PHONE             CL200103500
        TO MVMSU-PHONE-TYPE                                          CL200103600
        MOVE PHONE-CODE          OF DCLA-METAVANCE-PHONE             CL200103700
        TO MVMSU-PHONE-CODE                                          CL200103800
     END-IF                                                          CL200103900
     .                                                               CL200104000
                                                                     CL200104100
 2655-EXIT.                                                          CL200104200
     EXIT.                                                           CL200104300
                                                                     CL200104400
                                                                     CL200104500
 2700-GET-EMAIL.                                                     CL200104600
                                                                     CL200104700
*************************************************************        CL200104800
*  NOTE:DATE CHECK NOT MEANINGFUL BECAUSE EFF DT = 01/01/0001        CL200104900
*  AND END DT = 12/31/9999 FOR ALL 'PS' TYPE                         CL200105000
*                                                   *                CL200105100
*  THIS PROCESS DOES THE SAME AS PREVIOUS DB2/SQL   *                CL200105200
*                                                   *                CL200105300
*************************************************************        CL200105400
                                                                     CL200105500
     PERFORM 4060-GET-SRC034                                         CL200105600
                                                                     CL200105700
     IF WS-SRC034-REC-FOUND                                          CL200105800
*---MOVE EMAIL FIELDS                                                CL200105900
        MOVE ELECTRONIC-ADDR-CD  OF DCLA-METAVANCE-ELEC-ADR          CL200106000
           TO MVMSU-ELECTRONIC-ADDR-CD                               CL200106100
        MOVE ELECT-ADDRESS       OF DCLA-METAVANCE-ELEC-ADR          CL200106200
           TO MVMSU-EMAIL                                            CL200106300
     END-IF                                                          CL200106400
     .                                                               CL200106500
                                                                     CL200106600
 2700-GET-EMAIL-EXIT.                                                CL200106700
     EXIT.                                                           CL200106800
                                                                     CL200106900
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200107000
 2750-PROCESS-AELPER.                                                CL200107100
                                                                     CL200107200
*************************************************************        CL200107300
*                                                           *        CL200107400
*  THIS PROCESS DOES THE SAME AS PREVIOUS DB2/SQL           *        CL200107500
*                                                           *        CL200107600
*************************************************************        CL200107700
                                                                     CL200107800
     PERFORM 4070-GET-AELPER                                         CL200107900
                                                                     CL200108000
     IF WS-AELPER-REC-FOUND                                          CL200108100
        PERFORM 2755-MOVE-AELPER                                     CL200108200
     ELSE                                                            CL200108300
        INITIALIZE MTV-TELPERS                                       CL200108400
        INITIALIZE MTV-PERSON-ADDTL-FIELDS                           CL200108500
     END-IF                                                          CL200108600
     .                                                               CL200108700
                                                                     CL200108800
 2750-PROCESS-AELPER-EXIT.                                           CL200108900
     EXIT.                                                           CL200109000
                                                                     CL200109100
 2755-MOVE-AELPER.                                                   CL200109200
*---MOVE AELPER FIELDS                                               CL200109300
                                                                     CL200109400
     MOVE ALT-PERS-ID         OF DCLA-AELPER                         CL200109500
     TO MVPRX-ALT-PERS-ID                                            CL200109600
                                                                     CL200109700
     IF EMPL-SIG-DATE-NI OF DCLA-AELPER NOT = '?'                    CL200109800
        MOVE EMPL-SIG-DATE OF DCLA-AELPER                            CL200109900
                      TO MVPRX-EMPL-SIG-DATE                         CL200110000
     ELSE                                                            CL200110100
        MOVE SPACES TO MVPRX-EMPL-SIG-DATE                           CL200110200
     END-IF                                                          CL200110300
                                                                     CL200110400
     MOVE PERS-KEY-TYPE-CD    OF DCLA-AELPER                         CL200110500
     TO MVPRX-PERS-KEY-TYPE-CD                                       CL200110600
                                                                     CL200110700
     MOVE LOGON-CREATE-IND    OF DCLA-AELPER                         CL200110800
     TO MVPRX-LOGON-CREATE-IND                                       CL200110900
                                                                     CL200111000
     IF ENCRYP-WS-OID-ID-LEN OF DCLA-AELPER <= 0                     CL200111100
        MOVE SPACES TO MVPRX-ENCRYP-WS-OID-ID                        CL200111200
     ELSE                                                            CL200111300
        MOVE ENCRYP-WS-OID-ID-TEXT OF DCLA-AELPER                    CL200111400
                              (1:ENCRYP-WS-OID-ID-LEN)               CL200111500
                              TO MVPRX-ENCRYP-WS-OID-ID              CL200111600
     END-IF                                                          CL200111700
                                                                     CL200111800
     MOVE ACTION-TIMESTAMP    OF DCLA-AELPER                         CL200111900
     TO MVPRX-ACTION-TIMESTAMP                                       CL200112000
                                                                     CL200112100
     MOVE DPND-STUD-IND       OF DCLA-AELPER                         CL200112200
     TO MVPRX-DPND-STUD-IND                                          CL200112300
                                                                     CL200112400
     IF ORIGINAL-EFF-DATE-NI OF DCLA-AELPER NOT = '?'                CL200112500
        MOVE ORIGINAL-EFF-DATE OF DCLA-AELPER                        CL200112600
                         TO MVPRX-ORIGINAL-EFF-DATE                  CL200112700
     ELSE                                                            CL200112800
        MOVE SPACES TO MVPRX-ORIGINAL-EFF-DATE                       CL200112900
     END-IF                                                          CL200113000
     .                                                               CL200113100
 2755-EXIT.                                                          CL200113200
     EXIT.                                                           CL200113300
*GXJ0129 05/07/07 CHANGES END                                        CL200113400
                                                                     CL200113500
                                                                     CL200113600
*****************************************************                CL200113700
*                                                   *                CL200113800
*  THIS PARAGRAPH WRITES THE MTV MEMBER DATA        *                CL200113900
*  TO THE EEODSMB2 OUTPUT FILE.                     *                CL200114000
*                                                   *                CL200114100
*****************************************************                CL200114200
                                                                     CL200114300
 3000-OUTPUT-EEODSMB2.                                               CL200114400
                                                                     CL200114500
     WRITE EEODSMB2-REC-OUT FROM MTV-MBR-CVG2                        CL200114600
                                                                     CL200114700
     ADD 1 TO MBR-OUTPUT-CNT.                                        CL200114800
                                                                     CL200114900
*VXS0066 07/27/14 CA#4380298 BEG                                     CL200115000
     IF MVMAD-UC-ADDRESS-1 = SPACES AND                              CL200115100
        MVMAD-ADDRESS-1 = SPACES AND                                 CL200115200
        MVMAD-ADDRESS-2 = SPACES AND                                 CL200115300
        MVMAD-ADDRESS-3 = SPACES                                     CL200115400
        PERFORM 3500-OUTPUT-MTVERR                                   CL200115500
        THRU 3500-OUTPUT-MTVERR-EXIT                                 CL200115600
     END-IF.                                                         CL200115700
*VXS0066 07/27/14 CA#4380298 END                                     CL200115800
 3000-OUTPUT-EEODSMB2-EXIT.                                          CL200115900
     EXIT.                                                           CL200116000
                                                                     CL200116100
                                                                     CL200116200
*****************************************************                CL200116300
*                                                   *                CL200116400
*  THIS PARAGRAPH WRITES THE MTV ERROR RECORDS TO   *                CL200116500
*  THE MTVERR OUTPUT FILE FOR CASES WHEN NO VALID   *                CL200116600
*  RECORD IS FOUND FOR THE MEMBER/PERSON LOOKUP.    *                CL200116700
*                                                   *                CL200116800
*****************************************************                CL200116900
                                                                     CL200117000
 3500-OUTPUT-MTVERR.                                                 CL200117100
                                                                     CL200117200
*VXS0066 07/27/14 CA#4380298 BEG                                     CL200117300
                                                                     CL200117400
     WRITE MTVERR-REC FROM MTV-MBR-CVG2                              CL200117500
                                                                     CL200117600
*    MOVE SPACES TO MTVERR-REC                                       CL200117700
*                                                                    CL200117800
*    MOVE MTV-MBR-CVG2 TO MTVERR-REC                                 CL200117900
*                                                                    CL200118000
*    WRITE MTVERR-REC                                                CL200118100
*                                                                    CL200118200
*VXS0066 07/27/14 CA#4380298 END                                     CL200118300
                                                                     CL200118400
     ADD 1 TO MBR-ERROR-CNT                                          CL200118500
     .                                                               CL200118600
 3500-OUTPUT-MTVERR-EXIT.                                            CL200118700
     EXIT.                                                           CL200118800
                                                                     CL200118900
                                                                     CL200119000
                                                                     CL200119100
*THIS PARA WILL PROCESS A GROUP OF ADDRESSES FOR THIS CONTRACT       CL200119200
*                                                                    CL200119300
*IT WILL GET ANY ADDRESSES AVAILABLE FOR THIS CONTRACT               CL200119400
*IT WILL ALWAYS USE THE PRIMARY IF PRESENT                           CL200119500
*IF NO PRIMARY IT WILL USE THE CURRENT IF PRESENT WITH DATE CHECKS   CL200119600
*OTHWISE, IT WILL USE THE NORMAL ADDRESS W/OUT DATE CHECKS.          CL200119700
*                                                                    CL200119800
*  THIS PROCESS DOES THE SAME AS PREVIOUS DB2/SQL ACCESS             CL200119900
*                                                                    CL200120000
 4020-GET-EEODSADD.                                                  CL200120100
                                                                     CL200120200
     IF WS-EEODSADD-NOT-EOF                                          CL200120300
     AND EEODSADD-WHO-IDENTIFIER    < MVMBR-PERSON-ID                CL200120400
        READ EEODSADD-FILE                                           CL200120500
          AT END                                                     CL200120600
             SET WS-EEODSADD-EOF   TO TRUE                           CL200120700
         NOT AT END                                                  CL200120800
           ADD 1                   TO EEODSADD-CNT                   CL200120900
           MOVE SPACES             TO WS-EEODSADD-PRIM               CL200121000
           MOVE SPACES             TO WS-EEODSADD-CURR               CL200121100
           MOVE SPACES             TO WS-EEODSADD-NORM               CL200121200
        END-READ                                                     CL200121300
     ELSE                                                            CL200121400
        IF MVMBR-PERSON-ID      NOT = WS-PREV-PERSON-ID              CL200121500
           MOVE SPACES             TO WS-EEODSADD-PRIM               CL200121600
           MOVE SPACES             TO WS-EEODSADD-CURR               CL200121700
           MOVE SPACES             TO WS-EEODSADD-NORM               CL200121800
        END-IF                                                       CL200121900
     END-IF                                                          CL200122000
                                                                     CL200122100
     SET WS-EEODSADD-NOT-END       TO TRUE                           CL200122200
     SET WS-EEODSADD-REC-NOT-FOUND TO TRUE                           CL200122300
                                                                     CL200122400
     PERFORM 4025-READ-EEODSADD                                      CL200122500
       UNTIL WS-EEODSADD-END                                         CL200122600
          OR WS-EEODSADD-EOF                                         CL200122700
          OR WS-EEODSADD-REC-FOUND                                   CL200122800
          OR EEODSADD-WHO-IDENTIFIER > MVMBR-PERSON-ID               CL200122900
     .                                                               CL200123000
                                                                     CL200123100
 4020-EXIT.                                                          CL200123200
     EXIT.                                                           CL200123300
                                                                     CL200123400
*THIS PARA WILL READ ONE OR MORE ADDRESS RECS FOR THIS CONTRACT      CL200123500
*                                                                    CL200123600
*IT WILL ATTEMPT TO FIND A PRIMARY ADDRESS & USE WS-EEODSADD-PRIM    CL200123700
*THEN A CURRENT ADDRESS AND STORE IN WS-EEODSADD-CURR                CL200123800
*THEN A NORMAL ADDRESS  AND STORE IN WS-EEODSADD-NORM                CL200123900
 4025-READ-EEODSADD.                                                 CL200124000
                                                                     CL200124100
     IF EEODSADD-WHO-IDENTIFIER     = MVMBR-PERSON-ID                CL200124200
        IF EEODSADD-EFFECTIVE-DATE <= WS-CURR-DATE                   CL200124300
        AND EEODSADD-END-DATE      >= WS-CURR-DATE                   CL200124400
           IF EEODSADD-PRIMARY-FLAG = 'Y'                            CL200124500
              SET WS-EEODSADD-REC-FOUND TO TRUE                      CL200124600
              SET WS-EEODSADD-END  TO TRUE                           CL200124700
              MOVE EEODSADD-REC    TO WS-EEODSADD-PRIM               CL200124800
           ELSE                                                      CL200124900
*             SET WS-EEODSADD-REC-FOUND TO TRUE                      CL200125000
              MOVE SPACES          TO WS-EEODSADD-PRIM               CL200125100
              MOVE EEODSADD-REC    TO WS-EEODSADD-CURR               CL200125200
           END-IF                                                    CL200125300
        ELSE                                                         CL200125400
           MOVE SPACES             TO WS-EEODSADD-PRIM               CL200125500
*          MOVE SPACES             TO WS-EEODSADD-CURR               CL200125600
           MOVE EEODSADD-REC       TO WS-EEODSADD-NORM               CL200125700
        END-IF                                                       CL200125800
     ELSE                                                            CL200125900
        IF EEODSADD-WHO-IDENTIFIER  > MVMBR-PERSON-ID                CL200126000
           SET WS-EEODSADD-END     TO TRUE                           CL200126100
       END-IF                                                        CL200126200
     END-IF                                                          CL200126300
                                                                     CL200126400
     IF WS-EEODSADD-NOT-END                                          CL200126500
     AND WS-EEODSADD-REC-NOT-FOUND                                   CL200126600
        READ EEODSADD-FILE                                           CL200126700
          AT END                                                     CL200126800
             SET WS-EEODSADD-EOF   TO TRUE                           CL200126900
         NOT AT END                                                  CL200127000
           ADD 1                   TO EEODSADD-CNT                   CL200127100
           IF EEODSADD-WHO-IDENTIFIER > MVMBR-PERSON-ID              CL200127200
              SET WS-EEODSADD-END  TO TRUE                           CL200127300
           END-IF                                                    CL200127400
        END-READ                                                     CL200127500
     END-IF                                                          CL200127600
     .                                                               CL200127700
                                                                     CL200127800
 4025-EXIT.                                                          CL200127900
     EXIT.                                                           CL200128000
                                                                     CL200128100
*THIS PARA POINTS TO THE FIRST HOME PHONE REC FOR WHO IDENTIFIER     CL200128200
*                                                                    CL200128300
*  THIS PROCESS DOES THE SAME AS PREVIOUS DB2/SQL ACCESS             CL200128400
*                                                                    CL200128500
 4030-GET-SRC033H.                                                   CL200128600
                                                                     CL200128700
     MOVE WS-SRC033H-REC           TO DCLA-METAVANCE-PHONE           CL200128800
                                                                     CL200128900
     IF WS-SRC033H-NOT-EOF                                           CL200129000
     AND WHO-IDENTIFIER            OF DCLA-METAVANCE-PHONE           CL200129100
                                    < MVMBR-PERSON-ID                CL200129200
        READ SRC033H-FILE        INTO DCLA-METAVANCE-PHONE           CL200129300
          AT END                                                     CL200129400
             SET WS-SRC033H-EOF    TO TRUE                           CL200129500
         NOT AT END                                                  CL200129600
            ADD 1                  TO SRC033H-CNT                    CL200129700
        END-READ                                                     CL200129800
     END-IF                                                          CL200129900
                                                                     CL200130000
     SET WS-SRC033H-NOT-END        TO TRUE                           CL200130100
     SET WS-SRC033H-REC-NOT-FOUND  TO TRUE                           CL200130200
                                                                     CL200130300
     PERFORM 4035-READ-SRC033H                                       CL200130400
       UNTIL WS-SRC033H-END                                          CL200130500
          OR WS-SRC033H-EOF                                          CL200130600
          OR WS-SRC033H-REC-FOUND                                    CL200130700
          OR WHO-IDENTIFIER        OF DCLA-METAVANCE-PHONE           CL200130800
                                    > MVMBR-PERSON-ID                CL200130900
     MOVE DCLA-METAVANCE-PHONE     TO WS-SRC033H-REC                 CL200131000
     .                                                               CL200131100
                                                                     CL200131200
 4030-EXIT.                                                          CL200131300
     EXIT.                                                           CL200131400
                                                                     CL200131500
*THIS PARA LOOKS AT THE FIRST PHONE RECORD FOR THE WHO IDENTIFIER    CL200131600
*IF DATE EDITS ARE NOT MET, IT WILL READ AGAIN                       CL200131700
 4035-READ-SRC033H.                                                  CL200131800
                                                                     CL200131900
     IF WHO-IDENTIFIER             OF DCLA-METAVANCE-PHONE           CL200132000
                                    = MVMBR-PERSON-ID                CL200132100
        IF EFFECTIVE-DATE          OF DCLA-METAVANCE-PHONE           CL200132200
                                   <= WS-MBR-EFF-DT                  CL200132300
        AND END-DATE               OF DCLA-METAVANCE-PHONE           CL200132400
                                   >= WS-MBR-EFF-DT                  CL200132500
           SET WS-SRC033H-END      TO TRUE                           CL200132600
           SET WS-SRC033H-REC-FOUND TO TRUE                          CL200132700
        END-IF                                                       CL200132800
     END-IF                                                          CL200132900
                                                                     CL200133000
     IF WHO-IDENTIFIER             OF DCLA-METAVANCE-PHONE           CL200133100
                                    > MVMBR-PERSON-ID                CL200133200
        SET WS-SRC033H-END         TO TRUE                           CL200133300
     END-IF                                                          CL200133400
                                                                     CL200133500
     IF WS-SRC033H-REC-NOT-FOUND                                     CL200133600
     AND WS-SRC033H-NOT-END                                          CL200133700
        READ SRC033H-FILE        INTO DCLA-METAVANCE-PHONE           CL200133800
          AT END                                                     CL200133900
             SET WS-SRC033H-EOF    TO TRUE                           CL200134000
         NOT AT END                                                  CL200134100
            ADD 1                  TO SRC033H-CNT                    CL200134200
        END-READ                                                     CL200134300
     END-IF                                                          CL200134400
     .                                                               CL200134500
                                                                     CL200134600
 4035-EXIT.                                                          CL200134700
     EXIT.                                                           CL200134800
                                                                     CL200134900
*THIS PARA POINTS TO THE FIRST WORK PHONE REC FOR WHO IDENTIFIER     CL200135000
*                                                                    CL200135100
*  THIS PROCESS DOES THE SAME AS PREVIOUS DB2/SQL ACCESS             CL200135200
*                                                                    CL200135300
 4040-GET-SRC033W.                                                   CL200135400
                                                                     CL200135500
     MOVE WS-SRC033W-REC           TO DCLA-METAVANCE-PHONE           CL200135600
                                                                     CL200135700
     IF WS-SRC033W-NOT-EOF                                           CL200135800
     AND WHO-IDENTIFIER            OF DCLA-METAVANCE-PHONE           CL200135900
                                    < MVMBR-PERSON-ID                CL200136000
        READ SRC033W-FILE        INTO DCLA-METAVANCE-PHONE           CL200136100
          AT END                                                     CL200136200
             SET WS-SRC033W-EOF    TO TRUE                           CL200136300
         NOT AT END                                                  CL200136400
            ADD 1                  TO SRC033W-CNT                    CL200136500
        END-READ                                                     CL200136600
     END-IF                                                          CL200136700
                                                                     CL200136800
     SET WS-SRC033W-NOT-END        TO TRUE                           CL200136900
     SET WS-SRC033W-REC-NOT-FOUND  TO TRUE                           CL200137000
                                                                     CL200137100
     PERFORM 4045-READ-SRC033W                                       CL200137200
       UNTIL WS-SRC033W-END                                          CL200137300
          OR WS-SRC033W-EOF                                          CL200137400
          OR WS-SRC033W-REC-FOUND                                    CL200137500
          OR WHO-IDENTIFIER        OF DCLA-METAVANCE-PHONE           CL200137600
                                    > MVMBR-PERSON-ID                CL200137700
     MOVE DCLA-METAVANCE-PHONE     TO WS-SRC033W-REC                 CL200137800
     .                                                               CL200137900
                                                                     CL200138000
 4040-EXIT.                                                          CL200138100
     EXIT.                                                           CL200138200
                                                                     CL200138300
*THIS PARA LOOKS AT THE FIRST PHONE RECORD FOR THE WHO IDENTIFIER    CL200138400
*IF DATE EDITS ARE NOT MET, IT WILL READ AGAIN                       CL200138500
 4045-READ-SRC033W.                                                  CL200138600
                                                                     CL200138700
     IF WHO-IDENTIFIER             OF DCLA-METAVANCE-PHONE           CL200138800
                                    = MVMBR-PERSON-ID                CL200138900
        IF EFFECTIVE-DATE          OF DCLA-METAVANCE-PHONE           CL200139000
                                   <= WS-MBR-EFF-DT                  CL200139100
        AND END-DATE               OF DCLA-METAVANCE-PHONE           CL200139200
                                   >= WS-MBR-EFF-DT                  CL200139300
           SET WS-SRC033W-END      TO TRUE                           CL200139400
           SET WS-SRC033W-REC-FOUND TO TRUE                          CL200139500
        END-IF                                                       CL200139600
     END-IF                                                          CL200139700
                                                                     CL200139800
     IF WHO-IDENTIFIER             OF DCLA-METAVANCE-PHONE           CL200139900
                                    > MVMBR-PERSON-ID                CL200140000
        SET WS-SRC033W-END         TO TRUE                           CL200140100
     END-IF                                                          CL200140200
                                                                     CL200140300
     IF WS-SRC033W-REC-NOT-FOUND                                     CL200140400
     AND WS-SRC033W-NOT-END                                          CL200140500
        READ SRC033W-FILE        INTO DCLA-METAVANCE-PHONE           CL200140600
          AT END                                                     CL200140700
             SET WS-SRC033W-EOF    TO TRUE                           CL200140800
         NOT AT END                                                  CL200140900
            ADD 1                  TO SRC033W-CNT                    CL200141000
        END-READ                                                     CL200141100
     END-IF                                                          CL200141200
                                                                     CL200141300
     .                                                               CL200141400
 4045-EXIT.                                                          CL200141500
     EXIT.                                                           CL200141600
                                                                     CL200141700
*THIS PARA POINTS TO THE FIRST WORK PHONE REC FOR WHO IDENTIFIER     CL200141800
*                                                                    CL200141900
*  THIS PROCESS DOES THE SAME AS PREVIOUS DB2/SQL ACCESS             CL200142000
*                                                                    CL200142100
 4050-GET-SRC033P.                                                   CL200142200
                                                                     CL200142300
     MOVE WS-SRC033P-REC           TO DCLA-METAVANCE-PHONE           CL200142400
                                                                     CL200142500
     IF WS-SRC033P-NOT-EOF                                           CL200142600
     AND WHO-IDENTIFIER            OF DCLA-METAVANCE-PHONE           CL200142700
                                    < MVMBR-PERSON-ID                CL200142800
        READ SRC033P-FILE        INTO DCLA-METAVANCE-PHONE           CL200142900
          AT END                                                     CL200143000
             SET WS-SRC033P-EOF    TO TRUE                           CL200143100
         NOT AT END                                                  CL200143200
            ADD 1                  TO SRC033P-CNT                    CL200143300
        END-READ                                                     CL200143400
     END-IF                                                          CL200143500
                                                                     CL200143600
     SET WS-SRC033P-NOT-END        TO TRUE                           CL200143700
     SET WS-SRC033P-REC-NOT-FOUND  TO TRUE                           CL200143800
                                                                     CL200143900
     PERFORM 4055-READ-SRC033P                                       CL200144000
       UNTIL WS-SRC033P-END                                          CL200144100
          OR WS-SRC033P-EOF                                          CL200144200
          OR WS-SRC033P-REC-FOUND                                    CL200144300
          OR WHO-IDENTIFIER        OF DCLA-METAVANCE-PHONE           CL200144400
                                    > MVMBR-PERSON-ID                CL200144500
     MOVE DCLA-METAVANCE-PHONE     TO WS-SRC033P-REC                 CL200144600
                                                                     CL200144700
     .                                                               CL200144800
 4050-EXIT.                                                          CL200144900
     EXIT.                                                           CL200145000
                                                                     CL200145100
*THIS PARA LOOKS AT THE FIRST PRIMARY PHONE REC FOR WHO IDENTIFIER   CL200145200
*IF PRIMARY/DATE EDITS ARE NOT MET, IT WILL READ AGAIN               CL200145300
*NOTE: THERE ARE ONLY PRIMARIES SELECTED IN THIS FILE, SO NO EDIT    CL200145400
 4055-READ-SRC033P.                                                  CL200145500
                                                                     CL200145600
     IF WHO-IDENTIFIER             OF DCLA-METAVANCE-PHONE           CL200145700
                                    = MVMBR-PERSON-ID                CL200145800
        IF EFFECTIVE-DATE          OF DCLA-METAVANCE-PHONE           CL200145900
                                   <= WS-MBR-EFF-DT                  CL200146000
        AND END-DATE               OF DCLA-METAVANCE-PHONE           CL200146100
                                   >= WS-MBR-EFF-DT                  CL200146200
           SET WS-SRC033P-REC-FOUND  TO TRUE                         CL200146300
        END-IF                                                       CL200146400
     END-IF                                                          CL200146500
                                                                     CL200146600
     IF WHO-IDENTIFIER             OF DCLA-METAVANCE-PHONE           CL200146700
                                    > MVMBR-PERSON-ID                CL200146800
        SET WS-SRC033P-END         TO TRUE                           CL200146900
     END-IF                                                          CL200147000
                                                                     CL200147100
     IF WS-SRC033P-REC-NOT-FOUND                                     CL200147200
     AND WS-SRC033P-NOT-END                                          CL200147300
        READ SRC033P-FILE        INTO DCLA-METAVANCE-PHONE           CL200147400
          AT END                                                     CL200147500
             SET WS-SRC033P-EOF    TO TRUE                           CL200147600
         NOT AT END                                                  CL200147700
            ADD 1                  TO SRC033P-CNT                    CL200147800
        END-READ                                                     CL200147900
     END-IF                                                          CL200148000
                                                                     CL200148100
     .                                                               CL200148200
 4055-EXIT.                                                          CL200148300
     EXIT.                                                           CL200148400
                                                                     CL200148500
*THIS PARA POINTS TO THE FIRST EMAIL ADDR REC FOR WHO IDENTIFIER     CL200148600
*                                                                    CL200148700
*  THIS PROCESS DOES THE SAME AS PREVIOUS DB2/SQL ACCESS             CL200148800
*                                                                    CL200148900
 4060-GET-SRC034.                                                    CL200149000
                                                                     CL200149100
     IF WS-SRC034-NOT-EOF                                            CL200149200
     AND WHO-IDENTIFIER            OF DCLA-METAVANCE-ELEC-ADR        CL200149300
                                    < MVMBR-PERSON-ID                CL200149400
        READ SRC034-FILE         INTO DCLA-METAVANCE-ELEC-ADR        CL200149500
          AT END                                                     CL200149600
             SET WS-SRC034-EOF     TO TRUE                           CL200149700
         NOT AT END                                                  CL200149800
            ADD 1                  TO SRC034-CNT                     CL200149900
        END-READ                                                     CL200150000
     END-IF                                                          CL200150100
                                                                     CL200150200
     SET WS-SRC034-NOT-END         TO TRUE                           CL200150300
     SET WS-SRC034-REC-NOT-FOUND   TO TRUE                           CL200150400
                                                                     CL200150500
     PERFORM 4065-READ-SRC034                                        CL200150600
       UNTIL WS-SRC034-END                                           CL200150700
          OR WS-SRC034-EOF                                           CL200150800
          OR WS-SRC034-REC-FOUND                                     CL200150900
          OR WHO-IDENTIFIER        OF DCLA-METAVANCE-ELEC-ADR        CL200151000
                                    > MVMBR-PERSON-ID                CL200151100
     .                                                               CL200151200
                                                                     CL200151300
 4060-EXIT.                                                          CL200151400
     EXIT.                                                           CL200151500
                                                                     CL200151600
*THIS PARA LOOKS AT THE FIRST EMAIL ADDR REC FOR WHO IDENTIFIER      CL200151700
*IF PRIMARY/DATE EDITS ARE NOT MET, IT WILL READ AGAIN               CL200151800
 4065-READ-SRC034.                                                   CL200151900
                                                                     CL200152000
     IF WHO-IDENTIFIER             OF DCLA-METAVANCE-ELEC-ADR        CL200152100
                                    = MVMBR-PERSON-ID                CL200152200
        IF EFF-DATE                OF DCLA-METAVANCE-ELEC-ADR        CL200152300
                                   <= WS-MBR-EFF-DT                  CL200152400
        AND END-DATE               OF DCLA-METAVANCE-ELEC-ADR        CL200152500
                                   >= WS-MBR-EFF-DT                  CL200152600
        AND PRIMARY-FLAG           OF DCLA-METAVANCE-ELEC-ADR        CL200152700
                                    = 'Y'                            CL200152800
           SET WS-SRC034-REC-FOUND TO TRUE                           CL200152900
        END-IF                                                       CL200153000
     END-IF                                                          CL200153100
                                                                     CL200153200
     IF WHO-IDENTIFIER             OF DCLA-METAVANCE-ELEC-ADR        CL200153300
                                    > MVMBR-PERSON-ID                CL200153400
        SET WS-SRC034-END          TO TRUE                           CL200153500
     END-IF                                                          CL200153600
                                                                     CL200153700
     IF WS-SRC034-REC-NOT-FOUND                                      CL200153800
     AND WS-SRC034-NOT-END                                           CL200153900
        READ SRC034-FILE         INTO DCLA-METAVANCE-ELEC-ADR        CL200154000
          AT END                                                     CL200154100
             SET WS-SRC034-EOF     TO TRUE                           CL200154200
         NOT AT END                                                  CL200154300
            ADD 1                  TO SRC034-CNT                     CL200154400
        END-READ                                                     CL200154500
     END-IF                                                          CL200154600
                                                                     CL200154700
     .                                                               CL200154800
 4065-EXIT.                                                          CL200154900
     EXIT.                                                           CL200155000
                                                                     CL200155100
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200155200
*THIS PARA POINTS TO THE FIRST AELPER REC FOR WHO IDENTIFIER         CL200155300
*                                                                    CL200155400
*  THIS PROCESS DOES THE SAME AS PREVIOUS DB2/SQL ACCESS             CL200155500
*                                                                    CL200155600
 4070-GET-AELPER.                                                    CL200155700
                                                                     CL200155800
     MOVE WS-AELPER-REC           TO DCLA-AELPER                     CL200155900
                                                                     CL200156000
     IF WS-AELPER-NOT-EOF                                            CL200156100
     AND PERS-PID-ID               OF DCLA-AELPER                    CL200156200
                                    < MVMBR-PERSON-ID                CL200156300
        READ AELPER-FILE        INTO DCLA-AELPER                     CL200156400
          AT END                                                     CL200156500
             SET WS-AELPER-EOF    TO TRUE                            CL200156600
         NOT AT END                                                  CL200156700
            ADD 1                  TO AELPER-CNT                     CL200156800
        END-READ                                                     CL200156900
     END-IF                                                          CL200157000
                                                                     CL200157100
     SET WS-AELPER-NOT-END        TO TRUE                            CL200157200
     SET WS-AELPER-REC-NOT-FOUND  TO TRUE                            CL200157300
                                                                     CL200157400
     PERFORM 4075-READ-AELPER                                        CL200157500
       UNTIL WS-AELPER-END                                           CL200157600
          OR WS-AELPER-EOF                                           CL200157700
          OR WS-AELPER-REC-FOUND                                     CL200157800
          OR PERS-PID-ID           OF DCLA-AELPER                    CL200157900
                                    > MVMBR-PERSON-ID                CL200158000
     MOVE DCLA-AELPER              TO WS-AELPER-REC                  CL200158100
                                                                     CL200158200
     .                                                               CL200158300
 4070-EXIT.                                                          CL200158400
     EXIT.                                                           CL200158500
                                                                     CL200158600
*THIS PARA LOOKS AT THE FIRST AELPER REC FOR WHO IDENTIFIER          CL200158700
*IF PRIMARY/DATE EDITS ARE NOT MET, IT WILL READ AGAIN               CL200158800
*NOTE: THERE ARE ONLY PRIMARIES SELECTED IN THIS FILE, SO NO EDIT    CL200158900
 4075-READ-AELPER.                                                   CL200159000
                                                                     CL200159100
     IF PERS-PID-ID                OF DCLA-AELPER                    CL200159200
                                    = MVMBR-PERSON-ID                CL200159300
           SET WS-AELPER-REC-FOUND  TO TRUE                          CL200159400
     END-IF                                                          CL200159500
                                                                     CL200159600
     IF PERS-PID-ID                OF DCLA-AELPER                    CL200159700
                                    > MVMBR-PERSON-ID                CL200159800
        SET WS-AELPER-END         TO TRUE                            CL200159900
     END-IF                                                          CL200160000
                                                                     CL200160100
     IF WS-AELPER-REC-NOT-FOUND                                      CL200160200
     AND WS-AELPER-NOT-END                                           CL200160300
        READ AELPER-FILE        INTO DCLA-AELPER                     CL200160400
          AT END                                                     CL200160500
             SET WS-AELPER-EOF    TO TRUE                            CL200160600
         NOT AT END                                                  CL200160700
            ADD 1                  TO AELPER-CNT                     CL200160800
        END-READ                                                     CL200160900
     END-IF                                                          CL200161000
                                                                     CL200161100
     .                                                               CL200161200
 4075-EXIT.                                                          CL200161300
     EXIT.                                                           CL200161400
*GXJ0129 05/07/07 CHANGES END                                        CL200161500
                                                                     CL200161600
*****************************************************                CL200161700
*                                                   *                CL200161800
*  THIS PARAGRAPH PERFORMS END OF PROGRAM PROCESSING*                CL200161900
*                                                   *                CL200162000
*****************************************************                CL200162100
                                                                     CL200162200
 9000-TERMINATION.                                                   CL200162300
                                                                     CL200162400
*SYNTEL START CHAIN OF TRUST                                         CL200162500
     OPEN INPUT  MTVCOT-IN1                                          CL200162600
                 MTVCOT-IN2                                          CL200162700
          OUTPUT MTVCOT-OUT.                                         CL200162800
                                                                     CL200162900
     READ MTVCOT-IN1                                                 CL200163000
          AT END                                                     CL200163100
             SET WS-MTVCOT1-EOF TO TRUE                              CL200163200
     END-READ.                                                       CL200163300
                                                                     CL200163400
     READ MTVCOT-IN2                                                 CL200163500
          AT END                                                     CL200163600
             SET WS-MTVCOT2-EOF TO TRUE                              CL200163700
     END-READ.                                                       CL200163800
*MXC0156 10/06/09 CA# 1326215 STARTS                                 CL200163900
     PERFORM 4026-READ-END-EEODSADD THRU 4026-EXIT                   CL200164000
     UNTIL WS-EEODSADD-EOF                                           CL200164100
*MXC0156 10/06/09 CA# 1326215 ENDS                                   CL200164200
                                                                     CL200164300
     IF (WS-MTVCOT1-NOT-EOF) AND (WS-MTVCOT2-NOT-EOF)                CL200164400
                                                                     CL200164500
       IF (MTVCOTI1-OUTPUT-CNT NOT = MBR-READ-CNT)                   CL200164600
       OR (MTVCOTI2-OUTPUT-CNT NOT = EEODSADD-CNT)                   CL200164700
         DISPLAY 'SEND E-MAIL TO ON CALL SUPPORT'                    CL200164800
         DISPLAY 'MTVCOTI1-OUTPUT-CNT ' MTVCOTI1-OUTPUT-CNT          CL200164900
         DISPLAY 'MBR-READ-CNT ' MBR-READ-CNT                        CL200165000
         DISPLAY 'MTVCOTI2-OUTPUT-CNT ' MTVCOTI2-OUTPUT-CNT          CL200165100
         DISPLAY 'EEODSADD-CNT ' EEODSADD-CNT                        CL200165200
         CALL 'ABEND'                                                CL200165300
       END-IF                                                        CL200165400
     END-IF                                                          CL200165500
                                                                     CL200165600
     MOVE 'EE305'            TO MTVCOT-PROGRAM-NAME                  CL200165700
     MOVE WS-CURR-DATE       TO MTVCOT-PROCESS-DATE                  CL200165800
     MOVE MBR-READ-CNT       TO MTVCOT-INPUT-CNT                     CL200165900
     MOVE ZERO               TO MTVCOT-ERROR-CNT                     CL200166000
     MOVE MBR-OUTPUT-CNT     TO MTVCOT-OUTPUT-CNT                    CL200166100
                                                                     CL200166200
     WRITE MTVCOT-OUT-REC FROM MTVCOT-RECORD                         CL200166300
                                                                     CL200166400
     IF MBR-READ-CNT         NOT = EEODSADD-CNT                      CL200166500
        MOVE 'EE305'         TO MTVCOT-PROGRAM-NAME                  CL200166600
        MOVE WS-CURR-DATE    TO MTVCOT-PROCESS-DATE                  CL200166700
        MOVE EEODSADD-CNT    TO MTVCOT-INPUT-CNT                     CL200166800
        MOVE ZERO            TO MTVCOT-ERROR-CNT                     CL200166900
        MOVE MBR-OUTPUT-CNT  TO MTVCOT-OUTPUT-CNT                    CL200167000
                                                                     CL200167100
        WRITE MTVCOT-OUT-REC FROM MTVCOT-RECORD                      CL200167200
     END-IF.                                                         CL200167300
                                                                     CL200167400
*SYNTEL END CHAIN OF TRUST                                           CL200167500
                                                                     CL200167600
* SAT5320 RLS 5.0 BEGIN                                              CL200167700
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200167800
                                                                     CL200167900
*    INITIALIZE MBRCNT-REC                                           CL200168000
*    MOVE  MBR-OUTPUT-CNT  TO   MBR-TRANS-CNT                        CL200168100
*    WRITE MBRCNT-REC      FROM EE305CNT-REC                         CL200168200
                                                                     CL200168300
*GXJ0129 05/07/07 CHANGES END                                        CL200168400
* SAT5320 RLS 5.0 END                                                CL200168500
                                                                     CL200168600
     CLOSE MTVMBR-FILE                                               CL200168700
           EEODSMB2-FILE                                             CL200168800
           MTVERR-FILE                                               CL200168900
* SAT5320 RLS 5.0 BEGIN                                              CL200169000
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200169100
*          MBRCNT-FILE                                               CL200169200
*GXJ0129 05/07/07 CHANGES END                                        CL200169300
* SAT5320 RLS 5.0 END                                                CL200169400
*SYNTEL START CHAIN OF TRUST                                         CL200169500
           MTVCOT-IN1                                                CL200169600
           MTVCOT-IN2                                                CL200169700
           MTVCOT-OUT.                                               CL200169800
*SYNTEL END CHAIN OF TRUST                                           CL200169900
                                                                     CL200170000
     CLOSE EEODSADD-FILE                                             CL200170100
     CLOSE SRC033H-FILE                                              CL200170200
     CLOSE SRC033W-FILE                                              CL200170300
     CLOSE SRC033P-FILE                                              CL200170400
     CLOSE SRC034-FILE                                               CL200170500
*GXJ01I9 05/07/07 CHANGES BEGIN                                      CL200170600
     CLOSE AELPER-FILE                                               CL200170700
*GXJ0129 05/07/07 CHANGES END                                        CL200170800
                                                                     CL200170900
*VXS0066 07/27/14 CA#4380298 BEG                                     CL200171000
*VXS0066 09/29/14 CA#4525890 BEG                                     CL200171100
*    IF MBR-ERROR-CNT > 200                                          CL200171200
     IF MBR-ERROR-CNT > 500                                          CL200171300
*VXS0066 09/29/14 CA#4525890 END                                     CL200171400
     MOVE 16    TO RETURN-CODE                                       CL200171500
     DISPLAY '** ABEND FOR INVALID      **'                          CL200171600
     DISPLAY '** ADDRESS FOUND FOR MORE **'                          CL200171700
     DISPLAY '** THAN 200 MEMEBRS       **'                          CL200171800
     DISPLAY '** MBR-ERROR-CNT          ::' MBR-ERROR-CNT            CL200171900
     DISPLAY '** ABEND ABEND ABEND      **'                          CL200172000
     END-IF                                                          CL200172100
*VXS0066 07/27/14 CA#4380298 END                                     CL200172200
                                                                     CL200172300
     DISPLAY '** MBR-READ-CNT   : ' MBR-READ-CNT                     CL200172400
     DISPLAY '** MBR-OUTPUT-CNT : ' MBR-OUTPUT-CNT                   CL200172500
     DISPLAY '** MBR-ERROR-CNT  : ' MBR-ERROR-CNT                    CL200172600
     DISPLAY '** EEODSADD-CNT   : ' EEODSADD-CNT                     CL200172700
     DISPLAY '** SRC033H-CNT    : ' SRC033H-CNT                      CL200172800
     DISPLAY '** SRC033W-CNT    : ' SRC033W-CNT                      CL200172900
     DISPLAY '** SRC033P-CNT    : ' SRC033P-CNT                      CL200173000
     DISPLAY '** SRC034-CNT     : ' SRC034-CNT                       CL200173100
*GXJ0129 05/07/07 CHANGES BEGIN                                      CL200173200
     DISPLAY '** AELPER-CNT     : ' AELPER-CNT                       CL200173300
*GXJ0129 05/07/07 CHANGES END                                        CL200173400
                                                                     CL200173500
     DISPLAY '******* END EE305 *******'.                            CL200173600
                                                                     CL200173700
 9000-TERMINATION-EXIT.                                              CL200173800
     EXIT.                                                           CL200173900
                                                                     CL200174000
*MXC0156 10/06/09 CA# 1326215 STARTS                                 CL200174100
 4026-READ-END-EEODSADD.                                             CL200174200
     READ EEODSADD-FILE                                              CL200174300
       AT END                                                        CL200174400
          SET WS-EEODSADD-EOF      TO TRUE                           CL200174500
      NOT AT END                                                     CL200174600
        ADD 1                      TO EEODSADD-CNT                   CL200174700
     END-READ.                                                       CL200174800
 4026-EXIT.                                                          CL200174900
     EXIT.                                                           CL200175000
*MXC0156 10/06/09 CA# 1326215 ENDS                                   CL200175100
                                                                     CL200175200
*****************************************************                CL200175300
*                                                   *                CL200175400
*  THIS PARAGRAPH IS FOR ABENDS                     *                CL200175500
*                                                   *                CL200175600
*****************************************************                CL200175700
                                                                     CL200175800
 9999-ABORT.                                                         CL200175900
                                                                     CL200176000
        DISPLAY '**********************************************'     CL200176100
        DISPLAY '**********************************************'     CL200176200
        DISPLAY '**    PROGRAM EE305 IS CALLING ABEND        **'     CL200176300
        DISPLAY '**                                          **'     CL200176400
        DISPLAY '** PARAGRAPH     : ', WS-PARAGRAPH-NAME             CL200176500
        DISPLAY '**                                          **'     CL200176600
        DISPLAY '**********************************************'     CL200176700
                                                                     CL200176800
     CALL 'ABEND'.                                                   CL200176900
                                                                     CL200177000
 9999-ABORT-EXIT.                                                    CL200177100
     EXIT.                                                           CL200177200
                                                                     CL200177300
