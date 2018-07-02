//EE0303   JOB 000,CLASS=P,MSGCLASS=C                                   00000100
//*            **                                                   **  00000200
//*            ** CREATE AND SPLIT EEODSMB2 FILE FOR THREAD PROCESS **  00000300
//*            **                                                   **  00000400
//STEP05   EXEC EEP304,                                                 00000500
//             COTIN='PRODEEN.EE300.EEFNRCN.COT(+0)',                   00000600
//             GDGCUR='(+0)',                                           00000700
//             GDGNEW='(+1)',                                           00000800
//             GHPROD=GHPROD,                                           00000900
//             MEMA=EEZ304,                                             00001000
//             MEMB=EEZ305,                                             00001100
//             PRODEEN2=PRODEEN,                                        00001200
//             SORTCYL=182,                                             00001300
//             UNTODSM2='TAPE,BLKSIZE=261000',                          00001400
//             UNTSRT01='TAPE,BLKSIZE=261000'                           00001500
//*            ** EASYTRIEVE PROGRAM - TO MATCH                      ** 00001600
//*            ** EEODSPER FILE TO EEODSMB1 FILES                    ** 00001700
//*            ** CREATING EEODSMB2 FILE                             ** 00001800
//STEP10   EXEC EEP307,                                                 00001900
//             COTIN1='PRODEEN.EEZ304.EEFNRCN.COT(+1)',                 00002000
//             COTIN2='PRODEEN.EEZ303.EEFNRCN.COT(+0)',                 00002100
//             EE305=EE305,                                             00002200
//             GDGCUR='(+0)',                                           00002300
//             GDGCUR2='(+1)',                                          00002400
//             GDGNEW='(+1)',                                           00002500
//             LINK1=GHPROD,                                            00002600
//             PRODEEN=PRODEEN,                                         00002700
//             PRODEEN2=PRODEEN,                                        00002800
//             SORTCYL=182,                                             00002900
//             WORKEEN=WORKEEN                                          00003000
//*            ** MERGE UNLOADED TABLES TO EEODSMB2 FILE             ** 00003100
//STEP15   EXEC EEP313,                                                 00003200
//             COTIN='PRODEEN.EE305.EEFNRCN.COT(+1)',                   00003300
//             EE303=EE303,                                             00003400
//             GDGCUR='(+1)',                                           00003500
//             GDGNEW='(+1)',                                           00003600
//             CURGDG='(+0)',                                           00003700
//             GHPROD=GHPROD,                                           00003800
//             GHDATA=GHDATA,                                           00003900
//             LINK1=GHPROD,                                            00004000
//             MEMA=EEZ303A,                                            00004100
//             MEMB=EEZ313,                                             00004200
//             MEMC=EED313,                                             00004300
//             PLTFORM=MTV,                                             00004400
//             PLTFORM1=MTVX,                                           00004500
//             PRODEEN=PRODEEN,                                         00004600
//             PRODEEN2=PRODEEN,                                        00004700
//             TBLNMND=SME0ND,                                          00004800
//             TBLNM05=SME005,                                          00004900
//             TBLNM19=SME019,                                          00005000
//             SORTCYL=120,                                             00005100
//             UNTSPLTA='TAPE,BLKSIZE=261300',                          00005200
//             UNTSPLTB='TAPE,BLKSIZE=261300',                          00005300
//             UNTSPLTC='TAPE,BLKSIZE=261300',                          00005400
//             UNTSPLTD='TAPE,BLKSIZE=261300',                          00005500
//             UNTSPLTE='TAPE,BLKSIZE=261300',                          00005600
//             UNTSPLTF='TAPE,BLKSIZE=261300',                          00005700
//             UNTSPLTG='TAPE,BLKSIZE=261300',                          00005800
//             UNTSPLTH='TAPE,BLKSIZE=261300',                          00005900
//             UNTSPLTI='TAPE,BLKSIZE=261300',                          00006000
//             UNTSPLTJ='TAPE,BLKSIZE=261300',                          00006100
//             UNTSPLTK='TAPE,BLKSIZE=261300',                          00006200
//             UNTSPLTL='TAPE,BLKSIZE=261300',                          00006300
//             UNTSPLTM='TAPE,BLKSIZE=261300',                          00006400
//             UNTSPLTN='TAPE,BLKSIZE=261300',                          00006500
//             UNTSPLTO='TAPE,BLKSIZE=261300',                          00006600
//             WORKEEN=WORKEEN                                          00006700
//*            ** SPLIT EEODSMB2 FILE FOR THREAD PROCESS             ** 00006800
