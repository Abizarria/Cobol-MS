       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN222.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCTR        ASSIGN  TO  DISK.

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-PRS.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCTR      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCTR.DAT".
       01  REG-CTR.
           03  DIA-CTR          PIC  9(02).
           03  DTI-CTR          PIC  9(06).
           03  DTC-CTR          PIC  9(06).
           03  DTD-CTR          PIC  9(06).
           03  FAT-CTR          PIC  9(06).
           03  PCT-CTR          PIC  9(02)V99.

       FD  CADPRM      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADPRM.DAT".
       01  REG-PRM.
           05  COD-PRM          PIC  9(05).
           05  CLI-PRM          PIC  9(02)  OCCURS  36.
           05  END-PRM          PIC  9(02)  OCCURS  35.
           05  CID-PRM          PIC  9(02)  OCCURS  20.
           05  EST-PRM          PIC  9(02)  OCCURS  02.
           05  CEP-PRM          PIC  9(08).
           05  CGC-PRM          PIC  9(14).
           05  INS-PRM          PIC  9(02)  OCCURS  15.

       FD  CADPRS      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADPRS.DAT".
       01  REG-PRS.
           03  CHV-PRS.
               05  TUR-PRS      PIC  9(01).
               05  CXA-PRS      PIC  9(01).
               05  ATV-PRS      PIC  9(01).
               05  LIN-PRS      PIC  9(03).
           03  DES-PRS          PIC  X(15).
           03  PRX-PRS          PIC  9(01).
           03  QTD-PRS          PIC  9(06)V9.
           03  CTD-PRS          PIC  9(10)V99.
           03  VDD-PRS          PIC S9(10)V99.
           03  QTM-PRS          PIC  9(07)V9.
           03  CTM-PRS          PIC  9(11)V99.
           03  VDM-PRS          PIC S9(11)V99.

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC  X(80).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  X(02).
           03  DES-AUX          PIC  X(15).
           03  VDA-AUX          PIC  9(07)V9.
           03  PRV-AUX          PIC  9(07)V9.
           03  TOT-AUX          PIC  9(07)V9.
           03  ACM-VDA          PIC  9(07)V9.
           03  ACM-PRV          PIC  9(07)V9.
           03  DES-TOT          PIC  X(15)  VALUE  "TOTAL DO GRUPO".

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  LIN-TAB          OCCURS  03.
               05  COL-TAB      OCCURS  09.
                   07  DES-TAB  PIC  X(15).
                   07  QTD-TAB  PIC  9(07)V9.

           03  TAB-DIA.
               05  FILLER       PIC  X(02)  VALUE  "31".
               05  FILLER       PIC  X(02)  VALUE  "28".
               05  FILLER       PIC  X(02)  VALUE  "31".
               05  FILLER       PIC  X(02)  VALUE  "30".
               05  FILLER       PIC  X(02)  VALUE  "31".
               05  FILLER       PIC  X(02)  VALUE  "30".
               05  FILLER       PIC  X(02)  VALUE  "31".
               05  FILLER       PIC  X(02)  VALUE  "31".
               05  FILLER       PIC  X(02)  VALUE  "30".
               05  FILLER       PIC  X(02)  VALUE  "31".
               05  FILLER       PIC  X(02)  VALUE  "30".
               05  FILLER       PIC  X(02)  VALUE  "31".
           03  RED-DIA          REDEFINES   TAB-DIA.
               05  DIA-TAB      PIC  9(02)  OCCURS  12.

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

       01  CAB-001.
           03  FILLER           PIC  X(22)  VALUE  "-DYNAMIC-".
           03  NOM-CB1          PIC  X(48).
           03  FILLER           PIC  X(10)  VALUE  "FOLHA : 01".

       01  CAB-002.
           03  REG-CB2          PIC  X(72)  VALUE  "PREVISAO DE VENDAS".
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "DESCRICAO           V. REALIZADA     MEDIA / DIA     V. PREV
           "ISTA     TOTAL / MES".

       01  DET-001.
           03  DES-DT1          PIC  X(15).
           03  FILLER           PIC  X(06)  VALUE  SPACES.
           03  VDA-DT1          PIC  Z.ZZZ.ZZ9,9.
           03  FILLER           PIC  X(07)  VALUE  SPACES.
           03  MED-DT1          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  PRV-DT1          PIC  Z.ZZZ.ZZ9,9.
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  TOT-DT1          PIC  Z.ZZZ.ZZ9,9.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 01 PIC X(67) FROM
               " PREVISAO DE VENDAS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 05 COLUMN 09 PIC X(62) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
            "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 13 PIC X(54) FROM
               "COMBUSTIVEL        REALIZADO     PREVISTO  TOTAL / MES"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR 14.
           03  LINE 06 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 07 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 08 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 09 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 15 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 16 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 09 PIC X(62) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
            "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" BACKGROUND-COLOR 1.
           03  LINE 17 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 18 COLUMN 13 PIC X(15) FROM "TOTAL GERAL"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.
           03  LINE 18 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 09 PIC X(62) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
            "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 1.
           03  LINE 19 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 11 PIC X(62) FROM SPACES.

       01  TLA-002.
           03  LINE 09 COLUMN 27 PIC X(26) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "Ä¿" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 27 PIC X(26) FROM " ³
               " ³" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 53 PIC X(02) FROM  SPACES.
           03  LINE 11 COLUMN 27 PIC X(26) FROM " ³  PREVISAO DE VENDAS
               " ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 53 PIC X(02) FROM  SPACES.
           03  LINE 12 COLUMN 27 PIC X(26) FROM " ³
               " ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 53 PIC X(02) FROM  SPACES.
           03  LINE 13 COLUMN 27 PIC X(26) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÙ" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 53 PIC X(02) FROM  SPACES.
           03  LINE 14 COLUMN 29 PIC X(26) FROM  SPACES.

       01  ENT-001.
           03  LINE 08 COLUMN 13 PIC X(15)       USING DES-AUX
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 30 PIC Z.ZZZ.ZZ9,9 USING VDA-AUX
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 43 PIC Z.ZZZ.ZZ9,9 USING PRV-AUX
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 56 PIC Z.ZZZ.ZZ9,9 USING TOT-AUX
               BACKGROUND-COLOR  01.

       01  ENT-002.
           03  LINE 09 COLUMN 13 PIC X(15)       USING DES-AUX
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 30 PIC Z.ZZZ.ZZ9,9 USING VDA-AUX
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 43 PIC Z.ZZZ.ZZ9,9 USING PRV-AUX
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 56 PIC Z.ZZZ.ZZ9,9 USING TOT-AUX
               BACKGROUND-COLOR  01.

       01  ENT-003.
           03  LINE 10 COLUMN 13 PIC X(15)       USING DES-AUX
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 30 PIC Z.ZZZ.ZZ9,9 USING VDA-AUX
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 43 PIC Z.ZZZ.ZZ9,9 USING PRV-AUX
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 56 PIC Z.ZZZ.ZZ9,9 USING TOT-AUX
               BACKGROUND-COLOR  01.

       01  ENT-004.
           03  LINE 11 COLUMN 13 PIC X(15)       USING DES-AUX
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 30 PIC Z.ZZZ.ZZ9,9 USING VDA-AUX
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 43 PIC Z.ZZZ.ZZ9,9 USING PRV-AUX
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 56 PIC Z.ZZZ.ZZ9,9 USING TOT-AUX
               BACKGROUND-COLOR  01.

       01  ENT-005.
           03  LINE 12 COLUMN 13 PIC X(15)       USING DES-AUX
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 30 PIC Z.ZZZ.ZZ9,9 USING VDA-AUX
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 43 PIC Z.ZZZ.ZZ9,9 USING PRV-AUX
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 56 PIC Z.ZZZ.ZZ9,9 USING TOT-AUX
               BACKGROUND-COLOR  01.

       01  ENT-006.
           03  LINE 13 COLUMN 13 PIC X(15)       USING DES-AUX
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 30 PIC Z.ZZZ.ZZ9,9 USING VDA-AUX
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 43 PIC Z.ZZZ.ZZ9,9 USING PRV-AUX
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 56 PIC Z.ZZZ.ZZ9,9 USING TOT-AUX
               BACKGROUND-COLOR  01.

       01  ENT-007.
           03  LINE 14 COLUMN 13 PIC X(15)       USING DES-AUX
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 30 PIC Z.ZZZ.ZZ9,9 USING VDA-AUX
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 43 PIC Z.ZZZ.ZZ9,9 USING PRV-AUX
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 56 PIC Z.ZZZ.ZZ9,9 USING TOT-AUX
               BACKGROUND-COLOR  01.

       01  ENT-008.
           03  LINE 15 COLUMN 13 PIC X(15)       USING DES-AUX
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 30 PIC Z.ZZZ.ZZ9,9 USING VDA-AUX
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 43 PIC Z.ZZZ.ZZ9,9 USING PRV-AUX
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 56 PIC Z.ZZZ.ZZ9,9 USING TOT-AUX
               BACKGROUND-COLOR  01.

       01  ENT-009.
           03  LINE 16 COLUMN 13 PIC X(15)       USING DES-AUX
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 30 PIC Z.ZZZ.ZZ9,9 USING VDA-AUX
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 43 PIC Z.ZZZ.ZZ9,9 USING PRV-AUX
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 56 PIC Z.ZZZ.ZZ9,9 USING TOT-AUX
               BACKGROUND-COLOR  01.

       01  ENT-010.
           03  LINE 18 COLUMN 30 PIC Z.ZZZ.ZZ9,9 USING ACM-VDA
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 18 COLUMN 43 PIC Z.ZZZ.ZZ9,9 USING ACM-PRV
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 18 COLUMN 56 PIC Z.ZZZ.ZZ9,9 USING TOT-AUX
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.

       01  LPA-001.
           03  LINE 05 COLUMN 09 PIC X(62) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 11 PIC X(62) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ACUMUL
                   "ANDO VALORES ...                                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR !!!  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO PREVISAO DE VENDAS ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC  AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  MEN-001
           OPEN     INPUT    CADCTR
                             CADPRS
           READ     CADCTR
           MOVE     1        TO  IND1  IND2.

       ROT-000-010.
           MOVE     SPACES   TO  DES-TAB (IND1, IND2)
           MOVE     ZEROS    TO  QTD-TAB (IND1, IND2)
           ADD      1        TO  IND2
           IF  IND2     <    10
               GO  TO        ROT-000-010.
           ADD      1        TO  IND1
           MOVE     1        TO  IND2
           IF  IND1     <    04
               GO  TO        ROT-000-010.

       ROT-000-020.
           READ     CADPRS   AT  END
                    GO  TO   ROT-000-030.
           IF  ATV-PRS   >   3
               GO  TO        ROT-000-020.
           MOVE     ATV-PRS  TO  IND1
           MOVE     PRX-PRS  TO  IND2
           ADD      QTM-PRS  TO  QTD-TAB (IND1, IND2)
           MOVE     DES-PRS  TO  DES-TAB (IND1, IND2)
           GO  TO   ROT-000-020.

       ROT-000-030.
           MOVE     DTD-CTR  TO  DTA-AUX
           IF  DIA-CTR   =   0
               MOVE      1   TO  MES-AUX.
           MOVE     ZEROS    TO  ACM-VDA  ACM-PRV
           MOVE     1        TO  IND1     IND2.

       ROT-100-000.
           MOVE     DES-TAB (IND1, IND2)  TO  DES-AUX
           MOVE     QTD-TAB (IND1, IND2)  TO  VDA-AUX
           COMPUTE  PRV-AUX  =  (DIA-CTR  /   DIA-AUX)
                             *  (DIA-TAB     (MES-AUX) -  DIA-AUX)
                             *  (QTD-TAB  (IND1, IND2) /  DIA-CTR)
           COMPUTE  TOT-AUX  =   VDA-AUX  +   PRV-AUX
           ADD      VDA-AUX  TO  ACM-VDA
           ADD      PRV-AUX  TO  ACM-PRV
           IF  IND2     =    01  DISPLAY  ENT-001.
           IF  IND2     =    02  DISPLAY  ENT-002.
           IF  IND2     =    03  DISPLAY  ENT-003.
           IF  IND2     =    04  DISPLAY  ENT-004.
           IF  IND2     =    05  DISPLAY  ENT-005.
           IF  IND2     =    06  DISPLAY  ENT-006.
           IF  IND2     =    07  DISPLAY  ENT-007.
           IF  IND2     =    08  DISPLAY  ENT-008.
           IF  IND2     =    09  DISPLAY  ENT-009.
           ADD      1        TO  IND2
           IF  IND2     <    10      AND
               DES-TAB (IND1, IND2)  NOT  =   SPACES
               GO  TO        ROT-100-000.
           COMPUTE  TOT-AUX  =   ACM-VDA  +   ACM-PRV
           DISPLAY  ENT-010      MEN-002
           MOVE     SPACES   TO  RSP-OPC.

       ROT-200-000.
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-000.
           DISPLAY  LPA-001  TLA-002
                             MEN-003
           OPEN     INPUT    CADPRM
                    OUTPUT   RELATO
           READ     CADPRM
           MOVE     1        TO  IND1.

       ROT-200-010.
           MOVE     CLI-PRM (IND1 + 1)  TO IND2
           MOVE     LET-TAB (IND2)  TO  LET-NOM (IND1)
           ADD      1   TO   IND1
           IF  IND1  <  36
               GO  TO        ROT-200-010.
           MOVE     SPACES   TO  LET-NOM (IND1)
           MOVE     NOM-TAB  TO  NOM-CB1
           MOVE     DIA-AUX  TO  DIA-CB2
           MOVE     MES-AUX  TO  MES-CB2
           MOVE     ANO-AUX  TO  ANO-CB2
           WRITE    REG-REL FROM CAB-001  AFTER  0
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-003
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL
           MOVE     ZEROS    TO  ACM-VDA
           MOVE     1        TO  IND1   IND2.

       ROT-200-020.
           MOVE     DES-TAB (IND1, IND2)  TO  DES-DT1
           MOVE     QTD-TAB (IND1, IND2)  TO  VDA-DT1
           COMPUTE  MED-DT1  =   QTD-TAB (IND1, IND2)  /  DIA-CTR
           COMPUTE  PRV-DT1  =  (DIA-CTR  /   DIA-AUX)
                             *  (DIA-TAB     (MES-AUX) -  DIA-AUX)
                             *  (QTD-TAB  (IND1, IND2) /  DIA-CTR)
           COMPUTE  TOT-DT1  =   QTD-TAB  (IND1, IND2)
                             +  (DIA-CTR  /   DIA-AUX)
                             *  (DIA-TAB     (MES-AUX) -  DIA-AUX)
                             *  (QTD-TAB  (IND1, IND2) /  DIA-CTR)
           WRITE    REG-REL FROM DET-001
           ADD      QTD-TAB (IND1, IND2)  TO  ACM-VDA
           ADD      1   TO   IND2
           IF  IND2     <    10      AND
               DES-TAB (IND1, IND2)  NOT  =   SPACES
               GO  TO        ROT-200-020.
           MOVE     DES-TOT  TO  DES-DT1
           MOVE     ACM-VDA  TO  VDA-DT1
           COMPUTE  MED-DT1  =   ACM-VDA  /   DIA-CTR
           COMPUTE  PRV-DT1  =  (DIA-CTR  /   DIA-AUX)
                             *  (DIA-TAB     (MES-AUX) -  DIA-AUX)
                             *  (ACM-VDA  /   DIA-CTR)
           COMPUTE  TOT-DT1  =   ACM-VDA  +  (DIA-CTR  /  DIA-AUX)
                             *  (DIA-TAB     (MES-AUX) -  DIA-AUX)
                             *  (ACM-VDA  /   DIA-CTR)
           WRITE    REG-REL FROM DET-001  AFTER  2
           ADD      1        TO  IND1
           MOVE     1        TO  IND2
           IF  IND1     >    03      OR
               DES-TAB (IND1, IND2)  =    SPACES
               GO  TO        ROT-200-030.
           MOVE     ZEROS    TO  ACM-VDA
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL               AFTER  2
           GO  TO   ROT-200-020.

       ROT-200-030.
           CLOSE    CADPRM  RELATO.

       ROT-300-000.
           CLOSE    CADCTR   CADPRS
           MOVE     5   TO   PRM-001
           CHAIN   "DYN200"  USING  PRM-001.
