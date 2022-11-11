       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN206.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS.

       DATA            DIVISION.
       FILE            SECTION.

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

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  FLG-AUX          PIC  9(01).
           03  VAL-AUX          PIC  9(10)V99.
           03  ACM-VAL          PIC  9(11)V99.

           03  REG-AUX.
               05  DES-AUX      PIC  X(16).
               05  FILLER       PIC  X(06)  VALUE  "....:".
               05  VDD-AUX      PIC  Z.ZZZ.ZZZ.ZZ9,99.

           03  ENT-AUX.
               05  LIN-001      PIC  X(38).
               05  LIN-002      PIC  X(38).
               05  LIN-003      PIC  X(38).
               05  LIN-004      PIC  X(38).
               05  LIN-005      PIC  X(38).
               05  LIN-006      PIC  X(38).
               05  LIN-007      PIC  X(38).
               05  LIN-008      PIC  X(38).
               05  LIN-009      PIC  X(38).

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 06 COLUMN 59 PIC X(14) FROM " ÚÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 07 COLUMN 59 PIC X(14) FROM " ³  SAIDAS  ³"
               BACKGROUND-COLOR  04.
           03  LINE 07 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 59 PIC X(14) FROM " ÀÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 61 PIC X(14) FROM SPACES.

       01  TLA-002.
           03  LINE 05 COLUMN 07 PIC X(46) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 07 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 08 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 09 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 15 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 16 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 07 PIC X(46) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"    BACKGROUND-COLOR 1.
           03  LINE 17 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 07 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 1.
           03  LINE 18 COLUMN 11 PIC X(21) FROM "Total Lancado ......:"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.
           03  LINE 18 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 07 PIC X(46) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR 1.
           03  LINE 19 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 09 PIC X(46) FROM SPACES.

       01  ENT-001.
           03  LINE 07 COLUMN 11 PIC X(38) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 11 PIC X(38) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 11 PIC X(38) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 11 PIC X(38) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 11 PIC X(38) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 11 PIC X(38) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 11 PIC X(38) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 11 PIC X(38) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 11 PIC X(38) USING LIN-009
               BACKGROUND-COLOR  01.

       01  ENT-002  LINE 18 COLUMN 32 PIC ZZ.ZZZ.ZZZ.ZZ9,99 USING
                    ACM-VAL BACKGROUND-COLOR 01 FOREGROUND-COLOR 15.

       01  ENT-003.
           03  VL1-ENT LINE 07 COLUMN 33 PIC Z.ZZZ.ZZZ.ZZ9,99
                       USING   VAL-AUX   BACKGROUND-COLOR  01.
           03  VL2-ENT LINE 08 COLUMN 33 PIC Z.ZZZ.ZZZ.ZZ9,99
                       USING   VAL-AUX   BACKGROUND-COLOR  01.
           03  VL3-ENT LINE 09 COLUMN 33 PIC Z.ZZZ.ZZZ.ZZ9,99
                       USING   VAL-AUX   BACKGROUND-COLOR  01.
           03  VL4-ENT LINE 10 COLUMN 33 PIC Z.ZZZ.ZZZ.ZZ9,99
                       USING   VAL-AUX   BACKGROUND-COLOR  01.
           03  VL5-ENT LINE 11 COLUMN 33 PIC Z.ZZZ.ZZZ.ZZ9,99
                       USING   VAL-AUX   BACKGROUND-COLOR  01.
           03  VL6-ENT LINE 12 COLUMN 33 PIC Z.ZZZ.ZZZ.ZZ9,99
                       USING   VAL-AUX   BACKGROUND-COLOR  01.
           03  VL7-ENT LINE 13 COLUMN 33 PIC Z.ZZZ.ZZZ.ZZ9,99
                       USING   VAL-AUX   BACKGROUND-COLOR  01.
           03  VL8-ENT LINE 14 COLUMN 33 PIC Z.ZZZ.ZZZ.ZZ9,99
                       USING   VAL-AUX   BACKGROUND-COLOR  01.
           03  VL9-ENT LINE 15 COLUMN 33 PIC Z.ZZZ.ZZZ.ZZ9,99
                       USING   VAL-AUX   BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LINE 05 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.

       01  LPA-002.
           03  LPA-VL1 LINE 07 COLUMN 33 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VL2 LINE 08 COLUMN 33 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VL3 LINE 09 COLUMN 33 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VL4 LINE 10 COLUMN 33 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VL5 LINE 11 COLUMN 33 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VL6 LINE 12 COLUMN 33 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VL7 LINE 13 COLUMN 33 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VL8 LINE 14 COLUMN 33 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VL9 LINE 15 COLUMN 33 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTO ...                          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR DO M
                   "OVIMENTO  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS LANCAMENTOS  -  [ESC] RETORNA ... "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05.

       PROCEDURE       DIVISION  CHAINING  PRM-002.

       ROT-000-000.
           DISPLAY  LPA-001  TLA-001
                    TLA-002  MEN-001
           OPEN     I-O      CADPRS
           MOVE     SPACES   TO  ENT-AUX  CHV-PRS
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     5        TO  ATV-PRS
           MOVE     ZEROS    TO  ACM-VAL  IND1
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-000-010.
           READ     CADPRS   NEXT
           IF  ATV-PRS   >   5
               GO  TO        ROT-100-000.
           ADD      1        TO  IND1
           ADD      VDD-PRS  TO  ACM-VAL
           MOVE     DES-PRS  TO  DES-AUX
           MOVE     VDD-PRS  TO  VDD-AUX
           IF  IND1  =  1   MOVE REG-AUX  TO  LIN-001.
           IF  IND1  =  2   MOVE REG-AUX  TO  LIN-002.
           IF  IND1  =  3   MOVE REG-AUX  TO  LIN-003.
           IF  IND1  =  4   MOVE REG-AUX  TO  LIN-004.
           IF  IND1  =  5   MOVE REG-AUX  TO  LIN-005.
           IF  IND1  =  6   MOVE REG-AUX  TO  LIN-006.
           IF  IND1  =  7   MOVE REG-AUX  TO  LIN-007.
           IF  IND1  =  8   MOVE REG-AUX  TO  LIN-008.
           IF  IND1  =  9   MOVE REG-AUX  TO  LIN-009.
           GO  TO   ROT-000-010.

       ROT-100-000.
           MOVE     6        TO  LIN
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     5        TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS
           DISPLAY  ENT-001      ENT-002  MEN-002.

       ROT-100-010.
           READ     CADPRS   NEXT
           IF  ATV-PRS   >   5
               GO  TO        ROT-200-030.
           ADD      1        TO  LIN.

       ROT-100-020.
           MOVE     VDD-PRS  TO  VAL-AUX
           IF  LIN   =   07      DISPLAY    LPA-VL1.
           IF  LIN   =   08      DISPLAY    LPA-VL2.
           IF  LIN   =   09      DISPLAY    LPA-VL3.
           IF  LIN   =   10      DISPLAY    LPA-VL4.
           IF  LIN   =   11      DISPLAY    LPA-VL5.
           IF  LIN   =   12      DISPLAY    LPA-VL6.
           IF  LIN   =   13      DISPLAY    LPA-VL7.
           IF  LIN   =   14      DISPLAY    LPA-VL8.
           IF  LIN   =   15      DISPLAY    LPA-VL9.

       ROT-100-030.
           ACCEPT  (LIN, 36) VAL-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  LIN   =   07  DISPLAY        VL1-ENT.
           IF  LIN   =   08  DISPLAY        VL2-ENT.
           IF  LIN   =   09  DISPLAY        VL3-ENT.
           IF  LIN   =   10  DISPLAY        VL4-ENT.
           IF  LIN   =   11  DISPLAY        VL5-ENT.
           IF  LIN   =   12  DISPLAY        VL6-ENT.
           IF  LIN   =   13  DISPLAY        VL7-ENT.
           IF  LIN   =   14  DISPLAY        VL8-ENT.
           IF  LIN   =   15  DISPLAY        VL9-ENT.
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.
           COMPUTE  ACM-VAL  =   ACM-VAL
                 +  VAL-AUX  -   VDD-PRS
           MOVE     VAL-AUX  TO  VDD-PRS
           REWRITE  REG-PRS
           DISPLAY  ENT-002
           GO  TO   ROT-100-010.

       ROT-100-040.
           COMPUTE  LIN-PRS  =   LIN-PRS  -  1
           READ     CADPRS   INVALID KEY
                    GO  TO   ROT-200-000.
           COMPUTE  LIN      =   LIN - 1
           GO  TO   ROT-100-020.

       ROT-200-000.
           MOVE     4        TO  FLG-AUX.

       ROT-200-010.
           MOVE     FLG-AUX  TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   FLG-AUX
               GO  TO        ROT-200-020.
           COMPUTE  FLG-AUX  =   FLG-AUX  -  1
           IF  FLG-AUX   >   0
               GO  TO        ROT-200-010.
           CLOSE    CADPRS
           CHAIN   "DYN201".

       ROT-200-020.
           IF  ATV-PRS   =   1  CHAIN  "DYN202"  USING  PRM-002.
           IF  ATV-PRS   =   2  CHAIN  "DYN203"  USING  PRM-002.
           IF  ATV-PRS   =   3  CHAIN  "DYN204"  USING  PRM-002.
           IF  ATV-PRS   =   4  CHAIN  "DYN205"  USING  PRM-002.

       ROT-200-030.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-003
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.

       ROT-200-040.
           IF  ATV-PRS   =   6  CHAIN  "DYN207"  USING  PRM-002.
           IF  ATV-PRS   =   7  CHAIN  "DYN208"  USING  PRM-002.
           IF  ATV-PRS   =   8  CHAIN  "DYN209"  USING  PRM-002.