       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN208.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADBCO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-BCO.

           SELECT      CADLCX        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCX.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADBCO      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADBCO.DAT".
       01  REG-BCO.
           03  CHV-BCO          PIC  9(03).
           03  NOM-BCO          PIC  X(15).
           03  SDI-BCO          PIC S9(11)V99.

       FD  CADLCX      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLCX.DAT".
       01  REG-LCX.
           03  CHV-LCX.
               05  TUR-LCX      PIC  9(01).
               05  CXA-LCX      PIC  9(01).
               05  ATV-LCX      PIC  9(01).
               05  PRX-LCX      PIC  9(01).
               05  SUF-LCX      PIC  9(03).
               05  LCT-LCX      PIC  9(03).
           03  DES-LCX          PIC  X(15).
           03  HST-LCX          PIC  X(20).
           03  QTD-LCX          PIC  9(06)V9.
           03  VAL-LCX          PIC  9(10)V99.

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
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  FLG-AUX          PIC  9(01).

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 08 COLUMN 60 PIC X(15) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 60 PIC X(15) FROM " ³ DEPOSITOS ³"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 75 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 60 PIC X(15) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 75 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 62 PIC X(15) FROM SPACES.

       01  TLA-002.
           03  LINE 07 COLUMN 06 PIC X(48) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"  BACKGROUND-COLOR 1.
           03  LINE 08 COLUMN 06 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR 1.
           03  LINE 08 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 06 PIC X(48) FROM " ³  Codigo ........:
               "   -                   ³"  BACKGROUND-COLOR 1.
           03  LINE 09 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 06 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 06 PIC X(48) FROM " ³  Historico .....:
               "                       ³"  BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 06 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 06 PIC X(48) FROM " ³  Valor .........:
               "                       ³"  BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 06 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 06 PIC X(48) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"  BACKGROUND-COLOR 1.
           03  LINE 15 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 06 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR 1.
           03  LINE 16 COLUMN 10 PIC X(16) FROM "Total Lancado .:"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.
           03  LINE 16 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 06 PIC X(48) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"  BACKGROUND-COLOR 1.
           03  LINE 17 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 08 PIC X(48) FROM SPACES.

       01  ENT-001 LINE 16 COLUMN 34 PIC Z.ZZZ.ZZZ.ZZ9,99 USING
           VDD-PRS BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.

       01  ENT-002.
           03  COD-ENT LINE 09 COLUMN 27 PIC 9(03) USING CHV-BCO
                       BACKGROUND-COLOR  01.
           03  NOM-ENT LINE 09 COLUMN 35 PIC X(15) USING NOM-BCO
                       BACKGROUND-COLOR  01.
           03  HST-ENT LINE 11 COLUMN 27 PIC X(20) USING HST-LCX
                       BACKGROUND-COLOR  01.
           03  VAL-ENT LINE 13 COLUMN 27 PIC Z.ZZZ.ZZZ.ZZ9,99
                       USING   VAL-LCX   BACKGROUND-COLOR  01.

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
           03  LPA-NOM LINE 09 COLUMN 35 PIC X(15) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-HST LINE 11 COLUMN 27 PIC X(20) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VAL LINE 13 COLUMN 27 PIC X(16) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO DEPOSITOS ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "BANCO  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O HISTORICO
                   "DO DEPOSITO  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR DEPO
                   "SITADO  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-002.

       ROT-000-000.
           DISPLAY  LPA-001  TLA-001
                    TLA-002  MEN-001
           OPEN     INPUT    CADBCO
                    I-O      CADPRS  CADLCX
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     7        TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-000-010.
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   7
               DISPLAY       ENT-001
               GO  TO        ROT-100-000.
           IF  ATV-PRS   =   8   CHAIN  "DYN209"  USING  PRM-002.

       ROT-100-000.
           DISPLAY  LPA-HST
                    LPA-NOM  MEN-002
           MOVE     0   TO   CHV-BCO
           ACCEPT   (09 27)  CHV-BCO  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           IF  CHV-BCO   =   ZEROS
               GO  TO        ROT-000-010.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADBCO   INVALID KEY
                    DISPLAY  MEN-003
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-000.
           DISPLAY  COD-ENT  NOM-ENT
           MOVE     SPACES   TO  HST-LCX
           MOVE     ZEROS    TO  VAL-LCX.

       ROT-100-010.
           DISPLAY  LPA-VAL  MEN-004
           ACCEPT   (11 27)  HST-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           IF  HST-LCX   =   SPACES
               GO  TO        ROT-100-010.
           DISPLAY  HST-ENT.

       ROT-100-020.
           DISPLAY  LPA-VAL  MEN-005
           ACCEPT   (13 27)  VAL-LCX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           IF  VAL-LCX   =   ZEROS
               GO  TO        ROT-100-020.
           DISPLAY  VAL-ENT  MEN-006
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           ADD      VAL-LCX  TO  VDD-PRS
           MOVE     TUR-PR2  TO  TUR-LCX
           MOVE     CXA-PR2  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     CHV-BCO  TO  SUF-LCX
           MOVE     DES-PRS  TO  DES-LCX
           MOVE     ZEROS    TO  QTD-LCX  LCT-LCX.

       ROT-100-030.
           ADD      1        TO  LCT-LCX
           WRITE    REG-LCX  INVALID KEY
                    GO  TO   ROT-100-030.
           REWRITE  REG-PRS
           DISPLAY  LPA-002  ENT-001
           GO  TO   ROT-100-000.

       ROT-200-000.
           COMPUTE  LIN-PRS  =   LIN-PRS  -  1
           READ     CADPRS   INVALID KEY
                    MOVE  6  TO  FLG-AUX
                    GO  TO   ROT-200-010.
           DISPLAY  ENT-001
           GO  TO   ROT-100-000.

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
           CLOSE    CADBCO
                    CADLCX   CADPRS
           CHAIN   "DYN201".

       ROT-200-020.
           IF  ATV-PRS   =   1  CHAIN  "DYN202"  USING  PRM-002.
           IF  ATV-PRS   =   2  CHAIN  "DYN203"  USING  PRM-002.
           IF  ATV-PRS   =   3  CHAIN  "DYN204"  USING  PRM-002.
           IF  ATV-PRS   =   4  CHAIN  "DYN205"  USING  PRM-002.
           IF  ATV-PRS   =   5  CHAIN  "DYN206"  USING  PRM-002.
           IF  ATV-PRS   =   6  CHAIN  "DYN207"  USING  PRM-002.