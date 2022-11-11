       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN203.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST.

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

       FD  CADEST      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADEST.DAT".
       01  REG-EST.
           03  CHV-EST.
               05  PRX-EST      PIC  9(01).
               05  SUF-EST      PIC  9(03).
           03  DES-EST          PIC  X(25).
           03  MIN-EST          PIC  9(06).
           03  SDO-EST          PIC S9(06)V9.
           03  CST-EST          PIC  9(07)V9999.
           03  CTM-EST          PIC  9(07)V9999.
           03  VDA-EST          PIC  9(07)V99.
           03  PCT-EST          PIC S9(03)V99.
           03  QVM-EST          PIC  9(07)V9.
           03  QVA-EST          PIC  9(08)V9.

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
           03  QTD-AUX          PIC  9(06)V9.

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 07 COLUMN 62 PIC X(14) FROM " ÚÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 62 PIC X(14) FROM " ³  VENDAS  ³"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 62 PIC X(14) FROM " ÀÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 64 PIC X(14) FROM SPACES.

       01  TLA-002.
           03  LINE 06 COLUMN 05 PIC X(52) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR 1.
           03  LINE 07 COLUMN 05 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 07 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 05 PIC X(52) FROM " ³  Codigo ..........:
               " -     -                   ³"   BACKGROUND-COLOR 1.
           03  LINE 08 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 05 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 09 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 05 PIC X(52) FROM " ³  Descricao .......:
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 05 PIC X(52) FROM " ³  Saldo Atual .....:
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 05 PIC X(52) FROM " ³  Preco Unitario ..:
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 05 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 05 PIC X(52) FROM " ³  Quantidade ......:
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 05 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 15 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 05 PIC X(52) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"   BACKGROUND-COLOR 1.
           03  LINE 16 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 05 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 17 COLUMN 09 PIC X(18) FROM "Total Lancado ...:
               BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.
           03  LINE 17 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 05 PIC X(52) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR 1.
           03  LINE 18 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 07 PIC X(52) FROM SPACES.

       01  ENT-001.
           03  PRX-ENT LINE 08 COLUMN 28 PIC 9(01) USING PRX-PRS
                       BACKGROUND-COLOR  01.
           03  ATV-ENT LINE 08 COLUMN 38 PIC X(15) USING DES-PRS
                       BACKGROUND-COLOR  01.
           03  TOT-ENT LINE 17 COLUMN 37 PIC Z.ZZZ.ZZZ.ZZ9,99 USING
               VDD-PRS BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.

       01  ENT-002.
           03  SUF-ENT LINE 08 COLUMN 30 PIC 9(03)        USING SUF-EST
                       BACKGROUND-COLOR  01.
           03  DES-ENT LINE 10 COLUMN 28 PIC X(25)        USING DES-EST
                       BACKGROUND-COLOR  01.
           03  SDO-ENT LINE 11 COLUMN 27 PIC ----.--9,9   USING SDO-EST
                       BACKGROUND-COLOR  01.
           03  VDA-ENT LINE 12 COLUMN 28 PIC Z.ZZZ.ZZ9,99 USING VDA-EST
                       BACKGROUND-COLOR  01.

       01  ENT-003  LINE 14 COLUMN 28 PIC ZZZ.ZZ9,9 USING QTD-LCX
                    BACKGROUND-COLOR  01.

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
           03  LPA-DES LINE 10 COLUMN 28 PIC X(25) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SDO LINE 11 COLUMN 27 PIC X(10) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VDA LINE 12 COLUMN 28 PIC X(12) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-QTD LINE 14 COLUMN 28 PIC X(09) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67)  FROM  "AGUARDE !!!  PESQ
                   "UISANDO PRODUTOS ...                        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67)  FROM  "DIGITE O CODIGO D
                   "O PRODUTO VENDIDO  -  [ESC] RETORNA ...     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67)  FROM  "CODIGO INEXISTENT
                   "E !!!  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67)  FROM  "DIGITE A QUANTIDA
                   "DE VENDIDA  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67)  FROM  "TECLE [ENTER] P/
                   "CONFIRMAR A VENDA  -  [ESC] RETORNA ...     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-002.

       ROT-000-000.
           DISPLAY  LPA-001  TLA-001
                    TLA-002  MEN-001
           OPEN     INPUT    CADEST
                    I-O      CADPRS  CADLCX
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     2        TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-000-010.
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   2
               DISPLAY       ENT-001
               GO  TO        ROT-100-000.
           IF  ATV-PRS   =   3   CHAIN  "DYN204"  USING  PRM-002.
           IF  ATV-PRS   =   4   CHAIN  "DYN205"  USING  PRM-002.
           IF  ATV-PRS   =   5   CHAIN  "DYN206"  USING  PRM-002.
           IF  ATV-PRS   =   6   CHAIN  "DYN207"  USING  PRM-002.
           IF  ATV-PRS   =   7   CHAIN  "DYN208"  USING  PRM-002.
           IF  ATV-PRS   =   8   CHAIN  "DYN209"  USING  PRM-002.

       ROT-100-000.
           DISPLAY  MEN-002
           MOVE     0   TO   SUF-EST
           ACCEPT   (08 30)  SUF-EST  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           IF  SUF-EST   =   ZEROS
               GO  TO        ROT-000-010.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     PRX-PRS  TO  PRX-EST
           READ     CADEST   INVALID KEY
                    DISPLAY  MEN-003
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-000.
           DISPLAY  ENT-002
           MOVE     TUR-PR2  TO  TUR-LCX
           MOVE     CXA-PR2  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     SUF-EST  TO  SUF-LCX
           MOVE     ZEROS    TO  LCT-LCX
           READ     CADLCX   INVALID KEY
                    GO  TO   ROT-100-030.
           MOVE     QTD-LCX  TO  QTD-AUX.

       ROT-100-010.
           DISPLAY  LPA-QTD  MEN-004
           ACCEPT   (14 28)  QTD-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-002
               GO  TO        ROT-100-000.
           MOVE     " "  TO  RSP-OPC
           DISPLAY  ENT-003  MEN-005
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           COMPUTE  VAL-LCX  =   QTD-LCX  *  VDA-EST
           COMPUTE  QTD-PRS  =   QTD-PRS  -  QTD-AUX  +  QTD-LCX
           COMPUTE  CTD-PRS  =   CTD-PRS  -  QTD-AUX  *  CTM-EST
                                          +  QTD-LCX  *  CTM-EST
           COMPUTE  VDD-PRS  =   VDD-PRS  -  QTD-AUX  *  VDA-EST
                                          +  QTD-LCX  *  VDA-EST
           REWRITE  REG-PRS
           DISPLAY  LPA-002  TOT-ENT
           IF  QTD-LCX   =   0
               GO  TO        ROT-100-020.
           REWRITE  REG-LCX
           GO  TO   ROT-100-000.

       ROT-100-020.
           DELETE   CADLCX
           CLOSE    CADLCX
           OPEN     I-O  CADLCX
           GO  TO   ROT-100-000.

       ROT-100-030.
           MOVE     ZEROS   TO  LCT-LCX
                                QTD-LCX
           MOVE     SPACES  TO  HST-LCX
           MOVE     DES-PRS TO  DES-LCX.

       ROT-100-040.
           DISPLAY  LPA-QTD  MEN-004
           ACCEPT   (14 28)  QTD-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-002
               GO  TO        ROT-100-000.
           IF  QTD-LCX   =   ZEROS
               GO  TO        ROT-100-040.
           MOVE     " "  TO  RSP-OPC
           DISPLAY  ENT-003  MEN-005
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.
           COMPUTE  VAL-LCX  =   QTD-LCX  *  VDA-EST
           COMPUTE  QTD-PRS  =   QTD-PRS  +  QTD-LCX
           COMPUTE  CTD-PRS  =   CTD-PRS  +  QTD-LCX  *  CTM-EST
           COMPUTE  VDD-PRS  =   VDD-PRS  +  QTD-LCX  *  VDA-EST
           WRITE    REG-LCX
           REWRITE  REG-PRS
           DISPLAY  LPA-002  TOT-ENT
           GO  TO   ROT-100-000.

       ROT-200-000.
           COMPUTE  LIN-PRS  =   LIN-PRS  -  1
           READ     CADPRS   INVALID KEY
                    GO  TO   ROT-200-010.
           DISPLAY  ENT-001
           GO  TO   ROT-100-000.

       ROT-200-010.
           MOVE     1        TO  FLG-AUX.

       ROT-200-020.
           MOVE     FLG-AUX  TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   FLG-AUX
               GO  TO        ROT-200-030.
           COMPUTE  FLG-AUX  =   FLG-AUX  -  1
           IF  FLG-AUX   >   0
               GO  TO        ROT-200-020.
           CLOSE    CADEST
                    CADLCX   CADPRS
           CHAIN   "DYN201".

       ROT-200-030.
           IF  ATV-PRS   =   1   CHAIN  "DYN202"  USING  PRM-002.