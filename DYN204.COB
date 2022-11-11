       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN204.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

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
           03  VAL-AUX          PIC  9(07)V99.
           03  TOT-AUX          PIC  9(10)V99.

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 07 COLUMN 62 PIC X(14) FROM " ÚÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 62 PIC X(14) FROM " ³ SERVICOS ³"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 62 PIC X(14) FROM " ÀÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 64 PIC X(14) FROM SPACES.

       01  TLA-002.
           03  LINE 06 COLUMN 04 PIC X(52) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR 1.
           03  LINE 07 COLUMN 04 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 07 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 04 PIC X(52) FROM " ³  Codigo ..........:
               " -     -                   ³"   BACKGROUND-COLOR 1.
           03  LINE 08 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 04 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 09 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 04 PIC X(52) FROM " ³  Historico .......:
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 04 PIC X(52) FROM " ³  Quantidade ......:
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 04 PIC X(52) FROM " ³  Preco Unitario ..:
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 04 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 04 PIC X(52) FROM " ³  Valor Total .....:
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 04 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 15 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 04 PIC X(52) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"   BACKGROUND-COLOR 1.
           03  LINE 16 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 04 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR 1.
           03  LINE 17 COLUMN 08 PIC X(18) FROM "Total Lancado ...:"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.
           03  LINE 17 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 04 PIC X(52) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR 1.
           03  LINE 18 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 06 PIC X(52) FROM SPACES.

       01  ENT-001.
           03  PRX-ENT LINE 08 COLUMN 27 PIC 9(01) USING PRX-PRS
                       BACKGROUND-COLOR  01.
           03  ATV-ENT LINE 08 COLUMN 37 PIC X(15) USING DES-PRS
                       BACKGROUND-COLOR  01.
           03  TOT-ENT LINE 17 COLUMN 36 PIC Z.ZZZ.ZZZ.ZZ9,99 USING
               VDD-PRS BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.

       01  ENT-002.
           03  SUF-ENT LINE 08 COLUMN 29 PIC 9(03) USING SUF-LCX
                       BACKGROUND-COLOR  01.
           03  HST-ENT LINE 10 COLUMN 27 PIC X(25) USING HST-LCX
                       BACKGROUND-COLOR  01.
           03  QTD-ENT LINE 11 COLUMN 27 PIC ZZZ.ZZ9,9
                       USING   QTD-LCX   BACKGROUND-COLOR  01.
           03  PRC-ENT LINE 12 COLUMN 27 PIC Z.ZZZ.ZZ9,99
                       USING   VAL-AUX   BACKGROUND-COLOR  01.
           03  VAL-ENT LINE 14 COLUMN 27 PIC Z.ZZZ.ZZZ.ZZ9,99
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
           03  LPA-HST LINE 10 COLUMN 27 PIC X(25) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-QTD LINE 11 COLUMN 27 PIC X(09) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-PRC LINE 12 COLUMN 27 PIC X(12) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VAL LINE 14 COLUMN 27 PIC X(16) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO SERVICOS ...                        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "LANCAMENTO  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O HISTORICO
                   "DO SERVICO  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A QUANTIDADE
                   " DE SERVICOS  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR UNIT
                   "ARIO DO SERVICO  -  [ESC] RETORNA ...     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR TOTA
                   "L DO SERVICO  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR O LANCAMENTO  -  [ESC] RETORNA ..."
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-002.

       ROT-000-000.
           DISPLAY  LPA-001  TLA-001
                    TLA-002  MEN-001
           OPEN     I-O      CADLCX
                             CADPRS
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     3        TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-000-010.
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   3
               DISPLAY       ENT-001
               GO  TO        ROT-100-000.
           IF  ATV-PRS   =   4   CHAIN  "DYN205"  USING  PRM-002.
           IF  ATV-PRS   =   5   CHAIN  "DYN206"  USING  PRM-002.
           IF  ATV-PRS   =   6   CHAIN  "DYN207"  USING  PRM-002.
           IF  ATV-PRS   =   7   CHAIN  "DYN208"  USING  PRM-002.
           IF  ATV-PRS   =   8   CHAIN  "DYN209"  USING  PRM-002.

       ROT-100-000.
           DISPLAY  LPA-002  MEN-002
           MOVE     0   TO   SUF-LCX
           ACCEPT   (08 29)  SUF-LCX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           IF  SUF-LCX   =   ZEROS
               GO  TO        ROT-000-010.
           MOVE     TUR-PR2  TO  TUR-LCX
           MOVE     CXA-PR2  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     ZEROS    TO  LCT-LCX
           READ     CADLCX   INVALID KEY
                    GO  TO   ROT-100-100.
           MOVE     QTD-LCX  TO  QTD-AUX
           MOVE     VAL-LCX  TO  TOT-AUX
           COMPUTE  VAL-AUX  =   VAL-LCX  /  QTD-LCX
           DISPLAY  ENT-002.

       ROT-100-010.
           DISPLAY  MEN-003
           ACCEPT   (10 27)  HST-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  HST-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.

       ROT-100-020.
           DISPLAY  LPA-QTD  MEN-004
           ACCEPT   (11 27)  QTD-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  QTD-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.

       ROT-100-030.
           DISPLAY  LPA-PRC  MEN-005
           ACCEPT   (12 27)  VAL-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  PRC-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           COMPUTE  VAL-LCX  =   QTD-LCX  *  VAL-AUX.

       ROT-100-040.
           DISPLAY  LPA-VAL  MEN-006
           ACCEPT   (14 27)  VAL-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  VAL-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           COMPUTE  VAL-AUX  =   VAL-LCX  /  QTD-LCX
           DISPLAY  PRC-ENT.

       ROT-100-050.
           MOVE     " "  TO  RSP-OPC
           DISPLAY  MEN-007
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.
           COMPUTE  QTD-PRS  =   QTD-PRS  -  QTD-AUX  +  QTD-LCX
           COMPUTE  VDD-PRS  =   VDD-PRS  -  TOT-AUX  +  VAL-LCX
           REWRITE  REG-PRS
           DISPLAY  LPA-002  TOT-ENT
           IF  QTD-LCX   =   0   AND
               VAL-LCX   =   0
               GO  TO        ROT-100-060.
           REWRITE  REG-LCX
           GO  TO   ROT-100-000.

       ROT-100-060.
           DELETE   CADLCX
           CLOSE    CADLCX
           OPEN     I-O  CADLCX
           GO  TO   ROT-100-000.

       ROT-100-100.
           DISPLAY  SUF-ENT
           MOVE     DES-PRS  TO  DES-LCX
           MOVE     SPACES   TO  HST-LCX
           MOVE     ZEROS    TO  QTD-LCX  LCT-LCX
                                 VAL-LCX  VAL-AUX.
       ROT-100-110.
           DISPLAY  LPA-QTD  MEN-003
           ACCEPT   (10 27)  HST-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           DISPLAY  HST-ENT.

       ROT-100-120.
           DISPLAY  LPA-PRC
                    LPA-QTD  MEN-004
           ACCEPT   (11 27)  QTD-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-110.
           IF  QTD-LCX   =   ZEROS
               GO  TO        ROT-100-120.
           DISPLAY  QTD-ENT.

       ROT-100-130.
           DISPLAY  LPA-VAL
                    LPA-PRC  MEN-005
           ACCEPT   (12 27)  VAL-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-120.
           COMPUTE  VAL-LCX  =   QTD-LCX  *  VAL-AUX
           DISPLAY  PRC-ENT.

       ROT-100-140.
           DISPLAY  LPA-VAL  MEN-006
           ACCEPT   (14 27)  VAL-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-130.
           IF  VAL-LCX   =   ZEROS
               GO  TO        ROT-100-130.
           MOVE     SPACES   TO  RSP-OPC
           COMPUTE  VAL-AUX  =   VAL-LCX  /  QTD-LCX
           DISPLAY  PRC-ENT      VAL-ENT     MEN-007
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-140.
           ADD      QTD-LCX  TO  QTD-PRS
           ADD      VAL-LCX  TO  VDD-PRS
           WRITE    REG-LCX
           REWRITE  REG-PRS
           DISPLAY  TOT-ENT
           GO  TO   ROT-100-000.

       ROT-200-000.
           COMPUTE  LIN-PRS  =   LIN-PRS  -  1
           READ     CADPRS   INVALID KEY
                    GO  TO   ROT-200-010.
           DISPLAY  ENT-001
           GO  TO   ROT-100-000.

       ROT-200-010.
           MOVE     2        TO  FLG-AUX.

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
           CLOSE    CADLCX   CADPRS
           CHAIN   "DYN201".

       ROT-200-030.
           IF  ATV-PRS   =   1  CHAIN  "DYN202"  USING  PRM-002.
           IF  ATV-PRS   =   2  CHAIN  "DYN203"  USING  PRM-002.