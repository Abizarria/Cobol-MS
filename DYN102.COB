       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN102.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADATV        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ATV.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST.

           SELECT      CADLCP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCP.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADATV      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADATV.DAT".
       01  REG-ATV.
           03  CHV-ATV          PIC  9(01).
           03  DES-ATV          PIC  X(15).

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

       FD  CADLCP      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLCP.DAT".
       01  REG-LCP.
           03  CHV-LCP.
               05  PRX-LCP      PIC  9(01).
               05  SUF-LCP      PIC  9(03).
               05  DTA-LCP      PIC  9(06).
               05  LCT-LCP      PIC  9(03).
           03  NTF-LCP          PIC  9(06).
           03  QTD-LCP          PIC S9(06).
           03  CST-LCP          PIC  9(07)V9999.

       FD  CADLDP      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLDP.DAT".
       01  REG-LDP.
           03  CHV-LDP.
               05  PRX-LDP      PIC  9(01).
               05  SUF-LDP      PIC  9(03).
               05  DTA-LDP      PIC  9(06).
               05  LCT-LDP      PIC  9(03).
           03  FLG-LDP          PIC  9(01).
           03  PGT-LDP          PIC  9(06).
           03  HST-LDP          PIC  X(20).
           03  VAL-LDP          PIC S9(10)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  TOT-AUX          PIC S9(10)V99.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  HST-AUX.
               05  FILLER       PIC  X(13)  VALUE  "COMPRAS - NF".
               05  NTF-AUX      PIC  999.999.

           03  SUF-AUX.
               05  SF1-AUX      PIC  9(01).
               05  SF2-AUX      PIC  9(03).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "LANCAMENTO DE COMPRAS  -  Ver. 7.11"
               BACKGROUND-COLOR  02 FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 11 PIC X(57) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 11 PIC X(57) FROM " ³
               "                                ³" BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 11 PIC X(57) FROM " ³ Codigo .....:  -
               " -                              ³" BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 11 PIC X(57) FROM " ³
               "                                ³" BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 11 PIC X(57) FROM " ³ Descricao ..:
               "                                ³" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 11 PIC X(57) FROM " ³
               "                                ³" BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 11 PIC X(57) FROM " ³ Custo Unit. :
               "                                ³" BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 11 PIC X(57) FROM " ³ Pre‡o Venda :
               "                                ³" BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 11 PIC X(57) FROM " ³
               "                                ³" BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 11 PIC X(57) FROM " ³ Quantidade .:
               "         Nota Fiscal :          ³" BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 11 PIC X(57) FROM " ³ Tot. Compra :
               "         Vencimento .:   /  /   ³" BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 11 PIC X(57) FROM " ³
               "                                ³" BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 11 PIC X(57) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 13 PIC X(57) FROM SPACES.

       01  ENT-001.
           03  PRX-ENT LINE 08 COLUMN 28 PIC 9(01) USING CHV-ATV
                       BACKGROUND-COLOR  01.
           03  SUF-ENT LINE 08 COLUMN 30 PIC 9(03) USING SUF-EST
                       BACKGROUND-COLOR  01.
           03  ATV-ENT LINE 08 COLUMN 38 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.

       01  ENT-002.
           03  DES-ENT LINE 10 COLUMN 28 PIC X(25) USING DES-EST
                       BACKGROUND-COLOR  01.
           03  CST-ENT LINE 12 COLUMN 28 PIC ZZZZZZ9,9999    USING
               CST-LCP BACKGROUND-COLOR  01.
           03  VDA-ENT LINE 13 COLUMN 28 PIC ZZZZZZ9,99      USING
               VDA-EST BACKGROUND-COLOR  01.
           03  QTD-ENT LINE 15 COLUMN 27 PIC ------9         USING
               QTD-LCP BACKGROUND-COLOR  01.
           03  TOT-ENT LINE 16 COLUMN 27 PIC ----------9,99  USING
               TOT-AUX BACKGROUND-COLOR  01.
           03  NTF-ENT LINE 15 COLUMN 57 PIC 9(06) USING NTF-LCP
                       BACKGROUND-COLOR  01.
           03  DIA-ENT LINE 16 COLUMN 57 PIC 9(02) USING DIA-AUX
                       BACKGROUND-COLOR  01.
           03  MES-ENT LINE 16 COLUMN 60 PIC 9(02) USING MES-AUX
                       BACKGROUND-COLOR  01.
           03  ANO-ENT LINE 16 COLUMN 63 PIC 9(02) USING ANO-AUX
                       BACKGROUND-COLOR  01.

       01  ENT-003  LINE 16 COLUMN 57 PIC X(07) FROM "A VISTA"
                    BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-SUF LINE 08 COLUMN 30 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-ATV LINE 08 COLUMN 38 PIC X(15) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  LPA-002.
           03  LPA-DES LINE 10 COLUMN 28 PIC X(25) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CST LINE 12 COLUMN 28 PIC X(12) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VDA LINE 13 COLUMN 28 PIC X(10) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-QTD LINE 15 COLUMN 27 PIC X(07) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-TOT LINE 16 COLUMN 27 PIC X(14) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-NTF LINE 15 COLUMN 57 PIC X(06) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-DIA LINE 16 COLUMN 57 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-MES LINE 16 COLUMN 60 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-ANO LINE 16 COLUMN 63 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  LPA-003  LINE 16 COLUMN 57 PIC X(08) FROM "  /  /"
                    BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "ITEM  -  [ESC] RETORNA ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A QUANTIDADE
                   " COMPRADA EM UNIDADES  -  [ESC] RETORNA ...     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CUSTO TOTA
                   "L DA COMPRA  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O PRECO DE C
                   "USTO UNITARIO  -  [ESC] RETORNA ...             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O PRECO DE V
                   "ENDA UNITARIO  -  [ESC] RETORNA ...             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "NOTA FISCAL  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA DO VE
                   "NCIMENTO  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001
           ACCEPT   DTA-SYS      FROM     DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.
           OPEN     INPUT    CADATV
                    I-O      CADEST
                             CADLCP  CADLDP.
       ROT-100-000.
           DISPLAY  LPA-001  MEN-001
           MOVE     0   TO   CHV-ATV
           ACCEPT   (08 28)  CHV-ATV  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           IF  CHV-ATV   =   0
               GO  TO        ROT-100-000.
           READ     CADATV   INVALID KEY
                    GO  TO   ROT-100-000.
           DISPLAY  PRX-ENT      ATV-ENT.

       ROT-100-010.
           DISPLAY  MEN-002  LPA-002
           MOVE     0   TO   SUF-EST
           ACCEPT   (08 30)  SUF-EST  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           IF  SUF-EST   =   ZEROS
               GO  TO        ROT-100-010.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     CHV-ATV  TO  PRX-EST
           READ     CADEST   INVALID KEY
                    GO  TO   ROT-100-140.
           MOVE     CST-EST  TO  CST-LCP
           DISPLAY  SUF-ENT      DES-ENT
           MOVE     ZEROS    TO  QTD-LCP  TOT-AUX
                                 NTF-LCP  DTA-AUX.
       ROT-100-020.
           DISPLAY  MEN-006
           ACCEPT   (12 28)  CST-LCP  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  CST-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.

       ROT-100-030.
           DISPLAY  LPA-VDA  MEN-007
           ACCEPT   (13 28)  VDA-EST  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  VDA-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           IF  VDA-EST   =   ZEROS
               GO  TO        ROT-100-030.

       ROT-100-040.
           DISPLAY  LPA-QTD  MEN-004
           ACCEPT   (15 27)  QTD-LCP  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  QTD-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           IF  QTD-LCP   =   ZEROS
               GO  TO        ROT-100-040.
           COMPUTE  TOT-AUX  =   CST-LCP  *  QTD-LCP
           DISPLAY  TOT-ENT.

       ROT-100-050.
           DISPLAY  LPA-TOT  MEN-005
           ACCEPT   (16 27)  TOT-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  TOT-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.
           IF  TOT-AUX   =   ZEROS
               GO  TO        ROT-100-050.
           COMPUTE  CST-LCP  =   TOT-AUX  /  QTD-LCP
           DISPLAY  CST-ENT.


       ROT-100-060.
           DISPLAY  MEN-008
           ACCEPT   (15 57)  NTF-LCP  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  NTF-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-050.
           IF  NTF-LCP   =   ZEROS
               GO  TO        ROT-100-060.

       ROT-100-070.
           DISPLAY  MEN-009
           ACCEPT   (16 57)  DIA-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  DIA-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-060.
           IF  DIA-AUX   =   0
               DISPLAY       ENT-003
               GO  TO        ROT-100-100.
           IF  DIA-AUX   >   31
               GO  TO        ROT-100-070.

       ROT-100-080.
           ACCEPT   (16 60)  MES-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  MES-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-070.
           IF  MES-AUX   <   1  OR  > 12
               GO  TO        ROT-100-080.

       ROT-100-090.
           DISPLAY  MEN-009
           ACCEPT   (16 63)  ANO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  ANO-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-080.

       ROT-100-100.
           DISPLAY  MEN-010
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =    01
               IF  DIA-AUX   =   0
                   DISPLAY   LPA-003
                   GO  TO    ROT-100-070
               ELSE
                   GO  TO    ROT-100-090.
           COMPUTE  CTM-EST  =  (CTM-EST  *  SDO-EST  +  CST-LCP
                             *   QTD-LCP) / (SDO-EST  +  QTD-LCP)
           COMPUTE  PCT-EST  =  (VDA-EST  -  CTM-EST)
                                 *   100  /  VDA-EST
           ADD      QTD-LCP  TO  SDO-EST
           MOVE     CST-LCP  TO  CST-EST
           REWRITE  REG-EST
           MOVE     PRX-EST  TO  PRX-LCP
           MOVE     SUF-EST  TO  SUF-LCP
           MOVE     DTA-SYS  TO  DTA-LCP
           MOVE     ZEROS    TO  LCT-LCP  SUF-AUX.

       ROT-100-110.
           ADD      1        TO  LCT-LCP
           WRITE    REG-LCP  INVALID KEY
                    GO  TO   ROT-100-110.
           IF  DIA-AUX   =   ZEROS
               GO  TO        ROT-100-130.
           MOVE     6        TO  PRX-LDP
           MOVE     PRX-EST  TO  SF1-AUX
           MOVE     SUF-AUX  TO  SUF-LDP
           MOVE     NTF-LCP  TO  NTF-AUX
           MOVE     DTA-SYS  TO  DTA-LDP
           MOVE     DTA-AUX  TO  PGT-LDP
           MOVE     HST-AUX  TO  HST-LDP
           MOVE     TOT-AUX  TO  VAL-LDP
           MOVE     1        TO  FLG-LDP
           MOVE     ZEROS    TO  LCT-LDP.

       ROT-100-120.
           ADD      1        TO  LCT-LDP
           WRITE    REG-LDP  INVALID KEY
                    GO  TO   ROT-100-120.

       ROT-100-130.
           DISPLAY  LPA-002
                    LPA-003
           GO  TO   ROT-100-010.

       ROT-100-140.
           DISPLAY  MEN-003
           ACCEPT   OPC-001
           MOVE     0   TO   SUF-EST
           GO  TO   ROT-100-010.

       ROT-200-000.
           MOVE     2    TO  PRM-001
           CLOSE    CADATV   CADEST
                    CADLCP   CADLDP
           CHAIN   "DYN100"  USING   PRM-001.
