       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN103.
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

           SELECT      CADLES        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LES.

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

       FD  CADLES      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLES.DAT".
       01  REG-LES.
           03  CHV-LES.
               05  PRX-LES      PIC  9(01).
               05  SUF-LES      PIC  9(03).
               05  DTA-LES      PIC  9(06).
               05  LCT-LES      PIC  9(03).
           03  SDA-LES          PIC S9(06)V9.
           03  SDC-LES          PIC  9(06)V9.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "ACERTOS DE SALDOS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 15 PIC X(49) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 15 PIC X(49) FROM " ³
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 15 PIC X(49) FROM " ³  Codigo .......:  -
               "    -                   ³" BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 15 PIC X(49) FROM " ³
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 15 PIC X(49) FROM " ³  Descricao ....:
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 15 PIC X(49) FROM " ³
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 15 PIC X(49) FROM " ³  Saldo Atual ..:
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 15 PIC X(49) FROM " ³
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 15 PIC X(49) FROM " ³  Saldo Correto :
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 15 PIC X(49) FROM " ³
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 15 PIC X(49) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 01.
           03  LINE 17 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 17 PIC X(49) FROM SPACES.
           03  LINE 19 COLUMN 42 PIC X(21) FROM SPACES
               BACKGROUND-COLOR  07.

       01  ENT-001.
           03  PRX-ENT LINE 09 COLUMN 35 PIC 9(01)     USING CHV-ATV
                       BACKGROUND-COLOR  01.
           03  SDA-ENT LINE 15 COLUMN 35 PIC ZZZZZ9,9  USING SDC-LES
                       BACKGROUND-COLOR  01.

       01  ENT-002.
           03  SUF-ENT LINE 09 COLUMN 37 PIC 9(03)     USING SUF-EST
                       BACKGROUND-COLOR  01.
           03  ATV-ENT LINE 09 COLUMN 45 PIC X(15)     USING DES-ATV
                       BACKGROUND-COLOR  01.
           03  DES-ENT LINE 11 COLUMN 35 PIC X(25)     USING DES-EST
                       BACKGROUND-COLOR  01.
           03  SDI-ENT LINE 13 COLUMN 34 PIC ------9,9 USING SDA-LES
                       BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-SUF LINE 09 COLUMN 37 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-ATV LINE 09 COLUMN 45 PIC X(15) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  LPA-002.
           03  LPA-DES LINE 11 COLUMN 35 PIC X(25) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SDI LINE 13 COLUMN 34 PIC X(09) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SDA LINE 15 COLUMN 35 PIC X(08) FROM SPACES
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
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O SALDO CORR
                   "ETO EM ESTOQUE  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001
           OPEN     INPUT    CADATV
                    I-O      CADEST
                             CADLES
           ACCEPT   DTA-SYS   FROM  DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.

       ROT-100-000.
           DISPLAY  LPA-001  MEN-001
           MOVE     0   TO   CHV-ATV
           ACCEPT   (09 35)  CHV-ATV  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           IF  CHV-ATV   =   ZEROS
               GO  TO        ROT-100-000.
           READ     CADATV   INVALID KEY
                    GO  TO   ROT-100-000.
           DISPLAY  PRX-ENT      ATV-ENT.

       ROT-100-010.
           DISPLAY  LPA-002  MEN-002
           MOVE     0   TO   SUF-EST
           ACCEPT   (09 37)  SUF-EST  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           IF  SUF-EST   =   ZEROS
               GO  TO        ROT-100-010.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     CHV-ATV  TO  PRX-EST
           READ     CADEST   INVALID KEY
                    GO  TO   ROT-100-040.
           MOVE     ZEROS    TO  SDC-LES
           MOVE     SDO-EST  TO  SDA-LES
           DISPLAY  ENT-002.

       ROT-100-020.
           DISPLAY  MEN-004
           ACCEPT   (15 35)  SDC-LES  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           DISPLAY  SDA-ENT  MEN-005
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           MOVE     SDC-LES  TO  SDO-EST
           REWRITE  REG-EST
           MOVE     PRX-EST  TO  PRX-LES
           MOVE     SUF-EST  TO  SUF-LES
           MOVE     DTA-SYS  TO  DTA-LES
           MOVE     ZEROS    TO  LCT-LES.

       ROT-100-030.
           ADD      1        TO  LCT-LES
           WRITE    REG-LES  INVALID KEY
                    GO  TO   ROT-100-030.
           DISPLAY  LPA-002
           GO  TO   ROT-100-010.

       ROT-100-040.
           DISPLAY  MEN-003
           ACCEPT   OPC-001
           MOVE     0   TO   SUF-EST
           GO  TO   ROT-100-010.

       ROT-200-000.
           MOVE     2    TO  PRM-001
           CLOSE    CADATV
                    CADEST   CADLES
           CHAIN   "DYN100"  USING   PRM-001.
