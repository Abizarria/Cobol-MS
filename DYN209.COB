       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN209.
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
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  FLG-AUX          PIC  9(01).

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 09 COLUMN 18 PIC X(44) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 18 PIC X(44) FROM " ³
               "                   ³" BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 18 PIC X(44) FROM " ³                   .:
               "                   ³" BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 18 PIC X(44) FROM " ³
               "                   ³" BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 18 PIC X(44) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 20 PIC X(44) FROM SPACES.

       01  ENT-001.
           03  ATV-ENT LINE 11 COLUMN 22 PIC X(15) USING DES-PRS
                       BACKGROUND-COLOR  01.
           03  VAL-ENT LINE 11 COLUMN 41 PIC --.---.---.--9,99
                       USING   VDD-PRS   BACKGROUND-COLOR  01.
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

       01  LPA-002 LINE 11 COLUMN 42 PIC X(16) FROM SPACES
                   BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO SALDOS ...                          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR TOTA
                   "L DA SALDO  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-002.

       ROT-000-000.
           DISPLAY  LPA-001
                    TLA-001  MEN-001
           OPEN     I-O      CADPRS
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     8        TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-000-010.
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   8
               DISPLAY       ENT-001
               GO  TO        ROT-100-000.
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           START    CADPRS   KEY GREATER  CHV-PRS
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   1   CHAIN   "DYN210"  USING  PRM-002.
           IF  ATV-PRS   =   2   CHAIN   "DYN211"  USING  PRM-002.
           IF  ATV-PRS   =   3   CHAIN   "DYN212"  USING  PRM-002.
           IF  ATV-PRS   =   4   CHAIN   "DYN213"  USING  PRM-002.
           IF  ATV-PRS   =   5   CHAIN   "DYN214"  USING  PRM-002.
           IF  ATV-PRS   >   5   CHAIN   "DYN215"  USING  PRM-002.

       ROT-100-000.
           DISPLAY  LPA-002  MEN-002
           ACCEPT   (11 42)  VDD-PRS  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           MOVE     " "  TO  RSP-OPC
           DISPLAY  VAL-ENT  MEN-003
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           REWRITE  REG-PRS
           GO  TO   ROT-000-010.

       ROT-100-010.
           COMPUTE  LIN-PRS  =   LIN-PRS  -  1
           READ     CADPRS   INVALID KEY
                    GO  TO   ROT-100-020.
           DISPLAY  ENT-001
           GO  TO   ROT-100-000.

       ROT-100-020.
           MOVE     7        TO  FLG-AUX.

       ROT-100-030.
           MOVE     FLG-AUX  TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   FLG-AUX
               GO  TO        ROT-100-040.
           COMPUTE  FLG-AUX  =   FLG-AUX  -  1
           IF  FLG-AUX   >   0
               GO  TO        ROT-100-030.
           CLOSE    CADPRS
           CHAIN   "DYN201".

       ROT-100-040.
           IF  ATV-PRS   =   1  CHAIN  "DYN202"  USING  PRM-002.
           IF  ATV-PRS   =   2  CHAIN  "DYN203"  USING  PRM-002.
           IF  ATV-PRS   =   3  CHAIN  "DYN204"  USING  PRM-002.
           IF  ATV-PRS   =   4  CHAIN  "DYN205"  USING  PRM-002.
           IF  ATV-PRS   =   5  CHAIN  "DYN206"  USING  PRM-002.
           IF  ATV-PRS   =   6  CHAIN  "DYN207"  USING  PRM-002.
           IF  ATV-PRS   =   7  CHAIN  "DYN208"  USING  PRM-002.