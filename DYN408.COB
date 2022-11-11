       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN408.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADBCO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-BCO.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADBCO      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADBCO.DAT".
       01  REG-BCO.
           03  CHV-BCO          PIC  9(03).
           03  NOM-BCO          PIC  X(15).
           03  SDI-BCO          PIC S9(11)V99.

       FD  CADLBC      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLBC.DAT".
       01  REG-LBC.
           03  CHV-LBC.
               05  BCO-LBC      PIC  9(03).
               05  DTA-LBC      PIC  9(06).
               05  LCT-LBC      PIC  9(03).
           03  FLG-LBC          PIC  9(01).
           03  DOC-LBC          PIC  9(06).
           03  HST-LBC          PIC  X(20).
           03  COD-LBC          PIC  9(01).
           03  VAL-LBC          PIC  9(10)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ACM-SDI          PIC S9(11)V99.
           03  ACM-DEP          PIC  9(11)V99.
           03  ACM-PGT          PIC  9(11)V99.
           03  ACM-REC          PIC  9(11)V99.
           03  ACM-DEV          PIC  9(11)V99.
           03  ACM-GCP          PIC  9(11)V99.
           03  ACM-PDT          PIC  9(11)V99.
           03  ACM-SDA          PIC S9(11)V99.
           03  ACM-SDD          PIC S9(11)V99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "POSICAO ATUAL BANCARIA  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.

       01  TLA-002.
           03  LINE 06 COLUMN 02 PIC X(76) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 02 PIC X(76) FROM " ³
               "                                                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 02 PIC X(76) FROM " ³ Saldo Inicial :
               "                                                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 02 PIC X(76) FROM " ³
               "                                                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 02 PIC X(76) FROM " ³ Depositos ....:
               "                 Pagamentos ...:                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 02 PIC X(76) FROM " ³
               "                                                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 02 PIC X(76) FROM " ³ Ch Recuperado :
               "                 Ch Devolvido .:                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 02 PIC X(76) FROM " ³
               "                                                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 02 PIC X(76) FROM " ³ Ganho Capital :
               "                 Ch Pendente ..:                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 02 PIC X(76) FROM " ³
               "                                                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 02 PIC X(76) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 02 PIC X(76) FROM " ³
               "                                                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 05 PIC X(53) FROM "Saldo Atual ..:
               "              Disponivel ...:"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 02 PIC X(76) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 78 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 04 PIC X(76) FROM SPACES.

       01  ENT-001.
           03  SDI-ENT LINE 08 COLUMN 20 PIC ---.---.---.--9,99 USING
               ACM-SDI BACKGROUND-COLOR  01.
           03  DEP-ENT LINE 10 COLUMN 21 PIC  ZZ.ZZZ.ZZZ.ZZ9,99 USING
               ACM-DEP BACKGROUND-COLOR  01.
           03  PGT-ENT LINE 10 COLUMN 58 PIC  ZZ.ZZZ.ZZZ.ZZ9,99 USING
               ACM-PGT BACKGROUND-COLOR  01.
           03  REC-ENT LINE 12 COLUMN 21 PIC  ZZ.ZZZ.ZZZ.ZZ9,99 USING
               ACM-REC BACKGROUND-COLOR  01.
           03  DEV-ENT LINE 12 COLUMN 58 PIC  ZZ.ZZZ.ZZZ.ZZ9,99 USING
               ACM-DEV BACKGROUND-COLOR  01.
           03  GCP-ENT LINE 14 COLUMN 21 PIC  ZZ.ZZZ.ZZZ.ZZ9,99 USING
               ACM-GCP BACKGROUND-COLOR  01.
           03  PDT-ENT LINE 14 COLUMN 58 PIC  ZZ.ZZZ.ZZZ.ZZ9,99 USING
               ACM-PDT BACKGROUND-COLOR  01.
           03  SDA-ENT LINE 17 COLUMN 20 PIC ---.---.---.--9,99 USING
               ACM-SDA BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  SDP-ENT LINE 17 COLUMN 57 PIC ---.---.---.--9,99 USING
               ACM-SDD BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ACUMUL
                   "ANDO MOVIMENTO ...                              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ESC] P/ RETO
                   "RNAR ...                                        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           MOVE     ZEROS    TO  ACM-SDI  ACM-DEP  ACM-PGT
                                 ACM-REC  ACM-DEV  ACM-GCP
                                 ACM-PDT  ACM-SDA  ACM-SDD
           OPEN     INPUT        CADBCO   CADLBC
           DISPLAY  TLA-001      TLA-002
                    ENT-001      MEN-001.

       ROT-100-000.
           READ     CADBCO   AT  END
                    GO  TO   ROT-200-000.
           ADD      SDI-BCO  TO  ACM-SDI
           ADD      SDI-BCO  TO  ACM-SDA
           ADD      SDI-BCO  TO  ACM-SDD
           DISPLAY  TLA-001
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           START    CADLBC   KEY GREATER  CHV-LBC
                    INVALID  KEY GO  TO   ROT-100-000.

       ROT-100-010.
           READ     CADLBC   NEXT AT END
                    GO  TO   ROT-100-000.
           IF  BCO-LBC  NOT  =   CHV-BCO
               GO  TO        ROT-100-000.
           IF  FLG-LBC   =   0
               GO  TO        ROT-100-030.
           ADD      VAL-LBC  TO  ACM-PDT
           SUBTRACT VAL-LBC FROM ACM-SDD.

       ROT-100-020.
           DISPLAY  ENT-001
           GO  TO   ROT-100-010.

       ROT-100-030.
           GO  TO   ROT-100-040, ROT-100-050, ROT-100-060, ROT-100-050,
                    ROT-100-070, ROT-100-080, DEPENDING    ON  COD-LBC.

       ROT-100-040.
           ADD      VAL-LBC  TO  ACM-DEP
           ADD      VAL-LBC  TO  ACM-SDA
           ADD      VAL-LBC  TO  ACM-SDD
           GO  TO   ROT-100-020.

       ROT-100-050.
           ADD      VAL-LBC  TO  ACM-PGT
           SUBTRACT VAL-LBC FROM ACM-SDA
           SUBTRACT VAL-LBC FROM ACM-SDD
           GO  TO   ROT-100-020.

       ROT-100-060.
           ADD      VAL-LBC  TO  ACM-GCP
           ADD      VAL-LBC  TO  ACM-SDA
           ADD      VAL-LBC  TO  ACM-SDD
           GO  TO   ROT-100-020.

       ROT-100-070.
           ADD      VAL-LBC  TO  ACM-DEV
           SUBTRACT VAL-LBC FROM ACM-SDA
           SUBTRACT VAL-LBC FROM ACM-SDD
           GO  TO   ROT-100-020.

       ROT-100-080.
           ADD      VAL-LBC  TO  ACM-REC
           ADD      VAL-LBC  TO  ACM-SDA
           ADD      VAL-LBC  TO  ACM-SDD
           GO  TO   ROT-100-020.


       ROT-200-000.
           DISPLAY  ENT-001      MEN-002.

       ROT-200-010.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO  NOT  =  1
               GO  TO        ROT-200-010.

       ROT-200-020.
           MOVE     5   TO   PRM-001
           CLOSE    CADBCO   CADLBC
           CHAIN   "DYN400"  USING  PRM-001.
