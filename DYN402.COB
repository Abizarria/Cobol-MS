       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN402.
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

           SELECT      CADCDV        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CDV.

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

       FD  CADCDV      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCDV.DAT".
       01  REG-CDV.
           03  CHV-CDV          PIC  9(06).
           03  DTA-CDV          PIC  9(06).
           03  NOM-CDV          PIC  X(30).
           03  CPF-CDV          PIC  9(11).
           03  VAL-CDV          PIC  9(10)V99.

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
           03  MOV-AUX          PIC  X(17).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  CPF-AUX.
               05  CP1-AUX      PIC  9(03).
               05  CP2-AUX      PIC  9(03).
               05  CP3-AUX      PIC  9(03).
               05  CP4-AUX      PIC  9(02).

           03  TAB-MOV.
               05  FILLER       PIC  X(17)  VALUE  "CREDITO E ACERTOS".
               05  FILLER       PIC  X(17)  VALUE  "DEBITOS E ACERTOS".
               05  FILLER       PIC  X(17)  VALUE  "GANHOS DE CAPITAL".
               05  FILLER       PIC  X(17)  VALUE  "EMISSAO DE CHEQUE".
               05  FILLER       PIC  X(17)  VALUE  "CHEQUE DEVOLVIDO ".
               05  FILLER       PIC  X(17)  VALUE  "CHEQUE RECUPERADO".
           03  RED-MOV          REDEFINES   TAB-MOV.
               05  MOV-TAB      PIC  X(17)  OCCURS  06.

       01  PRM-001              PIC  9(01).

       01  PRM-002.
           03  BCO-PR2          PIC  9(03).
           03  NOM-PR2          PIC  X(15).
           03  DTA-PR2.
               05  ANO-PR2      PIC  9(02).
               05  MES-PR2      PIC  9(02).
               05  DIA-PR2      PIC  9(02).
           03  DOC-PR2          PIC  9(06).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "MOVIMENTO BANCARIO  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.

       01  TLA-002.
           03  LINE 06 COLUMN 17 PIC X(44) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 17 PIC X(44) FROM " ³
              "                   ³" BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 17 PIC X(44) FROM " ³  Banco .....:      -
              "                   ³" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 17 PIC X(44) FROM " ³
              "                   ³" BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(44) FROM " ³  Data ......:   /  /
              "                   ³" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(44) FROM " ³  Documento .:
              "                   ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(44) FROM " ³
              "                   ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(44) FROM " ³  Movimento .:    -
              "                   ³" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(44) FROM " ³  Historico .:
              "                   ³" BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(44) FROM " ³
              "                   ³" BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 17 PIC X(44) FROM " ³  Valor .....:
              "                   ³" BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 17 PIC X(44) FROM " ³
              "                   ³" BACKGROUND-COLOR 01.
           03  LINE 17 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 17 PIC X(44) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 01.
           03  LINE 18 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 19 PIC X(44) FROM SPACES.

       01  TLA-003.
           03  LINE 07 COLUMN 24 PIC X(29) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄ¿" BACKGROUND-COLOR    04.
           03  LINE 08 COLUMN 24 PIC X(29) FROM " ³
               "    ³" BACKGROUND-COLOR    04.
           03  LINE 08 COLUMN 26 PIC X(25) FROM "  Codigos de Movimentos
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  01.
           03  LINE 08 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 24 PIC X(29) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄ´" BACKGROUND-COLOR    04.
           03  LINE 09 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 24 PIC X(29) FROM " ³  1 - Credito e Acert
               "os  ³" BACKGROUND-COLOR    04.
           03  LINE 10 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 24 PIC X(29) FROM " ³  2 - Debitos e Acert
               "os  ³" BACKGROUND-COLOR    04.
           03  LINE 11 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 24 PIC X(29) FROM " ³  3 - Ganhos de Capit
               "al  ³" BACKGROUND-COLOR    04.
           03  LINE 12 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 24 PIC X(29) FROM " ³  4 - Emissao de Cheq
               "ue  ³" BACKGROUND-COLOR    04.
           03  LINE 13 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 24 PIC X(29) FROM " ³  5 - Cheque Devolvid
               "o   ³" BACKGROUND-COLOR    04.
           03  LINE 14 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 24 PIC X(29) FROM " ³  6 - Cheque Recupera
               "do  ³" BACKGROUND-COLOR    04.
           03  LINE 15 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 24 PIC X(29) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÙ" BACKGROUND-COLOR    04.
           03  LINE 16 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 26 PIC X(29) FROM SPACES.

       01  TLA-004.
           03  LINE 13 COLUMN 21 PIC X(50) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 21 PIC X(50) FROM " ³
              "                         ³" BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 21 PIC X(50) FROM " ³  Emitente .:
              "                         ³" BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 21 PIC X(50) FROM " ³
              "                         ³" BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 21 PIC X(50) FROM " ³  C.P.F. ...:    .
              ".   -                    ³" BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 21 PIC X(50) FROM " ³
              "                         ³" BACKGROUND-COLOR  04.
           03  LINE 18 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 21 PIC X(50) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  04.
           03  LINE 19 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 23 PIC X(50) FROM SPACES.

       01  ENT-001.
           03  CHV-ENT LINE 08 COLUMN 34 PIC 9(03) USING CHV-BCO
               BACKGROUND-COLOR  01.
           03  NOM-ENT LINE 08 COLUMN 42 PIC X(15) USING NOM-BCO
               BACKGROUND-COLOR  01.
           03  DIA-ENT LINE 10 COLUMN 34 PIC 9(02) USING DIA-AUX
               BACKGROUND-COLOR  01.
           03  MES-ENT LINE 10 COLUMN 37 PIC 9(02) USING MES-AUX
               BACKGROUND-COLOR  01.
           03  ANO-ENT LINE 10 COLUMN 40 PIC 9(02) USING ANO-AUX
               BACKGROUND-COLOR  01.
           03  DOC-ENT LINE 11 COLUMN 34 PIC 9(06) USING DOC-LBC
               BACKGROUND-COLOR  01.

       01  ENT-002.
           03  COD-ENT LINE 13 COLUMN 34 PIC 9(01) USING COD-LBC
               BACKGROUND-COLOR  01.
           03  MOV-ENT LINE 13 COLUMN 40 PIC X(17) USING MOV-AUX
               BACKGROUND-COLOR  01.
           03  HST-ENT LINE 14 COLUMN 34 PIC X(20) USING HST-LBC
               BACKGROUND-COLOR  01.
           03  VAL-ENT LINE 16 COLUMN 34 PIC Z.ZZZ.ZZZ.ZZ9,99 USING
               VAL-LBC BACKGROUND-COLOR  01.

       01  ENT-003.
           03  EMT-ENT LINE 15 COLUMN 37 PIC X(30) USING NOM-CDV
               BACKGROUND-COLOR  04.
           03  CP1-ENT LINE 17 COLUMN 37 PIC 9(03) USING CP1-AUX
               BACKGROUND-COLOR  04.
           03  CP2-ENT LINE 17 COLUMN 41 PIC 9(03) USING CP2-AUX
               BACKGROUND-COLOR  04.
           03  CP3-ENT LINE 17 COLUMN 45 PIC 9(03) USING CP3-AUX
               BACKGROUND-COLOR  04.
           03  CP4-ENT LINE 17 COLUMN 49 PIC 9(02) USING CP4-AUX
               BACKGROUND-COLOR  04.

       01  LPA-001.
           03  LPA-NOM LINE 08 COLUMN 42 PIC X(15) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LPA-DIA LINE 10 COLUMN 34 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LPA-MES LINE 10 COLUMN 37 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LPA-ANO LINE 10 COLUMN 40 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LPA-DOC LINE 11 COLUMN 34 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LPA-COD LINE 13 COLUMN 34 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LPA-MOV LINE 13 COLUMN 40 PIC X(17) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LPA-HST LINE 14 COLUMN 34 PIC X(20) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LPA-VAL LINE 16 COLUMN 34 PIC X(16) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LPA-CP1 LINE 17 COLUMN 37 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  04.
           03  LPA-CP2 LINE 17 COLUMN 41 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  04.
           03  LPA-CP3 LINE 17 COLUMN 45 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  04.
           03  LPA-CP4 LINE 17 COLUMN 49 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  04.

       01  LPA-002.
           03  LINE 20 COLUMN 23 PIC X(50) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 63 PIC X(10) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 63 PIC X(10) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 63 PIC X(10) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 63 PIC X(10) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 63 PIC X(10) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 63 PIC X(10) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 63 PIC X(08) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "BANCO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA DO MO
                   "VIMENTO  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO DO MOVIMENTO
                   " ?  [ ]  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DO
                   "DOCUMENTO  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O HISTORICO
                   "DO MOVIMENTO  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR DO M
                   "OVIMENTO  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "CHEQUE JA LANCADO
                   "-  [ESC] RETORNA ...                            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "CHEQUE INEXISTENTE
                   " -  [ESC] RETORNA ...                           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NOME DO EM
                   "ITENTE DO CHEQUE  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-012 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO NO
                   "C.P.F. DO EMITENTE  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 38 PIC 9(01) USING COD-LBC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     INPUT    CADBCO
                    I-O      CADLBC  CADCDV.

       ROT-000-010.
           DISPLAY  LPA-NOM
                    LPA-DIA  MEN-001
           MOVE     0   TO   CHV-BCO
           ACCEPT   (08 34)  CHV-BCO  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-500-000.
           IF  CHV-BCO   =   ZEROS
               GO  TO        ROT-000-010.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADBCO   INVALID KEY
                    DISPLAY  MEN-002
                    ACCEPT   OPC-001
                    GO  TO   ROT-000-010.
           DISPLAY  CHV-ENT      NOM-ENT
           MOVE     ZEROS    TO  DTA-AUX.

       ROT-000-020.
           DISPLAY  MEN-003  LPA-MES
           ACCEPT   (10 34)  DIA-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           IF  DIA-AUX   <   1  OR >  31
               GO  TO        ROT-000-020.
           DISPLAY  DIA-ENT.

       ROT-000-030.
           DISPLAY  LPA-ANO
           ACCEPT   (10 37)  MES-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           IF  MES-AUX   <   1  OR >  12
               GO  TO        ROT-000-030.
           DISPLAY  MES-ENT.

       ROT-000-040.
           DISPLAY  LPA-DOC  MEN-003
           ACCEPT   (10 40)  ANO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           DISPLAY  ANO-ENT
           MOVE     CHV-BCO  TO  BCO-LBC
           MOVE     DTA-AUX  TO  DTA-LBC.

       ROT-000-050.
           MOVE     SPACES   TO  HST-LBC
           MOVE     ZEROS    TO  DOC-LBC
                                 VAL-LBC  LCT-LBC.
       ROT-000-060.
           DISPLAY  MEN-005
           ACCEPT   (11 34)  DOC-LBC  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-040.
           DISPLAY  DOC-ENT.

       ROT-000-070.
           DISPLAY  TLA-003  MEN-004.

       ROT-000-080.
           MOVE     0   TO   COD-LBC
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       TLA-002
                             ENT-001
               GO  TO        ROT-000-060.
           IF  COD-LBC   <   1  OR  >  6
               GO  TO        ROT-000-080.
           MOVE     MOV-TAB (COD-LBC) TO  MOV-AUX
           IF  COD-LBC   =   4
               GO  TO        ROT-200-000.
           IF  COD-LBC   =   5
               GO  TO        ROT-300-000.
           IF  COD-LBC   =   6
               GO  TO        ROT-400-000.

       ROT-100-000.
           DISPLAY  TLA-002  ENT-001
                    COD-ENT  MOV-ENT.

       ROT-100-010.
           DISPLAY  LPA-VAL  MEN-006
           ACCEPT   (14 34)  HST-LBC  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.
           IF  HST-LBC   =   SPACES
               GO  TO        ROT-100-010.
           DISPLAY  HST-ENT.

       ROT-100-020.
           DISPLAY  LPA-VAL  MEN-007
           ACCEPT   (16 34)  VAL-LBC  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           IF  VAL-LBC   =   ZEROS
               GO  TO        ROT-100-020.
           IF  COD-LBC   =   6   AND
               VAL-LBC   <   VAL-CDV
               GO  TO        ROT-100-020.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  VAL-ENT      MEN-008
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           MOVE     ZEROS    TO  FLG-LBC
           IF  COD-LBC   =   5
               GO  TO        ROT-300-080.
           IF  COD-LBC   =   6
               GO  TO        ROT-400-020.

       ROT-100-030.
           ADD      1        TO  LCT-LBC
           WRITE    REG-LBC  INVALID KEY
                    GO  TO   ROT-100-030.
           DISPLAY  LPA-VAL      LPA-HST
                    LPA-MOV      LPA-COD
           GO  TO   ROT-000-050.

       ROT-100-040.
           IF  COD-LBC   =   5
               DISPLAY       TLA-004
               GO  TO        ROT-300-070.
           IF  COD-LBC   =   6
               GO  TO        ROT-400-010.
           DISPLAY  LPA-MOV
           GO  TO   ROT-000-070.

       ROT-200-000.
           CLOSE    CADBCO   CADCDV
                    CADLBC
           MOVE     CHV-BCO  TO  BCO-PR2
           MOVE     NOM-BCO  TO  NOM-PR2
           MOVE     DTA-AUX  TO  DTA-PR2
           MOVE     DOC-LBC  TO  DOC-PR2
           CALL    "DYN403"  USING   PRM-001
                                     PRM-002
           OPEN     INPUT    CADBCO
                    I-O      CADCDV  CADLBC
           IF  PRM-001   =   1
               DISPLAY       LPA-VAL
                             LPA-HST
                             LPA-MOV
                             LPA-COD
               GO  TO        ROT-000-050.
           IF  PRM-001   =   2
               GO  TO        ROT-000-070.

       ROT-300-000.
           DISPLAY  TLA-002      ENT-001
                    COD-ENT      MOV-ENT
           MOVE     DOC-LBC  TO  CHV-CDV
           READ     CADCDV   INVALID KEY
                    GO  TO   ROT-300-010.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-009
           ACCEPT   OPC-001
           DISPLAY  LPA-MOV
           GO  TO   ROT-000-070.

       ROT-300-010.
           DISPLAY  TLA-004
           MOVE     ZEROS    TO  CPF-AUX
           MOVE     SPACES   TO  NOM-CDV.

       ROT-300-020.
           DISPLAY  LPA-CP1  MEN-011
           ACCEPT   (15 37)  NOM-CDV  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       TLA-002
                             NOM-ENT
                             LPA-002
               GO  TO        ROT-000-070.
           IF  NOM-CDV   =   SPACES
               GO  TO        ROT-300-020.
           DISPLAY  EMT-ENT  MEN-012.

       ROT-300-030.
           DISPLAY  LPA-CP2
           ACCEPT   (17 37)  CP1-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-020.
           DISPLAY  CP1-ENT.

       ROT-300-040.
           DISPLAY  LPA-CP3
           ACCEPT   (17 41)  CP2-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-030.
           DISPLAY  CP2-ENT.

       ROT-300-050.
           DISPLAY  LPA-CP4
           ACCEPT   (17 45)  CP3-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-040.
           DISPLAY  CP3-ENT.

       ROT-300-060.
           DISPLAY  MEN-012
           ACCEPT   (17 49)  CP4-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-050.

       ROT-300-070.
           DISPLAY  ENT-003  MEN-008
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-060.
           DISPLAY  LPA-002
           GO  TO   ROT-100-000.

       ROT-300-080.
           MOVE     DTA-AUX  TO  DTA-CDV
           MOVE     CPF-AUX  TO  CPF-CDV
           MOVE     VAL-LBC  TO  VAL-CDV
           WRITE    REG-CDV
           GO  TO   ROT-100-030.

       ROT-400-000.
           DISPLAY  TLA-002      ENT-001
                    COD-ENT      MOV-ENT
           MOVE     DOC-LBC  TO  CHV-CDV
           MOVE     SPACES   TO  RSP-OPC
           READ     CADCDV   INVALID KEY
                    DISPLAY  MEN-010
                    ACCEPT   OPC-001
                    DISPLAY  LPA-MOV
                    GO  TO   ROT-000-070.
           MOVE     CPF-CDV  TO  CPF-AUX.

       ROT-400-010.
           DISPLAY  TLA-004
                    ENT-003      MEN-008
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       TLA-002
                             NOM-ENT
                             LPA-002
               GO  TO        ROT-000-070.
           MOVE     VAL-CDV  TO  VAL-LBC
           DISPLAY  LPA-002
           GO  TO   ROT-100-000.

       ROT-400-020.
           DELETE   CADCDV
           GO  TO   ROT-100-030.

       ROT-500-000.
           MOVE     2   TO  PRM-001
           CLOSE    CADBCO
                    CADLBC   CADCDV
           CHAIN   "DYN400"  USING  PRM-001.
