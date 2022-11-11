       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN105.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCMB        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CMB.

           SELECT      CADTMD        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMD.

           SELECT      CADTNQ        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TNQ.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCMB      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCMB.DAT".
       01  REG-CMB.
           03  CHV-CMB          PIC  9(01).
           03  DES-CMB          PIC  X(15).
           03  VVM-CMB          PIC  9(11)V99.
           03  LUC-CMB          PIC  9(04)V9999.

       FD  CADTMD      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADTMD.DAT".
       01  REG-TMD.
           03  CHV-TMD.
               05  TNQ-TMD      PIC  9(02).
               05  LTR-TMD      PIC  9(03)V9.
           03  QTD-TMD          PIC  9(05).
           03  LTM-TMD          PIC  9(02)V9.

       FD  CADTNQ      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADTNQ.DAT".
       01  REG-TNQ.
           03  CHV-TNQ          PIC  9(02).
           03  CMB-TNQ          PIC  9(01).
           03  SDI-TNQ          PIC S9(05)V9.
           03  SDA-TNQ          PIC S9(05)V9.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  NUM-OPC          PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  OPC-TLA          PIC  X(09).

       01  TAB-OPC.
           03  FILLER           PIC  X(09)  VALUE  "INCLUSAO ".
           03  FILLER           PIC  X(09)  VALUE  "ALTERACAO".
           03  FILLER           PIC  X(09)  VALUE  "CONSULTA ".
           03  FILLER           PIC  X(09)  VALUE  "EXCLUSAO ".
       01  RED-OPC              REDEFINES   TAB-OPC.
           03  OPC-TAB          PIC  X(09)  OCCURS  04.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "CADASTRO DE TELEMED  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 09 COLUMN 56 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 56 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 56 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 56 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 56 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 56 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 56 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 56 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 56 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 56 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 42 PIC X(21) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 06 COLUMN 07 PIC X(47) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 07 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 07 PIC X(47) FROM " ³  Num. do Tanque :
               "  -                   ³"   BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 07 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 07 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 07 PIC X(47) FROM " ³  Leitura .......:
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 07 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 07 PIC X(47) FROM " ³  Quantidade ....:
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 07 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 07 PIC X(47) FROM " ³  Leitura Minima :
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 07 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 07 PIC X(47) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR 01.
           03  LINE 17 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 09 PIC X(47) FROM SPACES.

       01  TLA-003.
           03  LINE 07 COLUMN 60 PIC X(15) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 60 PIC X(17) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 60 PIC X(17) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 62 PIC X(15) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 21 PIC X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 21 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 23 PIC X(17) FROM "     Telemed"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 01.
           03  LINE 08 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 21 PIC X(21) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 21 PIC X(21) FROM " ³  1 - Incluir    ³
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 21 PIC X(21) FROM " ³  2 - Alterar    ³
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 21 PIC X(21) FROM " ³  3 - Consultar  ³
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 21 PIC X(21) FROM " ³  4 - Excluir    ³
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 21 PIC X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 23 PIC X(21) FROM SPACES.

       01  TLA-004.
           03  LINE 07 COLUMN 60 PIC X(15) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 60 PIC X(03) FROM " ³"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 63 PIC X(09) USING OPC-TLA
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 72 PIC X(03) FROM " ³"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 75 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 60 PIC X(15) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 75 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 62 PIC X(15) FROM SPACES.

       01  ENT-001.
           03  TNQ-ENT LINE 08 COLUMN 28 PIC 9(02) USING CHV-TNQ
                       BACKGROUND-COLOR  01.
           03  CMB-ENT LINE 08 COLUMN 35 PIC X(15) USING DES-CMB
                       BACKGROUND-COLOR  01.
           03  LTR-ENT LINE 11 COLUMN 28 PIC ZZ9,9 USING LTR-TMD
                       BACKGROUND-COLOR  01.

       01  ENT-002.
           03  QTD-ENT LINE 13 COLUMN 28 PIC ZZZZ9 USING QTD-TMD
                       BACKGROUND-COLOR  01.
           03  LTM-ENT LINE 15 COLUMN 28 PIC  Z9,9 USING LTM-TMD
                       BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-TNQ LINE 08 COLUMN 28 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CMB LINE 08 COLUMN 35 PIC X(15) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-LTR LINE 11 COLUMN 28 PIC X(05) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  LPA-002.
           03  LPA-QTD LINE 13 COLUMN 28 PIC X(05) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-LTM LINE 15 COLUMN 28 PIC X(04) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DES
                   "EJADA ? [ ]  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "TANQUE  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "LEITURA  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "TANQUE INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "LEITURA JA CADASTRA
                   "DA !!!  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "LEITURA INEXISTENTE
                   " !!!  -  [ESC] RETORNA ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A QUANTIDADE
                   " DA LEITURA  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A LEITURA MI
                   "NIMA  -  [ESC] RETORNA ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR A EXCLUSAO  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NSULTAR OUTRA LEITURA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     I-O      CADTMD
                    INPUT    CADCMB  CADTNQ.

       ROT-000-010.
           DISPLAY  TLA-003  MEN-001.

       ROT-000-020.
           MOVE     0   TO   NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-500-000.
           IF  NUM-OPC   <   1  OR  >  4
               GO  TO        ROT-000-020.
           MOVE     OPC-TAB (NUM-OPC) TO  OPC-TLA
           DISPLAY  TLA-002  TLA-004.

       ROT-000-030.
           DISPLAY  LPA-001  MEN-002
           MOVE     0   TO   CHV-TNQ
           ACCEPT   (08 28)  CHV-TNQ  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           IF  CHV-TNQ   =   0
               GO  TO        ROT-000-030.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADTNQ   INVALID KEY
                    DISPLAY  MEN-004
                    ACCEPT   OPC-002
                    GO  TO   ROT-000-030.
           MOVE     CMB-TNQ  TO  CHV-CMB
           READ     CADCMB
           DISPLAY  TNQ-ENT  CMB-ENT
           MOVE     0   TO   LTR-TMD.

       ROT-000-040.
           DISPLAY  MEN-003
           ACCEPT   (11 28)  LTR-TMD  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           IF  LTR-TMD   =   ZEROS
               GO  TO        ROT-000-040.
           DISPLAY  LTR-ENT
           MOVE     SPACES   TO  RSP-OPC
           MOVE     CHV-TNQ  TO  TNQ-TMD
           READ     CADTMD   INVALID KEY
                    GO  TO   ROT-100-000.
           IF  NUM-OPC   =   1
               GO  TO        ROT-000-060.
           DISPLAY  ENT-002
           GO  TO   ROT-100-000, ROT-200-000, ROT-300-000,
                    ROT-400-000, DEPENDING    ON  NUM-OPC.

       ROT-000-050.
           DISPLAY  LPA-002
           ADD      1   TO  LTR-TMD
           GO  TO   ROT-000-040.

       ROT-000-060.
           DISPLAY  MEN-005
           ACCEPT   OPC-002
           MOVE     0   TO  LTR-TMD
           GO  TO   ROT-000-040.

       ROT-100-000.
           IF  NUM-OPC   >   1
               GO  TO        ROT-100-030.
           MOVE     ZEROS    TO  QTD-TMD
                                 LTM-TMD.
       ROT-100-010.
           DISPLAY  LPA-LTM  MEN-007
           ACCEPT   (13 28)  QTD-TMD  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-QTD
               GO  TO        ROT-000-040.
           IF  QTD-TMD   =   0
               GO  TO        ROT-100-010.
           DISPLAY  QTD-ENT.

       ROT-100-020.
           DISPLAY  MEN-008
           ACCEPT   (15 28)  LTM-TMD  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           DISPLAY  LTM-ENT  MEN-009
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           WRITE    REG-TMD
           GO  TO   ROT-000-050.

       ROT-100-030.
           DISPLAY  MEN-006
           ACCEPT   OPC-002
           MOVE     0   TO  LTR-TMD
           GO  TO   ROT-000-040.

       ROT-200-000.
           DISPLAY  MEN-007
           ACCEPT   (13 28)  QTD-TMD  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  QTD-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-000-050.
           IF  QTD-TMD   =   0
               GO  TO        ROT-200-000.

       ROT-200-010.
           DISPLAY  MEN-008
           ACCEPT   (15 28)  LTM-TMD  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  LTM-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           DISPLAY  MEN-009
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-010.
           REWRITE  REG-TMD
           GO  TO   ROT-000-050.

       ROT-300-000.
           DISPLAY  MEN-011
           ACCEPT   OPC-002
           GO  TO   ROT-000-050.

       ROT-400-000.
           DISPLAY  MEN-010
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM  ESCAPE KEY
           IF  TECLADO   =   00
               DELETE    CADTMD.
           GO  TO   ROT-000-050.

       ROT-500-000.
           MOVE     3   TO   PRM-001
           CLOSE    CADCMB
                    CADTMD   CADTNQ
           CHAIN   "DYN100"  USING  PRM-001.
