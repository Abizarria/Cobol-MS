       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN219.
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

           SELECT      CADECR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ECR.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCMB      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCMB.DAT".
       01  REG-CMB.
           03  CHV-CMB          PIC  9(01).
           03  DES-CMB          PIC  X(15).
           03  VVM-CMB          PIC  9(11)V99.
           03  LUC-CMB          PIC  9(04)V9999.

       FD  CADECR      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADECR.DAT".
       01  REG-ECR.
           03  CHV-ECR          PIC  9(02).
           03  CMB-ECR          PIC  9(01).
           03  TQ1-ECR          PIC  9(02).
           03  TQ2-ECR          PIC  9(02).
           03  CX1-ECR          PIC  9(01).
           03  CX2-ECR          PIC  9(01).
           03  IDL-ECR          PIC  9(06)V9.
           03  ICL-ECR          PIC  9(06)V9.
           03  FCL-ECR          PIC  9(06)V9.
           03  FLG-ECR          PIC  9(01).
           03  ITL-ECR          PIC  9(06)V9.
           03  FTL-ECR          PIC  9(06)V9.
           03  AFR-ECR          PIC  9(04).
           03  EXT-ECR          PIC  9(04).
           03  VAL-ECR          PIC  9(04)V9999.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 01 PIC X(67) FROM
               " GERADOR DE ENCERRANTES  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 27 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 27 PIC X(29) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 64 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 64 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 64 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 64 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 64 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 64 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 34 PIC X(31) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 36 PIC X(29) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 42 PIC X(23) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 08 COLUMN 19 PIC X(43) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 19 PIC X(43) FROM " ³
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 19 PIC X(43) FROM " ³  Bomba .....:     -
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 19 PIC X(43) FROM " ³
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 19 PIC X(43) FROM " ³  Final .....:
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 19 PIC X(43) FROM " ³
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 19 PIC X(43) FROM " ³  Inicial ...:
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 19 PIC X(43) FROM " ³
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 19 PIC X(43) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 21 PIC X(43) FROM SPACES.

       01  TLA-003.
           03  LINE 09 COLUMN 19 PIC X(43) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 19 PIC X(43) FROM " ³
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 19 PIC X(43) FROM " ³  Bomba .....:     -
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 19 PIC X(43) FROM " ³
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 19 PIC X(43) FROM " ³  Preco .....:
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 19 PIC X(43) FROM " ³
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 19 PIC X(43) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 21 PIC X(43) FROM SPACES.

       01  TLA-004.
           03  LINE 09 COLUMN 19 PIC X(43) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 19 PIC X(43) FROM " ³
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 19 PIC X(43) FROM " ³  Combustivel :    -
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 19 PIC X(43) FROM " ³
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 19 PIC X(43) FROM " ³  Lucro Bruto :
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 19 PIC X(43) FROM " ³
               "                  ³" BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 19 PIC X(43) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 21 PIC X(43) FROM SPACES.

       01  ENT-001.
           03  LINE 10 COLUMN 36 PIC 9(02) USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 43 PIC X(15) USING DES-CMB
               BACKGROUND-COLOR  01.

       01  ENT-002.
           03  LINE 11 COLUMN 36 PIC 9(02) USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 43 PIC X(15) USING DES-CMB
               BACKGROUND-COLOR  01.

       01  ENT-003  LINE 12 COLUMN 36 PIC 999999,9   USING FTL-ECR
                    BACKGROUND-COLOR  01.

       01  ENT-004  LINE 14 COLUMN 36 PIC 999999,9   USING ITL-ECR
                    BACKGROUND-COLOR  01.

       01  ENT-005  LINE 14 COLUMN 36 PIC 999999,9   USING ICL-ECR
                    BACKGROUND-COLOR  01.

       01  ENT-006  LINE 13 COLUMN 36 PIC Z.ZZ9,9999 USING VAL-ECR
                    BACKGROUND-COLOR  01.

       01  ENT-007.
           03  LINE 11 COLUMN 37 PIC 9(01) USING CHV-CMB
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 43 PIC X(15) USING DES-CMB
               BACKGROUND-COLOR  01.

       01  ENT-008  LINE 13 COLUMN 37 PIC Z.ZZ9,9999 USING LUC-CMB
                    BACKGROUND-COLOR  01.

       01  LPA-001  LINE 14 COLUMN 36 PIC X(08) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  LPA-002  LINE 13 COLUMN 36 PIC X(11) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O ENCERRANTE
                   " FINAL ANTERIOR  -  [ESC] RETORNA ...      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O ENCERRANTE
                   " INICIAL ATUAL  -  [ESC] RETORNA ...       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O PRECO DE V
                   "ENDA POR LITRO  -  [ESC] RETORNA ...       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR DO L
                   "UCRO BRUTO POR LITRO  -  [ESC] RETORNA ... "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR !!!  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001.

       ROT-000-000.
           OPEN     I-O      CADCMB
                             CADECR
           DISPLAY  TLA-001
           GO  TO   ROT-100-000, ROT-200-000,
                    ROT-300-000, DEPENDING  ON  PRM-001.

       ROT-000-010.
           MOVE     2   TO   PRM-001
           CLOSE    CADCMB   CADECR
           CHAIN   "DYN200"  USING  PRM-001.

       ROT-100-000.
           DISPLAY  TLA-002
           MOVE     ZEROS    TO  CHV-ECR
           START    CADECR   KEY GREATER  CHV-ECR.

       ROT-100-010.
           READ     CADECR   NEXT AT END
                    GO  TO   ROT-000-010.
           MOVE     CMB-ECR  TO  CHV-CMB
           READ     CADCMB
           DISPLAY  ENT-001
           IF  ICL-ECR   =   IDL-ECR
               GO  TO        ROT-100-040.
           MOVE     ICL-ECR  TO  ITL-ECR
                                 FTL-ECR.

       ROT-100-020.
           DISPLAY  LPA-001  MEN-001
           ACCEPT   (12 36)  FTL-ECR  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           DISPLAY  ENT-003.

       ROT-100-030.
           DISPLAY  MEN-002
           ACCEPT   (14 36)  ITL-ECR  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           DISPLAY  ENT-004      MEN-005
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           MOVE     1        TO  FLG-ECR
           REWRITE  REG-ECR
           GO  TO   ROT-100-010.

       ROT-100-040.
           DISPLAY  ENT-003  MEN-002
           ACCEPT   (14 36)  ICL-ECR  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           DISPLAY  ENT-005      MEN-005
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           MOVE     ICL-ECR  TO  IDL-ECR
           REWRITE  REG-ECR
           GO  TO   ROT-100-010.

       ROT-200-000.
           DISPLAY  TLA-003
           MOVE     ZEROS    TO  CHV-ECR
           START    CADECR   KEY GREATER  CHV-ECR.

       ROT-200-010.
           READ     CADECR   NEXT AT END
                    GO  TO   ROT-000-010.
           MOVE     CMB-ECR  TO  CHV-CMB
           READ     CADCMB
           DISPLAY  ENT-002.

       ROT-200-020.
           DISPLAY  LPA-002  MEN-003
           ACCEPT   (13 36)  VAL-ECR  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           DISPLAY  ENT-006      MEN-005
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-020.
           REWRITE  REG-ECR
           GO  TO   ROT-200-010.

       ROT-300-000.
           DISPLAY  TLA-004
           MOVE     ZEROS    TO  CHV-CMB
           START    CADCMB   KEY GREATER  CHV-CMB.

       ROT-300-010.
           READ     CADCMB   NEXT AT END
                    GO  TO   ROT-000-010.
           DISPLAY  ENT-007.

       ROT-300-020.
           DISPLAY  LPA-002  MEN-004
           ACCEPT   (13 37)  LUC-CMB  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           DISPLAY  ENT-008      MEN-005
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-020.
           REWRITE  REG-CMB
           GO  TO   ROT-300-010.
