       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN504.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCPD        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CPD.

           SELECT      CADLCH        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCH.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCPD      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCPD.DAT".
       01  REG-CPD.
           03  CHV-CPD.
               05  DTA-CPD      PIC  9(06).
               05  NUM-CPD      PIC  9(06).
               05  LCT-CPD      PIC  9(03).
           03  BCO-CPD          PIC  9(03).
           03  NOM-CPD          PIC  X(30).
           03  VAL-CPD          PIC  9(09)V99.

       FD  CADLCH      LABEL         RECORD  IS  STANDARD
                       VALUE     OF  FILE-ID     "CADLCH.DAT".
       01  REG-LCH.
           03  CHV-LCH          PIC  9(06).
           03  DTA-LCH          PIC  9(06).
           03  BCO-LCH          PIC  9(03).
           03  NOM-LCH          PIC  X(30).
           03  VAL-LCH          PIC  9(09)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  RSP-OPC          PIC  X(01).
           03  NUM-OPC          PIC  9(01).
           03  TECLADO          PIC  9(02).
           03  ACM-QTD          PIC  9(03).
           03  COL-AUX          PIC  9(03)V99.
           03  VAL-AUX          PIC  9(09)V99.
           03  ACM-VAL          PIC  9(10)V99.

           03  REG-TAB          PIC  X(68)  OCCURS  510.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  TLA-AUX.
               05  NUM-TLA      PIC  9(06).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  BCO-TLA      PIC  9(03).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  DIA-TLA      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-TLA      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-TLA      PIC  9(02).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  NOM-TLA      PIC  X(32).
               05  VAL-TLA      PIC  ZZ.ZZZ.ZZ9,99.

           03  LIN-AUX.
               05  LIN-001      PIC  X(68).
               05  LIN-002      PIC  X(68).
               05  LIN-003      PIC  X(68).
               05  LIN-004      PIC  X(68).
               05  LIN-005      PIC  X(68).
               05  LIN-006      PIC  X(68).
               05  LIN-007      PIC  X(68).
               05  LIN-008      PIC  X(68).
               05  LIN-009      PIC  X(68).
               05  LIN-010      PIC  X(68).
               05  LIN-011      PIC  X(68).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "LANCAMENTO DE CHEQUES-PRE  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 13 COLUMN 64 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 64 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 64 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 64 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 64 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 64 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 64 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 06 COLUMN 17 PIC X(45) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"  BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 17 PIC X(45) FROM " ³
              "                    ³"  BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 17 PIC X(45) FROM " ³ Numero :
              "                    ³"  BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 17 PIC X(45) FROM " ³
              "                    ³"  BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(45) FROM " ³ Banco .:
              "   Data .:   /  /   ³"  BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(45) FROM " ³
              "                    ³"  BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(45) FROM " ³ Nome ..:
              "                    ³"  BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(45) FROM " ³
              "                    ³"  BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(45) FROM " ³ Valor .:
              "                    ³"  BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(45) FROM " ³
              "                    ³"  BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 17 PIC X(45) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"  BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 17 PIC X(45) FROM " ³              ³     ³
              "                    ³"  BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 17 PIC X(45) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"  BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 19 PIC X(45) FROM SPACES.

       01  TLA-003.
           03  LINE 07 COLUMN 29 PIC X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 29 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 31 PIC X(17) FROM "   Cheques-pre"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  01.
           03  LINE 08 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 29 PIC X(21) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 29 PIC X(21) FROM " ³  1 - Correcao   ³
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 29 PIC X(21) FROM " ³  2 - Consulta   ³
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 29 PIC X(21) FROM " ³  3 - Liberacao  ³
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 29 PIC X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 31 PIC X(21) FROM SPACES.

       01  TLA-004.
           03  LINE 05 COLUMN 03 PIC X(74) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 03 PIC X(74) FROM " ³ NUMERO BANCO   DATA
               "   NOME DO EMITENTE                VALOR LANCADO ³"
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 03 PIC X(74) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 05 PIC X(74) FROM SPACES.

       01  TLA-006.
           03  LINE 08 COLUMN 06 PIC X(68) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 06 PIC X(68) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 06 PIC X(68) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 06 PIC X(68) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 06 PIC X(68) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 06 PIC X(68) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 06 PIC X(68) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 06 PIC X(68) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 06 PIC X(68) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 06 PIC X(68) USING LIN-010
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 06 PIC X(68) USING LIN-011
               BACKGROUND-COLOR  01.

       01  TLA-007.
           03  LINE 09 COLUMN 11 PIC X(58) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 69 PIC X(02) FROM  SPACES.
           03  LINE 11 COLUMN 11 PIC X(58) FROM " ³  0% ................
               "....50%....................100%  ³" BACKGROUND-COLOR 04.
           03  LINE 11 COLUMN 69 PIC X(02) FROM  SPACES.
           03  LINE 12 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 12 COLUMN 69 PIC X(02) FROM  SPACES.
           03  LINE 13 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 13 COLUMN 69 PIC X(02) FROM  SPACES.
           03  LINE 14 COLUMN 11 PIC X(58) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 04.
           03  LINE 14 COLUMN 69 PIC X(02) FROM  SPACES.
           03  LINE 15 COLUMN 13 PIC X(58) FROM  SPACES.

       01  COL-001  LINE 12 COLUMN 15 PIC X(05) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-002  LINE 12 COLUMN 20 PIC X(05) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-003  LINE 12 COLUMN 25 PIC X(05) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-004  LINE 12 COLUMN 30 PIC X(05) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-005  LINE 12 COLUMN 35 PIC X(05) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-006  LINE 12 COLUMN 40 PIC X(05) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-007  LINE 12 COLUMN 45 PIC X(05) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-008  LINE 12 COLUMN 50 PIC X(05) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-009  LINE 12 COLUMN 55 PIC X(05) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-010  LINE 12 COLUMN 60 PIC X(05) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  ENT-001.
           03  LINE 17 COLUMN 20 PIC X(12) FROM "Tot. Lancado"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 35 PIC  ZZ9 USING ACM-QTD
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 41 PIC X(02) FROM "R$"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 43 PIC Z.ZZZ.ZZZ.ZZ9,99 USING ACM-VAL
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.

       01  ENT-002.
           03  NUM-ENT  LINE 08 COLUMN 29 PIC 9(06) USING CHV-LCH
                        BACKGROUND-COLOR  01.
           03  BCO-ENT  LINE 10 COLUMN 29 PIC 9(03) USING BCO-LCH
                        BACKGROUND-COLOR  01.
           03  DIA-ENT  LINE 10 COLUMN 51 PIC 9(02) USING DIA-AUX
                        BACKGROUND-COLOR  01.
           03  MES-ENT  LINE 10 COLUMN 54 PIC 9(02) USING MES-AUX
                        BACKGROUND-COLOR  01.
           03  ANO-ENT  LINE 10 COLUMN 57 PIC 9(02) USING ANO-AUX
                        BACKGROUND-COLOR  01.
           03  NOM-ENT  LINE 12 COLUMN 29 PIC X(30) USING NOM-LCH
                        BACKGROUND-COLOR  01.
           03  VAL-ENT  LINE 14 COLUMN 29 PIC ZZZ.ZZZ.ZZ9,99
                        USING   VAL-LCH   BACKGROUND-COLOR  01.
       01  LPA-001.
           03  LINE 04 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 05 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 21 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.

       01  LPA-002.
           03  LPA-NUM  LINE 08 COLUMN 29 PIC X(06) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-BCO  LINE 10 COLUMN 29 PIC X(03) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-DIA  LINE 10 COLUMN 51 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-MES  LINE 10 COLUMN 54 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-ANO  LINE 10 COLUMN 57 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-NOM  LINE 12 COLUMN 29 PIC X(30) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-VAL  LINE 14 COLUMN 29 PIC X(14) FROM SPACES
                        BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ACUMUL
                   "ANDO CHEQUES-PRE LANCADOS ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DO
                   "CHEQUE  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "BANCO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O DATA P/ DE
                   "POSITO  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NOME DO EM
                   "ITENTE  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR DO C
                   "HEQUE  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DES
                   "EJADA ? [ ]  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ LI
                   "BERAR OS CHEQUES-PRE  -  [ESC] RETORNA ...      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  LIBERA
                   "NDO CHEQUES-PRE LANCADOS ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  MONTAN
                   "DO TELA DE CONSULTA ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-012 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-013 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05.
       01  MEN-014 LINE 22 COLUMN 14 PIC X(67) FROM "LIBERACAO COMPLETA
                   " -  [ESC] RETORNA ...                           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-015 LINE 22 COLUMN 14 PIC X(67) FROM "MEMORIA INSUFICIENT
                   "E P/ CONSULTA EM TELA  -  TECLE [ESC] !!!       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002  LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001
                    TLA-002  MEN-001
           OPEN     I-O      CADLCH
           MOVE     ZEROS    TO  CHV-LCH
                                 ACM-QTD  ACM-VAL
           START    CADLCH   KEY GREATER  CHV-LCH
                    INVALID  KEY GO  TO   ROT-100-000.

       ROT-000-010.
           READ     CADLCH   NEXT AT END
                    GO  TO   ROT-000-020.
           ADD      1        TO  ACM-QTD
           ADD      VAL-LCH  TO  ACM-VAL
           DISPLAY  ENT-001
           GO  TO   ROT-000-010.

       ROT-000-020.
           DISPLAY  TLA-003  MEN-008.

       ROT-000-030.
           MOVE     0   TO   NUM-OPC
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-010.
           IF  NUM-OPC   =   2
               GO  TO        ROT-200-000.
           IF  NUM-OPC   =   3
               GO  TO        ROT-300-000.
           IF  NUM-OPC  NOT  =   1
               GO  TO        ROT-000-030.
           DISPLAY  TLA-002.

       ROT-100-000.
           DISPLAY  ENT-001
                    LPA-002  MEN-002.
       ROT-100-010.
           MOVE     0   TO   CHV-LCH
           ACCEPT   (08 29)  CHV-LCH  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           IF  CHV-LCH   =   ZEROS
               GO  TO        ROT-100-010.
           READ     CADLCH   INVALID KEY
                    GO  TO   ROT-100-100.
           MOVE     DTA-LCH  TO  DTA-AUX
           MOVE     VAL-LCH  TO  VAL-AUX
           DISPLAY  ENT-002.

       ROT-100-020.
           DISPLAY  MEN-003
           ACCEPT   (10 29)  BCO-LCH  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           DISPLAY  BCO-ENT  MEN-004.

       ROT-100-030.
           ACCEPT   (10 51)  DIA-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  DIA-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           IF  DIA-AUX   <   01  OR > 31
               GO  TO        ROT-100-030.

       ROT-100-040.
           ACCEPT   (10 54)  MES-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  MES-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           IF  MES-AUX   <   01  OR > 12
               GO  TO        ROT-100-040.

       ROT-100-050.
           DISPLAY  MEN-004
           ACCEPT   (10 57)  ANO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  ANO-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.

       ROT-100-060.
           DISPLAY  MEN-005
           ACCEPT   (12 29)  NOM-LCH  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  NOM-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-050.

       ROT-100-070.
           DISPLAY  LPA-VAL  MEN-006
           ACCEPT   (14 29)  VAL-LCH  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  VAL-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-060.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-007
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-070.
           MOVE     DTA-AUX  TO  DTA-LCH
           COMPUTE  ACM-VAL  =   ACM-VAL  -  VAL-AUX  +  VAL-LCH
           IF  VAL-LCH   =   0
               GO  TO            ROT-100-080.
           REWRITE  REG-LCH
           GO  TO   ROT-100-000.

       ROT-100-080.
           DELETE   CADLCH
           COMPUTE  ACM-QTD  =   ACM-QTD  -  1
           GO  TO   ROT-100-000.

       ROT-100-100.
           DISPLAY  NUM-ENT
           MOVE     SPACES   TO  NOM-LCH
           MOVE     ZEROS    TO  BCO-LCH
                                 DTA-AUX  VAL-LCH.

       ROT-100-110.
           DISPLAY  LPA-DIA  LPA-MES
                    LPA-ANO  MEN-003
           ACCEPT   (10 29)  BCO-LCH  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-BCO
               GO  TO        ROT-100-000.
           DISPLAY  BCO-ENT  MEN-004.

       ROT-100-120.
           DISPLAY  LPA-MES
           ACCEPT   (10 51)  DIA-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-110.
           IF  DIA-AUX   <   01  OR > 31
               GO  TO        ROT-100-120.
           DISPLAY  DIA-ENT.

       ROT-100-130.
           DISPLAY  LPA-ANO
           ACCEPT   (10 54)  MES-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-120.
           IF  MES-AUX   <   01  OR > 12
               GO  TO        ROT-100-130.
           DISPLAY  MES-ENT.

       ROT-100-140.
           DISPLAY  LPA-NOM  MEN-004
           ACCEPT   (10 57)  ANO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-130.
           DISPLAY  ANO-ENT.

       ROT-100-150.
           DISPLAY  LPA-VAL  MEN-005
           ACCEPT   (12 29)  NOM-LCH  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-140.
           DISPLAY  NOM-ENT.

       ROT-100-160.
           DISPLAY  LPA-VAL  MEN-006
           ACCEPT   (14 29)  VAL-LCH  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-150.
           IF  VAL-LCH   =   ZEROS
               GO  TO        ROT-100-160.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  VAL-ENT      MEN-007
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-160.
           MOVE     DTA-AUX  TO  DTA-LCH
           WRITE    REG-LCH
           ADD      1        TO  ACM-QTD
           ADD      VAL-LCH  TO  ACM-VAL
           GO  TO   ROT-100-000.

       ROT-200-000.
           DISPLAY  MEN-011      TLA-004
           MOVE     ZEROS    TO  CHV-LCH
           START    CADLCH   KEY GREATER  CHV-LCH
           MOVE     1        TO  IND1.

       ROT-200-010.
           READ     CADLCH   NEXT AT END
                    GO  TO   ROT-200-020.
           MOVE     IND1     TO  IND2
           MOVE     CHV-LCH  TO  NUM-TLA
           MOVE     BCO-LCH  TO  BCO-TLA
           MOVE     DTA-LCH  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-TLA
           MOVE     MES-AUX  TO  MES-TLA
           MOVE     ANO-AUX  TO  ANO-TLA
           MOVE     NOM-LCH  TO  NOM-TLA
           MOVE     VAL-LCH  TO  VAL-TLA
           MOVE     TLA-AUX  TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1  <  500
               GO  TO        ROT-200-010.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-015
           ACCEPT   OPC-001
           GO  TO   ROT-200-060.

       ROT-200-020.
           MOVE     SPACES   TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1  <  501
               GO  TO        ROT-200-020.
           MOVE     ZEROS    TO  IND1.

       ROT-200-030.
           MOVE     REG-TAB (IND1 + 01)  TO  LIN-001
           MOVE     REG-TAB (IND1 + 02)  TO  LIN-002
           MOVE     REG-TAB (IND1 + 03)  TO  LIN-003
           MOVE     REG-TAB (IND1 + 04)  TO  LIN-004
           MOVE     REG-TAB (IND1 + 05)  TO  LIN-005
           MOVE     REG-TAB (IND1 + 06)  TO  LIN-006
           MOVE     REG-TAB (IND1 + 07)  TO  LIN-007
           MOVE     REG-TAB (IND1 + 08)  TO  LIN-008
           MOVE     REG-TAB (IND1 + 09)  TO  LIN-009
           MOVE     REG-TAB (IND1 + 10)  TO  LIN-010
           MOVE     REG-TAB (IND1 + 11)  TO  LIN-011
           ADD      11  TO   IND1
           DISPLAY  TLA-006  MEN-012.

       ROT-200-040.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-013
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-200-050.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-200-060.
           IF  IND1  NOT  <  IND2
               GO  TO        ROT-200-040.
           GO  TO   ROT-200-030.

       ROT-200-050.
           IF  IND1   <  12
               GO  TO        ROT-200-040.
           COMPUTE  IND1  =  IND1  -  22
           GO  TO   ROT-200-030.

       ROT-200-060.
           DISPLAY  LPA-001
                    TLA-002
                    ENT-001
           GO  TO   ROT-000-020.

       ROT-300-000.
           DISPLAY  LPA-001
                    TLA-007  MEN-009
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-060.
           DISPLAY  MEN-010
           OPEN     I-O      CADCPD
           COMPUTE  COL-AUX  =   ACM-QTD  /  9
           MOVE     ZEROS    TO  CHV-LCH  ACM-QTD
           START    CADLCH   KEY GREATER  CHV-LCH.

       ROT-300-010.
           READ     CADLCH   NEXT AT END
                    GO  TO   ROT-300-030.
           MOVE     DTA-LCH  TO  DTA-CPD
           MOVE     CHV-LCH  TO  NUM-CPD
           MOVE     BCO-LCH  TO  BCO-CPD
           MOVE     NOM-LCH  TO  NOM-CPD
           MOVE     VAL-LCH  TO  VAL-CPD
           MOVE     ZEROS    TO  LCT-CPD.

       ROT-300-020.
           ADD      1        TO  LCT-CPD
           WRITE    REG-CPD  INVALID KEY
                    GO  TO   ROT-300-020.
           ADD      1        TO  ACM-QTD
           IF  ACM-QTD  NOT  <  (COL-AUX  *  1)  DISPLAY  COL-001.
           IF  ACM-QTD  NOT  <  (COL-AUX  *  2)  DISPLAY  COL-002.
           IF  ACM-QTD  NOT  <  (COL-AUX  *  3)  DISPLAY  COL-003.
           IF  ACM-QTD  NOT  <  (COL-AUX  *  4)  DISPLAY  COL-004.
           IF  ACM-QTD  NOT  <  (COL-AUX  *  5)  DISPLAY  COL-005.
           IF  ACM-QTD  NOT  <  (COL-AUX  *  6)  DISPLAY  COL-006.
           IF  ACM-QTD  NOT  <  (COL-AUX  *  7)  DISPLAY  COL-007.
           IF  ACM-QTD  NOT  <  (COL-AUX  *  8)  DISPLAY  COL-008.
           IF  ACM-QTD  NOT  <  (COL-AUX  *  9)  DISPLAY  COL-009.
           GO  TO   ROT-300-010.

       ROT-300-030.
           CLOSE    CADLCH   CADCPD
           OPEN     OUTPUT   CADLCH
           CLOSE    CADLCH
           OPEN     I-O      CADLCH
           MOVE     0    TO  ACM-QTD  ACM-VAL
           MOVE     " "  TO  RSP-OPC
           DISPLAY  COL-010  MEN-014
           ACCEPT   OPC-001
           DISPLAY  LPA-001
                    TLA-002
           GO  TO   ROT-100-000.

       ROT-400-000.
           IF  ACM-QTD   >   0
               GO  TO        ROT-000-020.

       ROT-400-010.
           MOVE     2   TO   PRM-001
           CLOSE    CADLCH
           CHAIN   "DYN500"   USING  PRM-001.
