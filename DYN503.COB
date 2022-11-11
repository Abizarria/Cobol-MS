       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN503.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADFAT        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-FAT.

           SELECT      CADLRC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LRC.

           SELECT      CADNTE        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTE.

           SELECT      CADSRT        ASSIGN  TO  DISK.


       DATA            DIVISION.
       FILE            SECTION.

       FD  CADFAT      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADFAT.DAT".
       01  REG-FAT.
           03  CHV-FAT          PIC  9(06).
           03  CLI-FAT          PIC  X(10).
           03  DTA-FAT          PIC  9(06).
           03  VAL-FAT          PIC S9(10)V99.

       FD  CADLRC      LABEL         RECORD  IS  STANDARD
                       VALUE     OF  FILE-ID     "CADLRC.DAT".
       01  REG-LRC.
           03  CHV-LRC.
               05  TIP-LRC      PIC  9(01).
               05  NUM-LRC      PIC  9(06).
           03  CLI-LRC          PIC  X(10).
           03  VAL-LRC          PIC S9(10)V99.

       FD  CADNTE      LABEL         RECORD  IS  STANDARD
                       VALUE     OF  FILE-ID     "CADNTE.DAT".
       01  REG-NTE.
           03  CHV-NTE          PIC  9(06).
           03  DTA-NTE          PIC  9(06).
           03  CLI-NTE          PIC  X(10).
           03  PCA-NTE          PIC  X(07).
           03  DES-NTE          PIC  X(20).
           03  VAL-NTE          PIC S9(09)V99.

       SD  CADSRT.

       01  REG-SRT.
           03  NUM-SRT          PIC  9(06).
           03  DTA-SRT          PIC  9(06).
           03  CLI-SRT          PIC  X(10).
           03  PCA-SRT          PIC  X(07).
           03  DES-SRT          PIC  X(20).
           03  VAL-SRT          PIC S9(10)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  RSP-OPC          PIC  X(01).
           03  NUM-OPC          PIC  9(01).
           03  TECLADO          PIC  9(02).
           03  CLI-AUX          PIC  X(10).
           03  ACM-QTD          PIC  9(03).
           03  COL-AUX          PIC  9(03)V99.
           03  ACM-VAL          PIC S9(11)V99.
           03  VAL-AUX          PIC S9(10)V99.

           03  REG-TAB          PIC  X(48)  OCCURS  510.

           03  TLA-AUX.
               05  TIP-TLA      PIC  X(06).
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  NUM-TLA      PIC  9(06).
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  CLI-TLA      PIC  X(13).
               05  VAL-TLA      PIC  --.---.---.--9,99.

           03  LIN-AUX.
               05  LIN-001      PIC  X(48).
               05  LIN-002      PIC  X(48).
               05  LIN-003      PIC  X(48).
               05  LIN-004      PIC  X(48).
               05  LIN-005      PIC  X(48).
               05  LIN-006      PIC  X(48).
               05  LIN-007      PIC  X(48).
               05  LIN-008      PIC  X(48).
               05  LIN-009      PIC  X(48).
               05  LIN-010      PIC  X(48).
               05  LIN-011      PIC  X(48).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "LANCAMENTO DE RECEBIMENTOS  -  Ver. 7.11"
               BACKGROUND-COLOR   02   FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 62 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 62 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 62 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 62 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 62 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 62 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 62 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 42 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 07 COLUMN 19 PIC X(41) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 19 PIC X(41) FROM " ³
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 19 PIC X(41) FROM " ³ Numero .:
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 19 PIC X(41) FROM " ³
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 19 PIC X(41) FROM " ³ Cliente :
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 19 PIC X(41) FROM " ³
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 19 PIC X(41) FROM " ³ Valor ..:
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 19 PIC X(41) FROM " ³
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 19 PIC X(41) FROM " ÃÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"   BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 19 PIC X(41) FROM " ³        ³     ³
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 19 PIC X(41) FROM " ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 21 PIC X(41) FROM SPACES.

       01  TLA-003.
           03  LINE 08 COLUMN 43 PIC X(12) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 53 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 53 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 53 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 26 PIC X(27) FROM
             " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"  BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 26 PIC X(27) FROM
             " ³                       ³"  BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 28 PIC X(23) FROM "   Tipo do Documento"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  01.
           03  LINE 10 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 26 PIC X(27) FROM
             " ÃÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ´"  BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 26 PIC X(27) FROM
             " ³ 1 - Fatura ³ 2 - Nota ³"  BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 26 PIC X(27) FROM
             " ÀÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ"  BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 28 PIC X(27) FROM SPACES.

       01  TLA-004.
           03  LINE 08 COLUMN 43 PIC X(12) FROM " ÚÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 43 PIC X(12) FROM " ³ Fatura ³"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 43 PIC X(12) FROM " ÀÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 45 PIC X(12) FROM SPACES.

       01  TLA-005.
           03  LINE 08 COLUMN 43 PIC X(12) FROM " ÚÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 43 PIC X(12) FROM " ³  Nota  ³"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 43 PIC X(12) FROM " ÀÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 45 PIC X(12) FROM SPACES.

       01  TLA-006.
           03  LINE 08 COLUMN 29 PIC X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 29 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 31 PIC X(17) FROM "   Recebimentos"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  01.
           03  LINE 09 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 29 PIC X(21) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 29 PIC X(21) FROM " ³  1 - Correcao   ³
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 29 PIC X(21) FROM " ³  2 - Consulta   ³
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 29 PIC X(21) FROM " ³  3 - Liberacao  ³
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 29 PIC X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 31 PIC X(21) FROM SPACES.

       01  TLA-007.
           03  LINE 05 COLUMN 12 PIC X(56) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 12 PIC X(56) FROM " ³   TIPO    NUMERO   C
               "LIENTE         VALOR RECEBIDO  ³" BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 12 PIC X(56) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 14 PIC X(56) FROM SPACES.

       01  TLA-008.
           03  LINE 08 COLUMN 16 PIC X(48) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 16 PIC X(48) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 16 PIC X(48) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 16 PIC X(48) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 16 PIC X(48) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 16 PIC X(48) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 16 PIC X(48) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 16 PIC X(48) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 16 PIC X(48) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 16 PIC X(48) USING LIN-010
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 16 PIC X(48) USING LIN-011
               BACKGROUND-COLOR  01.

       01  TLA-009.
           03  LINE 09 COLUMN 11 PIC X(58) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 69 PIC X(02) FROM  SPACES.
           03  LINE 11 COLUMN 11 PIC X(58) FROM " ³  0%.................
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
           03  LINE 16 COLUMN 22 PIC X(06) FROM "Totais"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 16 COLUMN 31 PIC  ZZ9 USING ACM-QTD
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 16 COLUMN 37 PIC X(02) FROM "R$"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 16 COLUMN 39 PIC ---.---.---.--9,99 USING ACM-VAL
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.

       01  ENT-002.
           03  NUM-ENT  LINE 09 COLUMN 32 PIC 9(06) USING NUM-LRC
                        BACKGROUND-COLOR  01.
           03  CLI-ENT  LINE 11 COLUMN 32 PIC X(10) USING CLI-AUX
                        BACKGROUND-COLOR  01.
           03  VAL-ENT  LINE 13 COLUMN 32 PIC --.---.---.--9,99 USING
               VAL-LRC  BACKGROUND-COLOR  01.
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
           03  LPA-NUM  LINE 09 COLUMN 32 PIC X(06) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-CLI  LINE 11 COLUMN 32 PIC X(10) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-VAL  LINE 13 COLUMN 32 PIC X(17) FROM SPACES
                        BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DES
                   "EJADA ? [ ]  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ACUMUL
                   "ANDO RECEBIMENTOS LANCADAS ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DO
                   "DOCUMENTO  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "LIBERACAO COMPLETA
                   "!!!  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "FATURA INEXISTENTE
                   " -  [ESC] RETORNA ...                           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "NOTA INEXISTENTE  -
                   "  [ESC] RETORNA ...                             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR RECE
                   "BIDO  -  [ESC] RETORNA ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ LI
                   "BERAR OS RECEBIMENTOS  -  [ESC] RETORNA ...     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  LIBERA
                   "NDO RECEBIMENTOS LANCADOS ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  MONTAN
                   "DO TELA DE CONSULTA ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-012 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-013 LINE 22 COLUMN 14 PIC X(67) FROM "MEMORIA INSUFICIENT
                   "E P/ CONSULTA EM TELA  -  TECLE [ESC] !!!       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-014 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002  LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001
                    TLA-002  MEN-002
           OPEN     I-O      CADLRC
                    CADFAT   CADNTE
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           MOVE     SPACES   TO  CHV-LRC
           START    CADLRC   KEY GREATER  CHV-LRC
                    INVALID  KEY GO  TO   ROT-100-000.

       ROT-000-010.
           READ     CADLRC   NEXT AT END
                    GO  TO   ROT-000-020.
           ADD      1        TO  ACM-QTD
           ADD      VAL-LRC  TO  ACM-VAL
           DISPLAY  ENT-001
           GO  TO   ROT-000-010.

       ROT-000-020.
           DISPLAY  TLA-006  MEN-001.

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
           DISPLAY  ENT-001  TLA-003.

       ROT-100-010.
           MOVE     0   TO   NUM-OPC
           DISPLAY  MEN-001
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           IF  NUM-OPC   <   1  OR  >  2
               GO  TO        ROT-100-010.
           DISPLAY  TLA-002      ENT-001
           IF  NUM-OPC   =   1   DISPLAY  TLA-004.
           IF  NUM-OPC   =   2   DISPLAY  TLA-005.

       ROT-100-020.
           MOVE     0   TO   NUM-LRC
           DISPLAY  LPA-002  MEN-003
           ACCEPT   (09 32)  NUM-LRC  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           IF  NUM-LRC   =   ZEROS
               GO  TO        ROT-100-020.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADLRC   INVALID KEY
                    GO  TO   ROT-100-100.
           MOVE     CLI-LRC  TO  CLI-AUX
           MOVE     VAL-LRC  TO  VAL-AUX
           DISPLAY  NUM-ENT      CLI-ENT.

       ROT-100-030.
           DISPLAY  LPA-VAL  MEN-007
           ACCEPT   (13 32)  VAL-LRC  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           DISPLAY  VAL-ENT  MEN-008
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           COMPUTE  ACM-VAL  =   ACM-VAL  -  VAL-AUX  +  VAL-LRC
           IF  VAL-LRC   =   0
               GO  TO            ROT-100-040.
           REWRITE  REG-LRC
           DISPLAY  ENT-001
           GO  TO   ROT-100-020.

       ROT-100-040.
           DELETE   CADLRC
           COMPUTE  ACM-QTD  =   ACM-QTD  -  1
           DISPLAY  ENT-001
           GO  TO   ROT-100-020.

       ROT-100-100.
           IF  NUM-OPC   =   2
               GO  TO        ROT-100-120.
           MOVE     NUM-LRC  TO  CHV-FAT
           READ     CADFAT   INVALID KEY
                    DISPLAY  MEN-005
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-020.
           MOVE     CLI-FAT  TO  CLI-AUX
           MOVE     VAL-FAT  TO  VAL-LRC
           DISPLAY  NUM-ENT      CLI-ENT.

       ROT-100-110.
           DISPLAY  LPA-VAL  MEN-007
           ACCEPT   (13 32)  VAL-LRC  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           IF  VAL-LRC   =   0
               GO  TO        ROT-100-110.
           DISPLAY  VAL-ENT  MEN-008
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-110.
           MOVE     CLI-FAT  TO  CLI-LRC
           MOVE     NUM-OPC  TO  TIP-LRC
           ADD      1        TO  ACM-QTD
           ADD      VAL-LRC  TO  ACM-VAL
           WRITE    REG-LRC
           DISPLAY  ENT-001
           GO  TO   ROT-100-020.

       ROT-100-120.
           MOVE     NUM-LRC  TO  CHV-NTE
           READ     CADNTE   INVALID KEY
                    DISPLAY  MEN-006
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-020.
           MOVE     CLI-NTE  TO  CLI-AUX
           MOVE     VAL-NTE  TO  VAL-LRC
           DISPLAY  NUM-ENT      CLI-ENT.

       ROT-100-130.
           DISPLAY  LPA-VAL  MEN-007
           ACCEPT   (13 32)  VAL-LRC  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           IF  VAL-LRC   =   0
               GO  TO        ROT-100-130.
           DISPLAY  VAL-ENT  MEN-008
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-130.
           MOVE     CLI-NTE  TO  CLI-LRC
           MOVE     NUM-OPC  TO  TIP-LRC
           ADD      1        TO  ACM-QTD
           ADD      VAL-LRC  TO  ACM-VAL
           WRITE    REG-LRC
           DISPLAY  ENT-001
           GO  TO   ROT-100-020.

       ROT-200-000.
           DISPLAY  MEN-011      TLA-007
           MOVE     SPACES   TO  CHV-LRC
           START    CADLRC   KEY GREATER  CHV-LRC
           MOVE     1        TO  IND1.

       ROT-200-010.
           READ     CADLRC   NEXT AT END
                    GO  TO   ROT-200-020.
           MOVE     IND1     TO  IND2
           IF  TIP-LRC   =   1   MOVE   "FATURA"  TO  TIP-TLA.
           IF  TIP-LRC   =   2   MOVE   " NOTA "  TO  TIP-TLA.
           MOVE     NUM-LRC  TO  NUM-TLA
           MOVE     CLI-LRC  TO  CLI-TLA
           MOVE     VAL-LRC  TO  VAL-TLA
           MOVE     TLA-AUX  TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1  <  500
               GO  TO        ROT-200-010.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-013
           ACCEPT   OPC-001
           GO  TO   ROT-200-060.

       ROT-200-020.
           MOVE     SPACES   TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1  <  510
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
           DISPLAY  TLA-008  MEN-012.

       ROT-200-040.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-014
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
                    TLA-009  MEN-009
           MOVE     " "  TO  RSP-OPC.

       ROT-300-010.
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-060.
           DISPLAY  MEN-010
           COMPUTE  COL-AUX  =   ACM-QTD  /  9
           MOVE     ZEROS    TO  CHV-FAT    ACM-QTD
           START    CADFAT   KEY GREATER    CHV-FAT
                    INVALID  KEY GO  TO     ROT-300-200.
           SORT     CADSRT   ON  ASCENDING  KEY NUM-SRT
                    INPUT        PROCEDURE  IS  ROT-300-100
                    OUTPUT       PROCEDURE  IS  ROT-300-140
           GO  TO   ROT-300-200.

       ROT-300-100  SECTION.

       ROT-300-110.
           READ     CADFAT   NEXT AT END
                    GO  TO   ROT-300-130.
           MOVE     1        TO  TIP-LRC
           MOVE     CHV-FAT  TO  NUM-LRC
           READ     CADLRC   INVALID KEY
                    GO  TO   ROT-300-120.
           ADD      1        TO  ACM-QTD
           IF  ACM-QTD  NOT  <  (COL-AUX * 1)  DISPLAY  COL-001.
           IF  ACM-QTD  NOT  <  (COL-AUX * 2)  DISPLAY  COL-002.
           IF  ACM-QTD  NOT  <  (COL-AUX * 3)  DISPLAY  COL-003.
           IF  ACM-QTD  NOT  <  (COL-AUX * 4)  DISPLAY  COL-004.
           IF  ACM-QTD  NOT  <  (COL-AUX * 5)  DISPLAY  COL-005.
           IF  ACM-QTD  NOT  <  (COL-AUX * 6)  DISPLAY  COL-006.
           IF  ACM-QTD  NOT  <  (COL-AUX * 7)  DISPLAY  COL-007.
           IF  ACM-QTD  NOT  <  (COL-AUX * 8)  DISPLAY  COL-008.
           IF  ACM-QTD  NOT  <  (COL-AUX * 9)  DISPLAY  COL-009.
           IF  VAL-FAT  NOT  >   VAL-LRC
               GO  TO        ROT-300-110.
           COMPUTE  VAL-FAT  =   VAL-FAT - VAL-LRC.

       ROT-300-120.
           MOVE     CHV-FAT  TO  NUM-SRT
           MOVE     CLI-FAT  TO  CLI-SRT
           MOVE     DTA-FAT  TO  DTA-SRT
           MOVE     VAL-FAT  TO  VAL-SRT
           RELEASE  REG-SRT
           GO  TO   ROT-300-110.

       ROT-300-130.
           CLOSE    CADFAT
           OPEN     OUTPUT   CADFAT.

       ROT-300-140  SECTION.

       ROT-300-150.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-300-160.
           MOVE     NUM-SRT  TO  CHV-FAT
           MOVE     CLI-SRT  TO  CLI-FAT
           MOVE     DTA-SRT  TO  DTA-FAT
           MOVE     VAL-SRT  TO  VAL-FAT
           WRITE    REG-FAT
           GO  TO   ROT-300-150.

       ROT-300-160.
           CLOSE    CADFAT
           OPEN     I-O      CADFAT.

       ROT-300-170  SECTION.

       ROT-300-200.
           MOVE     ZEROS    TO  CHV-NTE
           START    CADNTE   KEY GREATER    CHV-NTE
                    INVALID  KEY GO  TO     ROT-300-290.
           SORT     CADSRT   ON  ASCENDING  KEY NUM-SRT
                    INPUT        PROCEDURE  IS  ROT-300-210
                    OUTPUT       PROCEDURE  IS  ROT-300-250
           GO  TO   ROT-300-290.

       ROT-300-210  SECTION.

       ROT-300-220.
           READ     CADNTE   NEXT AT END
                    GO  TO   ROT-300-240.
           MOVE     2        TO  TIP-LRC
           MOVE     CHV-NTE  TO  NUM-LRC
           READ     CADLRC   INVALID KEY
                    GO  TO   ROT-300-230.
           ADD      1        TO  ACM-QTD
           IF  ACM-QTD   >  (COL-AUX * 1)  DISPLAY  COL-001.
           IF  ACM-QTD   >  (COL-AUX * 2)  DISPLAY  COL-002.
           IF  ACM-QTD   >  (COL-AUX * 3)  DISPLAY  COL-003.
           IF  ACM-QTD   >  (COL-AUX * 4)  DISPLAY  COL-004.
           IF  ACM-QTD   >  (COL-AUX * 5)  DISPLAY  COL-005.
           IF  ACM-QTD   >  (COL-AUX * 6)  DISPLAY  COL-006.
           IF  ACM-QTD   >  (COL-AUX * 7)  DISPLAY  COL-007.
           IF  ACM-QTD   >  (COL-AUX * 8)  DISPLAY  COL-008.
           IF  ACM-QTD   >  (COL-AUX * 9)  DISPLAY  COL-009.
           IF  VAL-NTE  NOT  >   VAL-LRC
               GO  TO        ROT-300-220.
           COMPUTE  VAL-NTE  =   VAL-NTE  -  VAL-LRC.

       ROT-300-230.
           MOVE     CHV-NTE  TO  NUM-SRT
           MOVE     CLI-NTE  TO  CLI-SRT
           MOVE     DTA-NTE  TO  DTA-SRT
           MOVE     PCA-NTE  TO  PCA-SRT
           MOVE     DES-NTE  TO  DES-SRT
           MOVE     VAL-NTE  TO  VAL-SRT
           RELEASE  REG-SRT
           GO  TO   ROT-300-220.

       ROT-300-240.
           CLOSE    CADNTE
           OPEN     OUTPUT   CADNTE.

       ROT-300-250  SECTION.

       ROT-300-260.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-300-270.
           MOVE     NUM-SRT  TO  CHV-NTE
           MOVE     CLI-SRT  TO  CLI-NTE
           MOVE     DTA-SRT  TO  DTA-NTE
           MOVE     PCA-SRT  TO  PCA-NTE
           MOVE     DES-SRT  TO  DES-NTE
           MOVE     VAL-SRT  TO  VAL-NTE
           WRITE    REG-NTE
           GO  TO   ROT-300-260.

       ROT-300-270.
           CLOSE    CADNTE
           OPEN     I-O      CADNTE.

       ROT-300-280  SECTION.

       ROT-300-290.
           CLOSE    CADLRC
           OPEN     OUTPUT   CADLRC
           CLOSE    CADLRC
           OPEN     I-O      CADLRC
           MOVE     0   TO   ACM-QTD  ACM-VAL
           DISPLAY  COL-010  MEN-004
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           DISPLAY  LPA-001  TLA-002
           GO  TO   ROT-100-000.

       ROT-400-000.
           IF  ACM-QTD   =   0
               GO  TO        ROT-400-010.
           DISPLAY  TLA-002
                    ENT-001
           GO  TO   ROT-000-020.

       ROT-400-010.
           MOVE     2   TO   PRM-001
           CLOSE    CADFAT
                    CADLRC   CADNTE
           CHAIN   "DYN500"  USING  PRM-001.
