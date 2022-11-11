       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN502.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCLI        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CLI.

           SELECT      CADLNT        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LNT.

           SELECT      CADNTA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTA.

           SELECT      CADNTE        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTE.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCLI      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCLI.DAT".
       01  REG-CLI.
           03  CHV-CLI          PIC  X(10).
           03  NOM-CLI          PIC  X(35).
           03  TEL-CLI          PIC  9(12).
           03  CNT-CLI          PIC  X(15).
           03  END-CLI          PIC  X(35).
           03  CID-CLI          PIC  X(20).
           03  CEP-CLI          PIC  9(08).
           03  EST-CLI          PIC  X(02).
           03  CGC-CLI          PIC  9(14).
           03  INS-CLI          PIC  X(15).
           03  CTG-CLI          PIC  9(01).

       FD  CADLNT      LABEL         RECORD  IS  STANDARD
                       VALUE     OF  FILE-ID     "CADLNT.DAT".
       01  REG-LNT.
           03  CHV-LNT          PIC  9(06).
           03  CLI-LNT          PIC  X(10).
           03  PCA-LNT          PIC  X(07).
           03  DES-LNT          PIC  X(20).
           03  VAL-LNT          PIC S9(09)V99.

       FD  CADNTA      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADNTA.DAT".
       01  REG-NTA.
           03  CHV-NTA.
               05  CLI-NTA      PIC  X(10).
               05  DTA-NTA      PIC  9(06).
               05  NUM-NTA      PIC  9(06).
           03  PCA-NTA          PIC  X(07).
           03  DES-NTA          PIC  X(20).
           03  VAL-NTA          PIC S9(09)V99.

       FD  CADNTE      LABEL         RECORD  IS  STANDARD
                       VALUE     OF  FILE-ID     "CADNTE.DAT".
       01  REG-NTE.
           03  CHV-NTE          PIC  9(06).
           03  DTA-NTE          PIC  9(06).
           03  CLI-NTE          PIC  X(10).
           03  PCA-NTE          PIC  X(07).
           03  DES-NTE          PIC  X(20).
           03  VAL-NTE          PIC S9(09)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  RSP-OPC          PIC  X(01).
           03  NUM-OPC          PIC  9(01).
           03  TECLADO          PIC  9(02).
           03  ACM-QTD          PIC  9(03).
           03  COL-AUX          PIC  9(03)V99.
           03  ACM-VAL          PIC S9(10)V99.
           03  VAL-AUX          PIC S9(09)V99.

           03  REG-TAB          PIC  X(66)  OCCURS  510.

           03  PCA-AUX.
               05  LET-PCA      PIC  X(03).
               05  NUM-PCA      PIC  9(04).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  TLA-AUX.
               05  NTA-TLA      PIC  9(06).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  CLI-TLA      PIC  X(12).
               05  LET-TLA      PIC  X(03).
               05  FILLER       PIC  X(01)  VALUE  "-".
               05  NUM-TLA      PIC  9(04).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  DES-TLA      PIC  X(21).
               05  VAL-TLA      PIC  ----.---.--9,99.

           03  LIN-AUX.
               05  LIN-001      PIC  X(66).
               05  LIN-002      PIC  X(66).
               05  LIN-003      PIC  X(66).
               05  LIN-004      PIC  X(66).
               05  LIN-005      PIC  X(66).
               05  LIN-006      PIC  X(66).
               05  LIN-007      PIC  X(66).
               05  LIN-008      PIC  X(66).
               05  LIN-009      PIC  X(66).
               05  LIN-010      PIC  X(66).
               05  LIN-011      PIC  X(66).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "LANCAMENTO DE REQUISICOES  -  Ver. 7.11"
               BACKGROUND-COLOR   02   FOREGROUND-COLOR  00.
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
           03  LINE 19 COLUMN 62 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 06 COLUMN 19 PIC X(41) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 19 PIC X(41) FROM " ³
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 19 PIC X(41) FROM " ³  Nota ......:
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 19 PIC X(41) FROM " ³  Cliente ...:
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 19 PIC X(41) FROM " ³
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 19 PIC X(41) FROM " ³  Placa .....:    -
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 19 PIC X(41) FROM " ³  Descricao .:
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 19 PIC X(41) FROM " ³
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 19 PIC X(41) FROM " ³  Valor .....:
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 19 PIC X(41) FROM " ³
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 19 PIC X(41) FROM " ÃÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"   BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 19 PIC X(41) FROM " ³        ³     ³
               "                ³"   BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 19 PIC X(41) FROM " ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 21 PIC X(41) FROM SPACES.

       01  TLA-003.
           03  LINE 07 COLUMN 29 PIC X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 29 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 31 PIC X(17) FROM "   Requisicoes"
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
           03  LINE 10 COLUMN 43 PIC X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 43 PIC X(21) FROM " ³ Data :   /  /   ³
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 43 PIC X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 45 PIC X(21) FROM SPACES.

       01  TLA-005.
           03  LINE 05 COLUMN 04 PIC X(72) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 04 PIC X(72) FROM " ³ NUMERO  CLIENTE
               " PLACA    DESCRICAO DA NOTA      VALOR LANCADO ³"
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 04 PIC X(72) FROM " ³
               "                                               ³"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 04 PIC X(72) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 06 PIC X(72) FROM SPACES.

       01  TLA-006.
           03  LINE 08 COLUMN 07 PIC X(66) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 07 PIC X(66) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 07 PIC X(66) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 07 PIC X(66) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 07 PIC X(66) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 07 PIC X(66) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 07 PIC X(66) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 07 PIC X(66) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 07 PIC X(66) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 07 PIC X(66) USING LIN-010
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 07 PIC X(66) USING LIN-011
               BACKGROUND-COLOR  01.

       01  TLA-007.
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
           03  LINE 17 COLUMN 22 PIC X(06) FROM "Totais"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 31 PIC  ZZ9 USING ACM-QTD
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 37 PIC X(02) FROM "R$"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 40 PIC --.---.---.--9,99 USING ACM-VAL
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.

       01  ENT-002.
           03  NTA-ENT  LINE 08 COLUMN 36 PIC 9(06) USING CHV-LNT
                        BACKGROUND-COLOR  01.
           03  CLI-ENT  LINE 09 COLUMN 36 PIC X(10) USING CHV-CLI
                        BACKGROUND-COLOR  01.
           03  LET-ENT  LINE 11 COLUMN 36 PIC X(03) USING LET-PCA
                        BACKGROUND-COLOR  01.
           03  NUM-ENT  LINE 11 COLUMN 40 PIC 9(04) USING NUM-PCA
                        BACKGROUND-COLOR  01.
           03  DES-ENT  LINE 12 COLUMN 36 PIC X(20) USING DES-LNT
                        BACKGROUND-COLOR  01.
           03  VAL-ENT  LINE 14 COLUMN 35 PIC ----.---.--9,99
                        USING   VAL-LNT   BACKGROUND-COLOR  01.
       01  ENT-003.
           03  DIA-ENT  LINE 11 COLUMN 53 PIC 9(02) USING DIA-AUX
                        BACKGROUND-COLOR  01.
           03  MES-ENT  LINE 11 COLUMN 56 PIC 9(02) USING MES-AUX
                        BACKGROUND-COLOR  01.
           03  ANO-ENT  LINE 11 COLUMN 59 PIC 9(02) USING ANO-AUX
                        BACKGROUND-COLOR  01.
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
           03  LPA-NTA  LINE 08 COLUMN 36 PIC X(06) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-CLI  LINE 09 COLUMN 36 PIC X(10) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-LET  LINE 11 COLUMN 36 PIC X(03) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-NUM  LINE 11 COLUMN 40 PIC X(04) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-DES  LINE 12 COLUMN 36 PIC X(20) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-VAL  LINE 14 COLUMN 35 PIC X(17) FROM SPACES
                        BACKGROUND-COLOR  01.
       01  LPA-003.
           03  LPA-DIA  LINE 11 COLUMN 53 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-MES  LINE 11 COLUMN 56 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-ANO  LINE 11 COLUMN 59 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.

       01  MEN-001  LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ACUMU
                    "LANDO REQUISICOES LANCADAS ...                   "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002  LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                    " NOTA  -  [ESC] RETORNA ...                      "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003  LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                    " CLIENTE  -  [ESC] RETORNA ...                   "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004  LINE 22 COLUMN 14 PIC X(67) FROM "CLIENTE INEXISTENT
                    "E  -  [ESC] RETORNA ...                          "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005  LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A PLACA DO
                    "VEICULO  -  [ESC] RETORNA ...                    "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006  LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DESCRICAO
                    " DA NOTA  -  [ESC] RETORNA ...                   "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007  LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR DA
                    " NOTA  -  [ESC] RETORNA ...                      "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008  LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ C
                    "ONFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009  LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DE
                    "SEJADA ? [ ]  -  [ESC] RETORNA ...               "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010  LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA DE E
                    "NTRADA DAS REQUISICOES  -  [ESC] RETORNA ...     "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011  LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ L
                    "IBERAR AS REQUISICOES  -  [ESC] RETORNA ...      "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-012  LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  LIBER
                    "ANDO REQUISICOES LANCADAS ...                    "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-013  LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  MONTA
                    "NDO TELA DE CONSULTA ...                         "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-014  LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ RO
                    "LAR A TELA  -  [ESC] RETORNA ...                 "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-015  LINE 22 COLUMN 80 PIC X(01) FROM " "
                    BACKGROUND-COLOR  05.
       01  MEN-016  LINE 22 COLUMN 14 PIC X(67) FROM "LIBERACAO COMPLETA
                    "  -  [ESC] RETORNA ...                           "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-017  LINE 22 COLUMN 14 PIC X(67) FROM "MEMORIA INSUFICIEN
                    "TE P/ CONSULTA EM TELA  -  TECLE [ESC] !!!       "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002  LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001
                    TLA-002  MEN-001
           OPEN     I-O      CADLNT
                    INPUT    CADCLI
           MOVE     SPACES   TO  CHV-CLI
           MOVE     ZEROS    TO  CHV-LNT
                                 ACM-QTD  ACM-VAL
           START    CADLNT   KEY GREATER  CHV-LNT
                    INVALID  KEY GO  TO   ROT-100-000.

       ROT-000-010.
           READ     CADLNT   NEXT AT END
                    GO  TO   ROT-000-020.
           ADD      1        TO  ACM-QTD
           ADD      VAL-LNT  TO  ACM-VAL
           DISPLAY  ENT-001
           GO  TO   ROT-000-010.

       ROT-000-020.
           DISPLAY  TLA-003  MEN-009.

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
           MOVE     SPACES   TO  CHV-CLI
           DISPLAY  TLA-002.

       ROT-100-000.
           DISPLAY  ENT-001
                    LPA-002  MEN-002.
       ROT-100-010.
           MOVE     0   TO   CHV-LNT
           ACCEPT   (08 36)  CHV-LNT  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           IF  CHV-LNT   =   ZEROS
               GO  TO        ROT-100-010.
           READ     CADLNT   INVALID KEY
                    GO  TO   ROT-100-100.
           MOVE     CLI-LNT  TO  CHV-CLI
           MOVE     PCA-LNT  TO  PCA-AUX
           MOVE     VAL-LNT  TO  VAL-AUX
           DISPLAY  ENT-002.

       ROT-100-020.
           DISPLAY  MEN-003
           ACCEPT   (09 36)  CHV-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           IF  CHV-CLI   =   SPACES
               GO  TO        ROT-100-020.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADCLI   INVALID KEY
                    DISPLAY  MEN-004
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-020.
           DISPLAY  CLI-ENT  MEN-005.

       ROT-100-030.
           ACCEPT   (11 36)  LET-PCA  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  LET-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.

       ROT-100-040.
           DISPLAY  MEN-005
           ACCEPT   (11 40)  NUM-PCA  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  NUM-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.

       ROT-100-050.
           DISPLAY  MEN-006
           ACCEPT   (12 36)  DES-LNT  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  DES-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.

       ROT-100-060.
           DISPLAY  LPA-VAL  MEN-007
           ACCEPT   (14 35)  VAL-LNT  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  VAL-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-050.
           DISPLAY  MEN-008
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-060.
           MOVE     CHV-CLI  TO  CLI-LNT
           MOVE     PCA-AUX  TO  PCA-LNT
           COMPUTE  ACM-VAL  =   ACM-VAL  -  VAL-AUX  +  VAL-LNT
           IF  VAL-LNT   =   0
               GO  TO            ROT-100-070.
           REWRITE  REG-LNT
           GO  TO   ROT-100-000.

       ROT-100-070.
           DELETE   CADLNT
           COMPUTE  ACM-QTD  =   ACM-QTD  -  1
           GO  TO   ROT-100-000.

       ROT-100-100.
           DISPLAY  NTA-ENT
           MOVE     ZEROS    TO  VAL-LNT  NUM-PCA
           MOVE     SPACES   TO  LET-PCA  DES-LNT.

       ROT-100-110.
           DISPLAY  LPA-LET  MEN-003
           ACCEPT   (09 36)  CHV-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-CLI
               GO  TO        ROT-100-000.
           IF  CHV-CLI   =   SPACES
               GO  TO        ROT-100-110.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADCLI   INVALID KEY
                    DISPLAY  MEN-004
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-110.
           DISPLAY  CLI-ENT  MEN-005.

       ROT-100-120.
           DISPLAY  LPA-NUM
           ACCEPT   (11 36)  LET-PCA  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-110.
           DISPLAY  LET-ENT.

       ROT-100-130.
           DISPLAY  LPA-DES  MEN-005
           ACCEPT   (11 40)  NUM-PCA  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-120.
           DISPLAY  NUM-ENT.

       ROT-100-140.
           DISPLAY  LPA-VAL  MEN-006
           ACCEPT   (12 36)  DES-LNT  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-130.
           DISPLAY  DES-ENT.

       ROT-100-150.
           DISPLAY  LPA-VAL  MEN-007
           ACCEPT   (14 35)  VAL-LNT  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-140.
           IF  VAL-LNT   =   ZEROS
               GO  TO        ROT-100-150.
           DISPLAY  VAL-ENT  MEN-008
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-150.
           MOVE     CHV-CLI  TO  CLI-LNT
           MOVE     PCA-AUX  TO  PCA-LNT
           WRITE    REG-LNT
           ADD      1        TO  ACM-QTD
           ADD      VAL-LNT  TO  ACM-VAL
           GO  TO   ROT-100-000.

       ROT-200-000.
           DISPLAY  MEN-013      TLA-005
           MOVE     ZEROS    TO  CHV-LNT
           START    CADLNT   KEY GREATER  CHV-LNT
           MOVE     1        TO  IND1.

       ROT-200-010.
           READ     CADLNT   NEXT AT END
                    GO  TO   ROT-200-020.
           MOVE     IND1     TO  IND2
           MOVE     CHV-LNT  TO  NTA-TLA
           MOVE     CLI-LNT  TO  CLI-TLA
           MOVE     PCA-LNT  TO  PCA-AUX
           MOVE     LET-PCA  TO  LET-TLA
           MOVE     NUM-PCA  TO  NUM-TLA
           MOVE     DES-LNT  TO  DES-TLA
           MOVE     VAL-LNT  TO  VAL-TLA
           MOVE     TLA-AUX  TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1  <  500
               GO  TO        ROT-200-010.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-017
           ACCEPT   OPC-001
           GO  TO   ROT-200-060.

       ROT-200-020.
           MOVE     SPACES   TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1  <  511
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
           DISPLAY  TLA-006  MEN-014.

       ROT-200-040.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-015
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
                    TLA-007  TLA-004
           MOVE     ZEROS    TO  DTA-AUX.

       ROT-300-010.
           DISPLAY  LPA-MES  MEN-010
           ACCEPT   (11 53)  DIA-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-090.
           IF  DIA-AUX   <   1  OR >  31
               GO  TO        ROT-300-010.
           DISPLAY  DIA-ENT.

       ROT-300-020.
           DISPLAY  LPA-ANO
           ACCEPT   (11 56)  MES-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-010.
           IF  MES-AUX   <   1  OR >  12
               GO  TO        ROT-300-020.
           DISPLAY  MES-ENT.

       ROT-300-030.
           DISPLAY  MEN-010
           ACCEPT   (11 59)  ANO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-020.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  ANO-ENT      MEN-011
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-030.
           DISPLAY  TLA-007
                    MEN-012
           OPEN     I-O      CADNTA
                             CADNTE
           COMPUTE  COL-AUX  =   ACM-QTD  /  9
           MOVE     ZEROS    TO  CHV-LNT  ACM-QTD
           START    CADLNT   KEY GREATER  CHV-LNT.

       ROT-300-040.
           READ     CADLNT   NEXT AT END
                    GO  TO   ROT-300-080.
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
           MOVE     CLI-LNT  TO  CHV-CLI
           READ     CADCLI
           IF  CTG-CLI   =   5
               GO  TO        ROT-300-060.
           MOVE     CHV-LNT  TO  NUM-NTA
           MOVE     CLI-LNT  TO  CLI-NTA
           MOVE     DTA-AUX  TO  DTA-NTA
           READ     CADNTA   INVALID KEY
                    GO  TO   ROT-300-050.
           ADD      VAL-LNT  TO  VAL-NTA
           REWRITE  REG-NTA
           GO  TO   ROT-300-040.

       ROT-300-050.
           MOVE     PCA-LNT  TO  PCA-NTA
           MOVE     DES-LNT  TO  DES-NTA
           MOVE     VAL-LNT  TO  VAL-NTA
           WRITE    REG-NTA
           GO  TO   ROT-300-040.

       ROT-300-060.
           MOVE     CHV-LNT  TO  CHV-NTE
           READ     CADNTE   INVALID KEY
                    GO  TO   ROT-300-070.
           ADD      VAL-LNT  TO  VAL-NTE
           REWRITE  REG-NTE
           GO  TO   ROT-300-040.

       ROT-300-070.
           MOVE     CLI-LNT  TO  CLI-NTE
           MOVE     DTA-AUX  TO  DTA-NTE
           MOVE     PCA-LNT  TO  PCA-NTE
           MOVE     DES-LNT  TO  DES-NTE
           MOVE     VAL-LNT  TO  VAL-NTE
           WRITE    REG-NTE
           GO  TO   ROT-300-040.

       ROT-300-080.
           CLOSE    CADLNT
                    CADNTA   CADNTE
           OPEN     OUTPUT   CADLNT
           CLOSE    CADLNT
           OPEN     I-O      CADLNT
           MOVE     0    TO  ACM-QTD  ACM-VAL
           MOVE     " "  TO  RSP-OPC
           DISPLAY  COL-010  MEN-016
           ACCEPT   OPC-001
           DISPLAY  LPA-001
                    TLA-002
           GO  TO   ROT-100-000.

       ROT-300-090.
           DISPLAY  LPA-001
                    TLA-002
                    ENT-001
           GO  TO   ROT-000-020.

       ROT-400-000.
           IF  ACM-QTD   >   0
               GO  TO        ROT-000-020.

       ROT-400-010.
           MOVE     2   TO   PRM-001
           CLOSE    CADCLI
                    CADLNT
           CHAIN   "DYN500"   USING  PRM-001.
