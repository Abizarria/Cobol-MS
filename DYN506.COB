       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN506.
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

           SELECT      CADCTR        ASSIGN  TO  DISK.

           SELECT      CADNTA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTA.

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      CADSRT        ASSIGN  TO  DISK.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

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

       FD  CADCTR      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCTR.DAT".
       01  REG-CTR.
           03  DIA-CTR          PIC  9(02).
           03  DTI-CTR          PIC  9(06).
           03  DTC-CTR          PIC  9(06).
           03  DTD-CTR          PIC  9(06).
           03  FAT-CTR          PIC  9(06).
           03  PCT-CTR          PIC  9(02)V99.

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

       FD  CADPRM      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADPRM.DAT".
       01  REG-PRM.
           03  COD-PRM          PIC  9(05).
           03  CLI-PRM          PIC  9(02)  OCCURS  36.
           03  END-PRM          PIC  9(02)  OCCURS  35.
           03  CID-PRM          PIC  9(02)  OCCURS  20.
           03  EST-PRM          PIC  9(02)  OCCURS  02.
           03  CEP-PRM          PIC  9(08).
           03  CGC-PRM          PIC  9(14).
           03  INS-PRM          PIC  9(02)  OCCURS  15.

       SD  CADSRT.

       01  REG-SRT.
           03  CHV-SRT.
               05  CLI-SRT      PIC  X(10).
               05  DTA-SRT      PIC  9(08).
               05  NUM-SRT      PIC  9(06).
           03  PCA-SRT          PIC  X(07).
           03  DES-SRT          PIC  X(20).
           03  VAL-SRT          PIC S9(09)V99.

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC  X(80).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  NUM-OPC          PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  FAT-AUX          PIC  9(06).
           03  PAG-AUX          PIC  X(09).
           03  MG1-AUX          PIC  X(40).
           03  MG2-AUX          PIC  X(40).
           03  MG3-AUX          PIC  X(40).
           03  ACM-LIN          PIC  9(02).
           03  ACM-QTD          PIC  9(03).
           03  TOT-QTD          PIC  9(04).
           03  ACM-VAL          PIC S9(10)V99.
           03  TOT-VAL          PIC S9(11)V99.

           03  REG-TAB          PIC  X(80)  OCCURS  510.

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

           03  TAB-PAG.
               05  FILLER       PIC  X(09)  VALUE  "SEMANAL  ".
               05  FILLER       PIC  X(09)  VALUE  "DEZENAL  ".
               05  FILLER       PIC  X(09)  VALUE  "QUINZENAL".
               05  FILLER       PIC  X(09)  VALUE  "MENSAL   ".
           03  RED-PAG          REDEFINES   TAB-PAG.
               05  PAG-TAB      PIC  X(09)  OCCURS  04.

           03  PCA-AUX.
               05  LET-PCA      PIC  X(03).
               05  NUM-PCA      PIC  9(04).

           03  TEL-AUX.
               05  DDD-AUX      PIC  9(04).
               05  PRX-AUX      PIC  9(04).
               05  SUF-AUX      PIC  9(04).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  DTA-LIM.
               05  ANO-LIM      PIC  9(04).
               05  MES-LIM      PIC  9(02).
               05  DIA-LIM      PIC  9(02).

           03  DTA-LCT.
               05  ANO-LCT      PIC  9(04).
               05  MES-LCT      PIC  9(02).
               05  DIA-LCT      PIC  9(02).

           03  TP1-CAB     PIC  X(21)  VALUE  "FATURA DE SERVICOS   ".
           03  TP2-CAB     PIC  X(21)  VALUE  "RESUMO DO FATURAMENTO".

           03  TLA-AUX.
               05  LIN-001      PIC  X(80).
               05  LIN-002      PIC  X(80).
               05  LIN-003      PIC  X(80).
               05  LIN-004      PIC  X(80).
               05  LIN-005      PIC  X(80).
               05  LIN-006      PIC  X(80).
               05  LIN-007      PIC  X(80).
               05  LIN-008      PIC  X(80).
               05  LIN-009      PIC  X(80).
               05  LIN-010      PIC  X(80).
               05  LIN-011      PIC  X(80).
               05  LIN-012      PIC  X(80).
               05  LIN-013      PIC  X(80).
               05  LIN-014      PIC  X(80).
               05  LIN-015      PIC  X(80).
               05  LIN-016      PIC  X(80).
               05  LIN-017      PIC  X(80).
               05  LIN-018      PIC  X(80).

       01  CAB-001.
           03  FILLER           PIC  X(22)  VALUE  "-DYNAMIC-".
           03  NOM-CB1          PIC  X(48).
           03  FILLER           PIC  X(08)  VALUE  "FOLHA :".
           03  PAG-CB1          PIC  9(02).

       01  CAB-002.
           03  TIP-CB2          PIC  X(72).
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004.
           03  FILLER           PIC  X(13)  VALUE  "| CLIENTE .:".
           03  NOM-CB4          PIC  X(41).
           03  FILLER           PIC  X(17)  VALUE  "| FATURA ....:".
           03  FAT-CB4          PIC  999.999.
           03  FILLER           PIC  X(02)  VALUE  " |".

       01  CAB-005              PIC  X(80)  VALUE
           "|   DATA   |  NOTA  | DESCRICAO            |  PLACA   |   VA
           "LOR DA NOTA | OBS: |".

       01  CAB-006              PIC  X(80)  VALUE
           "|-----------------------------------------------------------
           "-------------------|".

       01  CAB-007              PIC  X(80)  VALUE  ALL  "- ".

       01  CAB-008              PIC  X(80)  VALUE
           "FATURA   CLIENTE          TELEFONE       NOME / CONTATO  NOT
           "AS   VALOR DA FATURA".

       01  DET-001.
           03  FILLER           PIC  X(02)  VALUE  "|".
           03  DIA-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DT1          PIC  9(02).
           03  FILLER           PIC  X(03)  VALUE  " |".
           03  NTA-DT1          PIC  9(06).
           03  FILLER           PIC  X(03)  VALUE  " |".
           03  DES-DT1          PIC  X(20).
           03  FILLER           PIC  X(03)  VALUE  " |".
           03  LET-DT1          PIC  X(03).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  NUM-DT1          PIC  9(04).
           03  FILLER           PIC  X(03)  VALUE  " |".
           03  VAL-DT1          PIC  ----.---.--9,99.
           03  FILLER           PIC  X(09)  VALUE  " | ____ |".

       01  DET-002.
           03  FAT-DT2          PIC  9(06).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  CLI-DT2          PIC  X(13).
           03  FILLER           PIC  X(01)  VALUE  "(".
           03  DDD-DT2          PIC  9(04).
           03  FILLER           PIC  X(02)  VALUE  ")".
           03  PRX-DT2          PIC  ZZZ9.
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DT2          PIC  9(04).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  CNT-DT2          PIC  X(17).
           03  QTD-DT2          PIC  ZZ9.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  VAL-DT2          PIC  --.---.---.--9,99.

       01  TOT-001.
           03  FILLER           PIC  X(16)  VALUE  "| REQUISICOES:".
           03  QTD-TT1          PIC  ZZ9.
           03  FILLER           PIC  X(35)  VALUE
               " | VALOR TOTAL DA FATURA ....... R$".
           03  VAL-TT1          PIC  --.---.---.--9,99.
           03  FILLER           PIC  X(09)  VALUE  " | ____ |".

       01  TOT-002.
           03  FILLER           PIC  X(56)  VALUE  "
               "  - TOTAL DO FATURAMENTO .........".
           03  QTD-TT2          PIC  Z.ZZ9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  VAL-TT2          PIC  ---.---.---.--9,99.

       01  MSG-001.
           03  FILLER           PIC  X(20)  VALUE  "|".
           03  DES-MG1          PIC  X(59).
           03  FILLER           PIC  X(01)  VALUE  "|".

       01  MSG-002.
           03  FILLER           PIC  X(20)  VALUE  "|".
           03  DES-MG2          PIC  X(59).
           03  FILLER           PIC  X(01)  VALUE  "|".

       01  MSG-003.
           03  FILLER           PIC  X(20)  VALUE  "|".
           03  DES-MG3          PIC  X(59).
           03  FILLER           PIC  X(01)  VALUE  "|".

       01  ROD-001              PIC  X(80)  VALUE
           "| RECEBIDO EM:                      /   /     ASSINATURA:
           "                   |".

       01  ROD-002              PIC  X(80)  VALUE
           "|             -----------------, -----------              --
           "------------------ |".

       01  PRM-001              PIC  9(01).

       01  PRM-002.
           03  CLI-PR2          PIC  X(10).
           03  CTG-PR2          PIC  9(01).
           03  DTA-PR2.
               05  ANO-PR2      PIC  9(02).
               05  MES-PR2      PIC  9(02).
               05  DIA-PR2      PIC  9(02).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "FATURAMENTO DO PERIODO  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 72 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 72 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 72 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 72 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 72 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 72 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 19 PIC X(58) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 36 PIC X(31) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 42 PIC X(25) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 48 PIC X(19) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 07 COLUMN 11 PIC X(59) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 11 PIC X(59) FROM " ³
              "                                  ³" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 11 PIC X(59) FROM " ³ Pagamento .:
              "          Data Limite .:   /  /   ³" BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 11 PIC X(59) FROM " ³
              "                                  ³" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 11 PIC X(59) FROM " ³ Mensagens .:
              "                                  ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 11 PIC X(59) FROM " ³
              "                                  ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 11 PIC X(59) FROM " ³
              "                                  ³" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 11 PIC X(59) FROM " ³
              "                                  ³" BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 11 PIC X(59) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 13 PIC X(59) FROM SPACES.

       01  TLA-003.
           03  LINE 08 COLUMN 22 PIC X(37) FROM
             " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 04.
           03  LINE 09 COLUMN 22 PIC X(37) FROM
             " ³                                 ³" BACKGROUND-COLOR 04.
           03  LINE 09 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 24 PIC X(33) FROM
               "       Forma de Pagamento"          BACKGROUND-COLOR 07
               FOREGROUND-COLOR  01.
           03  LINE 10 COLUMN 22 PIC X(37) FROM
             " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 22 PIC X(37) FROM
             " ³  1 - Semanal  ³  3 - Quinzenal  ³" BACKGROUND-COLOR 04.
           03  LINE 11 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 22 PIC X(37) FROM
             " ³  2 - Dezenal  ³  4 - Mensal     ³" BACKGROUND-COLOR 04.
           03  LINE 12 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 22 PIC X(37) FROM
             " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 04.
           03  LINE 13 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 24 PIC X(37) FROM SPACES.

       01  TLA-004.
           03  LINE 11 COLUMN 17 PIC X(58) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 04.
           03  LINE 12 COLUMN 17 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 12 COLUMN 75 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(58) FROM " ³  0%.................
               "....50%....................100%  ³" BACKGROUND-COLOR 04.
           03  LINE 13 COLUMN 75 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 14 COLUMN 75 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 15 COLUMN 75 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 17 PIC X(58) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 04.
           03  LINE 16 COLUMN 75 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 19 PIC X(58) FROM SPACES.

       01  ENT-001.
           03  PAG-ENT  LINE 09 COLUMN 27 PIC X(09) USING PAG-AUX
                        BACKGROUND-COLOR  01.
           03  DIA-ENT  LINE 09 COLUMN 59 PIC 9(02) USING DIA-PR2
                        BACKGROUND-COLOR  01.
           03  MES-ENT  LINE 09 COLUMN 62 PIC 9(02) USING MES-PR2
                        BACKGROUND-COLOR  01.
           03  ANO-ENT  LINE 09 COLUMN 65 PIC 9(02) USING ANO-PR2
                        BACKGROUND-COLOR  01.
           03  MG1-ENT  LINE 11 COLUMN 27 PIC X(40) USING MG1-AUX
                        BACKGROUND-COLOR  01.
           03  MG2-ENT  LINE 12 COLUMN 27 PIC X(40) USING MG2-AUX
                        BACKGROUND-COLOR  01.
           03  MG3-ENT  LINE 13 COLUMN 27 PIC X(40) USING MG3-AUX
                        BACKGROUND-COLOR  01.
       01  LPA-001.
           03  LPA-MES  LINE 09 COLUMN 62 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-ANO  LINE 09 COLUMN 65 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-MG1  LINE 11 COLUMN 27 PIC X(40) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-MG2  LINE 12 COLUMN 27 PIC X(40) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-MG3  LINE 13 COLUMN 27 PIC X(40) FROM SPACES
                        BACKGROUND-COLOR  01.
       01  LPA-002.
           03  LINE 04 COLUMN 01 PIC X(80) USING LIN-001
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 05 COLUMN 01 PIC X(80) USING LIN-002
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 06 COLUMN 01 PIC X(80) USING LIN-003
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 07 COLUMN 01 PIC X(80) USING LIN-004
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 08 COLUMN 01 PIC X(80) USING LIN-005
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 09 COLUMN 01 PIC X(80) USING LIN-006
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 10 COLUMN 01 PIC X(80) USING LIN-007
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 11 COLUMN 01 PIC X(80) USING LIN-008
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 12 COLUMN 01 PIC X(80) USING LIN-009
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 13 COLUMN 01 PIC X(80) USING LIN-010
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 14 COLUMN 01 PIC X(80) USING LIN-011
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 15 COLUMN 01 PIC X(80) USING LIN-012
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 16 COLUMN 01 PIC X(80) USING LIN-013
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 17 COLUMN 01 PIC X(80) USING LIN-014
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 18 COLUMN 01 PIC X(80) USING LIN-015
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 19 COLUMN 01 PIC X(80) USING LIN-016
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 20 COLUMN 01 PIC X(80) USING LIN-017
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 21 COLUMN 01 PIC X(80) USING LIN-018
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DES
                   "EJADA ? [ ]  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTO ...                             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA LIMIT
                   "E DO FATURAMENTO  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE MOVIMENT
                   "O NO PERIODO !!!  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE AS MENSGENS
                   "DO FATURAMENTO  -  [ESC] RETORNA ...           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA CONSULTAR NA
                   " TELA ? [S/N]  [ ]  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  MONTAN
                   "DO TELA DE CONSULTA ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO FATURAMENTO DO PERIODO ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-012 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ LI
                   "BERAR O FATURAMENTO  -  [ESC] RETORNA ...       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-013 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  LIBERA
                   "NDO FATURAMENTO DO PERIODO ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-014 LINE 22 COLUMN 14 PIC X(67) FROM "MEMORIA INSUFICIENT
                   "E P/ CONSULTA EM TELA  -  TECLE [ESC] !!!       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-003  LINE 22 COLUMN 49 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     INPUT    CADCLI  CADCTR
                             CADNTA  CADPRM
           READ     CADCTR
           READ     CADPRM
           MOVE     1   TO   IND1.

       ROT-000-010.
           MOVE     CLI-PRM (IND1 + 1)  TO IND2
           MOVE     LET-TAB (IND2)  TO  LET-NOM (IND1)
           ADD      1   TO   IND1
           IF  IND1  <  36
               GO  TO        ROT-000-010.
           MOVE     SPACES   TO  LET-NOM (IND1)
           ACCEPT   DTA-AUX  FROM   DATE
           MOVE     12       TO  ANO-AUX
           IF  MES-AUX   <   12  MOVE 13  TO  ANO-AUX.
           MOVE     NOM-TAB  TO  NOM-CB1
           MOVE     DIA-AUX  TO  DIA-CB2
           MOVE     MES-AUX  TO  MES-CB2
           MOVE     ANO-AUX  TO  ANO-CB2.

       ROT-000-020.
           DISPLAY  TLA-003  MEN-001.

       ROT-000-030.
           MOVE     0   TO   NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-030.
           IF  NUM-OPC   <   1  OR  >  4
               GO  TO        ROT-000-030.
           MOVE     PAG-TAB (NUM-OPC) TO  PAG-AUX
           DISPLAY  TLA-002  PAG-ENT
           MOVE     ZEROS    TO  DTA-PR2
           MOVE     SPACES   TO  MG1-AUX
                                 MG2-AUX  MG3-AUX.
       ROT-000-040.
           DISPLAY  LPA-MES  MEN-003
           ACCEPT   (09 59)  DIA-PR2  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           IF  DIA-PR2   <   1  OR >  31
               GO  TO        ROT-000-040.
           DISPLAY  DIA-ENT.

       ROT-000-050.
           DISPLAY  LPA-ANO
           ACCEPT   (09 62)  MES-PR2  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-040.
           IF  MES-PR2   <   1  OR >  12
               GO  TO        ROT-000-050.
           DISPLAY  MES-ENT.

       ROT-000-060.
           DISPLAY  LPA-MG1  MEN-003
           ACCEPT   (09 65)  ANO-PR2  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-050.
           DISPLAY  ANO-ENT  MEN-002
           MOVE     DIA-PR2  TO  DIA-LIM
           MOVE     MES-PR2  TO  MES-LIM
           MOVE     ANO-PR2  TO  ANO-LIM
           IF  ANO-PR2   <   10
               ADD     2000  TO  ANO-LIM
           ELSE
               ADD     1900  TO  ANO-LIM.
           MOVE     SPACES   TO  CHV-CLI  RSP-OPC
           START    CADCLI   KEY GREATER  CHV-CLI
                    INVALID  KEY GO  TO   ROT-400-020.

       ROT-000-070.
           READ     CADCLI   NEXT AT END
                    GO  TO   ROT-400-020.
           IF  CTG-CLI  NOT  =   NUM-OPC
               GO  TO        ROT-000-070.
           MOVE     SPACES   TO  CHV-NTA
           MOVE     CHV-CLI  TO  CLI-NTA
           START    CADNTA   KEY GREATER  CHV-NTA
                    INVALID  KEY GO  TO   ROT-400-020.
           READ     CADNTA   NEXT
           IF  CLI-NTA NOT   =   CHV-CLI
               GO  TO        ROT-000-070.

       ROT-000-080.
           DISPLAY  LPA-MG2  MEN-005
           ACCEPT   (11 27)  MG1-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-060.
           DISPLAY  MG1-ENT.

       ROT-000-090.
           DISPLAY  LPA-MG3
           ACCEPT   (12 27)  MG2-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-080.
           DISPLAY  MG2-ENT.

       ROT-000-100.
           DISPLAY  MEN-005
           ACCEPT   (13 27)  MG3-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-090.
           DISPLAY  MG3-ENT  MEN-007
           MOVE     SPACES   TO  CHV-CLI
           START    CADCLI   KEY GREATER         CHV-CLI
           SORT     CADSRT   ON  ASCENDING  KEY  CHV-SRT
                    INPUT        PROCEDURE  IS   ROT-100-000
                    OUTPUT       PROCEDURE  IS   ROT-100-040
           GO  TO   ROT-000-020.

       ROT-100-000  SECTION.

       ROT-100-010.
           READ     CADCLI   NEXT AT END
                    GO  TO   ROT-100-030.
           IF  CTG-CLI  NOT  =   NUM-OPC
               GO  TO        ROT-100-010.
           MOVE     SPACES   TO  CHV-NTA
           MOVE     CHV-CLI  TO  CLI-NTA
           START    CADNTA   KEY GREATER  CHV-NTA
                    INVALID  KEY GO  TO   ROT-100-030.

       ROT-100-020.
           READ     CADNTA   NEXT AT END
                    GO  TO   ROT-100-030.
           IF  CLI-NTA  NOT   =  CHV-CLI
               GO  TO        ROT-100-010.
           MOVE     DTA-NTA  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-LCT
           MOVE     MES-AUX  TO  MES-LCT
           MOVE     ANO-AUX  TO  ANO-LCT
           IF  ANO-AUX   <   10
               ADD     2000  TO  ANO-LCT
           ELSE
               ADD     1900  TO  ANO-LCT.
           IF  DTA-LCT   >   DTA-LIM
               GO  TO        ROT-100-020.
           MOVE     CLI-NTA  TO  CLI-SRT
           MOVE     DTA-LCT  TO  DTA-SRT
           MOVE     NUM-NTA  TO  NUM-SRT
           MOVE     PCA-NTA  TO  PCA-SRT
           MOVE     DES-NTA  TO  DES-SRT
           MOVE     VAL-NTA  TO  VAL-SRT
           RELEASE  REG-SRT  GO  ROT-100-020.

       ROT-100-030.

       ROT-100-040  SECTION.

       ROT-100-060.
           DISPLAY  MEN-006
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-003
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           IF  RSP-OPC  NOT  =  "S"  AND  "N"
               GO  TO        ROT-100-060.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-400-000.
           MOVE     CLI-SRT  TO  CHV-CLI
           MOVE     FAT-CTR  TO  FAT-AUX
           MOVE     TP2-CAB  TO  TIP-CB2
           MOVE     ZEROS    TO  PAG-CB1  IND1
                                 ACM-QTD  ACM-VAL
                                 TOT-QTD  TOT-VAL
           IF  RSP-OPC   =   "N"
               GO  TO        ROT-300-000.

       ROT-200-000.
           ADD      1        TO  PAG-CB1
           IF  PAG-CB1   >   8
               GO  TO        ROT-200-090.
           MOVE     CAB-001  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           MOVE     CAB-002  TO  REG-TAB (IND1 + 3)
           MOVE     CAB-003  TO  REG-TAB (IND1 + 4)
           MOVE     CAB-008  TO  REG-TAB (IND1 + 5)
           MOVE     CAB-003  TO  REG-TAB (IND1 + 6)
           MOVE     ZEROS    TO  ACM-LIN
           ADD      6        TO  IND1.

       ROT-200-010.
           ADD      1        TO  ACM-QTD
           ADD      VAL-SRT  TO  ACM-VAL.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-200-030.
           IF  CLI-SRT   =   CHV-CLI
               GO   TO       ROT-200-010.

       ROT-200-020.
           ADD      1        TO  ACM-LIN
           IF  ACM-LIN   <   54
               PERFORM       ROT-200-030
               GO  TO        ROT-200-010.
           MOVE     SPACES   TO  REG-TAB (IND1 + 1)
           MOVE     CAB-007  TO  REG-TAB (IND1 + 2)
           MOVE     SPACES   TO  REG-TAB (IND1 + 3)
           ADD      3        TO  IND1
           PERFORM  ROT-200-000
           PERFORM  ROT-200-030  GO     ROT-200-010.

       ROT-200-030.
           ADD      1        TO  FAT-AUX
           MOVE     FAT-AUX  TO  FAT-DT2
           MOVE     CHV-CLI  TO  CLI-DT2
           READ     CADCLI
           MOVE     TEL-CLI  TO  TEL-AUX
           MOVE     DDD-AUX  TO  DDD-DT2
           MOVE     PRX-AUX  TO  PRX-DT2
           MOVE     SUF-AUX  TO  SUF-DT2
           MOVE     CNT-CLI  TO  CNT-DT2
           MOVE     ACM-QTD  TO  QTD-DT2
           MOVE     ACM-VAL  TO  VAL-DT2
           MOVE     DET-002  TO  REG-TAB (IND1 + 1)
           ADD      ACM-QTD  TO  TOT-QTD
           ADD      ACM-VAL  TO  TOT-VAL
           ADD      1        TO  IND1
           MOVE     CLI-SRT  TO  CHV-CLI
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL.

       ROT-200-040.
           MOVE     TOT-QTD  TO  QTD-TT2
           MOVE     TOT-VAL  TO  VAL-TT2
           MOVE     SPACES   TO  REG-TAB (IND1 + 1)
           MOVE     TOT-002  TO  REG-TAB (IND1 + 2)
           ADD      2        TO  IND1
           MOVE     IND1     TO  IND2.

       ROT-200-050.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1  <  500
               GO  TO        ROT-200-050.
           MOVE     ZEROS    TO  IND1.

       ROT-200-060.
           MOVE     REG-TAB  (IND1 + 01)  TO  LIN-001
           MOVE     REG-TAB  (IND1 + 02)  TO  LIN-002
           MOVE     REG-TAB  (IND1 + 03)  TO  LIN-003
           MOVE     REG-TAB  (IND1 + 04)  TO  LIN-004
           MOVE     REG-TAB  (IND1 + 05)  TO  LIN-005
           MOVE     REG-TAB  (IND1 + 06)  TO  LIN-006
           MOVE     REG-TAB  (IND1 + 07)  TO  LIN-007
           MOVE     REG-TAB  (IND1 + 08)  TO  LIN-008
           MOVE     REG-TAB  (IND1 + 09)  TO  LIN-009
           MOVE     REG-TAB  (IND1 + 10)  TO  LIN-010
           MOVE     REG-TAB  (IND1 + 11)  TO  LIN-011
           MOVE     REG-TAB  (IND1 + 12)  TO  LIN-012
           MOVE     REG-TAB  (IND1 + 13)  TO  LIN-013
           MOVE     REG-TAB  (IND1 + 14)  TO  LIN-014
           MOVE     REG-TAB  (IND1 + 15)  TO  LIN-015
           MOVE     REG-TAB  (IND1 + 16)  TO  LIN-016
           MOVE     REG-TAB  (IND1 + 17)  TO  LIN-017
           MOVE     REG-TAB  (IND1 + 18)  TO  LIN-018
           ADD      18  TO    IND1
           DISPLAY  LPA-002   MEN-010.

       ROT-200-070.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-011
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-200-080.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-400-000.
           IF  IND1  NOT  <  IND2
               GO  TO        ROT-200-070.
           GO  TO   ROT-200-060.

       ROT-200-080.
           IF  IND1   <  36
               GO  TO    ROT-200-070.
           SUBTRACT  36  FROM   IND1
           GO  TO        ROT-200-060.

       ROT-200-090.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-014
           ACCEPT   OPC-002
           GO  TO   ROT-400-000.

       ROT-300-000.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-008
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           DISPLAY  MEN-009
           OPEN     OUTPUT   RELATO
           MOVE     1        TO  PAG-CB1
           MOVE     TP1-CAB  TO  TIP-CB2
           MOVE     MG1-AUX  TO  DES-MG1
           MOVE     MG2-AUX  TO  DES-MG2
           MOVE     MG3-AUX  TO  DES-MG3
           WRITE    REG-REL FROM CAB-001  AFTER 0.

       ROT-300-010.
           ADD      1        TO  FAT-AUX
           MOVE     CLI-SRT  TO  CHV-CLI
           READ     CADCLI
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL.

       ROT-300-020.
           MOVE     NOM-CLI  TO  NOM-CB4
           MOVE     FAT-AUX  TO  FAT-CB4
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-006
           WRITE    REG-REL FROM CAB-005
           WRITE    REG-REL FROM CAB-006
           MOVE     ZEROS    TO  ACM-LIN.

       ROT-300-030.
           MOVE     DTA-SRT  TO  DTA-LCT
           IF  ANO-LCT   <  2000
               COMPUTE           ANO-LCT  =  ANO-LCT  -  1900
           ELSE
               COMPUTE           ANO-LCT  =  ANO-LCT  -  2000.
           MOVE     DIA-LCT  TO  DIA-DT1
           MOVE     MES-LCT  TO  MES-DT1
           MOVE     ANO-LCT  TO  ANO-DT1
           MOVE     NUM-SRT  TO  NTA-DT1
           MOVE     DES-SRT  TO  DES-DT1
           MOVE     PCA-SRT  TO  PCA-AUX
           MOVE     LET-PCA  TO  LET-DT1
           MOVE     NUM-PCA  TO  NUM-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      1        TO  ACM-QTD
           ADD      VAL-SRT  TO  ACM-VAL
           WRITE    REG-REL FROM DET-001
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-300-060.
           IF  CLI-SRT  NOT  =   CHV-CLI
               PERFORM       ROT-300-060
               THRU          ROT-300-070
               GO  TO        ROT-300-050.
           ADD      1        TO  ACM-LIN
           IF  ACM-LIN   <   41
               GO  TO        ROT-300-030.
           PERFORM  ROT-300-070.

       ROT-300-040.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           GO  TO   ROT-300-020.

       ROT-300-050.
           MOVE     1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           GO  TO   ROT-300-010.

       ROT-300-060.
           MOVE     ACM-QTD  TO  QTD-TT1
           MOVE     ACM-VAL  TO  VAL-TT1
           WRITE    REG-REL FROM CAB-006
           WRITE    REG-REL FROM TOT-001.

       ROT-300-070.
           WRITE    REG-REL FROM CAB-006
           WRITE    REG-REL FROM MSG-001
           WRITE    REG-REL FROM MSG-002
           WRITE    REG-REL FROM MSG-003
           WRITE    REG-REL FROM CAB-006
           WRITE    REG-REL FROM ROD-001
           WRITE    REG-REL FROM ROD-002
           WRITE    REG-REL FROM CAB-003.

       ROT-300-080.
           CLOSE    RELATO
           DISPLAY  MEN-012
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           DISPLAY  TLA-004  MEN-013
           CLOSE    CADCLI   CADCTR
                    CADNTA   CADPRM
           MOVE     2        TO  PRM-001
           MOVE     NUM-OPC  TO  CTG-PR2
           MOVE     SPACES   TO  CLI-PR2
           CHAIN   "DYN507"      USING  PRM-001  PRM-002.

       ROT-400-000.
           MOVE     SPACES   TO  TLA-AUX
           DISPLAY  LPA-002      TLA-002.

       ROT-400-010  SECTION.

       ROT-400-020.
           DISPLAY  MEN-004
           ACCEPT   OPC-002
           DISPLAY  LPA-ANO
           GO  TO   ROT-000-040.

       ROT-400-030.
           MOVE     3   TO   PRM-001
           CLOSE    CADCLI   CADCTR
                    CADNTA   CADPRM
           CHAIN   "DYN500"   USING  PRM-001.
