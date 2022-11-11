       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN403.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADDSP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-DSP.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP.

           SELECT      CADSRT        ASSIGN  TO  DISK.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADDSP      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADDSP.DAT".
       01  REG-DSP.
           03  CHV-DSP.
               05  PRX-DSP      PIC  9(01).
               05  SUF-DSP      PIC  9(03).
           03  DES-DSP          PIC  X(25).

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

       FD  CADLDP      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLDP.DAT".
       01  REG-LDP.
           03  CHV-LDP.
               05  COD-LDP      PIC  9(04).
               05  DTA-LDP      PIC  9(06).
               05  LCT-LDP      PIC  9(03).
           03  FLG-LDP          PIC  9(01).
           03  PGT-LDP          PIC  9(06).
           03  HST-LDP          PIC  X(20).
           03  VAL-LDP          PIC S9(10)V99.

       SD  CADSRT.

       01  REG-SRT.
           03  CHV-SRT.
               05  COD-SRT      PIC  9(04).
               05  DTA-SRT      PIC  9(06).
               05  LCT-SRT      PIC  9(03).
           03  FLG-SRT          PIC  9(01).
           03  PGT-SRT          PIC  9(06).
           03  HST-SRT          PIC  X(20).
           03  VAL-SRT          PIC S9(10)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  IND3             PIC  9(03).
           03  IND4             PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  PGT-AUX          PIC  9(01).
           03  ATV-AUX          PIC  X(18).
           03  ACM-VAL          PIC S9(10)V99.
           03  SET-AUX          PIC  X(01)  VALUE  "¯".
           03  FLG-AUX          PIC  X(01)  VALUE  "û".

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  TAB-ATV.
               05  FILLER       PIC  X(18)  VALUE  "OBRIG. TRABALHISTA".
               05  FILLER       PIC  X(18)  VALUE  "OBRIGACOES FISCAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESP. OPERACIONAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESPESA DE CAPITAL".
               05  FILLER       PIC  X(18)  VALUE  "CREDITOS DE SOCIOS".
               05  FILLER       PIC  X(18)  VALUE  "COMPRA DE PRODUTOS".
           03  RED-ATV          REDEFINES   TAB-ATV.
               05  ATV-TAB      PIC  X(18)  OCCURS  06.

           03  TAB-CTA                      OCCURS 300.
               05  CHV-TAB      PIC  X(13).
               05  REG-TAB.
                   07  SET-TAB  PIC  X(02).
                   07  FLG-TAB  PIC  X(02).
                   07  DET-TAB  PIC  X(55).

           03  LIN-001          PIC  X(61).
           03  LIN-002          PIC  X(61).
           03  LIN-003          PIC  X(61).
           03  LIN-004          PIC  X(61).

       01  DET-001.
           03  DIA-DET          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DET          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DET          PIC  9(02).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  PRX-DET          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DET          PIC  9(03).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  HST-DET          PIC  X(21).
           03  VAL-DET          PIC  --.---.---.--9,99.

       LINKAGE         SECTION.

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
           03  LINE 13 COLUMN 27 PIC X(03) FROM " 4"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 09 COLUMN 30 PIC X(19) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 30 PIC X(19) FROM " ³               ³
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 32 PIC X(15) FROM "   Pagamentos"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 11 COLUMN 30 PIC X(19) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 30 PIC X(19) FROM " ³  1 - A Vista  ³
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 30 PIC X(19) FROM " ³  2 - A Prazo  ³
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 30 PIC X(19) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 32 PIC X(19) FROM SPACES.

       01  TLA-002.
           03  LINE 06 COLUMN 17 PIC X(44) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 17 PIC X(44) FROM " ³
              "                   ³" BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 17 PIC X(44) FROM " ³  Banco .....:      -
              "                   ³" BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 17 PIC X(44) FROM " ³
              "                   ³" BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(44) FROM " ³  Data ......:   /  /
              "                   ³" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(44) FROM " ³  Documento .:
              "                   ³" BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(44) FROM " ³
              "                   ³" BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(44) FROM " ³  Movimento .:    -
              "                   ³" BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(44) FROM " ³  Historico .:
              "                   ³" BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(44) FROM " ³
              "                   ³" BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 17 PIC X(44) FROM " ³  Valor .....:
              "                   ³" BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 17 PIC X(44) FROM " ³
              "                   ³" BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 17 PIC X(44) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  01.
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
           03  LINE 13 COLUMN 21 PIC X(48) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"  BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 21 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 21 PIC X(48) FROM " ³  Conta ....:  -
               "-                      ³"  BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 21 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 21 PIC X(48) FROM " ³  Descricao :
               "                       ³"  BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 21 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  04.
           03  LINE 18 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 21 PIC X(48) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"  BACKGROUND-COLOR  04.
           03  LINE 19 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 23 PIC X(48) FROM SPACES.

       01  TLA-005.
           03  LINE 12 COLUMN 08 PIC X(65) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 08 PIC X(65) FROM " ³    VENCIMENTO CONTA
               " HISTORICO                VALOR A PAGAR ³"
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 08 PIC X(65) FROM " ³
               "                                        ³"
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 08 PIC X(65) FROM " ³
               "                                        ³"
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 08 PIC X(65) FROM " ³
               "                                        ³"
               BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 08 PIC X(65) FROM " ³
               "                                        ³"
               BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 08 PIC X(65) FROM " ³
               "                                        ³"
               BACKGROUND-COLOR  04.
           03  LINE 18 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 08 PIC X(65) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 19 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 10 PIC X(65) FROM SPACES.

       01  ENT-001.
           03  CHV-ENT LINE 08 COLUMN 34 PIC 9(03) USING BCO-PR2
                       BACKGROUND-COLOR  01.
           03  NOM-ENT LINE 08 COLUMN 42 PIC X(15) USING NOM-PR2
                       BACKGROUND-COLOR  01.
           03  DIA-ENT LINE 10 COLUMN 34 PIC 9(02) USING DIA-PR2
                       BACKGROUND-COLOR  01.
           03  MES-ENT LINE 10 COLUMN 37 PIC 9(02) USING MES-PR2
                       BACKGROUND-COLOR  01.
           03  ANO-ENT LINE 10 COLUMN 40 PIC 9(02) USING ANO-PR2
                       BACKGROUND-COLOR  01.
           03  DOC-ENT LINE 11 COLUMN 34 PIC 9(06) USING DOC-PR2
                       BACKGROUND-COLOR  01.

       01  ENT-002.
           03  COD-ENT LINE 13 COLUMN 34 PIC 9(01) FROM 4
                       BACKGROUND-COLOR  01.
           03  MOV-ENT LINE 13 COLUMN 40 PIC X(17) FROM
               "EMISSAO DE CHEQUE"    BACKGROUND-COLOR  01.
           03  HST-ENT LINE 14 COLUMN 34 PIC X(20) USING HST-LBC
                       BACKGROUND-COLOR  01.
           03  VAL-ENT LINE 16 COLUMN 34 PIC Z.ZZZ.ZZZ.ZZ9,99 USING
               VAL-LBC BACKGROUND-COLOR  01.

       01  ENT-003.
           03  PRX-ENT LINE 15 COLUMN 37 PIC 9(01) USING PRX-DSP
                       BACKGROUND-COLOR  04.
           03  SUF-ENT LINE 15 COLUMN 39 PIC 9(03) USING SUF-DSP
                       BACKGROUND-COLOR  04.
           03  ATV-ENT LINE 15 COLUMN 47 PIC X(18) USING ATV-AUX
                       BACKGROUND-COLOR  04.
           03  DES-ENT LINE 17 COLUMN 37 PIC X(25) USING DES-DSP
                       BACKGROUND-COLOR  04.

       01  ENT-004.
           03  LINE 15 COLUMN 11 PIC X(59) USING LIN-001
               BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 11 PIC X(59) USING LIN-002
               BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 11 PIC X(59) USING LIN-003
               BACKGROUND-COLOR  04.
           03  LINE 18 COLUMN 11 PIC X(59) USING LIN-004
               BACKGROUND-COLOR  04.

       01  LPA-001.
           03  LPA-MOV LINE 13 COLUMN 40 PIC X(17) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VAL LINE 16 COLUMN 34 PIC X(16) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SUF LINE 15 COLUMN 39 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  04.
           03  LPA-ATV LINE 15 COLUMN 47 PIC X(18) FROM SPACES
                       BACKGROUND-COLOR  04.
           03  LPA-DES LINE 17 COLUMN 37 PIC X(25) FROM SPACES
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

       01  LPA-003.
           03  LINE 20 COLUMN 10 PIC X(65) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 08 PIC X(67) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 08 PIC X(67) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 08 PIC X(67) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 08 PIC X(67) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 08 PIC X(67) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 08 PIC X(67) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 08 PIC X(67) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 08 PIC X(65) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO INEXISTENTE
                   " !!!  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O HISTORICO
                   "DO MOVIMENTO  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR DO M
                   "OVIMENTO  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "TIPO DE PAGAMENTO ?
                   "  [ ]  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DA
                   "CONTA  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO CONTAS PENDENTES ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE CONTAS P
                   "ENDENTES  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR, [F1] P/ MARCAR OU [ESC] P/ RETORNAR ...     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 80 PIC X(01) FROM SPACES
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 36 PIC 9(01) USING PGT-AUX AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  USING  PRM-001  PRM-002.

       ROT-000-000.
           OPEN     INPUT    CADDSP
                    I-O      CADLBC  CADLDP.

       ROT-000-010.
           DISPLAY  TLA-001
                    MEN-005
           MOVE     SPACES   TO  HST-LBC
           MOVE     ZEROS    TO  VAL-LBC.

       ROT-000-020.
           MOVE     0   TO   PGT-AUX
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               MOVE  2  TO   PRM-001
               GO  TO        ROT-300-000.
           IF  PGT-AUX   =   2
               GO  TO        ROT-200-000.
           IF  PGT-AUX  NOT  =   1
               GO  TO        ROT-000-020.

       ROT-100-000.
           DISPLAY  TLA-002
                    ENT-001  TLA-004.

       ROT-100-010.
           DISPLAY  LPA-SUF
                    LPA-ATV  MEN-006
           MOVE     0    TO  PRX-DSP
           ACCEPT   (15 37)  PRX-DSP  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-002
                             TLA-002
                             NOM-ENT
                             TLA-003
               GO  TO        ROT-000-010.
           IF  PRX-DSP   <   1  OR  >  6
               GO  TO        ROT-100-010.
           MOVE     ATV-TAB (PRX-DSP) TO  ATV-AUX
           DISPLAY  PRX-ENT  ATV-ENT.

       ROT-100-020.
           DISPLAY  LPA-DES  MEN-007
           MOVE     0    TO  SUF-DSP
           ACCEPT   (15 39)  SUF-DSP  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           IF  SUF-DSP   =   ZEROS
               GO  TO        ROT-100-020.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADDSP   INVALID KEY
                    DISPLAY  MEN-001
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-020.

       ROT-100-030.
           DISPLAY  ENT-003  MEN-004
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.

       ROT-100-040.
           DISPLAY  LPA-002
                    TLA-002  ENT-001
                    COD-ENT  MOV-ENT.

       ROT-100-050.
           DISPLAY  LPA-VAL  MEN-002
           ACCEPT   (14 34)  HST-LBC  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       TLA-004
               GO  TO        ROT-100-030.
           IF  HST-LBC   =   SPACES
               GO  TO        ROT-100-050.
           DISPLAY  HST-ENT.

       ROT-100-060.
           DISPLAY  LPA-VAL  MEN-003
           ACCEPT   (16 34)  VAL-LBC  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-050.
           IF  VAL-LBC   =   ZEROS
               GO  TO        ROT-100-060.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  VAL-ENT      MEN-004
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-060.
           MOVE     BCO-PR2  TO  BCO-LBC
           MOVE     DTA-PR2  TO  DTA-LBC
                                 DTA-LDP  PGT-LDP
           MOVE     DOC-PR2  TO  DOC-LBC
           MOVE     1        TO  FLG-LBC
           MOVE     4        TO  COD-LBC
           MOVE     2        TO  FLG-LDP
           MOVE     CHV-DSP  TO  COD-LDP
           MOVE     HST-LBC  TO  HST-LDP
           MOVE     VAL-LBC  TO  VAL-LDP
           MOVE     ZEROS    TO  LCT-LBC  LCT-LDP.

       ROT-100-070.
           ADD      1        TO  LCT-LBC
           WRITE    REG-LBC  INVALID KEY
                    GO  TO   ROT-100-070.

       ROT-100-080.
           ADD      1        TO  LCT-LDP
           WRITE    REG-LDP  INVALID KEY
                    GO  TO   ROT-100-080.
           MOVE     1        TO  PRM-001
           GO  TO   ROT-300-000.

       ROT-200-000.
           DISPLAY  TLA-002      ENT-001
                    COD-ENT      MOV-ENT    MEN-008
           MOVE     SPACES   TO  CHV-LDP
           START    CADLDP   KEY GREATER    CHV-LDP
                    INVALID  KEY GO  TO     ROT-200-010.
           SORT     CADSRT   ON  ASCENDING  KEY PGT-SRT
                    INPUT        PROCEDURE  IS  ROT-200-020
                    OUTPUT       PROCEDURE  IS  ROT-200-060
           IF  IND1   >   1
               GO  TO            ROT-200-100.

       ROT-200-010.
           MOVE     " "  TO  RSP-OPC
           DISPLAY  MEN-009
           ACCEPT   OPC-001
           DISPLAY  LPA-MOV  TLA-003
           GO  TO   ROT-000-010.

       ROT-200-020  SECTION.

       ROT-200-030.
           READ     CADLDP   NEXT AT END
                    GO  TO   ROT-200-040.
           IF  FLG-LDP  NOT  =   1
               GO  TO        ROT-200-030.
           MOVE     CHV-LDP  TO  CHV-SRT
           MOVE     FLG-LDP  TO  FLG-SRT
           MOVE     HST-LDP  TO  HST-SRT
           MOVE     PGT-LDP  TO  PGT-SRT
           MOVE     VAL-LDP  TO  VAL-SRT
           RELEASE  REG-SRT
           GO  TO   ROT-200-030.

       ROT-200-040.
           MOVE     ZEROS    TO  IND1.

       ROT-200-050.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
                                 CHV-TAB (IND1)
           IF  IND1  <  300
               GO  TO        ROT-200-050.
           MOVE     1        TO  IND1.

       ROT-200-060  SECTION.

       ROT-200-070.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-200-080.
           MOVE     PGT-SRT  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DET
           MOVE     MES-AUX  TO  MES-DET
           MOVE     ANO-AUX  TO  ANO-DET
           MOVE     COD-SRT  TO  CHV-DSP
           MOVE     PRX-DSP  TO  PRX-DET
           MOVE     SUF-DSP  TO  SUF-DET
           MOVE     HST-SRT  TO  HST-DET
           MOVE     VAL-SRT  TO  VAL-DET
           MOVE     CHV-SRT  TO  CHV-TAB (IND1)
           MOVE     DET-001  TO  DET-TAB (IND1)
           ADD      1        TO  IND1
           GO  TO   ROT-200-070.

       ROT-200-080.
           EXIT.

       ROT-200-090  SECTION.

       ROT-200-100.
           DISPLAY  TLA-005
           COMPUTE  IND3     =   IND1  -  1
           MOVE     1        TO  IND2
                                 IND4
           MOVE     ZEROS    TO  IND1
           MOVE     SET-AUX  TO  SET-TAB (IND2).

       ROT-200-110.
           MOVE     REG-TAB (IND1 + 01)  TO  LIN-001
           MOVE     REG-TAB (IND1 + 02)  TO  LIN-002
           MOVE     REG-TAB (IND1 + 03)  TO  LIN-003
           MOVE     REG-TAB (IND1 + 04)  TO  LIN-004
           DISPLAY  ENT-004  MEN-010.

       ROT-200-120.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  MEN-011
           IF  TECLADO   =   99
               GO  TO        ROT-200-130.
           IF  TECLADO   =   02
               GO  TO        ROT-200-140.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-200-150.
           IF  IND2  NOT  <  IND3
               GO  TO        ROT-200-120.
           MOVE     SPACES   TO  SET-TAB (IND2)
           ADD      1        TO  IND2
           MOVE     SET-AUX  TO  SET-TAB (IND2)
           IF  IND4   <   4
               ADD    1      TO  IND4
           ELSE
               ADD    1      TO  IND1.
           GO  TO   ROT-200-110.

       ROT-200-130.
           IF  IND2   =   1
               GO  TO        ROT-200-120.
           MOVE     SPACES   TO  SET-TAB (IND2)
           COMPUTE  IND2     =   IND2  -  1
           MOVE     SET-AUX  TO  SET-TAB (IND2)
           IF  IND4   >   1
               COMPUTE IND4  =   IND4  -  1
           ELSE
               COMPUTE IND1  =   IND1  -  1.
           GO  TO   ROT-200-110.

       ROT-200-140.
           IF  FLG-TAB (IND2)  =   SPACES
               MOVE   FLG-AUX  TO  FLG-TAB (IND2)
           ELSE
               MOVE   SPACES   TO  FLG-TAB (IND2).
           GO  TO     ROT-200-110.

       ROT-200-150.
           MOVE     ZEROS    TO  IND1
                                 ACM-VAL
           MOVE     SPACES   TO  SET-TAB (IND2).

       ROT-200-160.
           ADD      1   TO   IND1
           IF  IND1     >    IND3
               GO  TO        ROT-200-170.
           IF  FLG-TAB (IND1)  =  "  "
               GO  TO        ROT-200-160.
           MOVE     CHV-TAB (IND1)   TO  CHV-LDP
           READ     CADLDP
           ADD      VAL-LDP  TO  ACM-VAL
           GO  TO   ROT-200-160.

       ROT-200-170.
           DISPLAY  LPA-003  TLA-002
           IF  ACM-VAL   =   ZEROS
               DISPLAY       NOM-ENT
                             TLA-003
               GO  TO        ROT-000-010.
           DISPLAY  ENT-001
                    COD-ENT
                    MOV-ENT
           MOVE     ACM-VAL  TO  VAL-LBC.

       ROT-200-180.
           DISPLAY  LPA-VAL  MEN-002
           ACCEPT   (14 34)  HST-LBC  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       TLA-005
               GO  TO        ROT-200-100.
           IF  HST-LBC   =   SPACES
               GO  TO        ROT-200-180.
           DISPLAY  HST-ENT.

       ROT-200-190.
           DISPLAY  LPA-VAL  MEN-003
           ACCEPT   (16 34)  VAL-LBC  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-180.
           IF  VAL-LBC   =   ZEROS
               GO  TO        ROT-200-190.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  VAL-ENT      MEN-004
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-190.
           MOVE     BCO-PR2  TO  BCO-LBC
           MOVE     DTA-PR2  TO  DTA-LBC
           MOVE     DOC-PR2  TO  DOC-LBC
           MOVE     4        TO  COD-LBC
           MOVE     1        TO  FLG-LBC
           MOVE     ZEROS    TO  LCT-LBC  IND1.

       ROT-200-200.
           ADD      1        TO  LCT-LBC
           WRITE    REG-LBC  INVALID KEY
                    GO  TO   ROT-200-200.

       ROT-200-210.
           ADD      1   TO   IND1
           IF  IND1     >    IND3
               MOVE     1    TO  PRM-001
               GO  TO        ROT-300-000.
           IF  FLG-TAB (IND1)  =  "  "
               GO  TO        ROT-200-210.
           MOVE     CHV-TAB (IND1)  TO  CHV-LDP
           READ     CADLDP
           MOVE     DTA-PR2  TO  PGT-LDP
           MOVE     2        TO  FLG-LDP
           REWRITE  REG-LDP
           GO  TO   ROT-200-210.

       ROT-300-000.
           CLOSE    CADDSP
                    CADLBC  CADLDP
           EXIT     PROGRAM.
