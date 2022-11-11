       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN301.
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

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADDSP      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADDSP.DAT".
       01  REG-DSP.
           03  CHV-DSP.
               05  PRX-DSP      PIC  9(01).
               05  SUF-DSP      PIC  9(03).
           03  DES-DSP          PIC  X(25).

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

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  NUM-OPC          PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  OPC-TLA          PIC  X(09).
           03  ATV-TLA          PIC  X(18).
           03  ACM-PAG          PIC S9(11)V99.
           03  ACM-CPG          PIC S9(11)V99.

       01  TAB-OPC.
           03  FILLER           PIC  X(09)  VALUE  "INCLUSAO ".
           03  FILLER           PIC  X(09)  VALUE  "ALTERACAO".
           03  FILLER           PIC  X(09)  VALUE  "CONSULTA ".
           03  FILLER           PIC  X(09)  VALUE  "EXCLUSAO ".
       01  RED-OPC              REDEFINES   TAB-OPC.
           03  OPC-TAB          PIC  X(09)  OCCURS  04.

       01  TAB-ATV.
           03  FILLER           PIC  X(18)  VALUE  "OBRIG. TRABALHISTA".
           03  FILLER           PIC  X(18)  VALUE  "OBRIGACOES FISCAIS".
           03  FILLER           PIC  X(18)  VALUE  "DESP. OPERACIONAIS".
           03  FILLER           PIC  X(18)  VALUE  "DESPESA DE CAPITAL".
           03  FILLER           PIC  X(18)  VALUE  "CREDITOS DE SOCIOS".
           03  FILLER           PIC  X(18)  VALUE  "COMPRA DE PRODUTOS".
       01  RED-ATV              REDEFINES   TAB-ATV.
           03  ATV-TAB          PIC  X(18)  OCCURS  06.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "CADASTRO DE CONTAS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 57 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 57 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 57 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 57 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 57 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 57 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 57 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 57 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 57 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 57 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 07 COLUMN 06 PIC X(49) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 06 PIC X(49) FROM " ³
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 06 PIC X(49) FROM " ³  Codigo ....:  -
               " -                      ³" BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 06 PIC X(49) FROM " ³
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 06 PIC X(49) FROM " ³  Descricao .:
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 06 PIC X(49) FROM " ³
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 06 PIC X(49) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 06 PIC X(49) FROM " ³
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 10 PIC X(21) FROM "Total Liquidado .. R$"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 14 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 06 PIC X(49) FROM " ³
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 06 PIC X(49) FROM " ³
               "                        ³" BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 10 PIC X(21) FROM "Total Pendente ... R$"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 16 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 06 PIC X(49) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 01.
           03  LINE 17 COLUMN 55 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 08 PIC X(49) FROM SPACES.

       01  TLA-003.
           03  LINE 08 COLUMN 60 PIC X(15) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 60 PIC X(17) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 60 PIC X(17) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 62 PIC X(15) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 21 PIC X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 21 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 23 PIC X(17) FROM "    Despesas"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 01.
           03  LINE 09 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 21 PIC X(21) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 21 PIC X(21) FROM " ³  1 - Incluir    ³
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 21 PIC X(21) FROM " ³  2 - Alterar    ³
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 21 PIC X(21) FROM " ³  3 - Consultar  ³
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 21 PIC X(21) FROM " ³  4 - Excluir    ³
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 21 PIC X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 23 PIC X(21) FROM SPACES.

       01  TLA-004.
           03  LINE 08 COLUMN 60 PIC X(15) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 60 PIC X(03) FROM " ³"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 63 PIC X(09) USING OPC-TLA
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 72 PIC X(03) FROM " ³"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 75 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 60 PIC X(15) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 75 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 62 PIC X(15) FROM SPACES.

       01  TLA-005.
           03  PRX-ENT LINE 09 COLUMN 23 PIC 9(01) USING PRX-DSP
                       BACKGROUND-COLOR  01.
           03  SUF-ENT LINE 09 COLUMN 25 PIC 9(03) USING SUF-DSP
                       BACKGROUND-COLOR  01.
           03  ATV-ENT LINE 09 COLUMN 33 PIC X(18) USING ATV-TLA
                       BACKGROUND-COLOR  01.

       01  TLA-006.
           03  DES-ENT LINE 11 COLUMN 23 PIC X(25) USING DES-DSP
                       BACKGROUND-COLOR  01.
           03  PAG-ENT LINE 14 COLUMN 33 PIC ---.---.---.--9,99 USING
               ACM-PAG BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  CPG-ENT LINE 16 COLUMN 33 PIC ---.---.---.--9,99 USING
               ACM-CPG BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.

       01  TLA-007.
           03  LPA-PRX LINE 09 COLUMN 23 PIC X(01) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SUF LINE 09 COLUMN 25 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-ATV LINE 09 COLUMN 33 PIC X(18) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  TLA-008.
           03  LPA-DES LINE 11 COLUMN 23 PIC X(25) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-PAG LINE 14 COLUMN 33 PIC X(18) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CPG LINE 16 COLUMN 33 PIC X(18) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DES
                   "EJADA ? [ ]  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "CONTA JA CADASTRADA
                   " !!!  -  [ESC] RETORNA ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "CONTA INEXISTENTE !
                   "!!  -  [ESC] RETORNA ...                        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DA
                   "CONTA  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DESCRICAO
                   "DA CONTA  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "CONTA COM MOVIMENTO
                   " NO PERIODO  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTOS ...                            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR A EXCLUSAO  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NSULTAR OUTRA CONTA ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.                                "

       01  OPC-001 LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001
                    TLA-002
           OPEN     I-O      CADDSP
                    INPUT    CADLDP.

       ROT-000-010.
           DISPLAY  TLA-007  TLA-003
                             MEN-001.
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
           DISPLAY  LPA-SUF  LPA-ATV
                             MEN-002
           MOVE     0   TO   PRX-DSP
           ACCEPT   (09 23)  PRX-DSP  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           IF  PRX-DSP   <   1  OR  >  6
               GO  TO        ROT-000-030.
           MOVE     ATV-TAB (PRX-DSP) TO  ATV-TLA
           DISPLAY  PRX-ENT  ATV-ENT
           MOVE     0    TO  SUF-DSP.

       ROT-000-040.
           DISPLAY  TLA-008  MEN-005
           ACCEPT   (09 25)  SUF-DSP  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           IF  SUF-DSP   =   ZEROS
               GO  TO        ROT-000-040.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADDSP   INVALID KEY
                    GO  TO   ROT-100-000.
           IF  NUM-OPC   =   1
               GO  TO        ROT-000-080.
           DISPLAY  SUF-ENT
                    DES-ENT      MEN-009
           MOVE     ZEROS    TO  ACM-PAG
                                 ACM-CPG
           MOVE     SPACES   TO  CHV-LDP
           MOVE     CHV-DSP  TO  COD-LDP
           START    CADLDP   KEY GREATER  CHV-LDP
                    INVALID  KEY GO  TO   ROT-000-050.

       ROT-000-050.
           READ     CADLDP   NEXT AT END
                    GO  TO   ROT-000-060.
           IF  COD-LDP  NOT  =   CHV-DSP
               GO  TO        ROT-000-060.
           IF  FLG-LDP   =   1
               ADD  VAL-LDP  TO  ACM-CPG
           ELSE
               ADD  VAL-LDP  TO  ACM-PAG.
           GO  TO   ROT-000-050.

       ROT-000-060.
           DISPLAY  PAG-ENT      CPG-ENT
           GO  TO   ROT-100-000, ROT-200-000, ROT-300-000,
                    ROT-400-000, DEPENDING    ON  NUM-OPC.

       ROT-000-070.
           DISPLAY  TLA-008
           ADD      1   TO  SUF-DSP
           GO  TO   ROT-000-040.

       ROT-000-080.
           DISPLAY  MEN-003
           ACCEPT   OPC-002
           MOVE     0   TO  SUF-DSP
           GO  TO   ROT-000-040.

       ROT-100-000.
           IF  NUM-OPC   >   1
               GO  TO        ROT-100-020.
           DISPLAY  SUF-ENT
           MOVE     SPACES   TO  DES-DSP
           MOVE     ZEROS    TO  ACM-PAG  ACM-CPG.

       ROT-100-010.
           DISPLAY  LPA-PAG  LPA-CPG
                             MEN-006
           ACCEPT   (11 23)  DES-DSP  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-040.
           DISPLAY  DES-ENT  PAG-ENT
                    CPG-ENT  MEN-007
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           WRITE    REG-DSP
           GO  TO   ROT-000-070.

       ROT-100-020.
           DISPLAY  MEN-004
           ACCEPT   OPC-002
           MOVE     0   TO  SUF-DSP
           GO  TO   ROT-000-040.

       ROT-200-000.
           DISPLAY  MEN-006
           ACCEPT   (11 23)  DES-DSP  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  DES-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-000-040.

       ROT-200-010.
           DISPLAY  MEN-007
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           REWRITE  REG-DSP
           GO  TO   ROT-000-070.

       ROT-300-000.
           DISPLAY  MEN-011
           ACCEPT   OPC-002
           GO  TO   ROT-000-070.

       ROT-400-000.
           DISPLAY  MEN-009
           MOVE     SPACES   TO  CHV-LDP
           MOVE     CHV-DSP  TO  COD-LDP
           START    CADLDP   KEY GREATER  CHV-LDP
                    INVALID  KEY GO  TO   ROT-400-020.
           READ     CADLDP   NEXT
           IF  COD-LDP  NOT  =   CHV-DSP
               GO  TO        ROT-400-020.

       ROT-400-010.
           DISPLAY  MEN-008
           ACCEPT   OPC-002
           GO  TO   ROT-000-070.

       ROT-400-020.
           DISPLAY  MEN-010
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM  ESCAPE KEY
           IF  TECLADO   =   00
               DELETE    CADDSP.
           GO  TO   ROT-000-070.

       ROT-500-000.
           MOVE     1   TO  PRM-001
           CLOSE    CADDSP  CADLDP
           CHAIN   "DYN300"  USING  PRM-001.
