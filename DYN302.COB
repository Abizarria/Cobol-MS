       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN302.
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
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ATV-AUX          PIC  X(18).
           03  VAL-AUX          PIC S9(10)V99.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  TAB-ATV.
               05  FILLER       PIC  X(18)  VALUE  "OBRIG. TRABALHISTA".
               05  FILLER       PIC  X(18)  VALUE  "OBRIGACOES FISCAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESP. OPERACIONAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESPESA DE CAPITAL".
               05  FILLER       PIC  X(18)  VALUE  "CREDITOS DE SOCIOS".
               05  FILLER       PIC  X(18)  VALUE  "COMPRA DE PRODUTOS".
           03  RED-ATV          REDEFINES   TAB-ATV.
               05  ATV-TAB      PIC  X(18)  OCCURS  06.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC  X(66)  FROM
               "LANCAMENTO DAS CONTAS A PAGAR  -  Ver. 7.11"
               BACKGROUND-COLOR  02   FOREGROUND-COLOR  00.

       01  TLA-002.
           03  LINE 06 COLUMN 15 PIC X(50) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 15 PIC X(50) FROM " ³
              "                         ³" BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 15 PIC X(50) FROM " ³  Codigo .....:  -
              "  -                      ³" BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 15 PIC X(50) FROM " ³
              "                         ³" BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 15 PIC X(50) FROM " ³  Descricao ..:
              "                         ³" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 15 PIC X(50) FROM " ³
              "                         ³" BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 15 PIC X(50) FROM " ³  Vencimento .:   /
              "/                        ³" BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 15 PIC X(50) FROM " ³  Historico ..:
              "                         ³" BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 15 PIC X(50) FROM " ³
              "                         ³" BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 15 PIC X(50) FROM " ³  Valor (+/-) :
              "                         ³" BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 15 PIC X(50) FROM " ³
              "                         ³" BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 15 PIC X(50) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 17 PIC X(50) FROM SPACES.

       01  ENT-001.
           03  PRX-ENT LINE 08 COLUMN 33 PIC 9(01) USING PRX-DSP
                       BACKGROUND-COLOR  01.
           03  SUF-ENT LINE 08 COLUMN 35 PIC 9(03) USING SUF-DSP
                       BACKGROUND-COLOR  01.
           03  ATV-ENT LINE 08 COLUMN 43 PIC X(18) USING ATV-AUX
                       BACKGROUND-COLOR  01.
           03  DES-ENT LINE 10 COLUMN 33 PIC X(25) USING DES-DSP
                       BACKGROUND-COLOR  01.
           03  DIA-ENT LINE 12 COLUMN 33 PIC 9(02) USING DIA-AUX
                       BACKGROUND-COLOR  01.
           03  MES-ENT LINE 12 COLUMN 36 PIC 9(02) USING MES-AUX
                       BACKGROUND-COLOR  01.
           03  ANO-ENT LINE 12 COLUMN 39 PIC 9(02) USING ANO-AUX
                       BACKGROUND-COLOR  01.
           03  HST-ENT LINE 13 COLUMN 33 PIC X(20) USING HST-LDP
                       BACKGROUND-COLOR  01.
           03  VAL-ENT LINE 15 COLUMN 32 PIC --.---.---.--9,99
                       USING   VAL-AUX   BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-SUF LINE 08 COLUMN 35 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-ATV LINE 08 COLUMN 43 PIC X(18) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-DES LINE 10 COLUMN 33 PIC X(25) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-DIA LINE 12 COLUMN 33 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-MES LINE 12 COLUMN 36 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-ANO LINE 12 COLUMN 39 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-HST LINE 13 COLUMN 33 PIC X(20) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VAL LINE 15 COLUMN 32 PIC X(17) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MENSAGENS.
           03  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO
               " DO GRUPO  -  [ESC] RETORNA ...                     "
               BACKGROUND-COLOR  05   FOREGROUND-COLOR  15.
           03  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO
               " DA CONTA  -  [ESC] RETORNA ...                     "
               BACKGROUND-COLOR  05   FOREGROUND-COLOR  15.
           03  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "CONTA INEXISTEN
               "TE !!!  -  [ESC] RETORNA ...                        "
               BACKGROUND-COLOR  05   FOREGROUND-COLOR  15.
           03  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA D
               "O VENCIMENTO  -  [ESC] RETORNA ...                  "
               BACKGROUND-COLOR  05   FOREGROUND-COLOR  15.
           03  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O HISTOR
               "ICO DO MOVIMENTO  -  [ESC] RETORNA ...              "
               BACKGROUND-COLOR  05   FOREGROUND-COLOR  15.
           03  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR
               "A PAGAR  -  [ESC] RETORNA ...                       "
               BACKGROUND-COLOR  05   FOREGROUND-COLOR  15.
           03  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P
               "/ CONFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
               BACKGROUND-COLOR  05   FOREGROUND-COLOR  15.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     I-O      CADLDP
                    INPUT    CADDSP
           ACCEPT   DTA-SYS    FROM    DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.

       ROT-100-000.
           DISPLAY  LPA-SUF
                    LPA-ATV  MEN-001
           MOVE     0   TO   PRX-DSP
           ACCEPT   (08 33)  PRX-DSP  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           IF  PRX-DSP   <   1  OR  >  6
               GO  TO        ROT-100-000.
           MOVE     ATV-TAB (PRX-DSP) TO  ATV-AUX
           DISPLAY  PRX-ENT  ATV-ENT.

       ROT-100-010.
           DISPLAY  LPA-DES
                    LPA-DIA  MEN-002
           MOVE     0    TO  SUF-DSP
           ACCEPT   (08 35)  SUF-DSP  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           IF  SUF-DSP   =   ZEROS
               GO  TO        ROT-100-010.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADDSP   INVALID KEY
                    DISPLAY  MEN-003
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-010.
           DISPLAY  SUF-ENT      DES-ENT
           MOVE     SPACES   TO  HST-LDP
           MOVE     ZEROS    TO  DTA-AUX  VAL-AUX.

       ROT-100-020.
           DISPLAY  LPA-MES  MEN-004
           ACCEPT   (12 33)  DIA-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           IF  DIA-AUX   <   1  OR  > 31
               GO  TO        ROT-100-020.
           DISPLAY  DIA-ENT.

       ROT-100-030.
           DISPLAY  LPA-ANO
           ACCEPT   (12 36)  MES-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           IF  MES-AUX   <   1  OR  > 12
               GO  TO        ROT-100-030.
           DISPLAY  MES-ENT.

       ROT-100-040.
           DISPLAY  LPA-HST  MEN-004
           ACCEPT   (12 39)  ANO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           DISPLAY  ANO-ENT.

       ROT-100-050.
           DISPLAY  LPA-VAL  MEN-005
           ACCEPT   (13 33)  HST-LDP  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.
           DISPLAY  HST-ENT.

       ROT-100-060.
           DISPLAY  LPA-VAL  MEN-006
           ACCEPT   (15 32)  VAL-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-050.
           IF  VAL-AUX   =   ZEROS
               GO  TO        ROT-100-060.
           DISPLAY  VAL-ENT  MEN-007
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-060.
           MOVE     CHV-DSP  TO  COD-LDP
           MOVE     DTA-SYS  TO  DTA-LDP
           MOVE     DTA-AUX  TO  PGT-LDP
           MOVE     VAL-AUX  TO  VAL-LDP
           MOVE     1        TO  FLG-LDP
           MOVE     ZEROS    TO  LCT-LDP.

       ROT-100-070.
           ADD      1        TO  LCT-LDP
           WRITE    REG-LDP  INVALID KEY
                    GO  TO   ROT-100-070.
           DISPLAY  LPA-001
           GO  TO   ROT-100-000.

       ROT-200-000.
           MOVE     1   TO  PRM-001
           CLOSE    CADDSP  CADLDP
           CHAIN   "DYN300"  USING  PRM-001.
