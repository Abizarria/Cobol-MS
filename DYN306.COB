       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN306.
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
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ACM-VAL          PIC S9(11)V99.

           03  REG-TAB          PIC  X(58)  OCCURS  510.

           03  LIN-001          PIC  X(58).
           03  LIN-002          PIC  X(58).
           03  LIN-003          PIC  X(58).
           03  LIN-004          PIC  X(58).
           03  LIN-005          PIC  X(58).
           03  LIN-006          PIC  X(58).
           03  LIN-007          PIC  X(58).
           03  LIN-008          PIC  X(58).
           03  LIN-009          PIC  X(58).
           03  LIN-010          PIC  X(58).
           03  LIN-011          PIC  X(58).

           03  DTA-PGT.
               05  ANO-PGT      PIC  9(02).
               05  MES-PGT      PIC  9(02).
               05  DIA-PGT      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

       01  DET-001.
           03  DIA-DET          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DET          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DET          PIC  9(02).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  PRX-DET          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DET          PIC  9(03).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  HST-DET          PIC  X(22).
           03  VAL-DET          PIC  --.---.---.--9,99.

       01  TOT-001.
           03  FILLER           PIC  X(40)  VALUE
               "                   TOTAL LIQUIDADO  R$".
           03  VAL-TOT          PIC ---.---.---.--9,99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "CONSULTA PAGAMENTOS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 27 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 27 PIC X(29) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 61 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 61 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 61 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 27 PIC X(37) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 29 PIC X(35) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 34 PIC X(30) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 36 PIC X(28) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 42 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 09 COLUMN 21 PIC X(38) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"  BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 21 PIC X(38) FROM " ³
               "             ³"  BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 21 PIC X(38) FROM " ³  Data do Pagamento .
               ".:   /  /    ³"  BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 21 PIC X(38) FROM " ³
               "             ³"  BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 21 PIC X(38) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"  BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 23 PIC X(38) FROM SPACES.

       01  TLA-003.
           03  LINE 05 COLUMN 07 PIC X(66) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 07 PIC X(66) FROM " ³    DATA     CONTA
               "HISTORICO                    VALOR PAGO  ³"
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 07 PIC X(66) FROM " ³
               "                                         ³"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 07 PIC X(66) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 09 PIC X(66) FROM SPACES.

       01  ENT-001.
           03  DIA-ENT LINE 11 COLUMN 47 PIC 9(02) USING DIA-PGT
                       BACKGROUND-COLOR  01.
           03  MES-ENT LINE 11 COLUMN 50 PIC 9(02) USING MES-PGT
                       BACKGROUND-COLOR  01.
           03  ANO-ENT LINE 11 COLUMN 53 PIC 9(02) USING ANO-PGT
                       BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-MES LINE 11 COLUMN 50 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-ANO LINE 11 COLUMN 53 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  LPA-002.
           03  LINE 08 COLUMN 11 PIC X(58) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 11 PIC X(58) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 11 PIC X(58) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 11 PIC X(58) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 11 PIC X(58) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 11 PIC X(58) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 11 PIC X(58) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 11 PIC X(58) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 11 PIC X(58) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 11 PIC X(58) USING LIN-010
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 11 PIC X(58) USING LIN-011
               BACKGROUND-COLOR  01.

       01  LPA-003.
           03  LINE 05 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 07 PIC X(68) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA DO PA
                   "GAMENTO  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTO  -  [ESC] RETORNA ...           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE PAGAMENT
                   "OS NESTA DATA !!!  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  MONTAN
                   "DO TELA DE CONSULTA ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "MEMORIA INSUFICIENT
                   "E P/ CONSULTA EM TELA  -  TECLE [ESC] !!!       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     INPUT    CADDSP
                             CADLDP
           MOVE     ZEROS    TO  DTA-PGT.

       ROT-100-000.
           DISPLAY  LPA-MES  MEN-001
           ACCEPT   (11 47)  DIA-PGT  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-000.
           IF  DIA-PGT   <   1  OR  > 31
               GO  TO        ROT-100-000.
           DISPLAY  DIA-ENT.

       ROT-100-010.
           DISPLAY  LPA-ANO
           ACCEPT   (11 50)  MES-PGT  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           IF  MES-PGT   <   1  OR  > 12
               GO  TO        ROT-100-010.
           DISPLAY  MES-ENT.

       ROT-100-020.
           DISPLAY  MEN-001
           ACCEPT   (11 53)  ANO-PGT  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           DISPLAY  ANO-ENT  MEN-003
           MOVE     SPACES   TO  CHV-LDP
           START    CADLDP   KEY GREATER  CHV-LDP
                    INVALID  KEY GO  TO   ROT-100-040.

       ROT-100-030.
           READ     CADLDP   NEXT AT END
                    GO  TO   ROT-100-040.
           IF  FLG-LDP   =   2   AND
               PGT-LDP   =   DTA-PGT
               GO  TO        ROT-200-000.
           GO  TO   ROT-100-030.

       ROT-100-040.
           MOVE     " "  TO  RSP-OPC
           DISPLAY  MEN-004
           ACCEPT   OPC-001
           DISPLAY  LPA-ANO
           GO  TO   ROT-100-000.

       ROT-200-000.
           DISPLAY  MEN-002
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           DISPLAY  MEN-005
           MOVE     1        TO  IND1
           MOVE     ZEROS    TO  ACM-VAL.

       ROT-200-010.
           MOVE     PGT-LDP  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DET
           MOVE     MES-AUX  TO  MES-DET
           MOVE     ANO-AUX  TO  ANO-DET
           MOVE     COD-LDP  TO  CHV-DSP
           READ     CADDSP
           MOVE     PRX-DSP  TO  PRX-DET
           MOVE     SUF-DSP  TO  SUF-DET
           MOVE     HST-LDP  TO  HST-DET
           MOVE     VAL-LDP  TO  VAL-DET
           ADD      VAL-LDP  TO  ACM-VAL
           MOVE     DET-001  TO  REG-TAB (IND1).

       ROT-200-020.
           READ     CADLDP   NEXT AT END
                    GO  TO   ROT-200-030.
           IF  FLG-LDP  NOT  =   2  OR
               PGT-LDP  NOT  =  DTA-PGT
               GO  TO        ROT-200-020.
           ADD      1        TO  IND1
           IF  IND1     <    500
               GO  TO        ROT-200-010.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-007
           ACCEPT   OPC-001
           DISPLAY  LPA-ANO
           GO  TO   ROT-100-000.

       ROT-200-030.
           MOVE     ACM-VAL  TO  VAL-TOT
           MOVE     SPACES   TO  REG-TAB (IND1 + 1)
           MOVE     TOT-001  TO  REG-TAB (IND1 + 2)
           ADD      2        TO  IND1
           MOVE     IND1     TO  IND2.

       ROT-200-040.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1 <  510
               GO  TO        ROT-200-040.
           DISPLAY  TLA-003
           MOVE     ZEROS    TO  IND1.

       ROT-200-050.
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
           DISPLAY  LPA-002  MEN-006.

       ROT-200-060.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-008
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-200-070.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-200-080.
           IF  IND1  NOT  <  IND2
               GO  TO        ROT-200-060.
           GO  TO   ROT-200-050.

       ROT-200-070.
           IF  IND1   <  12
               GO  TO        ROT-200-060.
           COMPUTE  IND1  =  IND1  -  22
           GO  TO   ROT-200-050.

       ROT-200-080.
           DISPLAY  LPA-003
                    TLA-002
           MOVE     ZEROS    TO  DTA-PGT
           GO  TO   ROT-100-000.

       ROT-300-000.
           MOVE     4   TO  PRM-001
           CLOSE    CADDSP  CADLDP
           CHAIN   "DYN300"  USING  PRM-001.
