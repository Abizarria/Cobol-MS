       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN304.
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
           03  DTA-SRT          PIC  9(08).
           03  HST-SRT          PIC  X(20).
           03  VAL-SRT          PIC S9(10)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  ATV-AUX          PIC  X(18).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ACM-VAL          PIC S9(11)V99.

           03  REG-TAB          PIC  X(50)  OCCURS  510.

           03  TAB-ATV.
               05  FILLER       PIC  X(18)  VALUE  "OBRIG. TRABALHISTA".
               05  FILLER       PIC  X(18)  VALUE  "OBRIGACOES FISCAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESP. OPERACIONAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESPESA DE CAPITAL".
               05  FILLER       PIC  X(18)  VALUE  "CREDITOS DE SOCIOS".
               05  FILLER       PIC  X(18)  VALUE  "COMPRA DE PRODUTOS".
           03  RED-ATV          REDEFINES   TAB-ATV.
               05  ATV-TAB      PIC  X(18)  OCCURS  06.

           03  LIN-001          PIC  X(50).
           03  LIN-002          PIC  X(50).
           03  LIN-003          PIC  X(50).
           03  LIN-004          PIC  X(50).
           03  LIN-005          PIC  X(50).
           03  LIN-006          PIC  X(50).
           03  LIN-007          PIC  X(50).
           03  LIN-008          PIC  X(50).
           03  LIN-009          PIC  X(50).
           03  LIN-010          PIC  X(50).
           03  LIN-011          PIC  X(50).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  DTA-LCT.
               05  ANO-LCT      PIC  9(04).
               05  MES-LCT      PIC  9(02).
               05  DIA-LCT      PIC  9(02).

       01  DET-001.
           03  DIA-DET          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DET          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DET          PIC  9(02).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  HST-DET          PIC  X(22).
           03  VAL-DET          PIC  --.---.---.--9,99.

       01  TOT-001.
           03  FILLER           PIC  X(32)  VALUE
               "           TOTAL DA CONTA .. R$".
           03  VAL-TOT          PIC  ---.---.---.--9,99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "CONSULTA MOVIMENTOS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 27 PIC X(24) FROM SPACES
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
           03  LINE 08 COLUMN 16 PIC X(48) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"  BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 16 PIC X(48) FROM " ³  Conta ....:  -
               "-                      ³"  BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 16 PIC X(48) FROM " ³  Descricao :
               "                       ³"   BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"   BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 16 PIC X(48) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"  BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 18 PIC X(48) FROM SPACES.

       01  TLA-003.
           03  LINE 05 COLUMN 11 PIC X(58) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 01.
           03  LINE 06 COLUMN 11 PIC X(58) FROM " ³    DATA     HISTORIC
               "O                 VALOR LANCADO  ³" BACKGROUND-COLOR 01.
           03  LINE 06 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 17 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 01.
           03  LINE 18 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 11 PIC X(58) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 01.
           03  LINE 19 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 13 PIC X(58) FROM SPACES.

       01  ENT-001.
           03  PRX-ENT LINE 10 COLUMN 32 PIC 9(01) USING PRX-DSP
                       BACKGROUND-COLOR  01.
           03  SUF-ENT LINE 10 COLUMN 34 PIC 9(03) USING SUF-DSP
                       BACKGROUND-COLOR  01.
           03  ATV-ENT LINE 10 COLUMN 42 PIC X(18) USING ATV-AUX
                       BACKGROUND-COLOR  01.
           03  DES-ENT LINE 12 COLUMN 32 PIC X(25) USING DES-DSP
                       BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-SUF LINE 10 COLUMN 34 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-ATV LINE 10 COLUMN 42 PIC X(18) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-DES LINE 12 COLUMN 32 PIC X(25) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  LPA-002.
           03  LINE 08 COLUMN 15 PIC X(50) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 15 PIC X(50) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 15 PIC X(50) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 15 PIC X(50) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 15 PIC X(50) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 15 PIC X(50) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 15 PIC X(50) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 15 PIC X(50) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 15 PIC X(50) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 15 PIC X(50) USING LIN-010
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 15 PIC X(50) USING LIN-011
               BACKGROUND-COLOR  01.

       01  LPA-003.
           03  LINE 05 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DA
                   "CONTA  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "CONTA INEXISTENTE !
                   "!!  -  [ESC] RETORNA ...                        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE MOVIMENT
                   "O NESTA CONTA !!!  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  MONTAN
                   "DO TELA DE CONSULTA ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTO ...                             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "MEMORIA INSUFICIENT
                   "E P/ CONSULTA EM TELA !!!  -  [ESC] RETORNA ... "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     INPUT    CADDSP
                             CADLDP.
       ROT-100-000.
           DISPLAY  LPA-SUF
                    LPA-ATV  MEN-001
           MOVE     0   TO   PRX-DSP
           ACCEPT   (10 32)  PRX-DSP  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           IF  PRX-DSP   <   1  OR  >  6
               GO  TO        ROT-100-000.
           MOVE     ATV-TAB (PRX-DSP) TO  ATV-AUX
           DISPLAY  PRX-ENT  ATV-ENT.

       ROT-100-010.
           DISPLAY  LPA-DES  MEN-002
           MOVE     0    TO  SUF-DSP
           ACCEPT   (10 34)  SUF-DSP  WITH  UPDATE  AUTO-SKIP
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
           DISPLAY  SUF-ENT
                    DES-ENT  MEN-007
           MOVE     SPACES   TO  CHV-LDP
           MOVE     CHV-DSP  TO  COD-LDP
           START    CADLDP   KEY GREATER  CHV-LDP
                    INVALID  KEY GO  TO   ROT-100-020.
           READ     CADLDP   NEXT
           IF  COD-LDP   =   CHV-DSP
               GO  TO        ROT-200-000.

       ROT-100-020.
           MOVE     " "  TO  RSP-OPC
           DISPLAY  MEN-004
           ACCEPT   OPC-001
           DISPLAY  LPA-001
           GO  TO   ROT-100-010.

       ROT-200-000.
           DISPLAY  MEN-008
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           DISPLAY  MEN-005
           SORT     CADSRT   ON  ASCENDING  KEY  DTA-SRT
                    INPUT        PROCEDURE  IS   ROT-200-010
                    OUTPUT       PROCEDURE  IS   ROT-200-040
           GO  TO   ROT-300-000.

       ROT-200-010  SECTION.

       ROT-200-020.
           MOVE     DTA-LDP  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-LCT
           MOVE     MES-AUX  TO  MES-LCT
           MOVE     ANO-AUX  TO  ANO-LCT
           IF  ANO-AUX  <    10
               ADD     2000  TO  ANO-LCT
           ELSE
               ADD     1900  TO  ANO-LCT.
           MOVE     DTA-LCT  TO  DTA-SRT
           MOVE     HST-LDP  TO  HST-SRT
           MOVE     VAL-LDP  TO  VAL-SRT
           RELEASE  REG-SRT
           READ     CADLDP   NEXT AT END
                    GO  TO   ROT-200-030.
           IF  COD-LDP   =   CHV-DSP
               GO  TO        ROT-200-020.

       ROT-200-030.
           MOVE     ZEROS    TO  IND1  ACM-VAL.

       ROT-200-040  SECTION.

       ROT-200-050.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-200-060.
           MOVE     DTA-SRT  TO  DTA-LCT
           IF  ANO-LCT   >   1999
               COMPUTE       ANO-LCT  =   ANO-LCT  -  2000
           ELSE
               COMPUTE       ANO-LCT  =   ANO-LCT  -  1900.
           MOVE     DIA-LCT  TO  DIA-DET
           MOVE     MES-LCT  TO  MES-DET
           MOVE     ANO-LCT  TO  ANO-DET
           MOVE     HST-SRT  TO  HST-DET
           MOVE     VAL-SRT  TO  VAL-DET
           ADD      VAL-SRT  TO  ACM-VAL
           ADD      1        TO  IND1
           MOVE     DET-001  TO  REG-TAB (IND1).
           IF  IND1      <   500
               GO  TO        ROT-200-050.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-009
           ACCEPT   OPC-001
           GO  TO   ROT-100-010.

       ROT-200-060.
           MOVE     ACM-VAL  TO  VAL-TOT
           MOVE     SPACES   TO  REG-TAB (IND1 + 1)
           MOVE     TOT-001  TO  REG-TAB (IND1 + 2)
           ADD      2        TO  IND1
           MOVE     IND1     TO  IND2.

       ROT-200-070  SECTION.

       ROT-300-000.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1 <  510
               GO  TO        ROT-300-000.
           DISPLAY  TLA-003
           MOVE     ZEROS    TO  IND1.

       ROT-300-010.
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

       ROT-300-020.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-010
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-300-030.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-300-040.
           IF  IND1  NOT  <  IND2
               GO  TO        ROT-300-020.
           GO  TO   ROT-300-010.

       ROT-300-030.
           IF  IND1   <  12
               GO  TO        ROT-300-020.
           COMPUTE  IND1  =  IND1  -  22
           GO  TO   ROT-300-010.

       ROT-300-040.
           DISPLAY  LPA-003
                    TLA-002
           GO  TO   ROT-100-000.

       ROT-400-000.
           MOVE     4   TO  PRM-001
           CLOSE    CADDSP  CADLDP
           CHAIN   "DYN300"  USING  PRM-001.
