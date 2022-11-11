       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN309.
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

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

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

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC  X(80).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  RSP-OPC          PIC  X(01).
           03  PRX-AUX          PIC  9(01).
           03  ATV-AUX          PIC  X(18).
           03  TECLADO          PIC  9(02).
           03  ACM-LIN          PIC  9(02).
           03  ACM-CT1          PIC S9(11)V99.
           03  ACM-CT2          PIC S9(11)V99.
           03  ACM-SB1          PIC S9(12)V99.
           03  ACM-SB2          PIC S9(12)V99.
           03  ACM-GP1          PIC S9(12)V99.
           03  ACM-GP2          PIC S9(12)V99.
           03  ACM-TT1          PIC S9(12)V99.
           03  ACM-TT2          PIC S9(12)V99.

           03  SUF-AUX.
               05  SF1-AUX      PIC  9(01).
               05  SF2-AUX      PIC  9(02).

           03  SUF-ANT.
               05  SF1-ANT      PIC  9(01).
               05  SF2-ANT      PIC  9(02).

           03  TAB-ATV.
               05  FILLER       PIC  X(18)  VALUE  "OBRIG. TRABALHISTA".
               05  FILLER       PIC  X(18)  VALUE  "OBRIGACOES FISCAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESP. OPERACIONAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESPESA DE CAPITAL".
               05  FILLER       PIC  X(18)  VALUE  "CREDITOS DE SOCIOS".
               05  FILLER       PIC  X(18)  VALUE  "COMPRA DE PRODUTOS".
           03  RED-ATV          REDEFINES   TAB-ATV.
               05  ATV-TAB      PIC  X(18)  OCCURS  06.

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

           03  REG-TAB          PIC  X(80)  OCCURS 510.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

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
           03  FILLER           PIC  X(38)  VALUE
               "RELACAO DE CONTAS ACUMULADAS".
           03  ATV-CB2          PIC  X(34).
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "CONTA     DESCRICAO DA CONTA               VALOR LIQUIDADO
           "      VALOR PENDENTE".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  PRX-DT1          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DT1          PIC  9(03).
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  DES-DT1          PIC  X(30).
           03  VL1-DT1          PIC  ---.---.---.--9,99.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  VL2-DT1          PIC  ---.---.---.--9,99.

       01  TOT-001.
           03  FILLER           PIC  X(39)  VALUE
               "          - SUB-TOTAL .............".
           03  VL1-TT1          PIC  ----.---.---.--9,99.
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  VL2-TT1          PIC  ----.---.---.--9,99.

       01  TOT-002.
           03  FILLER           PIC  X(39)  VALUE
               "          - TOTAL DO GRUPO ........".
           03  VL1-TT2          PIC  ----.---.---.--9,99.
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  VL2-TT2          PIC  ----.---.---.--9,99.

       01  TOT-003.
           03  FILLER           PIC  X(39)  VALUE
               "          - TOTAL GERAL ...........".
           03  VL1-TT3          PIC  ----.---.---.--9,99.
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  VL2-TT3          PIC  ----.---.---.--9,99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "RELACAO DAS CONTAS ACUMULADAS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 27 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 27 PIC X(29) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 27 PIC X(31) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 27 PIC X(35) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 27 PIC X(37) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 27 PIC X(38) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 27 PIC X(40) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 27 PIC X(40) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 27 PIC X(40) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 29 PIC X(38) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 29 PIC X(38) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 31 PIC X(36) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 35 PIC X(29) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 42 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 09 COLUMN 29  PIC  X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR   01.
           03  LINE 10 COLUMN 29  PIC  X(21) FROM " ³                 ³
               BACKGROUND-COLOR   01.
           03  LINE 10 COLUMN 50  PIC  X(02) FROM SPACES.
           03  LINE 11 COLUMN 29  PIC  X(21) FROM " ³  Relacao Total  ³
               BACKGROUND-COLOR   01.
           03  LINE 11 COLUMN 50  PIC  X(02) FROM SPACES.
           03  LINE 12 COLUMN 29  PIC  X(21) FROM " ³                 ³
               BACKGROUND-COLOR   01.
           03  LINE 12 COLUMN 50  PIC  X(02) FROM SPACES.
           03  LINE 13 COLUMN 29  PIC  X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR   01.
           03  LINE 13 COLUMN 50  PIC  X(02) FROM SPACES.
           03  LINE 14 COLUMN 31  PIC  X(21) FROM SPACES.

       01  TLA-003.
           03  LINE 09 COLUMN 20 PIC  X(40) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 20 PIC  X(40) FROM " ³
               "                ³"    BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 60 PIC  X(02) FROM SPACES.
           03  LINE 11 COLUMN 20 PIC  X(40) FROM " ³  Grupo :    -
               "                ³"    BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 60 PIC  X(02) FROM SPACES.
           03  LINE 12 COLUMN 20 PIC  X(40) FROM " ³
               "                ³"    BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 60 PIC  X(02) FROM SPACES.
           03  LINE 13 COLUMN 20 PIC  X(40) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 60 PIC  X(02) FROM SPACES.
           03  LINE 14 COLUMN 22 PIC  X(40) FROM SPACES.

       01  ENT-001.
           03  LINE 11 COLUMN 32  PIC  9(01) USING  PRX-AUX
               BACKGROUND-COLOR   01.
           03  LINE 11 COLUMN 38  PIC  X(18) USING  ATV-AUX
               BACKGROUND-COLOR   01.

       01  LPA-001.
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

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR !!!  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO RELACAO DE CONTAS ACUMULADAS ...           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE CONTAS C
                   "ADASTRADAS !!!  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE CONTAS C
                   "ADASTRADAS NO GRUPO !!!  -  [ESC] RETORNA ...   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA CONSULTAR NA
                   " TELA ? [S/N]  [ ]  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  MONTAN
                   "DO TELA DE CONSULTA ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "MEMORIA INSUFICIENT
                   "E P/ CONSULTA EM TELA  -  TECLE [ESC] !!!       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.
       01  MEN-010 LINE 22 COLUMN 80 PIC X(01) FROM " "
               BACKGROUND-COLOR  05.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002  LINE 22 COLUMN 49 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001.

       ROT-000-000.
           DISPLAY  TLA-001
           IF  PRM-001   =   1   DISPLAY  TLA-002.
           IF  PRM-001   =   2   DISPLAY  TLA-003.
           OPEN     INPUT  CADDSP
                           CADLDP CADPRM
           READ     CADPRM
           MOVE     1   TO   IND2.

       ROT-000-010.
           MOVE     CLI-PRM (IND2 + 1)  TO IND1
           MOVE     LET-TAB (IND1)  TO  LET-NOM (IND2)
           ADD      1   TO   IND2
           IF  IND2  <  36
               GO  TO        ROT-000-010.
           MOVE     SPACES   TO  LET-NOM (IND2)
           ACCEPT   DTA-SYS  FROM   DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.
           MOVE     NOM-TAB  TO  NOM-CB1
           MOVE     DIA-SYS  TO  DIA-CB2
           MOVE     MES-SYS  TO  MES-CB2
           MOVE     ANO-SYS  TO  ANO-CB2
           GO  TO   ROT-100-000, ROT-200-000,
                    DEPENDING    ON  PRM-001.

       ROT-000-020.
           MOVE     6    TO  PRM-001
           CLOSE    CADDSP
                    CADLDP   CADPRM
           CHAIN   "DYN300"   USING  PRM-001.

       ROT-100-000.
           MOVE     SPACES   TO  CHV-DSP  RSP-OPC
           START    CADDSP   KEY GREATER  CHV-DSP
                    INVALID  KEY GO  TO   ROT-100-090.
           READ     CADDSP   NEXT
           DISPLAY  MEN-001
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE    KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           DISPLAY  MEN-002
           OPEN     OUTPUT   RELATO
           MOVE     1        TO  PAG-CB1
           MOVE     SPACES   TO  ATV-CB2
           MOVE     ZEROS    TO  ACM-SB1  ACM-SB2
                                 ACM-GP1  ACM-GP2
                                 ACM-TT1  ACM-TT2
           WRITE    REG-REL FROM CAB-001  AFTER  0.

       ROT-100-010.
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-003
           MOVE     ZEROS    TO  ACM-LIN.

       ROT-100-020.
           MOVE     SPACES   TO  CHV-LDP
           MOVE     CHV-DSP  TO  COD-LDP
           MOVE     ZEROS    TO  ACM-CT1  ACM-CT2
           START    CADLDP   KEY GREATER  CHV-LDP
                    INVALID  KEY GO  TO   ROT-100-040.

       ROT-100-030.
           READ     CADLDP   NEXT AT END
                    GO  TO   ROT-100-040.
           IF  COD-LDP  NOT  =   CHV-DSP
               GO  TO        ROT-100-040.
           IF  FLG-LDP   =   1
               ADD  VAL-LDP  TO  ACM-CT2
           ELSE
               ADD  VAL-LDP  TO  ACM-CT1.
           GO  TO   ROT-100-030.

       ROT-100-040.
           IF  ACM-LIN   <   52
               GO  TO        ROT-100-050.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           PERFORM  ROT-100-010.

       ROT-100-050.
           MOVE     PRX-DSP  TO  PRX-DT1
           MOVE     SUF-DSP  TO  SUF-DT1  SUF-ANT
           MOVE     DES-DSP  TO  DES-DT1
           MOVE     ACM-CT1  TO  VL1-DT1
           MOVE     ACM-CT2  TO  VL2-DT1
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  ACM-LIN
           ADD      ACM-CT1  TO  ACM-SB1
           ADD      ACM-CT2  TO  ACM-SB2
           READ     CADDSP   NEXT AT END
                    GO  TO   ROT-100-060.
           IF  PRX-DSP   >   5
               GO  TO        ROT-100-060.
           MOVE     SUF-DSP  TO  SUF-AUX
           IF  PRX-DSP  NOT  =   PRX-DT1
               PERFORM       ROT-100-060
               PERFORM       ROT-100-070.
           IF  SF1-AUX  NOT  =   SF1-ANT
               PERFORM       ROT-100-060.
           GO  TO   ROT-100-020.

       ROT-100-060.
           MOVE     ACM-SB1  TO  VL1-TT1
           MOVE     ACM-SB2  TO  VL2-TT1
           WRITE    REG-REL FROM TOT-001  AFTER  2
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL
           ADD      3        TO  ACM-LIN
           ADD      ACM-SB1  TO  ACM-GP1
           ADD      ACM-SB2  TO  ACM-GP2
           MOVE     SUF-AUX  TO  SUF-ANT
           MOVE     ZEROS    TO  ACM-SB1  ACM-SB2.

       ROT-100-070.
           MOVE     ACM-GP1  TO  VL1-TT2
           MOVE     ACM-GP2  TO  VL2-TT2
           WRITE    REG-REL FROM TOT-002
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL
           ADD      2        TO  ACM-LIN
           ADD      ACM-GP1  TO  ACM-TT1
           ADD      ACM-GP2  TO  ACM-TT2
           MOVE     ZEROS    TO  ACM-GP1  ACM-GP2.

       ROT-100-080.
           MOVE     ACM-TT1  TO  VL1-TT3
           MOVE     ACM-TT2  TO  VL2-TT3
           WRITE    REG-REL FROM TOT-003
           CLOSE    RELATO
           GO  TO   ROT-000-020.

       ROT-100-090.
           DISPLAY  MEN-003
           ACCEPT   OPC-001
           GO  TO   ROT-000-020.

       ROT-200-000.
           DISPLAY  MEN-004
           MOVE     0   TO   PRX-AUX
           ACCEPT   (11 32)  PRX-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           IF  PRX-AUX   <   1  OR  >  6
               GO  TO        ROT-200-000.
           MOVE     ATV-TAB (PRX-AUX) TO  ATV-AUX  ATV-CB2
           DISPLAY  ENT-001
           MOVE     SPACES   TO  CHV-DSP  RSP-OPC
           MOVE     PRX-AUX  TO  PRX-DSP
           START    CADDSP   KEY GREATER  CHV-DSP
                    INVALID  KEY GO  TO   ROT-200-010.
           READ     CADDSP   NEXT
           IF  PRX-DSP   =   PRX-AUX
               GO  TO        ROT-200-020.

       ROT-200-010.
           DISPLAY  MEN-005
           ACCEPT   OPC-001
           DISPLAY  TLA-003
           GO  TO   ROT-200-000.

       ROT-200-020.
           DISPLAY  MEN-006
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       TLA-003
               GO  TO        ROT-200-000.
           IF  RSP-OPC   =   "S"
               GO  TO        ROT-200-200.
           IF  RSP-OPC  NOT  =   "N"
               MOVE     " "  TO  RSP-OPC
               GO  TO        ROT-200-020.

       ROT-200-100.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-001
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE    KEY
           IF  TECLADO   =   01
               DISPLAY       TLA-003
               GO  TO        ROT-200-000.
           DISPLAY  MEN-002
           OPEN     OUTPUT   RELATO
           MOVE     1        TO  PAG-CB1
           MOVE     ZEROS    TO  ACM-SB1  ACM-SB2
                                 ACM-GP1  ACM-GP2
           WRITE    REG-REL FROM CAB-001  AFTER  0.

       ROT-200-110.
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-003
           MOVE     ZEROS    TO  ACM-LIN.

       ROT-200-120.
           MOVE     SPACES   TO  CHV-LDP
           MOVE     CHV-DSP  TO  COD-LDP
           MOVE     ZEROS    TO  ACM-CT1  ACM-CT2
           START    CADLDP   KEY GREATER  CHV-LDP
                    INVALID  KEY GO  TO   ROT-200-140.

       ROT-200-130.
           READ     CADLDP   NEXT AT END
                    GO  TO   ROT-200-140.
           IF  COD-LDP  NOT  =   CHV-DSP
               GO  TO        ROT-200-140.
           IF  FLG-LDP   =   1
               ADD  VAL-LDP  TO  ACM-CT2
           ELSE
               ADD  VAL-LDP  TO  ACM-CT1.
           GO  TO   ROT-200-130.

       ROT-200-140.
           IF  ACM-LIN   <   52
               GO  TO        ROT-200-150.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           PERFORM  ROT-200-110.

       ROT-200-150.
           MOVE     PRX-DSP  TO  PRX-DT1
           MOVE     SUF-DSP  TO  SUF-DT1  SUF-ANT
           MOVE     DES-DSP  TO  DES-DT1
           MOVE     ACM-CT1  TO  VL1-DT1
           MOVE     ACM-CT2  TO  VL2-DT1
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  ACM-LIN
           ADD      ACM-CT1  TO  ACM-SB1
           ADD      ACM-CT2  TO  ACM-SB2
           READ     CADDSP   NEXT AT END
                    GO  TO   ROT-200-160.
           MOVE     SUF-DSP  TO  SUF-AUX
           IF  PRX-AUX   =   6
               IF  SF1-AUX   >   0   AND
                   SF1-ANT   =   0
                   PERFORM   ROT-200-160
                   GO  TO    ROT-200-120
               ELSE
                   GO  TO    ROT-200-120.
           IF  PRX-DSP  NOT  =   PRX-AUX
               GO  TO        ROT-200-160.
           IF  SF1-AUX  NOT  =   SF1-ANT
               PERFORM       ROT-200-160.
           GO  TO   ROT-200-120.

       ROT-200-160.
           MOVE     ACM-SB1  TO  VL1-TT1
           MOVE     ACM-SB2  TO  VL2-TT1
           WRITE    REG-REL FROM TOT-001  AFTER  2
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL
           ADD      3        TO  ACM-LIN
           ADD      ACM-SB1  TO  ACM-GP1
           ADD      ACM-SB2  TO  ACM-GP2
           MOVE     SUF-AUX  TO  SUF-ANT
           MOVE     ZEROS    TO  ACM-SB1  ACM-SB2.

       ROT-200-170.
           MOVE     ACM-GP1  TO  VL1-TT2
           MOVE     ACM-GP2  TO  VL2-TT2
           WRITE    REG-REL FROM TOT-002
           CLOSE    RELATO
           DISPLAY  TLA-003
           GO  TO   ROT-200-000.

       ROT-200-200.
           DISPLAY  MEN-007
           MOVE     ZEROS    TO  PAG-CB1  IND1
                                 ACM-SB1  ACM-SB2
                                 ACM-GP1  ACM-GP2.
       ROT-200-210.
           ADD      1        TO  PAG-CB1
           IF  PAG-CB1   >   8
               GO  TO        ROT-200-320.
           MOVE     CAB-001  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           MOVE     CAB-002  TO  REG-TAB (IND1 + 3)
           MOVE     CAB-003  TO  REG-TAB (IND1 + 4)
           MOVE     CAB-004  TO  REG-TAB (IND1 + 5)
           MOVE     CAB-003  TO  REG-TAB (IND1 + 6)
           MOVE     ZEROS    TO  ACM-LIN
           ADD      7        TO  IND1.

       ROT-200-220.
           MOVE     SPACES   TO  CHV-LDP
           MOVE     CHV-DSP  TO  COD-LDP
           MOVE     ZEROS    TO  ACM-CT1  ACM-CT2
           START    CADLDP   KEY GREATER  CHV-LDP
                    INVALID  KEY GO  TO   ROT-200-240.

       ROT-200-230.
           READ     CADLDP   NEXT AT END
                    GO  TO   ROT-200-240.
           IF  COD-LDP  NOT  =   CHV-DSP
               GO  TO        ROT-200-240.
           IF  FLG-LDP   =   1
               ADD  VAL-LDP  TO  ACM-CT2
           ELSE
               ADD  VAL-LDP  TO  ACM-CT1.
           GO  TO   ROT-200-230.

       ROT-200-240.
           MOVE     PRX-DSP  TO  PRX-DT1
           MOVE     SUF-DSP  TO  SUF-DT1  SUF-ANT
           MOVE     DES-DSP  TO  DES-DT1
           MOVE     ACM-CT1  TO  VL1-DT1
           MOVE     ACM-CT2  TO  VL2-DT1
           MOVE     DET-001  TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           ADD      1        TO  ACM-LIN
           ADD      ACM-CT1  TO  ACM-SB1
           ADD      ACM-CT2  TO  ACM-SB2
           READ     CADDSP   NEXT AT END
                    GO  TO   ROT-200-250.
           MOVE     SUF-DSP  TO  SUF-AUX
           IF  PRX-AUX   =   6
               IF  SF1-AUX   >   0   AND
                   SF1-ANT   =   0
                   PERFORM   ROT-200-250
                   GO  TO    ROT-200-220
               ELSE
                   GO  TO    ROT-200-220.
           IF  PRX-DSP  NOT  =   PRX-AUX
               GO  TO        ROT-200-250.
           IF  SF1-AUX  NOT  =   SF1-ANT
               PERFORM       ROT-200-250.
           IF  ACM-LIN   <   52
               GO  TO        ROT-200-220.
           MOVE     SPACES   TO  REG-TAB (IND1)
           MOVE     CAB-005  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           ADD      2        TO  IND1
           GO  TO   ROT-200-210.

       ROT-200-250.
           MOVE     ACM-SB1  TO  VL1-TT1
           MOVE     ACM-SB2  TO  VL2-TT1
           MOVE     SPACES   TO  REG-TAB (IND1)
           MOVE     TOT-001  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           ADD      3        TO  IND1
           ADD      3        TO  ACM-LIN
           ADD      ACM-SB1  TO  ACM-GP1
           ADD      ACM-SB2  TO  ACM-GP2
           MOVE     SUF-AUX  TO  SUF-ANT
           MOVE     ZEROS    TO  ACM-SB1  ACM-SB2.

       ROT-200-260.
           MOVE     ACM-GP1  TO  VL1-TT2
           MOVE     ACM-GP2  TO  VL2-TT2
           MOVE     TOT-002  TO  REG-TAB (IND1)
           MOVE     IND1     TO  IND2
           ADD      1        TO  IND1.

       ROT-200-270.
           MOVE     SPACES   TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1 <  510
               GO  TO        ROT-200-270.
           MOVE     ZEROS    TO  IND1.

       ROT-200-280.
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
           DISPLAY  LPA-001   MEN-008.

       ROT-200-290.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-010
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-200-300.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-200-310.
           IF  IND1  NOT  <  IND2
               GO  TO        ROT-200-290.
           GO  TO   ROT-200-280.

       ROT-200-300.
           IF  IND1   <  36
               GO  TO    ROT-200-290.
           SUBTRACT  36  FROM   IND1
           GO  TO   ROT-200-280.

       ROT-200-310.
           MOVE     SPACES   TO  TLA-AUX
           DISPLAY  LPA-001
                    TLA-003      ENT-001
           MOVE     SPACES   TO  CHV-DSP
           MOVE     PRX-AUX  TO  PRX-DSP
           START    CADDSP   KEY GREATER  CHV-DSP
           READ     CADDSP   NEXT
           GO  TO   ROT-200-100.

       ROT-200-320.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-009
           ACCEPT   OPC-001
           GO  TO   ROT-200-310.
