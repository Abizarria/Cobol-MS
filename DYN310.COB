       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN310.
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

           SELECT      CADSRT        ASSIGN  TO  DISK.

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
               05  COD-LDP.
                   07  PRX-LDP  PIC  9(01).
                   07  SUF-LDP  PIC  9(03).
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

       SD  CADSRT.

       01  REG-SRT.
           03  COD-SRT.
               05  PRX-SRT      PIC  9(01).
               05  SUF-SRT      PIC  9(03).
           03  DTA-SRT          PIC  9(08).
           03  FLG-SRT          PIC  9(01).
           03  HST-SRT          PIC  X(20).
           03  VAL-SRT          PIC S9(10)V99.

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC  X(80).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  RSP-OPC          PIC  X(01).
           03  ATV-AUX          PIC  X(18).
           03  TECLADO          PIC  9(02).
           03  ACM-LIN          PIC  9(02).
           03  ACM-001          PIC S9(11)V99.
           03  ACM-002          PIC S9(11)V99.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-LCT.
               05  ANO-LCT      PIC  9(04).
               05  MES-LCT      PIC  9(02).
               05  DIA-LCT      PIC  9(02).

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
           03  FILLER           PIC  X(19)  VALUE  "EXTRATO DE CONTA :".
           03  PRX-CB2          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-CB2          PIC  9(03).
           03  FILLER           PIC  X(05)  VALUE  "  -".
           03  DES-CB2          PIC  X(43).
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "  DATA        HISTORICO                   VALOR LIQUIDADO
           "      VALOR PENDENTE".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  DIA-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DT1          PIC  9(02).
           03  FILLER           PIC  X(06)  VALUE  SPACES.
           03  HST-DT1          PIC  X(26).
           03  VL1-DT1          PIC  --.---.---.--9,99 BLANK WHEN ZEROS.
           03  FILLER           PIC  X(06)  VALUE  SPACES.
           03  VL2-DT1          PIC  --.---.---.--9,99 BLANK WHEN ZEROS.

       01  TOT-001.
           03  FILLER           PIC  X(39)  VALUE
               "              - TOTAL DA CONTA ...".
           03  VL1-TT1          PIC  ---.---.---.--9,99.
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  VL2-TT1          PIC  ---.---.---.--9,99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "EXTRATO DE CONTAS  -  Ver. 7.11"
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
           03  LINE 19 COLUMN 40 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 42 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 08 COLUMN 16 PIC X(48) FROM " 旼컴컴컴컴컴컴컴컴컴컴
               "컴컴컴컴컴컴컴컴컴컴컴커"  BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 16 PIC X(48) FROM " �
               "                       �"  BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 16 PIC X(48) FROM " �  Conta ....:  -
               "-                      �"  BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 16 PIC X(48) FROM " �
               "                       �"  BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 16 PIC X(48) FROM " �  Descricao :
               "                       �"   BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 16 PIC X(48) FROM " �
               "                       �"   BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 16 PIC X(48) FROM " 읕컴컴컴컴컴컴컴컴컴컴
               "컴컴컴컴컴컴컴컴컴컴컴켸"  BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 18 PIC X(48) FROM SPACES.

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
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR !!!  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO EXTRATO DE CONTA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA CONSULTAR NA
                   " TELA ? [S/N]  [ ]  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  MONTAN
                   "DO TELA DE CONSULTA ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "MEMORIA INSUFICIENT
                   "E P/ CONSULTA EM TELA  -  TECLE [ESC] !!!       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTO ...                             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-012 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002  LINE 22 COLUMN 49 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     INPUT    CADDSP
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
           MOVE     ANO-SYS  TO  ANO-CB2.

       ROT-000-020.
           DISPLAY  LPA-SUF
                    LPA-ATV  MEN-001
           MOVE     0   TO   PRX-DSP
           ACCEPT   (10 32)  PRX-DSP  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-500-000.
           IF  PRX-DSP   <   1  OR  >  6
               GO  TO        ROT-000-020.
           MOVE     ATV-TAB (PRX-DSP) TO  ATV-AUX
           DISPLAY  PRX-ENT  ATV-ENT.

       ROT-000-030.
           DISPLAY  LPA-DES  MEN-002
           MOVE     0    TO  SUF-DSP
           ACCEPT   (10 34)  SUF-DSP  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           IF  SUF-DSP   =   ZEROS
               GO  TO        ROT-000-030.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADDSP   INVALID KEY
                    DISPLAY  MEN-003
                    ACCEPT   OPC-001
                    GO  TO   ROT-000-030.
           DISPLAY  SUF-ENT
                    DES-ENT  MEN-011
           MOVE     SPACES   TO  CHV-LDP  RSP-OPC
           MOVE     CHV-DSP  TO  COD-LDP
           START    CADLDP   KEY GREATER  CHV-LDP
                    INVALID  KEY GO  TO   ROT-000-040.
           READ     CADLDP   NEXT
           IF  COD-LDP  NOT  =   CHV-DSP
               GO  TO        ROT-000-040.
           SORT     CADSRT   ON  ASCENDING  KEY  DTA-SRT
                    INPUT        PROCEDURE  IS   ROT-100-000
                    OUTPUT       PROCEDURE  IS   ROT-100-040
           GO  TO   ROT-000-030.

       ROT-000-040.
           DISPLAY  MEN-004
           ACCEPT   OPC-001
           GO  TO   ROT-000-030.

       ROT-100-000  SECTION.

       ROT-100-010.
           MOVE     COD-LDP  TO  COD-SRT
           MOVE     DTA-LDP  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-LCT
           MOVE     MES-AUX  TO  MES-LCT
           MOVE     ANO-AUX  TO  ANO-LCT
           IF  ANO-AUX   <   10
               ADD     2000  TO  ANO-LCT
           ELSE
               ADD     1900  TO  ANO-LCT.
           MOVE     DTA-LCT  TO  DTA-SRT
           MOVE     FLG-LDP  TO  FLG-SRT
           MOVE     HST-LDP  TO  HST-SRT
           MOVE     VAL-LDP  TO  VAL-SRT
           RELEASE  REG-SRT.

       ROT-100-020.
           READ     CADLDP   NEXT AT END
                    GO  TO   ROT-100-030.
           IF  COD-LDP   =   CHV-DSP
               GO  TO        ROT-100-010.

       ROT-100-030.

       ROT-100-040  SECTION.

       ROT-100-050.
           DISPLAY  MEN-007
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           IF  RSP-OPC   =   "S"
               GO  TO        ROT-300-000.
           IF  RSP-OPC  NOT  =   "N"
               MOVE     " "  TO  RSP-OPC
               GO  TO        ROT-100-050.

       ROT-200-000.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-005
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE    KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           DISPLAY  MEN-006
           OPEN     OUTPUT   RELATO
           MOVE     1        TO  PAG-CB1
           MOVE     PRX-DSP  TO  PRX-CB2
           MOVE     SUF-DSP  TO  SUF-CB2
           MOVE     DES-DSP  TO  DES-CB2
           MOVE     ZEROS    TO  ACM-001  ACM-002
           WRITE    REG-REL FROM CAB-001  AFTER  0.

       ROT-200-010.
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-003
           MOVE     ZEROS    TO  ACM-LIN.

       ROT-200-020.
           RETURN   CADSRT   AT  END
                    GO       ROT-200-030.
           MOVE     DTA-SRT  TO  DTA-LCT
           IF  ANO-LCT   >   1999
               COMPUTE       ANO-LCT  =   ANO-LCT  -  2000
           ELSE
               COMPUTE       ANO-LCT  =   ANO-LCT  -  1900.
           MOVE     DIA-LCT  TO  DIA-DT1
           MOVE     MES-LCT  TO  MES-DT1
           MOVE     ANO-LCT  TO  ANO-DT1
           MOVE     HST-SRT  TO  HST-DT1
           IF  FLG-SRT   =   1
               MOVE ZEROS    TO  VL1-DT1
               MOVE VAL-SRT  TO  VL2-DT1
               ADD  VAL-SRT  TO  ACM-002
           ELSE
               MOVE VAL-SRT  TO  VL1-DT1
               MOVE ZEROS    TO  VL2-DT1
               ADD  VAL-SRT  TO  ACM-001.
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  ACM-LIN
           IF  ACM-LIN   <   52
               GO  TO        ROT-200-020.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           GO  TO   ROT-200-010.

       ROT-200-030.
           MOVE     ACM-001  TO  VL1-TT1
           MOVE     ACM-002  TO  VL2-TT1
           WRITE    REG-REL FROM TOT-001  AFTER  2
           CLOSE    RELATO       GO  TO   ROT-400-000.

       ROT-300-000.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-300-030.
           DISPLAY  MEN-008
           MOVE     PRX-DSP  TO  PRX-CB2
           MOVE     SUF-DSP  TO  SUF-CB2
           MOVE     DES-DSP  TO  DES-CB2
           MOVE     ZEROS    TO  PAG-CB1  IND1
                                 ACM-001  ACM-002.
       ROT-300-010.
           ADD      1        TO  PAG-CB1
           IF  PAG-CB1       >   8
               GO  TO        ROT-300-090.
           MOVE     CAB-001  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           MOVE     CAB-002  TO  REG-TAB (IND1 + 3)
           MOVE     CAB-003  TO  REG-TAB (IND1 + 4)
           MOVE     CAB-004  TO  REG-TAB (IND1 + 5)
           MOVE     CAB-003  TO  REG-TAB (IND1 + 6)
           MOVE     ZEROS    TO  ACM-LIN
           ADD      7        TO  IND1.

       ROT-300-020.
           MOVE     DTA-SRT  TO  DTA-LCT
           IF  ANO-LCT   >   1999
               COMPUTE       ANO-LCT  =   ANO-LCT  -  2000
           ELSE
               COMPUTE       ANO-LCT  =   ANO-LCT  -  1900.
           MOVE     DIA-LCT  TO  DIA-DT1
           MOVE     MES-LCT  TO  MES-DT1
           MOVE     ANO-LCT  TO  ANO-DT1
           MOVE     HST-SRT  TO  HST-DT1
           MOVE     HST-SRT  TO  HST-DT1
           IF  FLG-SRT   =   1
               MOVE ZEROS    TO  VL1-DT1
               MOVE VAL-SRT  TO  VL2-DT1
               ADD  VAL-SRT  TO  ACM-002
           ELSE
               MOVE VAL-SRT  TO  VL1-DT1
               MOVE ZEROS    TO  VL2-DT1
               ADD  VAL-SRT  TO  ACM-001.
           MOVE     DET-001  TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-300-030.
           ADD      1        TO  ACM-LIN
           IF  ACM-LIN   <   52
               GO  TO        ROT-300-020.
           MOVE     SPACES   TO  REG-TAB (IND1)
           MOVE     CAB-005  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           ADD      2        TO  IND1
           GO  TO   ROT-300-010.

       ROT-300-030.
           MOVE     ACM-001  TO  VL1-TT1
           MOVE     ACM-002  TO  VL2-TT1
           MOVE     SPACES   TO  REG-TAB (IND1)
           MOVE     TOT-001  TO  REG-TAB (IND1 + 1)
           ADD      2        TO  IND1
           COMPUTE  IND2     =   IND1  -  1.

       ROT-300-040.
           MOVE     SPACES   TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1 <  510
               GO  TO        ROT-300-040.
           MOVE     ZEROS    TO  IND1.

       ROT-300-050.
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
           DISPLAY  LPA-002   MEN-009.

       ROT-300-060.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-012
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-300-070.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-300-080.
           IF  IND1  NOT  <  IND2
               GO  TO        ROT-300-060.
           GO  TO   ROT-300-050.

       ROT-300-070.
           IF  IND1   <  36
               GO  TO    ROT-300-060.
           SUBTRACT  36  FROM   IND1
           GO  TO   ROT-300-050.

       ROT-300-080.
           MOVE     SPACES   TO  TLA-AUX
           DISPLAY  LPA-002      TLA-002
                    ENT-001      GO  TO   ROT-400-000.

       ROT-300-090.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-010
           ACCEPT   OPC-001
           GO  TO   ROT-300-080.

       ROT-400-000.

       ROT-400-010  SECTION.

       ROT-500-000.
           MOVE     6    TO  PRM-001
           CLOSE    CADDSP
                    CADLDP   CADPRM
           CHAIN   "DYN300"  USING  PRM-001.
