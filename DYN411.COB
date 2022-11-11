       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN411.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC.

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      CADSRT        ASSIGN  TO  DISK.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

       DATA            DIVISION.
       FILE            SECTION.

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
               05  BCO-SRT      PIC  9(03).
               05  DTA-SRT      PIC  9(08).
           03  DOC-SRT          PIC  9(06).
           03  HST-SRT          PIC  X(20).
           03  VAL-SRT          PIC  9(10)V99.

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC  X(80).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ACM-LIN          PIC  9(02).
           03  ACM-VAL          PIC  9(11)V99.
           03  ACM-TOT          PIC  9(12)V99.

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

           03  REG-TAB          PIC  X(80)  OCCURS 600.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-LCT.
               05  ANO-LCT      PIC  9(04).
               05  MES-LCT      PIC  9(02).
               05  DIA-LCT      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

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
           03  FILLER           PIC  X(72)  VALUE
               "RELACAO DE CHEQUES PENDENTES".
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "  DATA   BANCO NUMERO  HISTORICO              VALOR DO CHEQU
           "E     TOTAL DO BANCO".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  DIA-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DT1          PIC  9(02).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  BCO-DT1          PIC  9(03).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  DOC-DT1          PIC  9(06).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  HST-DT1          PIC  X(21).
           03  VAL-DT1          PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  TOT-DT1          PIC  ZZ.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.

       01  TOT-001.
           03  FILLER           PIC  X(62)  VALUE  "
               "   TOTAL GERAL ..........................".
           03  VAL-TT1          PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "RELACAO DOS CHEQUES PENDENTES  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.

       01  TLA-002.
           03  LINE 09 COLUMN 29 PIC X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 29 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 29 PIC X(21) FROM " ³  Relacao Total  ³
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 29 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 29 PIC X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 31 PIC X(21) FROM SPACES.

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

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA CONSULTAR NA
                   " TELA ? [S/N]  [ ]  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  MONTAN
                   "DO TELA DE CONSULTA ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "MEMORIA INSUFICIENT
                   "E P/ CONSULTA EM TELA  -  TECLE [ESC] !!!       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR !!!  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO RELACAO DE CHEQUES PENDENTES ...           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTO ...                             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE MOVIMENT
                   "O NO PERIODO !!!  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05.

       01  OPC-001 LINE 22 COLUMN 49 PIC X(01) USING  RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 80 PIC X(01) USING  RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           MOVE     SPACES   TO  TLA-AUX
           DISPLAY  TLA-001  LPA-001
           DISPLAY  TLA-002  MEN-007
           OPEN     INPUT    CADLBC
                             CADPRM
           READ     CADPRM
           MOVE     1   TO   IND2.

       ROT-000-010.
           MOVE     CLI-PRM (IND2 + 1)  TO IND1
           MOVE     LET-TAB (IND1)  TO  LET-NOM (IND2)
           ADD      1   TO   IND2
           IF  IND2  <  36
               GO  TO        ROT-000-010.
           MOVE     SPACES   TO  LET-NOM (IND2)
           ACCEPT   DTA-SYS FROM DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.
           MOVE     NOM-TAB  TO  NOM-CB1
           MOVE     DIA-SYS  TO  DIA-CB2
           MOVE     MES-SYS  TO  MES-CB2
           MOVE     ANO-SYS  TO  ANO-CB2
           MOVE     ZEROS    TO  ACM-VAL  ACM-TOT
           MOVE     SPACES   TO  CHV-LBC  RSP-OPC
           START    CADLBC   KEY GREATER  CHV-LBC
                    INVALID  KEY GO  TO   ROT-000-030.

       ROT-000-020.
           READ     CADLBC   NEXT AT END
                    GO  TO   ROT-000-030.
           IF  FLG-LBC   =   1
               GO  TO        ROT-100-000.
           GO  TO   ROT-000-020.

       ROT-000-030.
           DISPLAY  MEN-008
           ACCEPT   OPC-002
           GO  TO   ROT-400-000.

       ROT-100-000.
           MOVE     SPACES   TO  CHV-LBC
           START    CADLBC   KEY GREATER         CHV-LBC
           SORT     CADSRT   ON  ASCENDING  KEY  CHV-SRT
                    INPUT        PROCEDURE  IS   ROT-100-010
                    OUTPUT       PROCEDURE  IS   ROT-100-030
           GO  TO   ROT-400-000.

       ROT-100-010  SECTION.

       ROT-100-020.
           READ     CADLBC   NEXT AT END
                    GO  TO   ROT-100-025.
           IF  FLG-LBC   =   0
               GO  TO        ROT-100-020.
           MOVE     BCO-LBC  TO  BCO-SRT
           MOVE     DTA-LBC  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-LCT
           MOVE     MES-AUX  TO  MES-LCT
           MOVE     ANO-AUX  TO  ANO-LCT
           IF  ANO-AUX   <   10
               ADD     2000  TO  ANO-LCT
           ELSE
               ADD     1900  TO  ANO-LCT.
           MOVE     DTA-LCT  TO  DTA-SRT
           MOVE     DOC-LBC  TO  DOC-SRT
           MOVE     HST-LBC  TO  HST-SRT
           MOVE     VAL-LBC  TO  VAL-SRT
           RELEASE  REG-SRT  GO  ROT-100-020.

       ROT-100-025.

       ROT-100-030  SECTION.

       ROT-100-040.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-400-000.
           DISPLAY  MEN-001
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           IF  RSP-OPC   =   "N"
               GO  TO        ROT-300-000.
           IF  RSP-OPC  NOT  =   "S"
               GO  TO        ROT-100-040.
           DISPLAY  MEN-002
           MOVE     ZEROS    TO  PAG-CB1  IND1.

       ROT-200-000.
           ADD      1        TO  PAG-CB1
           IF  PAG-CB1   >   9
               GO  TO        ROT-200-090.
           MOVE     CAB-001  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           MOVE     CAB-002  TO  REG-TAB (IND1 + 3)
           MOVE     CAB-003  TO  REG-TAB (IND1 + 4)
           MOVE     CAB-004  TO  REG-TAB (IND1 + 5)
           MOVE     CAB-003  TO  REG-TAB (IND1 + 6)
           ADD      7        TO  IND1
           MOVE     ZEROS    TO  ACM-LIN.

       ROT-200-010.
           IF  ACM-LIN   <   52
               GO  TO        ROT-200-020.
           MOVE     SPACES   TO  REG-TAB (IND1)
           MOVE     CAB-005  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           ADD      2        TO  IND1
           PERFORM  ROT-200-000.

       ROT-200-020.
           MOVE     DTA-SRT  TO  DTA-LCT
           IF  ANO-LCT   <  2000
               COMPUTE           ANO-LCT  =
                                 ANO-LCT  -  1900
           ELSE
               COMPUTE           ANO-LCT  =
                                 ANO-LCT  -  2000.
           MOVE     DIA-LCT  TO  DIA-DT1
           MOVE     MES-LCT  TO  MES-DT1
           MOVE     ANO-LCT  TO  ANO-DT1
           MOVE     BCO-SRT  TO  BCO-DT1
           MOVE     DOC-SRT  TO  DOC-DT1
           MOVE     HST-SRT  TO  HST-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      VAL-SRT  TO  ACM-VAL.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-200-030.
           IF  BCO-SRT  NOT  =   BCO-DT1
               PERFORM       ROT-200-030.
           MOVE     DET-001  TO  REG-TAB (IND1)
           MOVE     ZEROS    TO  TOT-DT1
           ADD      1        TO  IND1
           ADD      1        TO  ACM-LIN
           GO  TO   ROT-200-010.

       ROT-200-030.
           MOVE     ACM-VAL  TO  TOT-DT1
           ADD      ACM-VAL  TO  ACM-TOT
           MOVE     ZEROS    TO  ACM-VAL.

       ROT-200-040.
           MOVE     ACM-TOT  TO  VAL-TT1
           MOVE     DET-001  TO  REG-TAB (IND1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 1)
           MOVE     TOT-001  TO  REG-TAB (IND1 + 2)
           ADD      2        TO  IND1
           MOVE     IND1     TO  IND2.

       ROT-200-050.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1 <  600
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
           DISPLAY  LPA-001   MEN-003.

       ROT-200-070.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-009
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
           GO  TO   ROT-200-060.

       ROT-200-090.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-004
           ACCEPT   OPC-002  GO  ROT-400-000.

       ROT-300-000.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-005
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           DISPLAY  MEN-006
           OPEN     OUTPUT   RELATO
           MOVE     1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  0.

       ROT-300-010.
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-003
           MOVE     ZEROS    TO  ACM-LIN.

       ROT-300-020.
           IF  ACM-LIN   <   52
               GO  TO        ROT-300-030.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           PERFORM  ROT-300-010.

       ROT-300-030.
           MOVE     DTA-SRT  TO  DTA-LCT
           IF  ANO-LCT   <  2000
               COMPUTE           ANO-LCT  =
                                 ANO-LCT  -  1900
           ELSE
               COMPUTE           ANO-LCT  =
                                 ANO-LCT  -  2000.
           MOVE     DIA-LCT  TO  DIA-DT1
           MOVE     MES-LCT  TO  MES-DT1
           MOVE     ANO-LCT  TO  ANO-DT1
           MOVE     BCO-SRT  TO  BCO-DT1
           MOVE     DOC-SRT  TO  DOC-DT1
           MOVE     HST-SRT  TO  HST-DT1
           MOVE     VAL-SRT  TO  VAL-DT1
           ADD      VAL-SRT  TO  ACM-VAL.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-300-040.
           IF  BCO-SRT  NOT  =   BCO-DT1
               PERFORM       ROT-300-040.
           WRITE    REG-REL FROM DET-001
           MOVE     ZEROS    TO  TOT-DT1
           ADD      1        TO  ACM-LIN
           GO  TO   ROT-300-020.

       ROT-300-040.
           MOVE     ACM-VAL  TO  TOT-DT1
           ADD      ACM-VAL  TO  ACM-TOT
           MOVE     ZEROS    TO  ACM-VAL.

       ROT-300-050.
           MOVE     ACM-TOT  TO  VAL-TT1
           WRITE    REG-REL FROM DET-001
           WRITE    REG-REL FROM TOT-001  AFTER  2
           CLOSE    RELATO.

       ROT-400-000.
           MOVE     6    TO  PRM-001
           CLOSE    CADLBC   CADPRM
           CHAIN   "DYN400"  USING   PRM-001.
