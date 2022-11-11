       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN113.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADATV        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ATV.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST.

           SELECT      CADLES        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LES.

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADATV      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADATV.DAT".
       01  REG-ATV.
           03  CHV-ATV          PIC  9(01).
           03  DES-ATV          PIC  X(15).

       FD  CADEST      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADEST.DAT".
       01  REG-EST.
           03  CHV-EST.
               05  PRX-EST      PIC  9(01).
               05  SUF-EST      PIC  9(03).
           03  DES-EST          PIC  X(25).
           03  MIN-EST          PIC  9(06).
           03  SDO-EST          PIC S9(06)V9.
           03  CST-EST          PIC  9(07)V9999.
           03  CTM-EST          PIC  9(07)V9999.
           03  VDA-EST          PIC  9(07)V99.
           03  PCT-EST          PIC S9(03)V99.
           03  QVM-EST          PIC  9(07)V9.
           03  QVA-EST          PIC  9(08)V9.

       FD  CADLES      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLES.DAT".
       01  REG-LES.
           03  CHV-LES.
               05  PRX-LES      PIC  9(01).
               05  SUF-LES      PIC  9(03).
               05  DTA-LES      PIC  9(06).
               05  LCT-LES      PIC  9(03).
           03  SDA-LES          PIC S9(06)V9.
           03  SDC-LES          PIC  9(06)V9.

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
           03  IND3             PIC  9(02).
           03  LET-OPC          PIC  X(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ACM-LIN          PIC  9(02).
           03  ACM-DIF          PIC S9(07)V9.

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

           03  REG-TAB          PIC  X(80)  OCCURS 504.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

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
           03  FILLER           PIC  X(36)  VALUE  "RELACAO DE ACERTOS".
           03  ATV-CB2          PIC  X(36).
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "CODGO DESCRICAO DO PRODUTO        DATA    ANTERIOR CORRIGIDO
           "  DIF. DIA  DIF. MES".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  PRX-DT1          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DT1          PIC  9(03).
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  DES-DT1          PIC  X(26).
           03  DIA-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DT1          PIC  9(02).
           03  SDI-DT1          PIC  ----.--9,9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  SDA-DT1          PIC  ZZZ.ZZ9,9.
           03  DIF-DT1          PIC  ----.--9,9.
           03  TOT-DT1          PIC  ----.--9,9    BLANK WHEN ZEROS.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "RELACAO DOS ACERTOS NO ESTOQUE  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.

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

       01  TLA-003.
           03  LINE 09 COLUMN 22 PIC X(37) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 22 PIC X(37) FROM " ³
               "            ³"   BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 22 PIC X(37) FROM " ³  Grupo :    -
               "            ³"   BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 22 PIC X(37) FROM " ³
               "            ³"   BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 22 PIC X(37) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 59 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 24 PIC X(37) FROM SPACES.

       01  ENT-001.
           03  LINE 11 COLUMN 34 PIC 9(01) USING CHV-ATV
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 40 PIC X(15) USING DES-ATV
               BACKGROUND-COLOR  01.

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
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE ACERTOS
                   "DE SALDOS NO PERIODO  -  [ESC] RETORNA ...      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO RELACAO DE ACERTOS ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA CONSULTAR NA
                   " TELA ? [S/N]  [ ]  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE ACERTOS
                   "LANCADOS NESTE GRUPO !!!  -  TECLE [ESC] ...    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  MONTAN
                   "DO TELA DE CONSULTA ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "MEMORIA INSUFICIENT
                   "E P/ CONSULTA EM TELA !!!  -  TECLE [ESC] ...   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05.

       01  OPC-001 LINE 22 COLUMN 49 PIC X(01) USING LET-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001.

       ROT-000-000.
           MOVE     SPACES   TO  TLA-AUX
           DISPLAY  TLA-001      LPA-001
           OPEN     INPUT  CADATV CADEST
                           CADLES CADPRM
           READ     CADPRM
           MOVE     1        TO   IND2.

       ROT-000-010.
           MOVE     CLI-PRM (IND2 + 1)  TO IND3
           MOVE     LET-TAB (IND3)  TO  LET-NOM (IND2)
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
           IF  PRM-001   =   1
               GO  TO        ROT-100-000.
           GO  TO   ROT-200-000.

       ROT-000-020.
           MOVE     5    TO  PRM-001
           CLOSE    CADATV   CADEST
                    CADLES   CADPRM
           CHAIN   "DYN100"  USING  PRM-001.

       ROT-100-000.
           DISPLAY  TLA-002
           MOVE     SPACES   TO  CHV-LES
                                 RSP-OPC  ATV-CB2
           START    CADLES   KEY GREATER  CHV-LES
                    INVALID  KEY GO  TO   ROT-100-070.
           READ     CADLES   NEXT
           DISPLAY  MEN-001
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.

       ROT-100-010.
           DISPLAY  MEN-003
           OPEN     OUTPUT       RELATO
           MOVE     1        TO  PAG-CB1
           MOVE     ZEROS    TO  ACM-LIN  ACM-DIF
           WRITE    REG-REL FROM CAB-001  AFTER 0.

       ROT-100-020.
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-003.

       ROT-100-030.
           MOVE     PRX-LES  TO  PRX-DT1  PRX-EST
           MOVE     SUF-LES  TO  SUF-DT1  SUF-EST
           READ     CADEST
           MOVE     DES-EST  TO  DES-DT1
           MOVE     DTA-LES  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     ANO-AUX  TO  ANO-DT1
           MOVE     SDA-LES  TO  SDI-DT1
           MOVE     SDC-LES  TO  SDA-DT1
           COMPUTE  DIF-DT1  =   SDC-LES
                             -   SDA-LES
           COMPUTE  ACM-DIF  =   ACM-DIF
                 +  SDC-LES  -   SDA-LES.
           READ     CADLES   NEXT AT END
                    GO  TO   ROT-100-050.
           IF  PRX-LES  NOT  =   PRX-EST  OR
               SUF-LES  NOT  =   SUF-EST
               PERFORM       ROT-100-050
               GO  TO        ROT-100-040.
           MOVE     ZEROS    TO  TOT-DT1
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  ACM-LIN.

       ROT-100-040.
           IF  ACM-LIN   <   54
               GO  TO        ROT-100-030.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           MOVE     ZEROS    TO  ACM-LIN
           GO  TO   ROT-100-020.

       ROT-100-050.
           MOVE     ACM-DIF  TO  TOT-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL
           ADD      2        TO  ACM-LIN
           MOVE     ZEROS    TO  ACM-DIF.

       ROT-100-060.
           CLOSE    RELATO
           GO  TO   ROT-000-020.

       ROT-100-070.
           MOVE     " "  TO  RSP-OPC
           DISPLAY  MEN-002
           ACCEPT   OPC-002
           GO  TO   ROT-000-020.

       ROT-200-000.
           DISPLAY  TLA-003.

       ROT-200-010.
           MOVE     0   TO   CHV-ATV
           DISPLAY  MEN-004
           ACCEPT   (11 34)  CHV-ATV  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           READ     CADATV   INVALID KEY
                    GO  TO   ROT-200-010.
           DISPLAY  ENT-001
           MOVE     CHV-ATV  TO  PRX-LES
           MOVE     ZEROS    TO  SUF-LES
           START    CADLES   KEY GREATER  CHV-LES
                    INVALID  KEY GO  TO   ROT-200-220.
           READ     CADLES   NEXT
           IF  PRX-LES  NOT  =   CHV-ATV
               GO  TO        ROT-200-220.
           MOVE     DES-ATV  TO  ATV-CB2.

       ROT-200-020.
           DISPLAY  MEN-005
           MOVE     " "  TO  LET-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           IF  LET-OPC   =   "S"
               GO  TO        ROT-200-100.
           IF  LET-OPC  NOT  =   "N"
               GO  TO        ROT-200-020.

       ROT-200-030.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-001
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           PERFORM  ROT-100-010   THRU    ROT-100-020.

       ROT-200-040.
           MOVE     PRX-LES  TO  PRX-DT1  PRX-EST
           MOVE     SUF-LES  TO  SUF-DT1  SUF-EST
           READ     CADEST
           MOVE     DES-EST  TO  DES-DT1
           MOVE     DTA-LES  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     ANO-AUX  TO  ANO-DT1
           MOVE     SDA-LES  TO  SDI-DT1
           MOVE     SDC-LES  TO  SDA-DT1
           COMPUTE  DIF-DT1  =   SDC-LES
                             -   SDA-LES
           COMPUTE  ACM-DIF  =   ACM-DIF
                 +  SDC-LES  -   SDA-LES
           READ     CADLES   NEXT AT END
                    GO  TO   ROT-200-060.
           IF  PRX-LES  NOT  =   PRX-EST
               GO  TO        ROT-200-060.
           IF  SUF-LES  NOT  =   SUF-EST
               PERFORM       ROT-100-050
               GO  TO        ROT-200-050.
           MOVE     ZEROS    TO  TOT-DT1
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  ACM-LIN.

       ROT-200-050.
           IF  ACM-LIN   <   54
               GO  TO        ROT-200-040.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           MOVE     ZEROS    TO  ACM-LIN
           PERFORM  ROT-100-020
           GO  TO   ROT-200-040.

       ROT-200-060.
           MOVE     ACM-DIF  TO  TOT-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL
           ADD      2        TO  ACM-LIN
           MOVE     ZEROS    TO  ACM-DIF.

       ROT-200-070.
           CLOSE    RELATO
           GO  TO   ROT-200-000.

       ROT-200-100.
           DISPLAY  MEN-007
           MOVE     ZEROS    TO  PAG-CB1
                                 ACM-DIF  IND1.
       ROT-200-110.
           ADD      1        TO  PAG-CB1
           IF  PAG-CB1   >   8
               GO  TO        ROT-200-210.
           MOVE     CAB-001  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           MOVE     CAB-002  TO  REG-TAB (IND1 + 3)
           MOVE     CAB-003  TO  REG-TAB (IND1 + 4)
           MOVE     CAB-004  TO  REG-TAB (IND1 + 5)
           MOVE     CAB-003  TO  REG-TAB (IND1 + 6)
           MOVE     ZEROS    TO  ACM-LIN
           ADD      7        TO  IND1.

       ROT-200-120.
           MOVE     PRX-LES  TO  PRX-DT1  PRX-EST
           MOVE     SUF-LES  TO  SUF-DT1  SUF-EST
           READ     CADEST
           MOVE     DES-EST  TO  DES-DT1
           MOVE     DTA-LES  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     ANO-AUX  TO  ANO-DT1
           MOVE     SDA-LES  TO  SDI-DT1
           MOVE     SDC-LES  TO  SDA-DT1
           COMPUTE  DIF-DT1  =   SDC-LES
                             -   SDA-LES
           COMPUTE  ACM-DIF  =   ACM-DIF
                 +  SDC-LES  -   SDA-LES
           READ     CADLES   NEXT AT END
                    GO  TO   ROT-200-140.
           IF  PRX-LES  NOT  =   PRX-EST
               GO  TO        ROT-200-140.
           IF  SUF-LES  NOT  =   SUF-EST
               PERFORM       ROT-200-140
               GO  TO        ROT-200-130.
           MOVE     ZEROS    TO  TOT-DT1
           MOVE     DET-001  TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           ADD      1        TO  ACM-LIN.

       ROT-200-130.
           IF  ACM-LIN   <   54
               GO  TO        ROT-200-120.
           MOVE     SPACES   TO  REG-TAB (IND1)
           MOVE     CAB-005  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           ADD      2        TO  IND1
           GO  TO   ROT-200-110.

       ROT-200-140.
           MOVE     ACM-DIF  TO  TOT-DT1
           MOVE     DET-001  TO  REG-TAB (IND1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 1)
           MOVE     IND1     TO  IND2
           ADD      2        TO  IND1
           ADD      2        TO  ACM-LIN
           MOVE     ZEROS    TO  ACM-DIF.

       ROT-200-150.
           MOVE     SPACES   TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1 <  511
               GO  TO        ROT-200-150.
           MOVE     ZEROS    TO  IND1.

       ROT-200-160.
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

       ROT-200-170.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-010
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-200-180.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-200-190.
           IF  IND1  NOT  <  IND2
               GO  TO        ROT-200-170.
           GO  TO   ROT-200-160.

       ROT-200-180.
           IF  IND1   <  36
               GO  TO    ROT-200-170.
           SUBTRACT  36  FROM   IND1
           GO  TO   ROT-200-160.

       ROT-200-190.
           MOVE     SPACES   TO  TLA-AUX
           DISPLAY  LPA-001
                    TLA-003      ENT-001.

       ROT-200-200.
           MOVE     SPACES   TO  CHV-LES
           MOVE     CHV-ATV  TO  PRX-LES
           START    CADLES   KEY GREATER  CHV-LES
           READ     CADLES   NEXT
           GO  TO   ROT-200-030.

       ROT-200-210.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-009
           ACCEPT   OPC-002
           GO  TO   ROT-200-200.

       ROT-200-220.
           MOVE     " " TO  RSP-OPC
           DISPLAY  MEN-006
           ACCEPT   OPC-002
           GO  TO   ROT-200-000.
