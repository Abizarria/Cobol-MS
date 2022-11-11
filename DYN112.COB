       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN112.
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

           SELECT      CADLCP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCP.

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

       FD  CADLCP      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLCP.DAT".
       01  REG-LCP.
           03  CHV-LCP.
               05  PRX-LCP      PIC  9(01).
               05  SUF-LCP      PIC  9(03).
               05  DTA-LCP      PIC  9(06).
               05  LCT-LCP      PIC  9(03).
           03  NTF-LCP          PIC  9(06).
           03  QTD-LCP          PIC S9(06).
           03  CST-LCP          PIC  9(07)V9999.

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
           03  ACM-QTD          PIC S9(07).
           03  ACM-VAL          PIC S9(11)V99.
           03  ACM-TOT          PIC S9(12)V99.

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

           03  REG-TAB          PIC  X(80)  OCCURS 504.

           03  TP1-TOT          PIC  X(34)  VALUE
               "- SUB-TOTAL ......................".
           03  TP2-TOT          PIC  X(34)  VALUE
               "- TOTAL GERAL ....................".

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
           03  FILLER           PIC  X(36)  VALUE  "RELACAO DE COMPRAS".
           03  ATV-CB2          PIC  X(36).
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "CODIGO   DATA    DESCRICAO DO PRODUTO      N. FISCAL   VOLUM
           "E     PRECO DE CUSTO".

       01  CAB-005              PIC  X(80)  VALUE  ALL  "- ".

       01  DET-001.
           03  PRX-DT1          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DT1          PIC  9(03).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  DIA-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DT1          PIC  9(02).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  DES-DT1          PIC  X(27).
           03  NTF-DT1          PIC  999.999.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  QTD-DT1          PIC  ----.--9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  VAL-DT1          PIC  ---.---.---.--9,99.

       01  TOT-001.
           03  FILLER           PIC  X(17)  VALUE  SPACES.
           03  TIP-TT1          PIC  X(34).
           03  QTD-TT1          PIC  --.---.--9    BLANK  WHEN  ZEROS.
           03  VAL-TT1          PIC  ----.---.---.--9,99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "RELACAO DE COMPRAS  -  Ver. 7.11"
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
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE COMPRAS
                   "LANCADAS !!!  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO RELACAO DE COMPRAS ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA CONSULTAR NA
                   " TELA ? [S/N]  [ ]  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE COMPRAS
                   "LANCADAS NESTE GRUPO !!!  -  TECLE [ESC] ...    "
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
                           CADLCP CADPRM
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
                    CADLCP   CADPRM
           CHAIN   "DYN100"  USING  PRM-001.

       ROT-100-000.
           DISPLAY  TLA-002
           MOVE     SPACES   TO  CHV-LCP
                                 RSP-OPC  ATV-CB2
           START    CADLCP   KEY GREATER  CHV-LCP
                    INVALID  KEY GO  TO   ROT-100-060.
           READ     CADLCP   NEXT
           DISPLAY  MEN-001
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.

       ROT-100-010.
           DISPLAY  MEN-003
           OPEN     OUTPUT   RELATO
           MOVE     1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  0
           MOVE     ZEROS    TO  ACM-LIN  ACM-QTD
                                 ACM-VAL  ACM-TOT.
       ROT-100-020.
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-003.

       ROT-100-030.
           MOVE     PRX-LCP  TO  PRX-DT1  PRX-EST
           MOVE     SUF-LCP  TO  SUF-DT1  SUF-EST
           READ     CADEST
           MOVE     DES-EST  TO  DES-DT1
           MOVE     DTA-LCP  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     ANO-AUX  TO  ANO-DT1
           MOVE     NTF-LCP  TO  NTF-DT1
           MOVE     QTD-LCP  TO  QTD-DT1
           COMPUTE  VAL-DT1  =   QTD-LCP  *    CST-LCP
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  ACM-LIN
           ADD      QTD-LCP  TO  ACM-QTD
           COMPUTE  ACM-VAL  =   ACM-VAL  +    QTD-LCP  *  CST-LCP
           READ     CADLCP   NEXT AT END
                    PERFORM  ROT-100-040
                    THRU     ROT-100-050
                    GO  TO   ROT-000-020.
           IF  PRX-LCP  NOT  =   PRX-EST  OR
              (SUF-LCP  NOT  =   SUF-EST  AND  PRX-LCP  =  0)
               PERFORM       ROT-100-040.
           IF  ACM-LIN   <   50
               GO  TO        ROT-100-030.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           MOVE     ZEROS    TO  ACM-LIN
           GO  TO   ROT-100-020.

       ROT-100-040.
           MOVE     TP1-TOT  TO  TIP-TT1
           MOVE     ACM-QTD  TO  QTD-TT1
           MOVE     ACM-VAL  TO  VAL-TT1
           WRITE    REG-REL FROM TOT-001  AFTER  2
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL               AFTER  2
           ADD      4        TO  ACM-LIN
           ADD      ACM-VAL  TO  ACM-TOT
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL.

       ROT-100-050.
           MOVE     TP2-TOT  TO  TIP-TT1
           MOVE     ZEROS    TO  QTD-TT1
           MOVE     ACM-TOT  TO  VAL-TT1
           WRITE    REG-REL FROM TOT-001  AFTER  0
           CLOSE    RELATO.

       ROT-100-060.
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
           MOVE     CHV-ATV  TO  PRX-LCP
           MOVE     ZEROS    TO  SUF-LCP
           START    CADLCP   KEY GREATER  CHV-LCP
                    INVALID  KEY GO  TO   ROT-200-220.
           READ     CADLCP   NEXT
           IF  PRX-LCP  NOT  =   CHV-ATV
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
           MOVE     PRX-LCP  TO  PRX-DT1  PRX-EST
           MOVE     SUF-LCP  TO  SUF-DT1  SUF-EST
           READ     CADEST
           MOVE     DES-EST  TO  DES-DT1
           MOVE     DTA-LCP  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     ANO-AUX  TO  ANO-DT1
           MOVE     NTF-LCP  TO  NTF-DT1
           MOVE     QTD-LCP  TO  QTD-DT1
           COMPUTE  VAL-DT1  =   QTD-LCP  *    CST-LCP
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  ACM-LIN
           ADD      QTD-LCP  TO  ACM-QTD
           COMPUTE  ACM-VAL  =   ACM-VAL  +    QTD-LCP  *  CST-LCP
           READ     CADLCP   NEXT AT END
                    GO  TO   ROT-200-050.
           IF  PRX-LCP  NOT  =   PRX-EST
               GO  TO        ROT-200-050.
           IF (SUF-LCP  NOT  =   SUF-EST  AND  PRX-LCP  =  0)
               PERFORM       ROT-100-040.
           IF  ACM-LIN   <   50
               GO  TO        ROT-200-040.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           MOVE     ZEROS    TO  ACM-LIN
           PERFORM  ROT-100-020
           GO  TO   ROT-200-040.

       ROT-200-050.
           PERFORM  ROT-100-040
              THRU  ROT-100-050
           GO  TO   ROT-200-000.

       ROT-200-100.
           DISPLAY  MEN-007
           MOVE     ZEROS    TO  PAG-CB1  IND1
                                 ACM-QTD  ACM-VAL  ACM-TOT.
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
           MOVE     PRX-LCP  TO  PRX-DT1
                                 PRX-EST
           MOVE     SUF-LCP  TO  SUF-DT1
                                 SUF-EST
           READ     CADEST
           MOVE     DES-EST  TO  DES-DT1
           MOVE     DTA-LCP  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-DT1
           MOVE     MES-AUX  TO  MES-DT1
           MOVE     ANO-AUX  TO  ANO-DT1
           MOVE     NTF-LCP  TO  NTF-DT1
           MOVE     QTD-LCP  TO  QTD-DT1
           COMPUTE  VAL-DT1  =   QTD-LCP
                             *   CST-LCP
           MOVE     DET-001  TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           ADD      1        TO  ACM-LIN
           ADD      QTD-LCP  TO  ACM-QTD
           COMPUTE  ACM-VAL  =   ACM-VAL
                 +  QTD-LCP  *   CST-LCP
           READ     CADLCP   NEXT AT END
                    GO  TO   ROT-200-130.
           IF  PRX-LCP  NOT  =   PRX-EST
               GO  TO        ROT-200-130.
           IF (PRX-LCP   =   0   AND
               SUF-LCP  NOT  =   SUF-EST)
               PERFORM       ROT-200-130.
           IF  ACM-LIN   <   50
               GO  TO        ROT-200-120.
           MOVE     SPACES   TO  REG-TAB (IND1)
           MOVE     CAB-005  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           ADD      2        TO  IND1
           GO  TO   ROT-200-110.

       ROT-200-130.
           MOVE     TP1-TOT  TO  TIP-TT1
           MOVE     ACM-QTD  TO  QTD-TT1
           MOVE     ACM-VAL  TO  VAL-TT1
           MOVE     SPACES   TO  REG-TAB (IND1)
           MOVE     TOT-001  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           MOVE     SPACES   TO  REG-TAB (IND1 + 3)
           ADD      4        TO  IND1
           ADD      4        TO  ACM-LIN
           ADD      ACM-VAL  TO  ACM-TOT
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL.

       ROT-200-140.
           MOVE     TP2-TOT  TO  TIP-TT1
           MOVE     ZEROS    TO  QTD-TT1
           MOVE     ACM-TOT  TO  VAL-TT1
           MOVE     TOT-001  TO  REG-TAB (IND1 - 1)
           COMPUTE  IND2     =   IND1  -  1.

       ROT-200-150.
           MOVE     SPACES   TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1 <  500
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
           MOVE     SPACES   TO  CHV-LCP
           MOVE     CHV-ATV  TO  PRX-LCP
           START    CADLCP   KEY GREATER  CHV-LCP
           READ     CADLCP   NEXT
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
