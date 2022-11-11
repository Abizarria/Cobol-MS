       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN404.
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

           SELECT      CADSRT        ASSIGN  TO  DISK.

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

       SD  CADSRT.

       01  REG-SRT.
           03  CHV-SRT.
               05  DTA-SRT      PIC  9(08).
               05  BCO-SRT      PIC  9(03).
               05  LCT-SRT      PIC  9(03).
           03  DOC-SRT          PIC  9(06).
           03  HST-SRT          PIC  X(20).
           03  VAL-SRT          PIC  9(10)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  IND3             PIC  9(03).
           03  IND4             PIC  9(02).
           03  IND5             PIC  9(03).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  SET-AUX          PIC  X(01)  VALUE  "¯".
           03  FLG-AUX          PIC  X(01)  VALUE  "û".

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  DTA-AX1.
               05  ANO-AX1      PIC  9(02).
               05  MES-AX1      PIC  9(02).
               05  DIA-AX1      PIC  9(02).

           03  DTA-LCT.
               05  ANO-LCT      PIC  9(04).
               05  MES-LCT      PIC  9(02).
               05  DIA-LCT      PIC  9(02).

           03  TAB-CHK                      OCCURS 300.
               05  CHV-TAB      PIC  X(14).
               05  REG-TAB.
                   07  SET-TAB  PIC  X(02).
                   07  FLG-TAB  PIC  X(02).
                   07  DET-TAB  PIC  X(64).

           03  LIN-001          PIC  X(68).
           03  LIN-002          PIC  X(68).
           03  LIN-003          PIC  X(68).
           03  LIN-004          PIC  X(68).
           03  LIN-005          PIC  X(68).
           03  LIN-006          PIC  X(68).
           03  LIN-007          PIC  X(68).
           03  LIN-008          PIC  X(68).
           03  LIN-009          PIC  X(68).
           03  LIN-010          PIC  X(68).
           03  LIN-011          PIC  X(68).

       01  DET-001.
           03  DIA-DET          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DET          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DET          PIC  9(02).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  BCO-DET          PIC  9(03).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  DOC-DET          PIC  9(06).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  HST-DET          PIC  X(22).
           03  VAL-DET          PIC  Z.ZZZ.ZZZ.ZZ9,99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "CONCILIACAO DO MOVIMENTO  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.

       01  TLA-002.
           03  LINE 05 COLUMN 03 PIC X(74) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 03 PIC X(74) FROM " ³       DATA    BANCO
               " CHEQUE   HISTORICO              VALOR DO CHEQUE ³"
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 03 PIC X(74) FROM " ³
               "                                                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 03 PIC X(74) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 05 PIC X(74) FROM SPACES.

       01  TLA-003.
           03  LINE 10 COLUMN 28 PIC X(24) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 28 PIC X(24) FROM " ³                    ³
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 28 PIC X(24) FROM " ³  Data .:   /  /    ³
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 28 PIC X(24) FROM " ³                    ³
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 28 PIC X(24) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 30 PIC X(24) FROM SPACES.

       01  ENT-001.
           03  LINE 08 COLUMN 06 PIC X(68) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 06 PIC X(68) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 06 PIC X(68) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 06 PIC X(68) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 06 PIC X(68) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 06 PIC X(68) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 06 PIC X(68) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 06 PIC X(68) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 06 PIC X(68) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 06 PIC X(68) USING LIN-010
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 06 PIC X(68) USING LIN-011
               BACKGROUND-COLOR  01.

       01  ENT-002.
           03  DIA-ENT LINE 12 COLUMN 40 PIC 9(02) USING DIA-AUX
                       BACKGROUND-COLOR  04.
           03  MES-ENT LINE 12 COLUMN 43 PIC 9(02) USING MES-AUX
                       BACKGROUND-COLOR  04.
           03  ANO-ENT LINE 12 COLUMN 46 PIC 9(02) USING ANO-AUX
                       BACKGROUND-COLOR  04.

       01  LPA-001.
           03  LPA-MES LINE 12 COLUMN 43 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  04.
           03  LPA-ANO LINE 12 COLUMN 46 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  04.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO CHEQUES PENDENTES ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE CHEQUES
                   "PENDENTES  -  [ESC]  RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR, [F1] P/ MARCAR OU [ESC] P/ RETORNAR ...     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA DA CO
                   "MPENSACAO DOS CHEQUES  -  [ESC] RETORNA ...     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  CONCIL
                   "IANDO MOVIMENTO BANCARIO ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 80 PIC X(01) FROM SPACES
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001
                    TLA-002  MEN-001
           OPEN     I-O      CADLBC.

       ROT-000-010.
           MOVE     SPACES   TO  CHV-LBC    RSP-OPC
           START    CADLBC   KEY GREATER    CHV-LBC
                    INVALID  KEY GO  TO     ROT-000-020.
           SORT     CADSRT   ON  ASCENDING  KEY CHV-SRT
                    INPUT        PROCEDURE  IS  ROT-100-000
                    OUTPUT       PROCEDURE  IS  ROT-100-040
           IF  IND1   >   1
               GO  TO            ROT-100-100.

       ROT-000-020.
           DISPLAY  MEN-002
           ACCEPT   OPC-001.

       ROT-000-030.
           CLOSE    CADLBC
           MOVE     1         TO     PRM-001
           CHAIN    "DYN400"  USING  PRM-001.

       ROT-100-000  SECTION.

       ROT-100-010.
           READ     CADLBC   NEXT AT END
                    GO  TO   ROT-100-020.
           IF  FLG-LBC  NOT  =   1
               GO  TO        ROT-100-010.
           MOVE     DTA-LBC  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-LCT
           MOVE     MES-AUX  TO  MES-LCT
           MOVE     ANO-AUX  TO  ANO-LCT
           IF  ANO-AUX   <   10
               ADD     2000  TO  ANO-LCT
           ELSE
               ADD     1900  TO  ANO-LCT.
           MOVE     DTA-LCT  TO  DTA-SRT
           MOVE     BCO-LBC  TO  BCO-SRT
           MOVE     LCT-LBC  TO  LCT-SRT
           MOVE     DOC-LBC  TO  DOC-SRT
           MOVE     HST-LBC  TO  HST-SRT
           MOVE     VAL-LBC  TO  VAL-SRT
           RELEASE  REG-SRT  GO  ROT-100-010.

       ROT-100-020.
           MOVE     ZEROS    TO  IND1.

       ROT-100-030.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
                                 CHV-TAB (IND1)
           IF  IND1  <  300
               GO  TO        ROT-100-030.
           MOVE     1        TO  IND1.

       ROT-100-040  SECTION.

       ROT-100-050.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-100-060.
           MOVE     DTA-SRT  TO  DTA-LCT
           IF  ANO-LCT   <  2000
               COMPUTE       ANO-LCT  =
                             ANO-LCT  -  1900
           ELSE
               COMPUTE       ANO-LCT  =
                             ANO-LCT  -  2000.
           MOVE     DIA-LCT  TO  DIA-DET
           MOVE     MES-LCT  TO  MES-DET
           MOVE     ANO-LCT  TO  ANO-DET
           MOVE     BCO-SRT  TO  BCO-DET
           MOVE     DOC-SRT  TO  DOC-DET
           MOVE     HST-SRT  TO  HST-DET
           MOVE     VAL-SRT  TO  VAL-DET
           MOVE     CHV-SRT  TO  CHV-TAB (IND1)
           MOVE     DET-001  TO  DET-TAB (IND1)
           ADD      1        TO  IND1
           GO  TO   ROT-100-050.

       ROT-100-060.
           EXIT.

       ROT-100-070  SECTION.

       ROT-100-100.
           COMPUTE  IND3     =   IND1  -  1
           MOVE     1        TO  IND2
                                 IND4
           MOVE     ZEROS    TO  IND1
           MOVE     SET-AUX  TO  SET-TAB (IND2).

       ROT-100-110.
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
           MOVE     IND1 TO  IND5
           DISPLAY  ENT-001  MEN-003.

       ROT-100-120.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  MEN-007
           IF  TECLADO   =   99
               GO  TO        ROT-100-130.
           IF  TECLADO   =   02
               GO  TO        ROT-100-140.
           IF  TECLADO  NOT  =   00
               MOVE      1   TO  IND1
               GO  TO        ROT-100-150.
           IF  IND2  NOT  <  IND3
               GO  TO        ROT-100-120.
           MOVE     SPACES   TO  SET-TAB (IND2)
           ADD      1        TO  IND2
           MOVE     SET-AUX  TO  SET-TAB (IND2)
           IF  IND4   <  11
               ADD    1      TO  IND4
           ELSE
               ADD    1      TO  IND1.
           GO  TO   ROT-100-110.

       ROT-100-130.
           IF  IND2   =   1
               GO  TO        ROT-100-120.
           MOVE     SPACES   TO  SET-TAB (IND2)
           COMPUTE  IND2     =   IND2  -  1
           MOVE     SET-AUX  TO  SET-TAB (IND2)
           IF  IND4   >   1
               COMPUTE IND4  =   IND4  -  1
           ELSE
               COMPUTE IND1  =   IND1  -  1.
           GO  TO   ROT-100-110.

       ROT-100-140.
           IF  FLG-TAB (IND2)  =   SPACES
               MOVE   FLG-AUX  TO  FLG-TAB (IND2)
           ELSE
               MOVE   SPACES   TO  FLG-TAB (IND2).
           GO  TO     ROT-100-110.

       ROT-100-150.
           IF  FLG-TAB (IND1)  =   FLG-AUX
               GO  TO          ROT-100-200.
           ADD  1  TO   IND1
           IF  IND1  >  IND3
               GO  TO          ROT-000-030.
           GO  TO     ROT-100-150.

       ROT-100-200.
           DISPLAY  TLA-003
                    MEN-004
           MOVE     ZEROS    TO  DTA-AUX.

       ROT-100-210.
           DISPLAY  LPA-MES
           ACCEPT   (12 40)  DIA-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               MOVE    IND5  TO  IND1
               GO  TO        ROT-100-110.
           IF  DIA-AUX   <   1  OR >  31
               GO  TO        ROT-100-210.
           DISPLAY  DIA-ENT.

       ROT-100-220.
           DISPLAY  LPA-ANO
           ACCEPT   (12 43)  MES-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-210.
           IF  MES-AUX   <   1  OR >  12
               GO  TO        ROT-100-220.
           DISPLAY  MES-ENT.

       ROT-100-230.
           DISPLAY  MEN-004
           ACCEPT   (12 46)  ANO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-220.
           MOVE     " "  TO  RSP-OPC
           DISPLAY  ANO-ENT  MEN-005
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-230.
           DISPLAY  MEN-006
           MOVE     ZEROS    TO  IND1.

       ROT-100-240.
           ADD      1    TO  IND1
           IF  IND1      >   IND3
               GO  TO        ROT-100-260.
           IF  FLG-TAB (IND1)  =  SPACES
               GO  TO        ROT-100-240.
           MOVE     CHV-TAB (IND1)   TO   CHV-SRT
           MOVE     DTA-SRT  TO   DTA-LCT
           IF  ANO-LCT   <  2000
               COMPUTE       ANO-LCT  =  ANO-LCT  -  1900
           ELSE
               COMPUTE       ANO-LCT  =  ANO-LCT  -  2000.
           MOVE     DIA-LCT  TO  DIA-AX1
           MOVE     MES-LCT  TO  MES-AX1
           MOVE     ANO-LCT  TO  ANO-AX1
           MOVE     BCO-SRT  TO  BCO-LBC
           MOVE     DTA-AX1  TO  DTA-LBC
           MOVE     LCT-SRT  TO  LCT-LBC
           READ     CADLBC
           DELETE   CADLBC
           MOVE     DTA-AUX  TO  DTA-LBC
           MOVE     ZEROS    TO  FLG-LBC  LCT-LBC.

       ROT-100-250.
           ADD      1        TO  LCT-LBC
           WRITE    REG-LBC  INVALID KEY
                    GO  TO   ROT-100-250.
           GO  TO   ROT-100-240.

       ROT-100-260.
           DISPLAY  TLA-002
           GO  TO   ROT-000-010.
