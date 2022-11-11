       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN406.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADBCO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-BCO.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC.

           SELECT      CADSRT        ASSIGN  TO  DISK.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADBCO      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADBCO.DAT".
       01  REG-BCO.
           03  CHV-BCO          PIC  9(03).
           03  NOM-BCO          PIC  X(15).
           03  SDI-BCO          PIC S9(11)V99.

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
           03  DTA-SRT          PIC  9(08).
           03  DOC-SRT          PIC  9(06).
           03  HST-SRT          PIC  X(20).
           03  COD-SRT          PIC  9(01).
           03  VAL-SRT          PIC  9(10)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ACM-VAL          PIC  9(12)V99.

           03  REG-TAB          PIC  X(60)  OCCURS  510.

           03  LIN-001          PIC  X(60).
           03  LIN-002          PIC  X(60).
           03  LIN-003          PIC  X(60).
           03  LIN-004          PIC  X(60).
           03  LIN-005          PIC  X(60).
           03  LIN-006          PIC  X(60).
           03  LIN-007          PIC  X(60).
           03  LIN-008          PIC  X(60).
           03  LIN-009          PIC  X(60).
           03  LIN-010          PIC  X(60).
           03  LIN-011          PIC  X(60).

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
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  DOC-DET          PIC  9(06).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  HST-DET          PIC  X(22).
           03  COD-DET          PIC  9(01).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  VAL-DET          PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       01  TOT-001.
           03  FILLER           PIC  X(42)  VALUE
               "                  TOTAL DE DEBITOS ...".
           03  VAL-TOT          PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "CONSULTA DOS DEBITOS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 27 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 27 PIC X(29) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 27 PIC X(35) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 29 PIC X(33) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 34 PIC X(28) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 36 PIC X(26) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 42 PIC X(20) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 09 COLUMN 20 PIC X(40) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 20 PIC X(40) FROM " ³
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 20 PIC X(40) FROM " ³  Banco .:      -
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 20 PIC X(40) FROM " ³
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 20 PIC X(40) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 22 PIC X(40) FROM SPACES.


       01  TLA-003.
           03  LINE 05 COLUMN 07 PIC X(66) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 07 PIC X(66) FROM " ³   DATA    NUMERO  HI
               "STORICO            C/L   VALOR DO DEBITO ³"
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
           03  BCO-ENT LINE 11 COLUMN 33 PIC 9(03) USING CHV-BCO
                       BACKGROUND-COLOR  01.
           03  NOM-ENT LINE 11 COLUMN 41 PIC X(15) USING NOM-BCO
                       BACKGROUND-COLOR  01.

       01  LPA-001  LINE 11 COLUMN 41 PIC X(15) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  LPA-002.
           03  LINE 08 COLUMN 10 PIC X(60) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 10 PIC X(60) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 10 PIC X(60) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 10 PIC X(60) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 10 PIC X(60) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 10 PIC X(60) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 10 PIC X(60) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 10 PIC X(60) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 10 PIC X(60) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 10 PIC X(60) USING LIN-010
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 10 PIC X(60) USING LIN-011
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

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "BANCO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "BANCO INEXISTENTE !
                   "!!  -  [ESC] RETORNA ...                        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE MOVIMENT
                   "O NO PERIODO !!!  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  MONTAN
                   "DO TELA DE CONSULTA ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTO ...                             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "MEMORIA INSUFICIENT
                   "P/ CONSULTA EM TELA  -  TECLE [ESC] !!!         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     INPUT    CADBCO
                             CADLBC.
       ROT-100-000.
           DISPLAY  LPA-001  MEN-001
           MOVE     0   TO   CHV-BCO
           ACCEPT   (11 33)  CHV-BCO  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           IF  CHV-BCO   =   ZEROS
               GO  TO        ROT-100-000.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADBCO   INVALID KEY
                    DISPLAY  MEN-002
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-000.
           DISPLAY  BCO-ENT      NOM-ENT  MEN-006
           MOVE     SPACES   TO  CHV-LBC  RSP-OPC
           MOVE     CHV-BCO  TO  BCO-LBC
           START    CADLBC   KEY GREATER  CHV-LBC
                    INVALID  KEY GO  TO   ROT-100-050.

       ROT-100-040.
           READ     CADLBC   NEXT AT END
                    GO  TO   ROT-100-050.
           IF  BCO-LBC  NOT  =   CHV-BCO
               GO  TO        ROT-100-050.
           IF  FLG-LBC   =   0   AND
              (COD-LBC   =   2 OR 4 OR 5)
               GO  TO        ROT-100-060.
           GO  TO   ROT-100-040.

       ROT-100-050.
           DISPLAY  MEN-003
           ACCEPT   OPC-001
           GO  TO   ROT-100-000.

       ROT-100-060.
           DISPLAY  MEN-007
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           DISPLAY  MEN-004
           SORT     CADSRT   ON  ASCENDING  KEY  DTA-SRT
                    INPUT        PROCEDURE  IS   ROT-200-000
                    OUTPUT       PROCEDURE  IS   ROT-200-040
           GO  TO   ROT-100-000.

       ROT-200-000  SECTION.

       ROT-200-010.
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
           MOVE     COD-LBC  TO  COD-SRT
           MOVE     VAL-LBC  TO  VAL-SRT
           RELEASE  REG-SRT.

       ROT-200-020.
           READ     CADLBC   NEXT AT END
                    GO  TO   ROT-200-030.
           IF  BCO-LBC  NOT  =   CHV-BCO
               GO  TO        ROT-200-030.
           IF  FLG-LBC   =   0   AND
              (COD-LBC   =   2 OR 4 OR 5)
               GO  TO        ROT-200-010.
           GO  TO   ROT-200-020.

       ROT-200-030.
           MOVE     1        TO  IND1
           MOVE     ZEROS    TO  ACM-VAL.

       ROT-200-040  SECTION.

       ROT-200-050.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-200-080.

       ROT-200-060.
           MOVE     DTA-SRT  TO  DTA-LCT
           IF  ANO-LCT   >   1999
               COMPUTE       ANO-LCT  =   ANO-LCT  -  2000
           ELSE
               COMPUTE       ANO-LCT  =   ANO-LCT  -  1900.
           MOVE     DIA-LCT  TO  DIA-DET
           MOVE     MES-LCT  TO  MES-DET
           MOVE     ANO-LCT  TO  ANO-DET
           MOVE     DOC-SRT  TO  DOC-DET
           MOVE     HST-SRT  TO  HST-DET
           MOVE     COD-SRT  TO  COD-DET
           MOVE     VAL-SRT  TO  VAL-DET
           ADD      VAL-SRT  TO  ACM-VAL
           MOVE     DET-001  TO  REG-TAB (IND1).

       ROT-200-070.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-200-080.
           ADD      1   TO   IND1
           IF  IND1  <  500
               GO  TO        ROT-200-060.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-008
           ACCEPT   OPC-001
           GO  TO   ROT-100-000.

       ROT-200-080.
           MOVE     ACM-VAL  TO  VAL-TOT
           MOVE     SPACES   TO  REG-TAB (IND1 + 1)
           MOVE     TOT-001  TO  REG-TAB (IND1 + 2)
           ADD      2        TO  IND1
           MOVE     IND1     TO  IND2.

       ROT-200-090.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1 <  510
               GO  TO        ROT-200-090.
           DISPLAY  TLA-003
           MOVE     ZEROS    TO  IND1.

       ROT-300-000.
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
           DISPLAY  LPA-002  MEN-005.

       ROT-300-010.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-009
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-300-020.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-300-030.
           IF  IND1  NOT  <  IND2
               GO  TO        ROT-300-010.
           GO  TO   ROT-300-000.

       ROT-300-020.
           IF  IND1   <  12
               GO  TO        ROT-300-010.
           COMPUTE  IND1  =  IND1  -  22
           GO  TO   ROT-300-000.

       ROT-300-030.
           DISPLAY  LPA-003  TLA-002.

       ROT-300-040  SECTION.

       ROT-400-000.
           MOVE     4   TO  PRM-001
           CLOSE    CADBCO  CADLBC
           CHAIN   "DYN400"  USING  PRM-001.
