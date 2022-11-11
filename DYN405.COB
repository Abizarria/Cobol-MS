       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN405.
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
           03  CHV-SRT.
               05  DTA-SRT      PIC  9(08).
               05  LCT-SRT      PIC  9(03).
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
           03  ACM-SDO          PIC S9(11)V99.

           03  DTA-EXT.
               05  ANO-EXT      PIC  9(02).
               05  MES-EXT      PIC  9(02).
               05  DIA-EXT      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  DTA-LCT.
               05  ANO-LCT      PIC  9(04).
               05  MES-LCT      PIC  9(02).
               05  DIA-LCT      PIC  9(02).

           03  REG-TAB          PIC  X(56)  OCCURS  510.

           03  LIN-001          PIC  X(56).
           03  LIN-002          PIC  X(56).
           03  LIN-003          PIC  X(56).
           03  LIN-004          PIC  X(56).
           03  LIN-005          PIC  X(56).
           03  LIN-006          PIC  X(56).
           03  LIN-007          PIC  X(56).
           03  LIN-008          PIC  X(56).
           03  LIN-009          PIC  X(56).
           03  LIN-010          PIC  X(56).
           03  LIN-011          PIC  X(56).

       01  DET-001.
           03  DOC-DET          PIC  9(06).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  HST-DET          PIC  X(23).
           03  COD-DET          PIC  9(01).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  VAL-DET          PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  SNL-DET          PIC  X(01).

       01  TOT-001.
           03  FILLER           PIC  X(17)  VALUE  "SALDO INICIAL EM".
           03  DIA-TT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-TT1          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-TT1          PIC  9(02).
           03  FILLER           PIC  X(10)  VALUE  " ...".
           03  SDO-TT1          PIC  ---.---.---.--9,99.

       01  TOT-002.
           03  FILLER           PIC  X(15)  VALUE  "SALDO FINAL EM".
           03  DIA-TT2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-TT2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-TT2          PIC  9(02).
           03  FILLER           PIC  X(12)  VALUE  " .....".
           03  SDO-TT2          PIC  ---.---.---.--9,99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "CONSULTA DE EXTRATOS  -  Ver. 7.11"
               BACKGROUND-COLOR  02 FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 27 PIC X(24) FROM SPACES
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
           03  LINE 08 COLUMN 20 PIC X(40) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 20 PIC X(40) FROM " ³
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 20 PIC X(40) FROM " ³  Banco .:      -
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 20 PIC X(40) FROM " ³
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 20 PIC X(40) FROM " ³  Data ..:   /  /
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 20 PIC X(40) FROM " ³
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 20 PIC X(40) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 22 PIC X(40) FROM SPACES.


       01  TLA-003.
           03  LINE 05 COLUMN 08 PIC X(64) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 08 PIC X(64) FROM " ³  NUMERO   HISTORICO
               "            C/L      VALOR LANCADO -/+ ³"
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 08 PIC X(64) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 10 PIC X(64) FROM SPACES.

       01  ENT-001.
           03  BCO-ENT LINE 10 COLUMN 33 PIC 9(03) USING CHV-BCO
                       BACKGROUND-COLOR  01.
           03  NOM-ENT LINE 10 COLUMN 41 PIC X(15) USING NOM-BCO
                       BACKGROUND-COLOR  01.
           03  DIA-ENT LINE 12 COLUMN 33 PIC 9(02) USING DIA-EXT
                       BACKGROUND-COLOR  01.
           03  MES-ENT LINE 12 COLUMN 36 PIC 9(02) USING MES-EXT
                       BACKGROUND-COLOR  01.
           03  ANO-ENT LINE 12 COLUMN 39 PIC 9(02) USING ANO-EXT
                       BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-NOM LINE 10 COLUMN 41 PIC X(15) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-DIA LINE 12 COLUMN 33 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-MES LINE 12 COLUMN 36 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-ANO LINE 12 COLUMN 39 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  LPA-002.
           03  LINE 08 COLUMN 12 PIC X(56) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 12 PIC X(56) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 12 PIC X(56) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 12 PIC X(56) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 12 PIC X(56) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 12 PIC X(56) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 12 PIC X(56) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 12 PIC X(56) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 12 PIC X(56) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 12 PIC X(56) USING LIN-010
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 12 PIC X(56) USING LIN-011
               BACKGROUND-COLOR  01.

       01  LPA-003.
           03  LINE 05 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 08 PIC X(66) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "BANCO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA DA CO
                   "NSULTA  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "BANCO INEXISTENTE !
                   "!!  -  [ESC] RETORNA ...                        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE MOVIMENT
                   "O NESTA DATA !!!  -  [ESC] RETORNA ...          "
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
                   "E P/ CONSULTA EM TELA  -  TECLE [ESC] !!!       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 80 PIC X(01) FROM " "
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
           ACCEPT   (10 33)  CHV-BCO  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-000.
           IF  CHV-BCO   =   ZEROS
               GO  TO        ROT-100-000.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADBCO   INVALID KEY
                    DISPLAY  MEN-003
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-000.
           MOVE     ZEROS    TO  DTA-EXT
           DISPLAY  BCO-ENT  NOM-ENT.

       ROT-100-010.
           DISPLAY  LPA-MES  MEN-002
           ACCEPT   (12 33)  DIA-EXT  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           IF  DIA-EXT   <   1  OR  > 31
               GO  TO        ROT-100-010.
           DISPLAY  DIA-ENT.

       ROT-100-020.
           DISPLAY  LPA-ANO
           ACCEPT   (12 36)  MES-EXT  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           IF  MES-EXT   <   1  OR  > 12
               GO  TO        ROT-100-020.
           DISPLAY  MES-ENT.

       ROT-100-030.
           DISPLAY  LPA-ANO  MEN-002
           ACCEPT   (12 39)  ANO-EXT  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           DISPLAY  ANO-ENT  MEN-007
           MOVE     SPACES   TO  CHV-LBC  RSP-OPC
           MOVE     CHV-BCO  TO  BCO-LBC
           MOVE     DTA-EXT  TO  DTA-LBC
           START    CADLBC   KEY GREATER  CHV-LBC
                    INVALID  KEY GO  TO   ROT-100-050.

       ROT-100-040.
           READ     CADLBC   NEXT AT END
                    GO  TO   ROT-100-050.
           IF  BCO-LBC  NOT  =   CHV-BCO
               OR   DTA-LBC  >   DTA-EXT
               GO  TO        ROT-100-050.
           IF  FLG-LBC   =   0
               GO  TO        ROT-100-060.
           GO  TO   ROT-100-040.

       ROT-100-050.
           DISPLAY  MEN-004
           ACCEPT   OPC-001
           GO  TO   ROT-100-000.

       ROT-100-060.
           DISPLAY  MEN-008
           ACCEPT   OPC-001
           ACCEPT   TECLADO FROM  ESCAPE   KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           DISPLAY  MEN-005
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           START    CADLBC   KEY GREATER        CHV-LBC
           SORT     CADSRT   ON  ASCENDING KEY  CHV-SRT
                    INPUT        PROCEDURE IS   ROT-100-100
                    OUTPUT       PROCEDURE IS   ROT-100-130
           GO  TO   ROT-100-000.

       ROT-100-100  SECTION.

       ROT-100-110.
           READ     CADLBC  NEXT AT  END
                    GO  TO   ROT-100-120.
           IF  BCO-LBC  NOT  =   CHV-BCO
               GO  TO        ROT-100-120.
           IF  FLG-LBC   =   1
               GO  TO        ROT-100-110.
           MOVE     DTA-LBC  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-LCT
           MOVE     MES-AUX  TO  MES-LCT
           MOVE     ANO-AUX  TO  ANO-LCT
           IF  ANO-AUX   <   10
               ADD     2000  TO  ANO-LCT
           ELSE
               ADD     1900  TO  ANO-LCT.
           MOVE     DTA-LCT  TO  DTA-SRT
           MOVE     LCT-LBC  TO  LCT-SRT
           MOVE     DOC-LBC  TO  DOC-SRT
           MOVE     HST-LBC  TO  HST-SRT
           MOVE     COD-LBC  TO  COD-SRT
           MOVE     VAL-LBC  TO  VAL-SRT
           RELEASE  REG-SRT
           GO  TO   ROT-100-110.

       ROT-100-120.
           MOVE     DIA-EXT  TO  DIA-LCT
           MOVE     MES-EXT  TO  MES-LCT
           MOVE     ANO-EXT  TO  ANO-LCT
           IF  ANO-EXT   <   10
               ADD     2000  TO  ANO-LCT
           ELSE
               ADD     1900  TO  ANO-LCT.
           MOVE     SDI-BCO  TO  ACM-SDO.

       ROT-100-130  SECTION.

       ROT-100-140.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-100-150.
           IF  DTA-SRT  NOT  <   DTA-LCT
               GO  TO        ROT-100-150.
           IF  COD-SRT   =   1 OR 3 OR 6
               COMPUTE       ACM-SDO   =
                             ACM-SDO   +  VAL-SRT
           ELSE
               COMPUTE       ACM-SDO   =
                             ACM-SDO   -  VAL-SRT.
           GO  TO   ROT-100-140.

       ROT-100-150.
           MOVE     DIA-EXT  TO  DIA-TT1
           MOVE     MES-EXT  TO  MES-TT1
           MOVE     ANO-EXT  TO  ANO-TT1
           MOVE     ACM-SDO  TO  SDO-TT1
           MOVE     TOT-001  TO  REG-TAB (1)
           MOVE     SPACES   TO  REG-TAB (2)
           MOVE     3        TO  IND1.

       ROT-100-160.
           MOVE     DOC-SRT  TO  DOC-DET
           MOVE     HST-SRT  TO  HST-DET
           MOVE     COD-SRT  TO  COD-DET
           MOVE     VAL-SRT  TO  VAL-DET
           IF  COD-SRT   =   1 OR 3 OR 6
               MOVE     "+"  TO  SNL-DET
               COMPUTE           ACM-SDO  =
                                 ACM-SDO  +  VAL-SRT
           ELSE
               MOVE     "-"  TO  SNL-DET
               COMPUTE           ACM-SDO  =
                                 ACM-SDO  -  VAL-SRT.
           MOVE     DET-001  TO  REG-TAB  (IND1)
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-100-170.
           IF  DTA-SRT   >   DTA-LCT
               GO  TO        ROT-100-170.
           ADD      1        TO  IND1
           IF  IND1  <  500
               GO  TO        ROT-100-160.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-009
           ACCEPT   OPC-001  GO  ROT-200-030.

       ROT-100-170.
           MOVE     DIA-EXT  TO  DIA-TT2
           MOVE     MES-EXT  TO  MES-TT2
           MOVE     ANO-EXT  TO  ANO-TT2
           MOVE     ACM-SDO  TO  SDO-TT2
           MOVE     SPACES   TO  REG-TAB (IND1 + 1)
           MOVE     TOT-002  TO  REG-TAB (IND1 + 2)
           ADD      2        TO  IND1
           MOVE     IND1     TO  IND2.

       ROT-100-180.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  REG-TAB (IND1)
           IF  IND1 <  510
               GO  TO        ROT-100-180.
           DISPLAY  TLA-003
           MOVE     ZEROS    TO  IND1.

       ROT-200-000.
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

       ROT-200-010.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-010
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-200-020.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-200-030.
           IF  IND1  NOT  <  IND2
               GO  TO        ROT-200-010.
           GO  TO   ROT-200-000.

       ROT-200-020.
           IF  IND1   <  12
               GO  TO        ROT-200-010.
           COMPUTE  IND1  =  IND1  -  22
           GO  TO   ROT-200-000.

       ROT-200-030.
           DISPLAY  LPA-003  TLA-002.

       ROT-200-040  SECTION.

       ROT-300-000.
           MOVE     4   TO  PRM-001
           CLOSE    CADBCO  CADLBC
           CHAIN   "DYN400"  USING  PRM-001.
