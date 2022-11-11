       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN217.
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

           SELECT      CADLCX        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCX.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADBCO      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADBCO.DAT".
       01  REG-BCO.
           03  CHV-BCO          PIC  9(03).
           03  NOM-BCO          PIC  X(15).
           03  SDI-BCO          PIC S9(11)V99.

       FD  CADLCX      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLCX.DAT".
       01  REG-LCX.
           03  CHV-LCX.
               05  TUR-LCX      PIC  9(01).
               05  CXA-LCX      PIC  9(01).
               05  ATV-LCX      PIC  9(01).
               05  PRX-LCX      PIC  9(01).
               05  SUF-LCX      PIC  9(03).
               05  LCT-LCX      PIC  9(03).
           03  DES-LCX          PIC  X(15).
           03  HST-LCX          PIC  X(20).
           03  QTD-LCX          PIC  9(06)V9.
           03  VAL-LCX          PIC  9(10)V99.

       FD  CADPRS      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADPRS.DAT".
       01  REG-PRS.
           03  CHV-PRS.
               05  TUR-PRS      PIC  9(01).
               05  CXA-PRS      PIC  9(01).
               05  ATV-PRS      PIC  9(01).
               05  LIN-PRS      PIC  9(03).
           03  DES-PRS          PIC  X(15).
           03  PRX-PRS          PIC  9(01).
           03  QTD-PRS          PIC  9(06)V9.
           03  CTD-PRS          PIC  9(10)V99.
           03  VDD-PRS          PIC S9(10)V99.
           03  QTM-PRS          PIC  9(07)V9.
           03  CTM-PRS          PIC  9(11)V99.
           03  VDM-PRS          PIC S9(11)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(03).
           03  IND2             PIC  9(03).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ATV-AUX          PIC  X(18).
           03  VAL-AUX          PIC  9(10)V99.

           03  DET-001.
               05  BCO-DT1      PIC  9(03).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  COD-DT1      PIC  9(03).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  HST-DT1      PIC  X(22).
               05  VAL-DT1      PIC  Z.ZZZ.ZZZ.ZZ9,99.

           03  REG-TAB          PIC  X(48)  OCCURS  100.

           03  LIN-AUX.
               05  LIN-001      PIC  X(48).
               05  LIN-002      PIC  X(48).
               05  LIN-003      PIC  X(48).
               05  LIN-004      PIC  X(48).
               05  LIN-005      PIC  X(48).
               05  LIN-006      PIC  X(48).
               05  LIN-007      PIC  X(48).
               05  LIN-008      PIC  X(48).
               05  LIN-009      PIC  X(48).
               05  LIN-010      PIC  X(48).

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       01  PRM-003.
           03  ATV-PR3          PIC  9(01).
           03  LIN-PR3          PIC  9(03).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 05 COLUMN 12 PIC X(56) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 14 PIC X(52) FROM
               " BANCO COD  HISTORICO                VALOR LANCADO"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 06 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 12 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 12 PIC X(56) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 14 PIC X(56) FROM SPACES.

       01  TLA-002.
           03  LINE 07 COLUMN 18 PIC X(45) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"     BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 18 PIC X(45) FROM " ³
               "                    ³"     BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 18 PIC X(45) FROM " ³  Banco ......:
               "-                   ³"     BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 18 PIC X(45) FROM " ³
               "                    ³"     BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 18 PIC X(45) FROM " ³  Codigo .....:
               "                    ³"     BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 18 PIC X(45) FROM " ³
               "                    ³"     BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 18 PIC X(45) FROM " ³  Historico ..:
               "                    ³"     BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 18 PIC X(45) FROM " ³
               "                    ³"     BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 18 PIC X(45) FROM " ³  Valor ......:
               "                    ³"     BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 18 PIC X(45) FROM " ³
               "                    ³"     BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 18 PIC X(45) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"     BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 20 PIC X(45) FROM SPACES.

       01  ENT-001.
           03  LINE 08 COLUMN 16 PIC X(48) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 16 PIC X(48) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 16 PIC X(48) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 16 PIC X(48) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 16 PIC X(48) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 16 PIC X(48) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 16 PIC X(48) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 16 PIC X(48) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 16 PIC X(48) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 16 PIC X(48) USING LIN-010
               BACKGROUND-COLOR  01.

       01  ENT-002.
           03  BCO-ENT  LINE 09 COLUMN 36 PIC 9(03) USING CHV-BCO
                        BACKGROUND-COLOR  04.
           03  NOM-ENT  LINE 09 COLUMN 44 PIC X(15) USING NOM-BCO
                        BACKGROUND-COLOR  04.
           03  COD-ENT  LINE 11 COLUMN 36 PIC 9(03) USING LCT-LCX
                        BACKGROUND-COLOR  04.
           03  HST-ENT  LINE 13 COLUMN 36 PIC X(20) USING HST-LCX
                        BACKGROUND-COLOR  04.
           03  VAL-ENT  LINE 15 COLUMN 36 PIC Z.ZZZ.ZZZ.ZZ9,99 USING
               VAL-LCX  BACKGROUND-COLOR  04.

       01  LPA-001.
           03  LPA-NOM  LINE 09 COLUMN 44 PIC X(15) FROM SPACES
                        BACKGROUND-COLOR  04.
           03  LPA-COD  LINE 11 COLUMN 36 PIC X(03) FROM SPACES
                        BACKGROUND-COLOR  04.
           03  LPA-HST  LINE 13 COLUMN 36 PIC X(20) FROM SPACES
                        BACKGROUND-COLOR  04.
           03  LPA-VAL  LINE 15 COLUMN 36 PIC X(16) FROM SPACES
                        BACKGROUND-COLOR  04.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO LANCAMENTOS ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA, [F1] P/ ALTERAR - [ESC] RETORNA ..."
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "BANCO  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "LANCAMENTO  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O HISTORICO
                   "DO MOVIMENTO  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR DEPO
                   "SITADO  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-002  PRM-003.

       ROT-000-000.
           DISPLAY  TLA-001  MEN-001
           OPEN     INPUT    CADBCO
                    I-O      CADLCX
                             CADPRS
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     ATV-PR3  TO  ATV-PRS
           MOVE     LIN-PR3  TO  LIN-PRS
           READ     CADPRS.

       ROT-100-000.
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     ZEROS    TO  SUF-LCX
                                 LCT-LCX  IND1
           START    CADLCX   KEY GREATER  CHV-LCX
                    INVALID  KEY GO  TO   ROT-100-020.

       ROT-100-010.
           READ     CADLCX   NEXT AT END
                    GO  TO   ROT-100-020.
           IF  TUR-LCX  NOT  =   TUR-PRS  OR
               CXA-LCX  NOT  =   CXA-PRS  OR
               ATV-LCX  NOT  =   ATV-PRS
               GO  TO        ROT-100-020.
           ADD      1        TO  IND1
           MOVE     SUF-LCX  TO  BCO-DT1
           MOVE     LCT-LCX  TO  COD-DT1
           MOVE     HST-LCX  TO  HST-DT1
           MOVE     VAL-LCX  TO  VAL-DT1
           MOVE     DET-001  TO  REG-TAB (IND1)
           GO  TO   ROT-100-010.

       ROT-100-020.
           MOVE     IND1     TO  IND2
           ADD      1        TO  IND1.

       ROT-100-030.
           IF  IND1  >  100
               MOVE  0   TO  IND1
               GO  TO        ROT-100-040.
           MOVE     SPACES   TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           GO  TO   ROT-100-030.

       ROT-100-040.
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
           ADD      10   TO  IND1
           DISPLAY  ENT-001.

       ROT-100-050.
           DISPLAY  MEN-002
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-009
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-100-060.
           IF  TECLADO   =   02
               GO  TO        ROT-200-000.
           IF  TECLADO   =   01
               GO  TO        ROT-300-000.
           IF  TECLADO  NOT  =  00
               GO  TO        ROT-100-050.
           IF  IND1     NOT  <   IND2
               GO  TO        ROT-100-050.
           GO  TO   ROT-100-040.

       ROT-100-060.
           IF  IND1  <  11
               GO  TO        ROT-100-050.
           COMPUTE  IND1  =  IND1  -  20
           GO  TO   ROT-100-040.

       ROT-200-000.
           DISPLAY  TLA-002.

       ROT-200-010.
           DISPLAY  LPA-COD
                    LPA-NOM  MEN-003
           MOVE     0   TO   CHV-BCO
           ACCEPT   (09 36)  CHV-BCO  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       TLA-001
               GO  TO        ROT-100-000.
           IF  CHV-BCO   =   ZEROS
               GO  TO        ROT-200-010.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADBCO   INVALID KEY
                    DISPLAY  MEN-004
                    ACCEPT   OPC-001
                    GO  TO   ROT-200-010.
           DISPLAY  BCO-ENT      NOM-ENT.

       ROT-200-020.
           DISPLAY  LPA-VAL
                    LPA-HST  MEN-005
           MOVE     0   TO   LCT-LCX
           ACCEPT   (11 36)  LCT-LCX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-010.
           IF  LCT-LCX   =   ZEROS
               GO  TO        ROT-200-020.
           DISPLAY  COD-ENT
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     CHV-BCO  TO  SUF-LCX
           READ     CADLCX   INVALID KEY
                    GO  TO   ROT-200-060.
           MOVE     VAL-LCX  TO  VAL-AUX.

       ROT-200-030.
           DISPLAY  VAL-ENT  MEN-006
           ACCEPT   (13 36)  HST-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-020.
           IF  HST-LCX   =   SPACES
               GO  TO        ROT-200-030.
           DISPLAY  HST-ENT.

       ROT-200-040.
           DISPLAY  LPA-VAL  MEN-007
           ACCEPT   (15 36)  VAL-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-030.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  VAL-ENT      MEN-008
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-040.
           COMPUTE  VDD-PRS  =   VDD-PRS
                 -  VAL-AUX  +   VAL-LCX
           REWRITE  REG-PRS
           IF  VAL-LCX   =   ZEROS
               GO  TO        ROT-200-050.
           REWRITE  REG-LCX
           GO  TO   ROT-200-020.

       ROT-200-050.
           DELETE   CADLCX
           CLOSE    CADLCX
           OPEN     I-O  CADLCX
           GO  TO   ROT-200-020.

       ROT-200-060.
           MOVE     DES-PRS  TO  DES-LCX
           MOVE     SPACES   TO  HST-LCX
           MOVE     ZEROS    TO  QTD-LCX
                                 VAL-LCX.
       ROT-200-070.
           DISPLAY  MEN-006
           ACCEPT   (13 36)  HST-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-020.
           IF  HST-LCX   =   SPACES
               GO  TO        ROT-200-070.
           DISPLAY  HST-ENT.

       ROT-200-080.
           DISPLAY  LPA-VAL  MEN-007
           ACCEPT   (15 36)  VAL-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  VAL-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-070.
           IF  VAL-LCX   =   ZEROS
               GO  TO        ROT-200-080.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-008
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-080.
           ADD      VAL-LCX  TO  VDD-PRS
           REWRITE  REG-PRS
           WRITE    REG-LCX
           GO  TO   ROT-200-020.

       ROT-300-000.
           CLOSE   CADBCO  CADLCX  CADPRS
           CHAIN  "DYN215" USING  PRM-002.