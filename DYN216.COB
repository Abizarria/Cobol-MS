       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN216.
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

       FD  CADDSP      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADDSP.DAT".
       01  REG-DSP.
           03  CHV-DSP.
               05  PRX-DSP      PIC  9(01).
               05  SUF-DSP      PIC  9(03).
           03  DES-DSP          PIC  X(25).

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

           03  TAB-ATV.
               05  FILLER       PIC  X(18)  VALUE  "OBRIG. TRABALHISTA".
               05  FILLER       PIC  X(18)  VALUE  "OBRIGACOES FISCAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESP. OPERACIONAIS".
               05  FILLER       PIC  X(18)  VALUE  "DESPESA DE CAPITAL".
               05  FILLER       PIC  X(18)  VALUE  "CREDITOS DE SOCIOS".
               05  FILLER       PIC  X(18)  VALUE  "COMPRA DE PRODUTOS".
           03  RED-ATV          REDEFINES   TAB-ATV.
               05  ATV-TAB      PIC  X(18)  OCCURS  06.

           03  DET-001.
               05  PRX-DT1      PIC  9(01).
               05  FILLER       PIC  X(01)  VALUE  "-".
               05  SUF-DT1      PIC  9(03).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  COD-DT1      PIC  9(03).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  HST-DT1      PIC  X(22).
               05  VAL-DT1      PIC  Z.ZZZ.ZZZ.ZZ9,99.

           03  REG-TAB          PIC  X(50)  OCCURS  100.

           03  LIN-AUX.
               05  LIN-001      PIC  X(50).
               05  LIN-002      PIC  X(50).
               05  LIN-003      PIC  X(50).
               05  LIN-004      PIC  X(50).
               05  LIN-005      PIC  X(50).
               05  LIN-006      PIC  X(50).
               05  LIN-007      PIC  X(50).
               05  LIN-008      PIC  X(50).
               05  LIN-009      PIC  X(50).
               05  LIN-010      PIC  X(50).

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       01  PRM-003.
           03  ATV-PR3          PIC  9(01).
           03  LIN-PR3          PIC  9(03).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 05 COLUMN 11 PIC X(58) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 13 PIC X(54) FROM
               "  CONTA  COD  HISTORICO                VALOR LANCADO"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 06 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 11 PIC X(58) FROM " ³
              "                                 ³" BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 11 PIC X(58) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 13 PIC X(58) FROM SPACES.

       01  TLA-002.
           03  LINE 07 COLUMN 15 PIC X(50) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"     BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 15 PIC X(50) FROM " ³
               "                         ³"     BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 15 PIC X(50) FROM " ³  Conta ......:  -
               "  -                      ³"     BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 15 PIC X(50) FROM " ³
               "                         ³"     BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 15 PIC X(50) FROM " ³  Descricao ..:
               "                         ³"     BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 15 PIC X(50) FROM " ³
               "                         ³"     BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 15 PIC X(50) FROM " ³  Codigo .....:
               "                         ³"     BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 15 PIC X(50) FROM " ³  Historico ..:
               "                         ³"     BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 15 PIC X(50) FROM " ³  Valor Pago .:
               "                         ³"     BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 15 PIC X(50) FROM " ³
               "                         ³"     BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 15 PIC X(50) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"     BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 17 PIC X(50) FROM SPACES.

       01  ENT-001.
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

       01  ENT-002.
           03  PRX-ENT  LINE 09 COLUMN 33 PIC 9(01) USING PRX-DSP
                        BACKGROUND-COLOR  04.
           03  SUF-ENT  LINE 09 COLUMN 35 PIC 9(03) USING SUF-DSP
                        BACKGROUND-COLOR  04.
           03  ATV-ENT  LINE 09 COLUMN 43 PIC X(18) USING ATV-AUX
                        BACKGROUND-COLOR  04.
           03  DES-ENT  LINE 11 COLUMN 33 PIC X(25) USING DES-DSP
                        BACKGROUND-COLOR  04.
           03  COD-ENT  LINE 13 COLUMN 33 PIC 9(03) USING LCT-LCX
                        BACKGROUND-COLOR  04.
           03  HST-ENT  LINE 14 COLUMN 33 PIC X(20) USING HST-LCX
                        BACKGROUND-COLOR  04.
           03  VAL-ENT  LINE 15 COLUMN 33 PIC Z.ZZZ.ZZZ.ZZ9,99 USING
               VAL-LCX  BACKGROUND-COLOR  04.

       01  LPA-001.
           03  LPA-SUF  LINE 09 COLUMN 35 PIC X(03) FROM SPACES
                        BACKGROUND-COLOR  04.
           03  LPA-ATV  LINE 09 COLUMN 43 PIC X(18) FROM SPACES
                        BACKGROUND-COLOR  04.
           03  LPA-DES  LINE 11 COLUMN 33 PIC X(25) FROM SPACES
                        BACKGROUND-COLOR  04.
           03  LPA-COD  LINE 13 COLUMN 33 PIC X(03) FROM SPACES
                        BACKGROUND-COLOR  04.
           03  LPA-HST  LINE 14 COLUMN 33 PIC X(20) FROM SPACES
                        BACKGROUND-COLOR  04.
           03  LPA-VAL  LINE 15 COLUMN 33 PIC X(16) FROM SPACES
                        BACKGROUND-COLOR  04.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO LANCAMENTOS ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA, [F1] P/ ALTERAR - [ESC] RETORNA ..."
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O PREFIXO DA
                   " CONTA  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DA
                   "CONTA  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "LANCAMENTO  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O HISTORICO
                   "DO MOVIMENTO  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR PAGO
                   "  -  [ESC] RETORNA ...                        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-002  PRM-003.

       ROT-000-000.
           DISPLAY  TLA-001  MEN-001
           OPEN     INPUT    CADDSP
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
           MOVE     ZEROS    TO  PRX-LCX  IND1
                                 SUF-LCX  LCT-LCX
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
           MOVE     PRX-LCX  TO  PRX-DT1
           MOVE     SUF-LCX  TO  SUF-DT1
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
           DISPLAY  MEN-010
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
           DISPLAY  LPA-SUF
                    LPA-ATV  MEN-003
           MOVE     0   TO   PRX-DSP
           ACCEPT   (09 33)  PRX-DSP  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       TLA-001
               GO  TO        ROT-100-000.
           IF  PRX-DSP   <   1  OR  >  6
               GO  TO        ROT-200-010.
           MOVE     ATV-TAB (PRX-DSP) TO  ATV-AUX
           DISPLAY  PRX-ENT  ATV-ENT.

       ROT-200-020.
           DISPLAY  LPA-COD
                    LPA-DES  MEN-004
           MOVE     0   TO   SUF-DSP
           ACCEPT   (09 35)  SUF-DSP  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-010.
           IF  SUF-DSP   =   ZEROS
               GO  TO        ROT-200-020.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADDSP   INVALID KEY
                    DISPLAY  MEN-005
                    ACCEPT   OPC-001
                    GO  TO   ROT-200-020.
           DISPLAY  SUF-ENT      DES-ENT.

       ROT-200-030.
           DISPLAY  LPA-VAL
                    LPA-HST  MEN-006
           MOVE     0   TO   LCT-LCX
           ACCEPT   (13 33)  LCT-LCX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-020.
           IF  LCT-LCX   =   ZEROS
               GO  TO        ROT-200-030.
           DISPLAY  COD-ENT
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-DSP  TO  PRX-LCX
           MOVE     SUF-DSP  TO  SUF-LCX
           READ     CADLCX   INVALID KEY
                    GO  TO   ROT-200-070.
           MOVE     VAL-LCX  TO  VAL-AUX.

       ROT-200-040.
           DISPLAY  VAL-ENT  MEN-007
           ACCEPT   (14 33)  HST-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-030.
           IF  HST-LCX   =   SPACES
               GO  TO        ROT-200-040.
           DISPLAY  HST-ENT.

       ROT-200-050.
           DISPLAY  LPA-VAL  MEN-008
           ACCEPT   (15 33)  VAL-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-040.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  VAL-ENT      MEN-009
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-050.
           COMPUTE  VDD-PRS  =   VDD-PRS
                 -  VAL-AUX  +   VAL-LCX
           REWRITE  REG-PRS
           IF  VAL-LCX   =   ZEROS
               GO  TO        ROT-200-060.
           REWRITE  REG-LCX
           GO  TO   ROT-200-030.

       ROT-200-060.
           DELETE   CADLCX
           CLOSE    CADLCX
           OPEN     I-O  CADLCX
           GO  TO   ROT-200-030.

       ROT-200-070.
           MOVE     DES-PRS  TO  DES-LCX
           MOVE     SPACES   TO  HST-LCX
           MOVE     ZEROS    TO  QTD-LCX
                                 VAL-LCX.
       ROT-200-080.
           DISPLAY  MEN-007
           ACCEPT   (14 33)  HST-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-030.
           IF  HST-LCX   =   SPACES
               GO  TO        ROT-200-080.
           DISPLAY  HST-ENT.

       ROT-200-090.
           DISPLAY  LPA-VAL  MEN-008
           ACCEPT   (15 33)  VAL-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  VAL-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-080.
           IF  VAL-LCX   =   ZEROS
               GO  TO        ROT-200-090.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-009
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-090.
           ADD      VAL-LCX  TO  VDD-PRS
           REWRITE  REG-PRS
           WRITE    REG-LCX
           GO  TO   ROT-200-030.

       ROT-300-000.
           CLOSE   CADDSP  CADLCX  CADPRS
           CHAIN  "DYN215" USING  PRM-002.