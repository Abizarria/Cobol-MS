       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN212.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

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
           03  NUM-OPC          PIC  9(01).
           03  FLG-AUX          PIC  9(01).
           03  TECLADO          PIC  9(02).
           03  ACM-QTD          PIC  9(06)V9.
           03  VAL-AUX          PIC  9(07)V99.
           03  ACM-VAL          PIC  9(11)V99.

           03  DET-001.
               05  NUM-DT1      PIC  9(02).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  ATV-DT1      PIC  X(17).
               05  QTD-DT1      PIC  ZZZ.ZZ9,9.
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  VAL-DT1      PIC  Z.ZZZ.ZZZ.ZZ9,99.

           03  DET-002.
               05  SUF-DT2      PIC  9(03).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  HST-DT2      PIC  X(22).
               05  QTD-DT2      PIC  ZZZ.ZZ9,9.
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  VAL-DT2      PIC  Z.ZZZ.ZZZ.ZZ9,99.

           03  TAB-REG          OCCURS  09.
               05  CHV-TAB      PIC  9(07).
               05  REG-TAB      PIC  X(48).

           03  LCT-TAB          PIC  X(54)  OCCURS  100.

           03  ENT-AUX.
               05  LIN-001      PIC  X(48).
               05  LIN-002      PIC  X(48).
               05  LIN-003      PIC  X(48).
               05  LIN-004      PIC  X(48).
               05  LIN-005      PIC  X(48).
               05  LIN-006      PIC  X(48).
               05  LIN-007      PIC  X(48).
               05  LIN-008      PIC  X(48).
               05  LIN-009      PIC  X(48).

           03  LCT-AUX.
               05  LCT-001      PIC  X(54).
               05  LCT-002      PIC  X(54).
               05  LCT-003      PIC  X(54).
               05  LCT-004      PIC  X(54).
               05  LCT-005      PIC  X(54).
               05  LCT-006      PIC  X(54).
               05  LCT-007      PIC  X(54).
               05  LCT-008      PIC  X(54).
               05  LCT-009      PIC  X(54).
               05  LCT-010      PIC  X(54).

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 05 COLUMN 13 PIC X(56) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 15 PIC X(52) FROM
               "  N§  SERVICO         QUANTIDADE     VALOR LANCADO"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 06 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 13 PIC X(56) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 13 PIC X(56) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 15 PIC X(56) FROM SPACES.

       01  TLA-002.
           03  LINE 05 COLUMN 09 PIC X(62) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
            "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 11 PIC X(58) FROM
            "  COD  HISTORICO             QUANTIDADE    VALOR LANCADO"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 06 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 07 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 08 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 09 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 15 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 16 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 17 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 09 PIC X(62) FROM " ³
            "                                     ³" BACKGROUND-COLOR 1.
           03  LINE 18 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 09 PIC X(62) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
            "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 1.
           03  LINE 19 COLUMN 71 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 11 PIC X(62) FROM SPACES.

       01  TLA-003.
           03  LINE 07 COLUMN 14 PIC X(52) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 14 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 14 PIC X(52) FROM " ³  Codigo ..........:
               " -     -                   ³"   BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 14 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 14 PIC X(52) FROM " ³  Historico .......:
               "                           ³"   BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 14 PIC X(52) FROM " ³  Quantidade ......:
               "                           ³"   BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 14 PIC X(52) FROM " ³  Preco Unitario ..:
               "                           ³"   BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 14 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 14 PIC X(52) FROM " ³  Valor Total .....:
               "                           ³"   BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 14 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 14 PIC X(52) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 16 PIC X(52) FROM SPACES.

       01  ENT-001.
           03  LINE 08 COLUMN 17 PIC X(48) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 17 PIC X(48) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 17 PIC X(48) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 17 PIC X(48) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 17 PIC X(48) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 17 PIC X(48) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 17 PIC X(48) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 17 PIC X(48) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 17 PIC X(48) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 17 PIC X(20) FROM "Sub-total .........:"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 18 COLUMN 38 PIC ZZZ.ZZ9,9  USING  ACM-QTD
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 18 COLUMN 48 PIC ZZ.ZZZ.ZZZ.ZZ9,99 USING ACM-VAL
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.

       01  ENT-002.
           03  LINE 08 COLUMN 13 PIC X(54) USING LCT-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 13 PIC X(54) USING LCT-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 13 PIC X(54) USING LCT-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 13 PIC X(54) USING LCT-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 13 PIC X(54) USING LCT-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 13 PIC X(54) USING LCT-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 13 PIC X(54) USING LCT-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 13 PIC X(54) USING LCT-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 13 PIC X(54) USING LCT-009
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 13 PIC X(54) USING LCT-010
               BACKGROUND-COLOR  01.

       01  ENT-003.
           03  LINE 09 COLUMN 37 PIC 9(01) USING PRX-PRS
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 47 PIC X(15) USING DES-PRS
               BACKGROUND-COLOR  04.

       01  ENT-004.
           03  SUF-ENT LINE 09 COLUMN 39 PIC 9(03) USING SUF-LCX
                       BACKGROUND-COLOR  04.
           03  HST-ENT LINE 11 COLUMN 37 PIC X(20) USING HST-LCX
                       BACKGROUND-COLOR  04.
           03  QTD-ENT LINE 12 COLUMN 36 PIC ZZZ.ZZ9,9        USING
               QTD-LCX BACKGROUND-COLOR  04.
           03  VAL-ENT LINE 13 COLUMN 37 PIC Z.ZZZ.ZZ9,99     USING
               VAL-AUX BACKGROUND-COLOR  04.
           03  TOT-ENT LINE 15 COLUMN 37 PIC Z.ZZZ.ZZZ.ZZ9,99 USING
               VAL-LCX BACKGROUND-COLOR  04.

       01  LPA-001.
           03  LPA-HST LINE 11 COLUMN 37 PIC X(20) FROM SPACES
                       BACKGROUND-COLOR  04.
           03  LPA-QTD LINE 12 COLUMN 36 PIC X(09) FROM SPACES
                       BACKGROUND-COLOR  04.
           03  LPA-VAL LINE 13 COLUMN 37 PIC X(12) FROM SPACES
                       BACKGROUND-COLOR  04.
           03  LPA-TOT LINE 15 COLUMN 37 PIC X(16) FROM SPACES
                       BACKGROUND-COLOR  04.

       01  LPA-002.
           03  LINE 05 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 09 PIC X(64) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  CALCUL
                   "ANDO SERVICOS LANCADOS ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [F1] P/ CORRI
                   "GIR  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DO
                   "SERVICO ?  [ ]  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO LANCAMENTOS ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA, [F1] P/ ALTERAR - [ESC] RETORNA ..."
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "SERVICO  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O HISTORICO
                   "DO SERVICO  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A QUANTIDADE
                   " DE SERVICOS  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR UNIT
                   "ARIO DO SERVICO  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR TOTA
                   "L DO SERVICO  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-012 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002  LINE 22 COLUMN 45 PIC 9(01) USING NUM-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-002.

       ROT-000-000.
           OPEN     I-O      CADLCX
                             CADPRS.
       ROT-000-010.
           DISPLAY  TLA-001
                    MEN-001
           MOVE     1   TO   IND1.

       ROT-000-020.
           MOVE     SPACES   TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1  <  10
               GO  TO        ROT-000-020.
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     3        TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS  IND1
                                 ACM-QTD  ACM-VAL
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-000-030.
           READ     CADPRS   NEXT
           IF  ATV-PRS  NOT  =  3
               GO  TO        ROT-000-040.
           ADD      1        TO  IND1
           MOVE     IND1     TO  NUM-DT1
           MOVE     DES-PRS  TO  ATV-DT1
           MOVE     QTD-PRS  TO  QTD-DT1
           MOVE     VDD-PRS  TO  VAL-DT1
           MOVE     CHV-PRS  TO  CHV-TAB (IND1)
           MOVE     DET-001  TO  REG-TAB (IND1)
           ADD      QTD-PRS  TO  ACM-QTD
           ADD      VDD-PRS  TO  ACM-VAL
           GO  TO   ROT-000-030.

       ROT-000-040.
           MOVE     REG-TAB (01) TO  LIN-001
           MOVE     REG-TAB (02) TO  LIN-002
           MOVE     REG-TAB (03) TO  LIN-003
           MOVE     REG-TAB (04) TO  LIN-004
           MOVE     REG-TAB (05) TO  LIN-005
           MOVE     REG-TAB (06) TO  LIN-006
           MOVE     REG-TAB (07) TO  LIN-007
           MOVE     REG-TAB (08) TO  LIN-008
           MOVE     REG-TAB (09) TO  LIN-009
           DISPLAY  ENT-001.

       ROT-000-050.
           DISPLAY  MEN-002
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               MOVE      2   TO  FLG-AUX
               GO  TO        ROT-000-060.
           IF  TECLADO   =   02
               GO  TO        ROT-100-000.
           IF  TECLADO  NOT  =   0
               GO  TO        ROT-000-050.
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     4        TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   4   CHAIN   "DYN213"  USING  PRM-002.
           IF  ATV-PRS   =   5   CHAIN   "DYN214"  USING  PRM-002.
           IF  ATV-PRS   >   5   CHAIN   "DYN215"  USING  PRM-002.

       ROT-000-060.
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     FLG-AUX  TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   FLG-AUX
               GO  TO        ROT-000-070.
           COMPUTE  FLG-AUX  =   FLG-AUX  -  1
           IF  FLG-AUX   >   0
               GO  TO        ROT-000-060.
           CLOSE    CADLCX   CADPRS
           CHAIN   "DYN209"  USING   PRM-002.

       ROT-000-070.
           IF  ATV-PRS   =   1  CHAIN  "DYN210"  USING  PRM-002.
           IF  ATV-PRS   =   2  CHAIN  "DYN211"  USING  PRM-002.

       ROT-100-000.
           MOVE     ZEROS    TO  NUM-OPC
           DISPLAY  MEN-003
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-050.
           IF  NUM-OPC   <   1 OR > IND1
               GO  TO        ROT-100-000.
           MOVE     CHV-TAB (NUM-OPC) TO  CHV-PRS
           READ     CADPRS.

       ROT-100-010.
           DISPLAY  TLA-002      MEN-004
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     ZEROS    TO  SUF-LCX
                                 LCT-LCX  IND1
           START    CADLCX   KEY GREATER  CHV-LCX
                    INVALID  KEY GO  TO   ROT-100-030.

       ROT-100-020.
           READ     CADLCX   NEXT AT END
                    GO  TO   ROT-100-030.
           IF  TUR-LCX  NOT  =   TUR-PRS  OR
               CXA-LCX  NOT  =   CXA-PRS  OR
               ATV-LCX  NOT  =   ATV-PRS  OR
               PRX-LCX  NOT  =   PRX-PRS
               GO  TO        ROT-100-030.
           ADD      1        TO  IND1
           MOVE     SUF-LCX  TO  SUF-DT2
           MOVE     HST-LCX  TO  HST-DT2
           MOVE     QTD-LCX  TO  QTD-DT2
           MOVE     VAL-LCX  TO  VAL-DT2
           MOVE     DET-002  TO  LCT-TAB (IND1)
           GO  TO   ROT-100-020.

       ROT-100-030.
           MOVE     IND1     TO  IND2
           ADD      1        TO  IND1.

       ROT-100-040.
           IF  IND1  >  100
               MOVE  0   TO  IND1
               GO  TO        ROT-100-050.
           MOVE     SPACES   TO  LCT-TAB (IND1)
           ADD      1        TO  IND1
           GO  TO   ROT-100-040.

       ROT-100-050.
           MOVE     LCT-TAB (IND1 + 01)  TO  LCT-001
           MOVE     LCT-TAB (IND1 + 02)  TO  LCT-002
           MOVE     LCT-TAB (IND1 + 03)  TO  LCT-003
           MOVE     LCT-TAB (IND1 + 04)  TO  LCT-004
           MOVE     LCT-TAB (IND1 + 05)  TO  LCT-005
           MOVE     LCT-TAB (IND1 + 06)  TO  LCT-006
           MOVE     LCT-TAB (IND1 + 07)  TO  LCT-007
           MOVE     LCT-TAB (IND1 + 08)  TO  LCT-008
           MOVE     LCT-TAB (IND1 + 09)  TO  LCT-009
           MOVE     LCT-TAB (IND1 + 10)  TO  LCT-010
           ADD      10   TO  IND1
           DISPLAY  ENT-002.

       ROT-100-060.
           DISPLAY  MEN-005
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-012
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-100-070.
           IF  TECLADO   =   01
               DISPLAY       LPA-002
               GO  TO        ROT-000-010.
           IF  TECLADO   =   02
               GO  TO        ROT-100-080.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-100-060.
           IF  IND1     NOT  <   IND2
               GO  TO        ROT-100-060.
           GO  TO   ROT-100-050.

       ROT-100-070.
           IF  IND1  <  11
               GO  TO        ROT-100-060.
           COMPUTE  IND1  =  IND1  -  20
           GO  TO   ROT-100-050.

       ROT-100-080.
           DISPLAY  TLA-003  ENT-003.

       ROT-100-090.
           DISPLAY  LPA-001  MEN-006
           MOVE     0   TO   SUF-LCX
           ACCEPT   (09 39)  SUF-LCX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           IF  SUF-LCX   =   ZEROS
               GO  TO        ROT-100-090.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     TUR-PRS  TO  TUR-LCX
           MOVE     CXA-PRS  TO  CXA-LCX
           MOVE     ATV-PRS  TO  ATV-LCX
           MOVE     PRX-PRS  TO  PRX-LCX
           MOVE     ZEROS    TO  LCT-LCX
           READ     CADLCX   INVALID KEY
                    GO  TO   ROT-100-150.
           MOVE     QTD-LCX  TO  ACM-QTD
           MOVE     VAL-LCX  TO  ACM-VAL
           COMPUTE  VAL-AUX  =   VAL-LCX  /  QTD-LCX
           DISPLAY  ENT-004.

       ROT-100-100.
           DISPLAY  MEN-007
           ACCEPT   (11 37)  HST-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  HST-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-090.

       ROT-100-110.
           DISPLAY  LPA-QTD  MEN-008
           ACCEPT   (12 37)  QTD-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  QTD-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-100.

       ROT-100-120.
           DISPLAY  LPA-VAL  MEN-009
           ACCEPT   (13 37)  VAL-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  VAL-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-110.
           COMPUTE  VAL-LCX  =   QTD-LCX  *  VAL-AUX.

       ROT-100-130.
           DISPLAY  LPA-TOT  MEN-010
           ACCEPT   (15 37)  VAL-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  TOT-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-100-120.
           COMPUTE  VAL-AUX  =   VAL-LCX  /  QTD-LCX
           DISPLAY  VAL-ENT      MEN-011
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-130.
           COMPUTE  QTD-PRS  =   QTD-PRS  -  ACM-QTD  +  QTD-LCX
           COMPUTE  VDD-PRS  =   VDD-PRS  -  ACM-VAL  +  VAL-LCX
           REWRITE  REG-PRS
           IF  QTD-LCX   =   ZEROS
               GO  TO        ROT-100-140.
           REWRITE  REG-LCX
           GO  TO   ROT-100-090.

       ROT-100-140.
           DELETE   CADLCX
           CLOSE    CADLCX
           OPEN     I-O  CADLCX
           GO  TO   ROT-100-090.

       ROT-100-150.
           DISPLAY  SUF-ENT
           MOVE     DES-PRS  TO  DES-LCX
           MOVE     SPACES   TO  HST-LCX
           MOVE     ZEROS    TO  QTD-LCX  VAL-AUX
                                 VAL-LCX  LCT-LCX.
       ROT-100-160.
           DISPLAY  LPA-QTD  MEN-007
           ACCEPT   (11 37)  HST-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-090.
           DISPLAY  HST-ENT.

       ROT-100-170.
           DISPLAY  LPA-VAL
                    LPA-QTD  MEN-008
           ACCEPT   (12 37)  QTD-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-160.
           IF  QTD-LCX   =   ZEROS
               GO  TO        ROT-100-170.
           DISPLAY  QTD-ENT.

       ROT-100-180.
           DISPLAY  LPA-TOT
                    LPA-VAL  MEN-009
           ACCEPT   (13 37)  VAL-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-170.
           COMPUTE  VAL-LCX  =   QTD-LCX  *  VAL-AUX
           DISPLAY  VAL-ENT.

       ROT-100-190.
           DISPLAY  LPA-TOT  MEN-010
           ACCEPT   (15 37)  VAL-LCX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-180.
           IF  VAL-LCX   =   ZEROS
               GO  TO        ROT-100-180.
           MOVE     SPACES   TO  RSP-OPC
           COMPUTE  VAL-AUX  =   VAL-LCX  /  QTD-LCX
           DISPLAY  VAL-ENT      TOT-ENT     MEN-011
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-190.
           ADD      QTD-LCX  TO  QTD-PRS
           ADD      VAL-LCX  TO  VDD-PRS
           REWRITE  REG-PRS
           WRITE    REG-LCX
           GO  TO   ROT-100-090.