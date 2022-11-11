       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN215.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS.

       DATA            DIVISION.
       FILE            SECTION.

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
           03  ACM-DIF          PIC S9(11)V99.

           03  DET-001.
               05  NUM-DT1      PIC  9(02).
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  ATV-DT1      PIC  X(16).
               05  VAL-DT1      PIC  --.---.---.--9,99.

           03  TAB-REG          OCCURS  09.
               05  CHV-TAB      PIC  9(07).
               05  REG-TAB      PIC  X(38).

           03  ENT-AUX.
               05  LIN-001      PIC  X(38).
               05  LIN-002      PIC  X(38).
               05  LIN-003      PIC  X(38).
               05  LIN-004      PIC  X(38).
               05  LIN-005      PIC  X(38).
               05  LIN-006      PIC  X(38).
               05  LIN-007      PIC  X(38).
               05  LIN-008      PIC  X(38).
               05  LIN-009      PIC  X(38).

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       01  PRM-003.
           03  ATV-PR3          PIC  9(01).
           03  LIN-PR3          PIC  9(03).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 05 COLUMN 17 PIC X(46) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 19 PIC X(42) FROM
               "  N§   DESCRICAO           VALOR LANCADO"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 06 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 17 PIC X(46) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"    BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 17 PIC X(46) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 19 PIC X(46) FROM SPACES.

       01  ENT-001.
           03  LINE 08 COLUMN 21 PIC X(38) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 21 PIC X(38) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 21 PIC X(38) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 21 PIC X(38) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 21 PIC X(38) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 21 PIC X(38) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 21 PIC X(38) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 21 PIC X(38) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 21 PIC X(38) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 21 PIC X(20) FROM "Diferenca do Caixa :"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 18 COLUMN 41 PIC ---.---.---.--9,99 USING ACM-DIF
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.

       01  LPA-001.
           03  LINE 05 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.

       01  LPA-002.
           03  LINE 08 COLUMN 43 PIC X(02) FROM "  " BACKGROUND-COLOR 1.
           03  LINE 09 COLUMN 43 PIC X(02) FROM "  " BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 43 PIC X(02) FROM "  " BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 43 PIC X(02) FROM "  " BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 43 PIC X(02) FROM "  " BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 43 PIC X(02) FROM "  " BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 43 PIC X(02) FROM "  " BACKGROUND-COLOR 1.
           03  LINE 15 COLUMN 43 PIC X(02) FROM "  " BACKGROUND-COLOR 1.
           03  LINE 16 COLUMN 43 PIC X(02) FROM "  " BACKGROUND-COLOR 1.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  CALCUL
                   "ANDO DIFERENCA DO CAIXA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [F1] P/ CORRI
                   "GIR  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DO
                   "ITEM ?  [ ]  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR CORR
                   "ETO  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-002.

       ROT-000-000.
           OPEN     I-O      CADPRS
           DISPLAY  LPA-001
                    TLA-001  MEN-001.

       ROT-000-010.
           MOVE     ZEROS    TO  ACM-DIF
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-000-020.
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   9
               GO  TO        ROT-000-030.
           IF  ATV-PRS   >   4
               COMPUTE  ACM-DIF  =   ACM-DIF  +  VDD-PRS
           ELSE
               COMPUTE  ACM-DIF  =   ACM-DIF  -  VDD-PRS.
           GO  TO   ROT-000-020.

       ROT-000-030.
           MOVE     ACM-DIF  TO  VDD-PRS
           REWRITE  REG-PRS
           MOVE     1        TO  IND1.

       ROT-100-000.
           MOVE     SPACES   TO  REG-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1  <  10
               GO  TO        ROT-100-000.
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     6        TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS  IND1
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-100-010.
           READ     CADPRS   NEXT
           IF  ATV-PRS   >   8
               GO  TO        ROT-100-020.
           ADD      1        TO  IND1
           MOVE     IND1     TO  NUM-DT1
           MOVE     DES-PRS  TO  ATV-DT1
           MOVE     VDD-PRS  TO  VAL-DT1
           MOVE     CHV-PRS  TO  CHV-TAB (IND1)
           MOVE     DET-001  TO  REG-TAB (IND1)
           GO  TO   ROT-100-010.

       ROT-100-020.
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

       ROT-100-030.
           DISPLAY  MEN-002
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               MOVE      5   TO  FLG-AUX
               GO  TO        ROT-100-040.
           IF  TECLADO   =   02
               GO  TO        ROT-200-000.
           IF  TECLADO   =   00
               GO  TO        ROT-300-000.
           GO  TO   ROT-100-030.

       ROT-100-040.
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     FLG-AUX  TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   FLG-AUX
               GO  TO        ROT-100-050.
           COMPUTE  FLG-AUX  =   FLG-AUX  -  1
           IF  FLG-AUX   >   0
               GO  TO        ROT-100-040.
           CLOSE    CADPRS
           CHAIN   "DYN209"  USING   PRM-002.

       ROT-100-050.
           IF  ATV-PRS   =   1   CHAIN   "DYN210"  USING  PRM-002.
           IF  ATV-PRS   =   2   CHAIN   "DYN211"  USING  PRM-002.
           IF  ATV-PRS   =   3   CHAIN   "DYN212"  USING  PRM-002.
           IF  ATV-PRS   =   4   CHAIN   "DYN213"  USING  PRM-002.
           IF  ATV-PRS   =   5   CHAIN   "DYN214"  USING  PRM-002.

       ROT-200-000.
           MOVE     ZEROS    TO  NUM-OPC
           DISPLAY  MEN-003
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           IF  NUM-OPC   <   1 OR > IND1
               GO  TO        ROT-200-000.
           MOVE     CHV-TAB (NUM-OPC) TO  CHV-PRS
           READ     CADPRS
           MOVE     ATV-PRS  TO  ATV-PR3
           MOVE     LIN-PRS  TO  LIN-PR3
           IF  ATV-PRS   =   6   CHAIN   "DYN216" USING PRM-002 PRM-003.
           IF  ATV-PRS   =   7   CHAIN   "DYN217" USING PRM-002 PRM-003.
           COMPUTE  LIN  =   NUM-OPC  +   07.

       ROT-200-010.
           DISPLAY  LPA-002  MEN-004
           ACCEPT  (LIN, 45) VDD-PRS  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           REWRITE  REG-PRS
           GO  TO   ROT-000-010.

       ROT-300-000.
           CHAIN   "DYN218"  USING  PRM-002.