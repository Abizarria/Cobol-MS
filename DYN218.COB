       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN218.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCTR        ASSIGN  TO  DISK.

           SELECT      CADCXA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CXA.

           SELECT      CADECR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ECR.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST.

           SELECT      CADLCX        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCX.

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS.

           SELECT      CADVDA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-VDA.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCTR      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCTR.DAT".
       01  REG-CTR.
           03  DIA-CTR          PIC  9(02).
           03  DTI-CTR          PIC  9(06).
           03  DTC-CTR          PIC  9(06).
           03  DTD-CTR          PIC  9(06).
           03  FAT-CTR          PIC  9(06).
           03  PCT-CTR          PIC  9(02)V99.

       FD  CADCXA      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCXA.DAT".
       01  REG-CXA.
           03  CHV-CXA.
               05  TUR-CXA      PIC  9(01).
               05  NUM-CXA      PIC  9(01).
           03  POS-CXA          PIC  9(01).
           03  DTA-CXA          PIC  9(06).
           03  DES-CXA          PIC  X(15).

       FD  CADECR      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADECR.DAT".
       01  REG-ECR.
           03  CHV-ECR          PIC  9(02).
           03  CMB-ECR          PIC  9(01).
           03  TQ1-ECR          PIC  9(02).
           03  TQ2-ECR          PIC  9(02).
           03  CX1-ECR          PIC  9(01).
           03  CX2-ECR          PIC  9(01).
           03  IDL-ECR          PIC  9(06)V9.
           03  ICL-ECR          PIC  9(06)V9.
           03  FCL-ECR          PIC  9(06)V9.
           03  FLG-ECR          PIC  9(01).
           03  ITL-ECR          PIC  9(06)V9.
           03  FTL-ECR          PIC  9(06)V9.
           03  AFR-ECR          PIC  9(04).
           03  EXT-ECR          PIC  9(04).
           03  VAL-ECR          PIC  9(04)V9999.

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

       FD  CADVDA      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADVDA.DAT".
       01  REG-VDA.
           03  CHV-VDA.
               05  ATV-VDA      PIC  9(01).
               05  PRX-VDA      PIC  9(01).
               05  SUF-VDA      PIC  9(03).
               05  LCT-VDA      PIC  9(03).
           03  DES-VDA          PIC  X(15).
           03  HST-VDA          PIC  X(20).
           03  QTD-VDA          PIC  9(06)V9.
           03  VAL-VDA          PIC  9(10)V99.

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC  X(80).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ATV-AUX          PIC  9(01).
           03  PRX-AUX          PIC  9(01).
           03  ACM-LIN          PIC  9(02).
           03  ACM-VAL          PIC  9(11)V99.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

           03  TAB-TUR.
               05  FILLER       PIC  X(05)  VALUE  "MANHA".
               05  FILLER       PIC  X(05)  VALUE  "TARDE".
               05  FILLER       PIC  X(05)  VALUE  "NOITE".
           03  RED-TUR          REDEFINES   TAB-TUR.
               05  TUR-TAB      PIC  X(05)  OCCURS  03.

       01  CAB-001.
           03  FILLER           PIC  X(22)  VALUE  "-DYNAMIC-".
           03  NOM-CB1          PIC  X(48).
           03  FILLER           PIC  X(08)  VALUE  "FOLHA :".
           03  PAG-CB1          PIC  9(02).

       01  CAB-002.
           03  FILLER           PIC  X(19)  VALUE  "RESUMO DO CAIXA .:".
           03  CXA-CB2          PIC  X(20).
           03  FILLER           PIC  X(09)  VALUE  "TURNO .:".
           03  TUR-CB2          PIC  X(24).
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "SINAL   DESCRICAO          QUANTIDADE        VALOR DO ITEM
           "      TOTAL DO GRUPO".

       01  CAB-005.
           03  FILLER           PIC  X(22)  VALUE
               "ANALITICO DO CAIXA .:".
           03  CXA-CB5          PIC  X(20).
           03  FILLER           PIC  X(09)  VALUE  "TURNO .:".
           03  TUR-CB5          PIC  X(21).
           03  DIA-CB5          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB5          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB5          PIC  9(02).

       01  CAB-006              PIC  X(80)  VALUE
           "  CODIGO DESCRICAO / HISTORICO     QUANTIDADE    VALOR DO IT
           "EM    TOTAL DO GRUPO".

       01  CAB-007.
           03  FILLER           PIC  X(02)  VALUE  "-".
           03  ATV-CB7          PIC  X(78).

       01  DET-001.
           03  FILLER           PIC  X(01)  VALUE  "<".
           03  SNL-DT1          PIC  X(01).
           03  FILLER           PIC  X(06)  VALUE  ">".
           03  DES-DT1          PIC  X(20).
           03  QTD-DT1          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  VAL-DT1          PIC  Z.ZZZ.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  TOT-DT1          PIC  ZZ.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.

       01  DET-002.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  PRX-DT2          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DT2          PIC  9(03).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  DES-DT2          PIC  X(27).
           03  QTD-DT2          PIC  ZZZ.ZZ9,9         BLANK WHEN ZEROS.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  VAL-DT2          PIC  Z.ZZZ.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  TOT-DT2          PIC  ZZ.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.

       01  TOT-001.
           03  FILLER           PIC  X(63)  VALUE  "        DIFERENCA DO
               " CAIXA ...............................".
           03  VAL-TT1          PIC  --.---.---.--9,99.

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 10 COLUMN 28 PIC X(23) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 28 PIC X(23) FROM " ³                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 51 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 28 PIC X(23) FROM " ³  RESUMO DO CAIXA  ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 51 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 28 PIC X(23) FROM " ³                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 51 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 28 PIC X(23) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 51 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 30 PIC X(23) FROM SPACES.

       01  TLA-002.
           03  LINE 10 COLUMN 27 PIC X(26) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "Ä¿" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 27 PIC X(26) FROM " ³
               " ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 27 PIC X(26) FROM " ³  ANALITICO DO CAIXA
               " ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 27 PIC X(26) FROM " ³
               " ³" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 27 PIC X(26) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÙ" BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 29 PIC X(26) FROM SPACES.

       01  TLA-003.
           03  LINE 09 COLUMN 11 PIC X(58) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 69 PIC X(02) FROM  SPACES.
           03  LINE 11 COLUMN 11 PIC X(58) FROM " ³  0%.................
               "....50%....................100%  ³" BACKGROUND-COLOR 04.
           03  LINE 11 COLUMN 69 PIC X(02) FROM  SPACES.
           03  LINE 12 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 12 COLUMN 69 PIC X(02) FROM  SPACES.
           03  LINE 13 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 13 COLUMN 69 PIC X(02) FROM  SPACES.
           03  LINE 14 COLUMN 11 PIC X(58) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 04.
           03  LINE 14 COLUMN 69 PIC X(02) FROM  SPACES.
           03  LINE 15 COLUMN 13 PIC X(58) FROM  SPACES.

       01  COL-TLA.
           03  COL-001  LINE 12 COLUMN 15 PIC X(10) FROM SPACES
                        BACKGROUND-COLOR  07.
           03  COL-002  LINE 12 COLUMN 25 PIC X(10) FROM SPACES
                        BACKGROUND-COLOR  07.
           03  COL-003  LINE 12 COLUMN 35 PIC X(10) FROM SPACES
                        BACKGROUND-COLOR  07.
           03  COL-004  LINE 12 COLUMN 45 PIC X(10) FROM SPACES
                        BACKGROUND-COLOR  07.
           03  COL-005  LINE 12 COLUMN 55 PIC X(10) FROM SPACES
                        BACKGROUND-COLOR  07.

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

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA EMITIR O RES
                   "UMO ? [S/N]  [ ]  -  [ESC] RETORNA ...     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA EMITIR O ANA
                   "LITICO ? [S/N]  [ ]  -  [ESC] RETORNA ...  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "CONFIRMA O CAIXA ?
                   "[S/N]  [ ]  -  [ESC] RETORNA ...           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR !!!  -  [ESC] RETORNA ...           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO RESUMO DO CAIXA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO ANALITICO DO CAIXA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ATUALI
                   "ZANDO ARQUIVOS ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 47 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 50 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-003 LINE 22 COLUMN 41 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-004 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-002.

       ROT-000-000.
           OPEN     INPUT    CADCXA  CADEST
                    CADLCX   CADPRM  CADPRS
           READ     CADPRM
           MOVE     1   TO   IND1.

       ROT-000-010.
           MOVE     CLI-PRM (IND1 + 1)  TO IND2
           MOVE     LET-TAB (IND2)  TO  LET-NOM (IND1)
           ADD      1   TO   IND1
           IF  IND1  <  36
               GO  TO        ROT-000-010.
           MOVE     SPACES   TO  LET-NOM (IND1)
           MOVE     NOM-TAB  TO  NOM-CB1
           MOVE     PRM-002  TO  CHV-CXA
           READ     CADCXA
           MOVE     DTA-CXA  TO  DTA-AUX.

       ROT-000-020.
           DISPLAY  LPA-001  TLA-001.

       ROT-000-030.
           DISPLAY  MEN-001
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-100.
           IF  RSP-OPC   =   "S"
               GO  TO        ROT-100-000.
           IF  RSP-OPC  NOT  =   "N"
               GO  TO        ROT-000-030.

       ROT-000-040.
           MOVE     SPACES   TO  CHV-LCX
           MOVE     TUR-CXA  TO  TUR-LCX
           MOVE     NUM-CXA  TO  CXA-LCX
           START    CADLCX   KEY GREATER  CHV-LCX
                    INVALID  KEY GO  TO   ROT-000-060.
           READ     CADLCX   NEXT
           IF  TUR-LCX  NOT  =   TUR-CXA  OR
               CXA-LCX  NOT  =   NUM-CXA
               GO  TO        ROT-000-060.
           DISPLAY  LPA-001      TLA-002.

       ROT-000-050.
           DISPLAY  MEN-002
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           IF  RSP-OPC   =   "S"
               GO  TO        ROT-200-000.
           IF  RSP-OPC  NOT  =   "N"
               GO  TO        ROT-000-050.

       ROT-000-060.
           DISPLAY  LPA-001  TLA-003.

       ROT-000-070.
           DISPLAY  MEN-003
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-003
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-080.
           IF  RSP-OPC   =   "S"
               GO  TO        ROT-300-000.
           IF  RSP-OPC   =   "N"
               GO  TO        ROT-300-090.
           GO  TO   ROT-000-070.

       ROT-000-080.
           MOVE     SPACES   TO  CHV-LCX
           MOVE     TUR-CXA  TO  TUR-LCX
           MOVE     NUM-CXA  TO  CXA-LCX
           START    CADLCX   KEY GREATER  CHV-LCX
                    INVALID  KEY GO  TO   ROT-000-020.
           READ     CADLCX   NEXT
           IF  TUR-LCX  NOT  =   TUR-CXA  OR
               CXA-LCX  NOT  =   NUM-CXA
               GO  TO        ROT-000-020.
           DISPLAY  LPA-001      TLA-002
           GO  TO   ROT-000-050.

       ROT-100-000.
           DISPLAY  MEN-004
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-004
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           DISPLAY  MEN-005
           OPEN     OUTPUT   RELATO
           MOVE     1        TO  PAG-CB1
           MOVE     DES-CXA  TO  CXA-CB2
           MOVE     TUR-TAB (TUR-CXA) TO  TUR-CB2
           MOVE     DIA-AUX  TO  DIA-CB2
           MOVE     MES-AUX  TO  MES-CB2
           MOVE     ANO-AUX  TO  ANO-CB2
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-CXA  TO  TUR-PRS
           MOVE     NUM-CXA  TO  CXA-PRS
           START    CADPRS   KEY GREATER  CHV-PRS
           READ     CADPRS   NEXT
           MOVE     ATV-PRS  TO  ATV-AUX
           MOVE     ZEROS    TO  ACM-VAL  TOT-DT1.

       ROT-100-010.
           WRITE    REG-REL FROM CAB-001  AFTER 0
           WRITE    REG-REL FROM CAB-002  AFTER 2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-003.

       ROT-100-020.
           IF  ATV-PRS   <   5
               MOVE    "+"   TO  SNL-DT1
           ELSE
               MOVE    "-"   TO  SNL-DT1.
           MOVE     DES-PRS  TO  DES-DT1
           MOVE     QTD-PRS  TO  QTD-DT1
           MOVE     VDD-PRS  TO  VAL-DT1
           ADD      VDD-PRS  TO  ACM-VAL
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   9
               GO  TO        ROT-100-030.
           IF  ATV-AUX   <   6   AND
               ATV-PRS  NOT  =   ATV-AUX
               PERFORM       ROT-100-030
               GO  TO        ROT-100-020.
           WRITE    REG-REL FROM DET-001
           GO  TO   ROT-100-020.

       ROT-100-030.
           MOVE     ACM-VAL  TO  TOT-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL
           MOVE     ATV-PRS  TO  ATV-AUX
           MOVE     ZEROS    TO  ACM-VAL  TOT-DT1.

       ROT-100-040.
           MOVE     VDD-PRS  TO  VAL-TT1
           WRITE    REG-REL FROM TOT-001  AFTER  2
           WRITE    REG-REL FROM TOT-001  AFTER  0
           CLOSE    RELATO
           GO  TO   ROT-000-040.

       ROT-200-000.
           DISPLAY  MEN-004
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-004
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-050.
           DISPLAY  MEN-006
           OPEN     OUTPUT   RELATO
           MOVE     1        TO  PAG-CB1
           MOVE     DES-CXA  TO  CXA-CB5
           MOVE     TUR-TAB (TUR-CXA) TO  TUR-CB5
           MOVE     DIA-AUX  TO  DIA-CB5
           MOVE     MES-AUX  TO  MES-CB5
           MOVE     ANO-AUX  TO  ANO-CB5
           MOVE     ATV-LCX  TO  ATV-AUX
           MOVE     PRX-LCX  TO  PRX-AUX
           MOVE     ZEROS    TO  ACM-VAL  TOT-DT2
           WRITE    REG-REL FROM CAB-001  AFTER 0.

       ROT-200-010.
           MOVE     DES-LCX  TO  ATV-CB7
           WRITE    REG-REL FROM CAB-005  AFTER 2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-006
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-007
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL
           MOVE     2        TO  ACM-LIN.

       ROT-200-020.
           MOVE     PRX-LCX  TO  PRX-DT2
           MOVE     SUF-LCX  TO  SUF-DT2
           MOVE     HST-LCX  TO  DES-DT2
           IF  ATV-LCX  NOT  =   2
               GO  TO        ROT-200-030.
           MOVE     PRX-LCX  TO  PRX-EST
           MOVE     SUF-LCX  TO  SUF-EST
           READ     CADEST
           MOVE     DES-EST  TO  DES-DT2.

       ROT-200-030.
           MOVE     QTD-LCX  TO  QTD-DT2
           MOVE     VAL-LCX  TO  VAL-DT2
           ADD      VAL-LCX  TO  ACM-VAL
           READ     CADLCX   NEXT AT END
                    GO  TO   ROT-200-060.
           IF  TUR-LCX  NOT  =   TUR-CXA  OR
               CXA-LCX  NOT  =   NUM-CXA
               GO  TO        ROT-200-060.
           IF  ATV-LCX  NOT  =   ATV-AUX  OR
              (PRX-LCX  NOT  =   PRX-AUX
               AND  ATV-AUX  <   4)
               GO  TO        ROT-200-050.
           WRITE    REG-REL FROM DET-002
           ADD      1        TO  ACM-LIN
           IF  ACM-LIN   <   52
               GO  TO        ROT-200-020.

       ROT-200-040.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           GO  TO   ROT-200-010.

       ROT-200-050.
           MOVE     ATV-LCX  TO  ATV-AUX
           MOVE     PRX-LCX  TO  PRX-AUX
           MOVE     ACM-VAL  TO  TOT-DT2
           WRITE    REG-REL FROM DET-002
           MOVE     ZEROS    TO  ACM-VAL  TOT-DT2
           IF  ACM-LIN   >   50
               GO  TO        ROT-200-040.
           MOVE     DES-LCX  TO  ATV-CB7
           WRITE    REG-REL FROM CAB-007  AFTER 2
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL
           ADD      4        TO  ACM-LIN
           GO  TO   ROT-200-020.

       ROT-200-060.
           MOVE     ACM-VAL  TO  TOT-DT2
           WRITE    REG-REL FROM DET-002
           CLOSE    RELATO
           GO  TO   ROT-000-060.

       ROT-300-000.
           DISPLAY  MEN-007  COL-001
           CLOSE    CADCXA
                    CADPRM   CADPRS
           OPEN     I-O      CADPRS
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-300-010.
           READ     CADPRS   NEXT
           IF  ATV-PRS   >   1
               GO  TO        ROT-300-020.
           MOVE     0        TO  PRX-EST
           MOVE     PRX-PRS  TO  SUF-EST
           READ     CADEST
           COMPUTE  CTD-PRS  =   QTD-PRS
                             *   CTM-EST
           REWRITE  REG-PRS
           GO  TO   ROT-300-010.

       ROT-300-020.
           CLOSE    CADPRS   CADEST
           OPEN     I-O      CADVDA
           DISPLAY  COL-002
           MOVE     SPACES   TO  CHV-LCX
           MOVE     TUR-PR2  TO  TUR-LCX
           MOVE     CXA-PR2  TO  CXA-LCX
           START    CADLCX   KEY GREATER  CHV-LCX
                    INVALID  KEY GO  TO   ROT-300-060.

       ROT-300-030.
           READ     CADLCX   NEXT AT END
                    GO  TO   ROT-300-060.
           IF  TUR-LCX  NOT  =   TUR-PR2  OR
               CXA-LCX  NOT  =   CXA-PR2
               GO  TO        ROT-300-060.
           MOVE     ATV-LCX  TO  ATV-VDA
           MOVE     PRX-LCX  TO  PRX-VDA
           MOVE     SUF-LCX  TO  SUF-VDA
           MOVE     LCT-LCX  TO  LCT-VDA
           READ     CADVDA   INVALID KEY
                    GO  TO   ROT-300-040.
           IF  ATV-LCX   >   2
               GO  TO        ROT-300-040.
           ADD      QTD-LCX  TO  QTD-VDA
           ADD      VAL-LCX  TO  VAL-VDA
           REWRITE  REG-VDA
           GO  TO   ROT-300-030.

       ROT-300-040.
           MOVE     DES-LCX  TO  DES-VDA
           MOVE     HST-LCX  TO  HST-VDA
           MOVE     QTD-LCX  TO  QTD-VDA
           MOVE     VAL-LCX  TO  VAL-VDA.

       ROT-300-050.
           WRITE    REG-VDA  INVALID KEY
                    ADD  1   TO  LCT-VDA
                    GO  TO   ROT-300-050.
           GO  TO   ROT-300-030.

       ROT-300-060.
           DISPLAY  COL-003
           CLOSE    CADLCX   CADVDA
           OPEN     I-O      CADECR
           MOVE     ZEROS    TO  CHV-ECR
           START    CADECR   KEY GREATER  CHV-ECR.

       ROT-300-070.
           READ     CADECR   NEXT AT END
                    GO  TO   ROT-300-080.
           IF  CXA-PR2  NOT  =   CX1-ECR
                        AND      CX2-ECR
               GO  TO        ROT-300-070.
           MOVE     FCL-ECR  TO  ICL-ECR
           ADD      AFR-ECR  TO  EXT-ECR
           MOVE     ZEROS    TO  FCL-ECR
                                 AFR-ECR
                                 FLG-ECR
           REWRITE  REG-ECR
           GO  TO   ROT-300-070.

       ROT-300-080.
           DISPLAY  COL-004
           CLOSE    CADECR
           OPEN     I-O      CADCTR
                             CADCXA
           READ     CADCTR
           MOVE     DTA-AUX  TO  DTC-CTR
           MOVE     PRM-002  TO  CHV-CXA
           READ     CADCXA
           MOVE     ZEROS    TO  DTA-CXA
           MOVE     2        TO  POS-CXA
           REWRITE  REG-CTR
           REWRITE  REG-CXA
           CLOSE    CADCTR
                    CADCXA
           DISPLAY  COL-005.

       ROT-300-090.
           CHAIN    "DYN201".

       ROT-300-100.
           CHAIN    "DYN215"  USING  PRM-002.