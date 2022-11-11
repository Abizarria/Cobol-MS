       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN225.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCTR        ASSIGN  TO  DISK.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST.

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS.

           SELECT      CADTNQ        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TNQ.

           SELECT      CADSDO        ASSIGN  TO  DISK.

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
           03  PCT-ECT          PIC S9(03)V99.
           03  QVM-EST          PIC  9(07)V9.
           03  QVA-EST          PIC  9(08)V9.

       FD  CADPRM      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADPRM.DAT".
       01  REG-PRM.
           05  COD-PRM          PIC  9(05).
           05  CLI-PRM          PIC  9(02)  OCCURS  36.
           05  END-PRM          PIC  9(02)  OCCURS  35.
           05  CID-PRM          PIC  9(02)  OCCURS  20.
           05  EST-PRM          PIC  9(02)  OCCURS  02.
           05  CEP-PRM          PIC  9(08).
           05  CGC-PRM          PIC  9(14).
           05  INS-PRM          PIC  9(02)  OCCURS  15.

       FD  CADPRS      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADPRS.DAT".
       01  REG-PRS.
           03  CHV-PRS.
               05  TUR-PRS      PIC  9(01).
               05  CXA-PRS      PIC  9(01).
               05  ATV-PRS      PIC  9(01).
               05  LIN-PRS      PIC  9(03).
           03  DES-PRS          PIC  X(15).
           03  PRX-PRS          PIC  X(01).
           03  QTD-PRS          PIC  9(06)V9.
           03  CTD-PRS          PIC  9(10)V99.
           03  VDD-PRS          PIC S9(10)V99.
           03  QTM-PRS          PIC  9(07)V9.
           03  CTM-PRS          PIC  9(11)V99.
           03  VDM-PRS          PIC S9(11)V99.

       FD  CADTNQ      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADTNQ.DAT".
       01  REG-TNQ.
           03  CHV-TNQ          PIC  9(02).
           03  CMB-TNQ          PIC  9(01).
           03  SDI-TNQ          PIC S9(05)V9.
           03  SDA-TNQ          PIC S9(05)V9.

       FD  CADSDO      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADSDO.DAT".
       01  REG-SDO.
           03  SDA-SDO          PIC  9(10)V99.
           03  CRD-SDO          PIC S9(11)V99  OCCURS  05.

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
           03  IND3             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  X(02).
           03  MED-AUX          PIC  9(06)V9.
           03  ACM-QTD          PIC  9(06)V9.
           03  ACM-QTM          PIC  9(07)V9.
           03  ACM-MED          PIC  9(06)V9.
           03  ACM-CRD          PIC  9(11)V99.
           03  ACM-DIF          PIC S9(11)V99.
           03  TOT-CST          PIC S9(12)V99.
           03  TOT-VDA          PIC S9(12)V99.

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  VDA-TAB          PIC  9(04)V99    OCCURS  09.
           03  CST-TAB          PIC  9(04)V9999  OCCURS  09.

           03  LIN-TAB          OCCURS  09.
               05  PRX-TAB      OCCURS  09.
                   07  ATV-TAB  PIC  X(15).
                   07  QTD-TAB  PIC  9(06)V9.
                   07  QTM-TAB  PIC  9(07)V9.
                   07  VDD-TAB  PIC S9(10)V99.
                   07  VDM-TAB  PIC S9(11)V99.
                   07  SDO-TAB  PIC S9(11)V99.

           03  TP1-AUX          PIC  X(16)  VALUE  "TOTAL DAS VENDAS".
           03  TP2-AUX          PIC  X(16)  VALUE  "TOTAL DA RECEITA".
           03  TP3-AUX          PIC  X(16)  VALUE  "TOTAL LIQUIDO   ".
           03  TP4-AUX          PIC  X(16)  VALUE  "SALDO DO CAIXA  ".
           03  TP5-AUX          PIC  X(16)  VALUE  "SALDO ANTERIOR  ".
           03  TP6-AUX          PIC  X(16)  VALUE  "SALDO DEPOSITADO".
           03  TP7-AUX          PIC  X(16)  VALUE  "SALDO PENDENTE  ".

       01  CAB-001.
           03  FILLER           PIC  X(22)  VALUE  "-DYNAMIC-".
           03  NOM-CB1          PIC  X(48).
           03  FILLER           PIC  X(08)  VALUE  "FOLHA :".
           03  PAG-CB1          PIC  9(02).

       01  CAB-002.
           03  FILLER           PIC  X(72)  VALUE  "DIARIO DE VENDAS".
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "+/- ATIVIDADE / ORIGEM     VALOR EM R$   | VENDA/DIA |  VEND
           "A/MES  |   MEDIA   |".

       01  DET-001.
           03  RG1-DT1          PIC  X(38).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  RG2-DT1          PIC  X(39).

       01  REG-001.
           03  SNL-RG1          PIC  X(04).
           03  DES-RG1          PIC  X(16).
           03  VDD-RG1          PIC  ---.---.---.--9,99.

       01  REG-002.
           03  FILLER           PIC  X(02)  VALUE  "|".
           03  QTD-RG2          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(03)  VALUE  " |".
           03  QTM-RG2          PIC  Z.ZZZ.ZZ9,9.
           03  FILLER           PIC  X(03)  VALUE  " |".
           03  MED-RG2          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(02)  VALUE  " |".

       01  REG-003              PIC  X(39)  VALUE
           "VALOR TOTAL NO MES   CREDITOS PENDENTES".

       01  REG-004.
           03  VDM-RG4          PIC ---.---.---.--9,99 BLANK WHEN ZEROS.
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  SDO-RG4          PIC ---.---.---.--9,99 BLANK WHEN ZEROS.

       01  REG-005              PIC  X(39)  VALUE
           "      TOTAL DO ESTOQUE ATUALIZADO".

       01  REG-006.
           03  FILLER           PIC  X(09)  VALUE  "PRECO DE".
           03  TPO-RG6          PIC  X(06).
           03  FILLER           PIC  X(05)  VALUE  ".....".
           03  VAL-RG6          PIC  ----.---.---.--9,99.

       01  PRM-001              PIC  9(01).

       01  PRM-002.
           03  BCO-PR2          PIC  9(03).
           03  VAL-PR2          PIC  9(10)V99.

       01  PRM-003.
           03  CMP-PR3          OCCURS  10.
               05  CMB-PR3      PIC  9(01).
               05  NTF-PR3      PIC  9(06).
               05  VCT-PR3      PIC  9(06).
               05  TQ1-PR3      PIC  9(02).
               05  TQ2-PR3      PIC  9(02).
               05  TQ3-PR3      PIC  9(02).
               05  TQ4-PR3      PIC  9(02).
               05  TQ5-PR3      PIC  9(02).
               05  QTD-PR3      PIC S9(05)V9.
               05  CST-PR3      PIC  9(04)V9999.
               05  VAL-PR3      PIC S9(10)V99.

       01  PRM-004.
           03  SDO-PR4          PIC S9(11)V99  OCCURS  05.

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 09 COLUMN 11 PIC X(58) FROM " 旼컴컴컴컴컴컴컴컴컴컴
               "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커" BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 11 PIC X(58) FROM " �
               "                                 �" BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 11 PIC X(58) FROM " �  0%.................
               "....50%....................100%  �" BACKGROUND-COLOR 04.
           03  LINE 11 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 11 PIC X(58) FROM " �
               "                                 �" BACKGROUND-COLOR 04.
           03  LINE 12 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 11 PIC X(58) FROM " �
               "                                 �" BACKGROUND-COLOR 04.
           03  LINE 13 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 11 PIC X(58) FROM " 읕컴컴컴컴컴컴컴컴컴컴
               "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸" BACKGROUND-COLOR 04.
           03  LINE 14 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 13 PIC X(58) FROM SPACES.

       01  TLA-002.
           03  LINE 10 COLUMN 28 PIC X(24) FROM " 旼컴컴컴컴컴컴컴컴컴커
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 28 PIC X(24) FROM " �                    �
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 28 PIC X(24) FROM " �  DIARIO DE VENDAS  �
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 28 PIC X(24) FROM " �                    �
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 28 PIC X(24) FROM " 읕컴컴컴컴컴컴컴컴컴켸
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 30 PIC X(24) FROM SPACES.

       01  LPA-001.
           03  LINE 05 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.

       01  COL-001  LINE 12 COLUMN 15 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-002  LINE 12 COLUMN 25 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-003  LINE 12 COLUMN 35 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-004  LINE 12 COLUMN 45 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-005  LINE 12 COLUMN 55 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  07.

       01  MEN-001  LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ACUMU
                    "LANDO VALORES DIVERSOS ...                       "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002  LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ I
                    "MPRIMIR !!!  -  [ESC] RETORNA ...                "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003  LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRI
                    "MINDO DIARIO DE VENDAS ...                       "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC  AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001  PRM-002  PRM-003.

       ROT-000-000.
           DISPLAY  LPA-001
                    TLA-001  MEN-001
           OPEN     INPUT    CADCTR
                             CADPRM
           READ     CADPRM
           READ     CADCTR
           MOVE     1        TO  IND1
           ADD      1        TO  DIA-CTR
           MOVE     DTC-CTR  TO  DTA-AUX
           MOVE     DIA-CTR  TO  PAG-CB1
           MOVE     DIA-AUX  TO  DIA-CB2
           MOVE     MES-AUX  TO  MES-CB2
           MOVE     ANO-AUX  TO  ANO-CB2.

       ROT-100-000.
           MOVE     CLI-PRM (IND1 + 1)  TO IND2
           MOVE     LET-TAB (IND2)  TO  LET-NOM (IND1)
           ADD      1   TO   IND1
           IF  IND1  <  36
               GO  TO        ROT-100-000.
           MOVE     SPACES   TO  LET-NOM (IND1)
           MOVE     NOM-TAB  TO  NOM-CB1.

       ROT-100-020.
           CLOSE    CADCTR   CADPRM
           OPEN     INPUT    CADEST
                    CADTNQ   CADVDA
           DISPLAY  COL-001
           MOVE     ZEROS    TO  CHV-EST  IND1
           START    CADEST   KEY GREATER  CHV-EST.

       ROT-100-030.
           READ     CADEST   NEXT AT END
                    GO  TO   ROT-100-040.
           IF  PRX-EST   >   0
               GO  TO        ROT-100-040.
           MOVE     CST-EST  TO  CST-TAB (SUF-EST)
           MOVE     VDA-EST  TO  VDA-TAB (SUF-EST)
           GO  TO   ROT-100-030.

       ROT-100-040.
           ADD      1    TO  IND1
           IF  IND1   >  10   OR    CMB-PR3 (IND1) = 0
               GO  TO               ROT-100-050.
           MOVE     CMB-PR3 (IND1)  TO  IND2
           MOVE     CST-PR3 (IND1)  TO  CST-TAB (IND2)
           GO  TO   ROT-100-040.

       ROT-100-050.
           MOVE     ZEROS    TO  CHV-TNQ
                                 TOT-CST  TOT-VDA
           START    CADTNQ   KEY GREATER  CHV-TNQ.

       ROT-100-060.
           READ     CADTNQ   NEXT AT END
                    GO  TO   ROT-100-070.
           COMPUTE  TOT-CST  =   TOT-CST
                 +  SDA-TNQ  *   CST-TAB (CMB-TNQ)
           COMPUTE  TOT-VDA  =   TOT-VDA
                 +  SDA-TNQ  *   VDA-TAB (CMB-TNQ)
           GO  TO   ROT-100-060.

       ROT-100-070.
           MOVE     1        TO  PRX-EST
           MOVE     ZEROS    TO  SUF-EST
           START    CADEST   KEY GREATER  CHV-EST
                    INVALID  KEY GO  TO   ROT-100-090.

       ROT-100-080.
           READ     CADEST   NEXT AT END
                    GO  TO   ROT-100-090.
           COMPUTE  TOT-CST  =   TOT-CST  +  SDO-EST  *  CST-EST
           COMPUTE  TOT-VDA  =   TOT-VDA  +  SDO-EST  *  VDA-EST
           GO  TO   ROT-100-080.

       ROT-100-090.
           DISPLAY  COL-002
           MOVE     SPACES   TO  CHV-VDA
           START    CADVDA   KEY GREATER  CHV-VDA
                    INVALID  KEY GO  TO   ROT-100-110.

       ROT-100-100.
           READ     CADVDA   NEXT AT END
                    GO  TO   ROT-100-110.
           IF  ATV-VDA   >   2
               GO  TO        ROT-100-110.
           MOVE     PRX-VDA  TO  PRX-EST
           MOVE     SUF-VDA  TO  SUF-EST
           READ     CADEST
           COMPUTE  TOT-CST  =   TOT-CST  -  SDO-EST  *  CST-EST
                             +  (SDO-EST  -  QTD-VDA) *  CST-EST
           COMPUTE  TOT-VDA  =   TOT-VDA  -  SDO-EST  *  VDA-EST
                             +  (SDO-EST  -  QTD-VDA) *  VDA-EST
           GO  TO   ROT-100-100.

       ROT-100-110.
           CLOSE    CADEST   CADTNQ
                             CADVDA
           OPEN     INPUT    CADPRS
                             CADSDO
           DISPLAY  COL-003
           MOVE     1        TO  IND1  IND2.

       ROT-100-120.
           MOVE     SPACES   TO  ATV-TAB (IND1, IND2)
           MOVE     ZEROS    TO  QTD-TAB (IND1, IND2)
                                 QTM-TAB (IND1, IND2)
                                 VDD-TAB (IND1, IND2)
                                 VDM-TAB (IND1, IND2)
                                 SDO-TAB (IND1, IND2)
           ADD      1        TO  IND2
           IF  IND2     <    10
               GO  TO        ROT-100-120.
           ADD      1        TO  IND1
           IF  IND1     <    10
               MOVE     1    TO  IND2
               GO  TO        ROT-100-120.
           MOVE     SPACES   TO  CHV-PRS
           MOVE     ZEROS    TO  ACM-QTD  ACM-QTM
                                 ACM-MED  ACM-DIF
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-100-130.
           READ     CADPRS   NEXT AT END
                    GO  TO   ROT-100-140.
           MOVE     ATV-PRS  TO  IND1
           MOVE     PRX-PRS  TO  IND2
           MOVE     DES-PRS  TO  ATV-TAB (IND1, IND2)
           ADD      QTD-PRS  TO  QTD-TAB (IND1, IND2)
           ADD      QTD-PRS  TO  QTM-TAB (IND1, IND2)
           ADD      QTM-PRS  TO  QTM-TAB (IND1, IND2)
           ADD      VDD-PRS  TO  VDD-TAB (IND1, IND2)
           ADD      VDD-PRS  TO  VDM-TAB (IND1, IND2)
           ADD      VDM-PRS  TO  VDM-TAB (IND1, IND2)
           GO  TO   ROT-100-130.

       ROT-100-140.
           DISPLAY  COL-004
           READ     CADSDO
           MOVE     1        TO  IND1
           MOVE     ZEROS    TO  ACM-CRD.

       ROT-100-150.
           COMPUTE  SDO-TAB (5, IND1)  =   CRD-SDO (IND1)  -
                    VDD-TAB (4, IND1)  +   VDD-TAB (5, IND1)
           MOVE     SDO-TAB (5, IND1)  TO  SDO-PR4 (IND1)
           ADD      SDO-TAB (5, IND1)  TO  ACM-CRD
           ADD      1    TO     IND1
           IF  IND1  <   06
               GO  TO    ROT-100-150.
           CLOSE    CADPRS       CADSDO
           DISPLAY  COL-005      LPA-001
                    TLA-002      MEN-002
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO  NOT  =   01
               GO  TO        ROT-200-000.
           MOVE     6        TO  PRM-001
           CHAIN   "DYN200"  USING   PRM-001.

       ROT-200-000.
           DISPLAY  MEN-003
           OPEN     OUTPUT       RELATO
           WRITE    REG-REL FROM CAB-001  AFTER  0
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-003
           MOVE     "<+>"    TO  SNL-RG1
           MOVE     1        TO  IND1  IND2  IND3.

       ROT-200-010.
           DIVIDE   QTM-TAB (IND1, IND2)  BY  DIA-CTR  GIVING  MED-AUX
           MOVE     ATV-TAB (IND1, IND2)  TO  DES-RG1
           MOVE     VDD-TAB (IND1, IND2)  TO  VDD-RG1
           MOVE     QTD-TAB (IND1, IND2)  TO  QTD-RG2
           MOVE     QTM-TAB (IND1, IND2)  TO  QTM-RG2
           ADD      QTD-TAB (IND1, IND2)  TO  ACM-QTD
           ADD      QTM-TAB (IND1, IND2)  TO  ACM-QTM
           ADD      VDD-TAB (IND1, IND2)  TO  ACM-DIF
           ADD      MED-AUX  TO  ACM-MED
           MOVE     MED-AUX  TO  MED-RG2
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-002  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  IND2
           IF  IND2    <     10      AND
               ATV-TAB (IND1, IND2)  NOT  =   SPACES
               GO  TO        ROT-200-010.
           MOVE     ACM-QTD  TO  QTD-RG2
           MOVE     ACM-QTM  TO  QTM-RG2
           MOVE     ACM-MED  TO  MED-RG2
           ADD      1        TO  IND1
           MOVE     1        TO  IND2.

       ROT-200-020.
           MOVE     ATV-TAB (IND1, IND2)  TO  DES-RG1
           MOVE     VDD-TAB (IND1, IND2)  TO  VDD-RG1
           ADD      VDD-TAB (IND1, IND2)  TO  ACM-DIF
           IF  IND3  =  1  OR  3
               MOVE     ALL  "-" TO  RG2-DT1.
           IF  IND3  =  2
               MOVE     REG-002  TO  RG2-DT1.
           MOVE     REG-001  TO  RG1-DT1
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  IND2
           ADD      1        TO  IND3
           MOVE     SPACES   TO  RG2-DT1
           IF  IND2    <     10      AND
               ATV-TAB (IND1, IND2)  NOT  =   SPACES
               GO  TO        ROT-200-020.
           ADD      1        TO  IND1
           MOVE     1        TO  IND2
           IF  IND1    <     4       AND
               ATV-TAB (IND1, IND2)  NOT  =   SPACES
               GO  TO        ROT-200-020.
           MOVE     4        TO  IND1.

       ROT-200-030.
           MOVE     ALL "-"  TO  RG1-DT1  RG2-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     "<=>"    TO  SNL-RG1
           MOVE     TP1-AUX  TO  DES-RG1
           MOVE     ACM-DIF  TO  VDD-RG1
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-003  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     SPACES   TO  RG1-DT1
           MOVE     ALL "-"  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     "<+>"    TO  SNL-RG1.

       ROT-200-040.
           MOVE     ATV-TAB (IND1, IND2)  TO  DES-RG1
           MOVE     VDD-TAB (IND1, IND2)  TO  VDD-RG1
           MOVE     VDM-TAB (IND1, IND2)  TO  VDM-RG4
           MOVE     SDO-TAB (IND1, IND2)  TO  SDO-RG4
           ADD      VDD-TAB (IND1, IND2)  TO  ACM-DIF
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-004  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001.

       ROT-200-050.
           ADD      1        TO  IND2
           IF  IND2    >     09
               GO  TO        ROT-200-060.
           IF  ATV-TAB (IND1, IND2)  NOT  =   SPACES
               GO  TO        ROT-200-040.
           GO  TO   ROT-200-050.

       ROT-200-060.
           MOVE     ALL "-"  TO  RG1-DT1
           MOVE     SPACES   TO  RG2-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     "<=>"    TO  SNL-RG1
           MOVE     TP2-AUX  TO  DES-RG1
           MOVE     ACM-DIF  TO  VDD-RG1
           MOVE     REG-001  TO  RG1-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     SPACES   TO  RG1-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     "<->"    TO  SNL-RG1
           ADD      1        TO  IND1
           MOVE     1        TO  IND2.

       ROT-200-070.
           MOVE     ATV-TAB (IND1, IND2)  TO  DES-RG1
           MOVE     VDD-TAB (IND1, IND2)  TO  VDD-RG1
           MOVE     VDM-TAB (IND1, IND2)  TO  VDM-RG4
           MOVE     SDO-TAB (IND1, IND2)  TO  SDO-RG4
           SUBTRACT VDD-TAB (IND1, IND2) FROM ACM-DIF
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-004  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001.

       ROT-200-080.
           ADD      1        TO  IND2
           IF  IND2    >     09
               GO  TO        ROT-200-090.
           IF  ATV-TAB (IND1, IND2)  NOT  =   SPACES
               GO  TO        ROT-200-070.
           GO  TO   ROT-200-080.

       ROT-200-090.
           ADD      1        TO  IND1
           MOVE     1        TO  IND2
           IF  IND1    <     08      AND
               ATV-TAB (IND1, IND2)  NOT  =  SPACES
               GO  TO        ROT-200-070.
           MOVE     ALL "-"  TO  RG1-DT1  RG2-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     "<=>"    TO  SNL-RG1
           MOVE     TP3-AUX  TO  DES-RG1
           MOVE     ACM-DIF  TO  VDD-RG1
           MOVE     ZEROS    TO  VDM-RG4
           MOVE     ACM-CRD  TO  SDO-RG4
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-004  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     SPACES   TO  RG1-DT1  RG2-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     "<+>"    TO  SNL-RG1
           MOVE     9        TO  IND1
           MOVE     1        TO  IND2.

       ROT-200-100.
           MOVE     ATV-TAB (IND1, IND2)  TO  DES-RG1
           MOVE     VDD-TAB (IND1, IND2)  TO  VDD-RG1
           MOVE     VDM-TAB (IND1, IND2)  TO  VDM-RG4
           MOVE     SDO-TAB (IND1, IND2)  TO  SDO-RG4
           ADD      VDD-TAB (IND1, IND2)  TO  ACM-DIF
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-004  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001.
           ADD      1        TO  IND2
           IF  IND2    <     10      AND
               ATV-TAB (IND1, IND2)  NOT  =   SPACES
               GO  TO        ROT-200-100.
           MOVE     ALL "-"  TO  RG1-DT1
           MOVE     SPACES   TO  RG2-DT1
           WRITE    REG-REL FROM DET-001.

       ROT-200-110.
           MOVE     "<=>"    TO  SNL-RG1
           MOVE     TP4-AUX  TO  DES-RG1
           MOVE     ACM-DIF  TO  VDD-RG1
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-005  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     SPACES   TO  RG1-DT1
           MOVE     ALL "-"  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001.

       ROT-200-120.
           MOVE     "<+>"    TO  SNL-RG1
           MOVE     TP5-AUX  TO  DES-RG1
           MOVE     SDA-SDO  TO  VDD-RG1
           MOVE     "CUSTO"  TO  TPO-RG6
           MOVE     TOT-CST  TO  VAL-RG6
           MOVE     REG-001  TO  RG1-DT1
           MOVE     REG-006  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001.

       ROT-200-130.
           MOVE     "<->"    TO  SNL-RG1
           MOVE     TP6-AUX  TO  DES-RG1
           MOVE     VAL-PR2  TO  VDD-RG1
           MOVE     REG-001  TO  RG1-DT1
           MOVE     SPACES   TO  RG2-DT1
           WRITE    REG-REL FROM DET-001.

       ROT-200-140.
           MOVE     "VENDA"  TO  TPO-RG6
           MOVE     TOT-VDA  TO  VAL-RG6
           MOVE     ALL "-"  TO  RG1-DT1
           MOVE     REG-006  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001.

       ROT-200-150.
           MOVE     "<=>"    TO  SNL-RG1
           MOVE     TP7-AUX  TO  DES-RG1
           COMPUTE  VDD-RG1  =   ACM-DIF
                 +  SDA-SDO  -   VAL-PR2
           MOVE     REG-001  TO  RG1-DT1
           MOVE     ALL "-"  TO  RG2-DT1
           WRITE    REG-REL FROM DET-001
           CLOSE    RELATO
           CHAIN   "DYN226"  USING   PRM-001  PRM-002
                                     PRM-003  PRM-004.
