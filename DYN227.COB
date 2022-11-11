       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN227.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCMB        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CMB.

           SELECT      CADCTR        ASSIGN  TO  DISK.

           SELECT      CADCXA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-CXA.

           SELECT      CADECR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-ECR.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC.

           SELECT      CADLCP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCP.

           SELECT      CADLCX        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCX.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP.

           SELECT      CADLES        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LES.

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-PRS.

           SELECT      CADSDO        ASSIGN  TO  DISK.

           SELECT      CADTNQ        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-TNQ.

           SELECT      CADVDA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-VDA.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCMB      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCMB.DAT".
       01  REG-CMB.
           03  CHV-CMB          PIC  9(01).
           03  DES-CMB          PIC  X(15).
           03  VVM-CMB          PIC  9(11)V99.
           03  LUC-CMB          PIC  9(04)V9999.

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

       FD  CADLDP      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLDP.DAT".
       01  REG-LDP.
           03  CHV-LDP.
               05  PRX-LDP      PIC  9(01).
               05  SUF-LDP      PIC  9(03).
               05  DTA-LDP      PIC  9(06).
               05  LCT-LDP      PIC  9(03).
           03  FLG-LDP          PIC  9(01).
           03  PGT-LDP          PIC  9(06).
           03  HST-LDP          PIC  X(20).
           03  VAL-LDP          PIC S9(10)V99.

       FD  CADLES      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLES.DAT".
       01  REG-LES.
           03  CHV-LES.
               05  PRX-LES      PIC  9(01).
               05  SUF-LES      PIC  9(03).
               05  DTA-LES      PIC  9(06).
               05  LCT-LES      PIC  9(03).
           03  SDA-LES          PIC S9(06)V9.
           03  SDC-LES          PIC  9(06)V9.

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
           03  PRX-PRS          PIC  9(01).
           03  QTD-PRS          PIC  9(06)V9.
           03  CTD-PRS          PIC  9(10)V99.
           03  VDD-PRS          PIC S9(10)V99.
           03  QTM-PRS          PIC  9(07)V9.
           03  CTM-PRS          PIC  9(11)V99.
           03  VDM-PRS          PIC S9(11)V99.

       FD  CADSDO      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADSDO.DAT".
       01  REG-SDO.
           03  SDA-SDO          PIC  9(10)V99.
           03  CRD-SDO          PIC S9(11)V99  OCCURS  05.

       FD  CADTNQ      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADTNQ.DAT".
       01  REG-TNQ.
           03  CHV-TNQ          PIC  9(02).
           03  CMB-TNQ          PIC  9(01).
           03  SDI-TNQ          PIC S9(05)V9.
           03  SDA-TNQ          PIC S9(05)V9.

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
           03  TECLADO          PIC  X(02).
           03  ATV-AUX          PIC  9(01).
           03  PRX-AUX          PIC  9(01).
           03  ACM-LIN          PIC  9(02).
           03  PCT-AUX          PIC  9(02)V99.
           03  ACM-VAL          PIC  9(11)V99.
           03  ACM-SDI          PIC S9(06)V9   OCCURS  09.
           03  ACM-CMP          PIC S9(06)V9   OCCURS  09.
           03  ACM-SDO          PIC S9(06)V9   OCCURS  09.
           03  ACM-QVD          PIC  9(06)V9   OCCURS  09.
           03  ACM-VDA          PIC  9(11)V99  OCCURS  09.

           03  HST-AUX.
               05  FILLER       PIC  X(13)  VALUE "COMPRAS - NF".
               05  NTF-AUX      PIC  999.999.

           03  DEP-AUX          PIC  X(20)  VALUE "DEP. SALDO DO CAIXA".
           03  ALG-AUX          PIC  X(20)  VALUE "ALUGUEL S/ COMPRAS ".

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

           03  DTA-VCT.
               05  ANO-VCT      PIC  9(02).
               05  MES-VCT      PIC  9(02).
               05  DIA-VCT      PIC  9(02).

       01  CAB-001.
           03  FILLER           PIC  X(22)  VALUE  "-DYNAMIC-".
           03  NOM-CB1          PIC  X(48).
           03  FILLER           PIC  X(08)  VALUE  "FOLHA :".
           03  PAG-CB1          PIC  9(02).

       01  CAB-002.
           03  REG-CB2          PIC  X(72)  VALUE "ANALITICO DE VENDAS".
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "  CODIGO DESCRICAO / HISTORICO     QUANTIDADE    VALOR DO IT
           "EM    TOTAL DO GRUPO".

       01  CAB-005.
           03  FILLER           PIC  X(02)  VALUE  "-".
           03  ATV-CB5          PIC  X(78).

       01  DET-001.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  PRX-DT1          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DT1          PIC  9(03).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  DES-DT1          PIC  X(27).
           03  QTD-DT1          PIC  ZZZ.ZZ9,9         BLANK WHEN ZEROS.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  VAL-DT1          PIC  Z.ZZZ.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  TOT-DT1          PIC  ZZ.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.

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
           03  LINE 10 COLUMN 26 PIC X(27) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄ¿"   BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 26 PIC X(27) FROM " ³
               "  ³"   BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 26 PIC X(27) FROM " ³  ANALITICO DE VENDAS
               "  ³"   BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 26 PIC X(27) FROM " ³
               "  ³"   BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 26 PIC X(27) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÙ"   BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 28 PIC X(27) FROM SPACES.

       01  TLA-002.
           03  LINE 09 COLUMN 11 PIC X(58) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 11 PIC X(58) FROM " ³  0%.................
               "....50%....................100%  ³" BACKGROUND-COLOR 04.
           03  LINE 11 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 12 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³" BACKGROUND-COLOR 04.
           03  LINE 13 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 11 PIC X(58) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 04.
           03  LINE 14 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 13 PIC X(58) FROM SPACES.

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

       01  LPA-001.
           03  LINE 09 COLUMN 11 PIC X(58) FROM SPACES
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
           03  LINE 15 COLUMN 13 PIC X(58) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001  LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA EMITIR O AN
                    "ALITICO ? [S/N]  [ ]  -  [ESC] RETORNA ...       "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002  LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRI
                    "MINDO O ANALITICO DE VENDAS ...                  "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003  LINE 22 COLUMN 14 PIC X(67) FROM "CONFIRMA TODOS OS
                    "RELATORIOS ? [S/N]  [ ]  -  [ESC] RETORNA ...    "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004  LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ATUAL
                    "IZANDO ARQUIVOS ...                              "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 50 PIC X(01) USING RSP-OPC  AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002  LINE 22 COLUMN 53 PIC X(01) USING RSP-OPC  AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001  PRM-002
                                           PRM-003  PRM-004.
       ROT-000-000.
           OPEN     I-O      CADCTR
                    INPUT    CADVDA
           READ     CADCTR
           MOVE     DTC-CTR  TO  DTA-AUX
           MOVE     PCT-CTR  TO  PCT-AUX
           MOVE     SPACES   TO  CHV-VDA
           START    CADVDA   KEY GREATER  CHV-VDA
                    INVALID  KEY GO  TO   ROT-000-030.

       ROT-000-010.
           DISPLAY  LPA-001
                    TLA-001  MEN-001.

       ROT-000-020.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-060.
           IF  RSP-OPC   =   "S"
               GO  TO        ROT-100-000.
           IF  RSP-OPC  NOT  =   "N"
               GO  TO        ROT-000-020.

       ROT-000-030.
           DISPLAY  TLA-002  MEN-003.

       ROT-000-040.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-050.
           IF  RSP-OPC   =   "S"
               GO  TO        ROT-200-000.
           IF  RSP-OPC  NOT  =   "N"
               GO  TO        ROT-000-040.
           MOVE     6    TO  PRM-001
           CHAIN   "DYN200"  USING   PRM-001.

       ROT-000-050.
           MOVE     SPACES   TO  CHV-VDA
           START    CADVDA   KEY GREATER  CHV-VDA
                    INVALID  KEY GO  TO   ROT-000-060.
           GO  TO   ROT-000-010.

       ROT-000-060.
           CHAIN   "DYN226"  USING  PRM-001  PRM-002
                                    PRM-003  PRM-004.

       ROT-100-000.
           DISPLAY  MEN-002
           OPEN     INPUT    CADEST
                             CADPRM
                    OUTPUT   RELATO
           READ     CADPRM
           MOVE     1   TO   IND1.

       ROT-100-010.
           MOVE     CLI-PRM (IND1 + 1)  TO IND2
           MOVE     LET-TAB (IND2)  TO  LET-NOM (IND1)
           ADD      1   TO   IND1
           IF  IND1  <  36
               GO  TO        ROT-100-010.
           MOVE     SPACES   TO  LET-NOM (IND1)
           MOVE     NOM-TAB  TO  NOM-CB1.

       ROT-100-030.
           MOVE     1        TO  PAG-CB1
           MOVE     DIA-AUX  TO  DIA-CB2
           MOVE     MES-AUX  TO  MES-CB2
           MOVE     ANO-AUX  TO  ANO-CB2
           READ     CADVDA  NEXT
           MOVE     ATV-VDA  TO  ATV-AUX
           MOVE     PRX-VDA  TO  PRX-AUX
           MOVE     ZEROS    TO  ACM-VAL  TOT-DT1
           WRITE    REG-REL FROM CAB-001  AFTER 0.

       ROT-100-040.
           MOVE     DES-VDA  TO  ATV-CB5
           WRITE    REG-REL FROM CAB-002  AFTER 2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-005
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL
           MOVE     2        TO  ACM-LIN.

       ROT-100-050.
           MOVE     PRX-VDA  TO  PRX-DT1
           MOVE     SUF-VDA  TO  SUF-DT1
           MOVE     HST-VDA  TO  DES-DT1
           IF  ATV-VDA  NOT  =   2
               GO  TO        ROT-100-060.
           MOVE     PRX-VDA  TO  PRX-EST
           MOVE     SUF-VDA  TO  SUF-EST
           READ     CADEST
           MOVE     DES-EST  TO  DES-DT1.

       ROT-100-060.
           MOVE     QTD-VDA  TO  QTD-DT1
           MOVE     VAL-VDA  TO  VAL-DT1
           ADD      VAL-VDA  TO  ACM-VAL
           READ     CADVDA   NEXT AT END
                    GO  TO   ROT-100-090.
           IF  ATV-VDA  NOT  =   ATV-AUX  OR
              (PRX-VDA  NOT  =   PRX-AUX
               AND  ATV-AUX  <   4)
               GO  TO        ROT-100-080.
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  ACM-LIN
           IF  ACM-LIN   <   52
               GO  TO        ROT-100-050.

       ROT-100-070.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           GO  TO   ROT-100-040.

       ROT-100-080.
           MOVE     ATV-VDA  TO  ATV-AUX
           MOVE     PRX-VDA  TO  PRX-AUX
           MOVE     ACM-VAL  TO  TOT-DT1
           WRITE    REG-REL FROM DET-001
           MOVE     ZEROS    TO  ACM-VAL  TOT-DT1
           IF  ACM-LIN   >   50
               GO  TO        ROT-100-070.
           MOVE     DES-VDA  TO  ATV-CB5
           WRITE    REG-REL FROM CAB-005  AFTER 2
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL
           ADD      4        TO  ACM-LIN
           GO  TO   ROT-100-050.

       ROT-100-090.
           MOVE     ACM-VAL  TO  TOT-DT1
           WRITE    REG-REL FROM DET-001
           CLOSE    CADEST
                    CADPRM
                    RELATO
           GO  TO   ROT-000-030.

       ROT-200-000.
           DISPLAY  MEN-004
           OPEN     I-O      CADEST
                    CADLBC   CADLDP
           IF  VAL-PR2   =   0
               GO  TO        ROT-200-020.
           MOVE     BCO-PR2  TO  BCO-LBC
           MOVE     DTA-AUX  TO  DTA-LBC
           MOVE     ZEROS    TO  LCT-LBC  FLG-LBC  DOC-LBC
           MOVE     DEP-AUX  TO  HST-LBC
           MOVE     1        TO  COD-LBC
           MOVE     VAL-PR2  TO  VAL-LBC.

       ROT-200-010.
           ADD      1        TO  LCT-LBC
           WRITE    REG-LBC  INVALID KEY
                    GO  TO   ROT-200-010.

       ROT-200-020.
           MOVE     1        TO  IND1
           MOVE     SPACES   TO  CHV-VDA
           START    CADVDA   KEY GREATER  CHV-VDA
                    INVALID  KEY GO  TO   ROT-200-080.

       ROT-200-030.
           READ     CADVDA   NEXT AT END
                    GO  TO   ROT-200-080.
           IF  ATV-VDA   =   6
               GO  TO        ROT-200-040.
           IF  ATV-VDA   =   7
               GO  TO        ROT-200-060.
           IF  ATV-VDA  NOT  =   2
               GO  TO        ROT-200-030.
           MOVE     PRX-VDA  TO  PRX-EST
           MOVE     SUF-VDA  TO  SUF-EST
           READ     CADEST
           COMPUTE  SDO-EST  =   SDO-EST  -  QTD-VDA
           ADD      QTD-VDA  TO  QVM-EST
           ADD      QTD-VDA  TO  QVA-EST
           REWRITE  REG-EST
           GO  TO   ROT-200-030.

       ROT-200-040.
           MOVE     PRX-VDA  TO  PRX-LDP
           MOVE     SUF-VDA  TO  SUF-LDP
           MOVE     DTA-AUX  TO  DTA-LDP  PGT-LDP
           MOVE     LCT-VDA  TO  LCT-LDP
           MOVE     2        TO  FLG-LDP
           MOVE     HST-VDA  TO  HST-LDP
           MOVE     VAL-VDA  TO  VAL-LDP.

       ROT-200-050.
           ADD      1        TO  LCT-LDP
           WRITE    REG-LDP  INVALID KEY
                    GO  TO   ROT-200-050.
           GO  TO   ROT-200-030.

       ROT-200-060.
           MOVE     SUF-VDA  TO  BCO-LBC
           MOVE     DTA-AUX  TO  DTA-LBC
           MOVE     LCT-VDA  TO  LCT-LBC
           MOVE     ZEROS    TO  FLG-LBC  DOC-LBC
           MOVE     HST-VDA  TO  HST-LBC
           MOVE     1        TO  COD-LBC
           MOVE     VAL-VDA  TO  VAL-LBC.

       ROT-200-070.
           ADD      1        TO  LCT-LBC
           WRITE    REG-LBC  INVALID KEY
                    GO  TO   ROT-200-070.
           GO  TO   ROT-200-030.

       ROT-200-080.
           DISPLAY  COL-001
           ADD      1        TO  DIA-CTR
           MOVE     DTC-CTR  TO  DTD-CTR
           REWRITE  REG-CTR
           CLOSE    CADCTR   CADLBC
                             CADVDA
           OPEN     OUTPUT   CADLCX
                             CADVDA
           CLOSE    CADLCX   CADVDA
           OPEN     I-O      CADCMB
                             CADLCP.

       ROT-200-090.
           MOVE     ZEROS    TO  ACM-SDI (IND1)
                                 ACM-QVD (IND1)  ACM-VDA (IND1)
                                 ACM-CMP (IND1)  ACM-SDO (IND1)
           ADD      1        TO  IND1
           IF  IND1     <    10
               GO  TO        ROT-200-090.
           MOVE     ZEROS    TO  ACM-VAL  IND1.

       ROT-200-100.
           ADD      1        TO  IND1
           IF  IND1  >  10   OR  CMB-PR3 (IND1) =  0
               GO  TO        ROT-200-160.
           MOVE     DTA-AUX  TO  DTA-LCP
           MOVE     ZEROS    TO  PRX-LCP  LCT-LCP
           MOVE     CMB-PR3 (IND1)   TO   SUF-LCP
           MOVE     NTF-PR3 (IND1)   TO   NTF-LCP
           MOVE     QTD-PR3 (IND1)   TO   QTD-LCP
           MOVE     CST-PR3 (IND1)   TO   CST-LCP
           MOVE     CMB-PR3 (IND1)   TO   IND2
           ADD      QTD-PR3 (IND1)   TO   ACM-CMP (IND2).

       ROT-200-110.
           ADD      1        TO  LCT-LCP
           WRITE    REG-LCP  INVALID KEY
                    GO  TO   ROT-200-110.
           MOVE     VCT-PR3 (IND1)   TO   DTA-VCT
           IF  DIA-VCT   =   0
               GO  TO        ROT-200-130.
           MOVE     6        TO  PRX-LDP
           MOVE     1        TO  FLG-LDP
           MOVE     DTA-AUX  TO  DTA-LDP
           MOVE     ZEROS    TO  LCT-LDP
           MOVE     CMB-PR3 (IND1)   TO   SUF-LDP
           MOVE     NTF-PR3 (IND1)   TO   NTF-AUX
           MOVE     VCT-PR3 (IND1)   TO   PGT-LDP
           MOVE     VAL-PR3 (IND1)   TO   VAL-LDP
           MOVE     HST-AUX          TO   HST-LDP.

       ROT-200-120.
           ADD      1        TO  LCT-LDP
           WRITE    REG-LDP  INVALID KEY
                    GO  TO   ROT-200-120.

       ROT-200-130.
           MOVE     ZEROS    TO  PRX-EST
           MOVE     CMB-PR3 (IND1)   TO   SUF-EST
           READ     CADEST
           COMPUTE  CTM-EST  =  (CTM-EST        *  SDO-EST
                             +   CST-PR3 (IND1) *  QTD-PR3 (IND1))
                             /  (SDO-EST        +  QTD-PR3 (IND1))
           COMPUTE  PCT-EST  =  (VDA-EST        -  CTM-EST)
                             *   100            /  VDA-EST
           MOVE     CST-PR3 (IND1)   TO   CST-EST
           ADD      QTD-PR3 (IND1)   TO   SDO-EST
           REWRITE  REG-EST
           IF  PCT-AUX   =   ZEROS
               GO  TO        ROT-200-100.

       ROT-200-140.
           MOVE     CMB-PR3 (IND1)   TO   CHV-CMB
           READ     CADCMB
           MOVE     3        TO  PRX-LDP
           MOVE     1        TO  SUF-LDP  FLG-LDP
           MOVE     DTA-AUX  TO  DTA-LDP
           MOVE     1        TO  DIA-AUX
           ADD      1        TO  MES-AUX
           IF  MES-AUX   >   12
               MOVE      1   TO  MES-AUX
               ADD       1   TO  ANO-AUX.
           MOVE     DTA-AUX  TO  PGT-LDP
           MOVE     DTA-LDP  TO  DTA-AUX
           MOVE     PGT-LDP  TO  DTA-LDP
           MOVE     ZEROS    TO  LCT-LDP
           READ     CADLDP   INVALID KEY
                    GO  TO   ROT-200-150.
           COMPUTE  VAL-LDP  =   VAL-LDP
                +   LUC-CMB  *   QTD-PR3  (IND1)
                             *   PCT-CTR  / 100.
           REWRITE  REG-LDP
           GO  TO   ROT-200-100.

       ROT-200-150.
           MOVE     ALG-AUX  TO  HST-LDP
           COMPUTE  VAL-LDP  =   LUC-CMB  *   QTD-PR3  (IND1)
                                          *   PCT-CTR  / 100.
           WRITE    REG-LDP
           GO  TO   ROT-200-100.

       ROT-200-160.
           CLOSE    CADLCP   CADLDP
           OPEN     I-O      CADPRS
                             CADTNQ
           DISPLAY  COL-002.

       ROT-200-170.
           READ     CADTNQ   AT  END
                    GO  TO   ROT-200-180.
           ADD      SDI-TNQ  TO  ACM-SDI (CMB-TNQ)
           ADD      SDA-TNQ  TO  ACM-SDO (CMB-TNQ)
           MOVE     SDA-TNQ  TO  SDI-TNQ
           MOVE     ZEROS    TO  SDA-TNQ
           REWRITE  REG-TNQ
           GO  TO   ROT-200-170.

       ROT-200-180.
           READ     CADPRS   AT  END
                    GO  TO   ROT-200-200.
           IF  ATV-PRS   >   1
               GO  TO        ROT-200-190.
           ADD      QTD-PRS  TO  ACM-QVD (PRX-PRS)
           ADD      VDD-PRS  TO  ACM-VDA (PRX-PRS).

       ROT-200-190.
           IF  ATV-PRS   =   8
               ADD  VDD-PRS  TO  ACM-VAL.
           ADD      QTD-PRS  TO  QTM-PRS
           ADD      CTD-PRS  TO  CTM-PRS
           ADD      VDD-PRS  TO  VDM-PRS
           MOVE     ZEROS    TO  QTD-PRS
                                 CTD-PRS
                                 VDD-PRS
           REWRITE  REG-PRS
           GO  TO   ROT-200-180.

       ROT-200-200.
           MOVE     ZEROS    TO  CHV-CMB
           START    CADCMB   KEY GREATER  CHV-CMB.

       ROT-200-210.
           READ     CADCMB   NEXT AT END
                    GO  TO   ROT-200-220.
           MOVE     ZEROS    TO  PRX-EST
           MOVE     CHV-CMB  TO  SUF-EST
           READ     CADEST
           ADD      ACM-VDA (CHV-CMB) TO  VVM-CMB
           MOVE     ACM-SDO (CHV-CMB) TO  SDO-EST
           ADD      ACM-QVD (CHV-CMB) TO  QVM-EST
           ADD      ACM-QVD (CHV-CMB) TO  QVA-EST
           REWRITE  REG-CMB
           REWRITE  REG-EST
           GO  TO   ROT-200-210.

       ROT-200-220.
           DISPLAY  COL-003
           MOVE     0   TO   IND1
           CLOSE    CADCMB   CADEST
                    CADPRS   CADTNQ
           OPEN     I-O      CADCXA
                             CADLES.

       ROT-200-230.
           ADD      1        TO  IND1
           IF  IND1  >  09
               GO  TO        ROT-200-250.
           MOVE     ACM-SDO  (IND1)  TO   SDC-LES
           COMPUTE  SDA-LES  =   ACM-SDI  (IND1)
                             +   ACM-CMP  (IND1)
                             -   ACM-QVD  (IND1)
           IF  SDC-LES   =   SDA-LES
               GO  TO        ROT-200-230.
           MOVE     ZEROS    TO  PRX-LES  LCT-LES
           MOVE     IND1     TO  SUF-LES
           MOVE     DTA-AUX  TO  DTA-LES.

       ROT-200-240.
           ADD      1        TO  LCT-LES
           WRITE    REG-LES  INVALID KEY
                    GO  TO   ROT-200-240.
           GO  TO   ROT-200-230.

       ROT-200-250.
           READ     CADCXA   AT  END
                    GO  TO   ROT-200-260.
           MOVE     1        TO  POS-CXA
           MOVE     ZEROS    TO  DTA-CXA
           REWRITE  REG-CXA
           GO  TO   ROT-200-250.

       ROT-200-260.
           DISPLAY  COL-004
           MOVE     1   TO   IND1
           CLOSE    CADCXA   CADLES
           OPEN     I-O      CADECR
                             CADSDO
           READ     CADSDO.

       ROT-200-270.
           MOVE     SDO-PR4 (IND1)   TO  CRD-SDO (IND1)
           ADD      1    TO  IND1
           IF  IND1  <   06
               GO  TO        ROT-200-270.
           COMPUTE  SDA-SDO  =   SDA-SDO
                 +  ACM-VAL  -   VAL-PR2
           REWRITE  REG-SDO.

       ROT-200-280.
           READ     CADECR   AT  END
                    GO  TO   ROT-200-290.
           MOVE     ICL-ECR  TO  IDL-ECR
           MOVE     ZEROS    TO  FCL-ECR  FLG-ECR
                                 ITL-ECR  FTL-ECR
                                 AFR-ECR  EXT-ECR
           REWRITE  REG-ECR
           GO  TO   ROT-200-280.

       ROT-200-290.
           MOVE     6   TO   PRM-001
           CLOSE    CADECR   CADSDO
           DISPLAY  COL-005
           CHAIN   "DYN200"  USING  PRM-001.
