       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN220.
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

           SELECT      CADCMB        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CMB.

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

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS.

           SELECT      CADSRT        ASSIGN  TO  DISK.

           SELECT      CADVDA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-VDA.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADBCO      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADBCO.DAT".
       01  REG-BCO.
           03  CHV-BCO          PIC  9(03).
           03  NOM-BCO          PIC  X(15).
           03  SDI-BCO          PIC S9(11)V99.

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
               05  COD-LDP      PIC  9(04).
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

       SD  CADSRT.

       01  REG-SRT.
           03  CHV-SRT          PIC  X(12).
           03  DES-SRT          PIC  X(40).

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

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 01 PIC X(67) FROM
               " ZERADOR DE ACUMULADORES  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 11 PIC X(58) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 07 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 07 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 11 PIC X(58) FROM " ³                 A T
               "E N C A O   ! ! !                ³"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 08 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 09 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 11 PIC X(58) FROM " ³  CERTIFIQUE-SE DE QU
               "E JA FOI IMPRESSO OU COPIADO EM  ³"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 10 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 11 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 11 PIC X(58) FROM " ³  DISQUETE O MOVIMENT
               "O DO MES A SER FINALIZADO, POIS  ³"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 12 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 13 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 11 PIC X(58) FROM " ³  AO CONFIRMAR ESTA O
               "PCAO, SERAO ELIMINADOS TODOS OS  ³"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 14 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 15 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 11 PIC X(58) FROM " ³  LANCAMENTOS, PERMAN
               "ECENDO APENAS OS PENDENTES ...   ³"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 16 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 17 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 11 PIC X(58) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04  FOREGROUND-COLOR  11.
           03  LINE 18 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 13 PIC X(58) FROM SPACES.

       01  TLA-002.
           03  LINE 10 COLUMN 28 PIC X(24) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 28 PIC X(24) FROM " ³                    ³
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 28 PIC X(24) FROM " ³  Data .:   /  /    ³
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 28 PIC X(24) FROM " ³                    ³
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 28 PIC X(24) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 30 PIC X(24) FROM SPACES.

       01  TLA-003.
           03  LINE 06 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 69 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
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
           03  LINE 15 COLUMN 11 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.

       01  COL-TLA.
           03  COL-001  LINE 12 COLUMN 15 PIC X(10)  FROM  SPACES
                        BACKGROUND-COLOR  07.
           03  COL-002  LINE 12 COLUMN 25 PIC X(10)  FROM  SPACES
                        BACKGROUND-COLOR  07.
           03  COL-003  LINE 12 COLUMN 35 PIC X(10)  FROM  SPACES
                        BACKGROUND-COLOR  07.
           03  COL-004  LINE 12 COLUMN 45 PIC X(10)  FROM  SPACES
                        BACKGROUND-COLOR  07.
           03  COL-005  LINE 12 COLUMN 55 PIC X(10)  FROM  SPACES
                        BACKGROUND-COLOR  07.

       01  ENT-001.
           03  DIA-ENT  LINE 12 COLUMN 40 PIC 9(02) USING DIA-AUX
                        BACKGROUND-COLOR  01.
           03  MES-ENT  LINE 12 COLUMN 43 PIC 9(02) USING MES-AUX
                        BACKGROUND-COLOR  01.
           03  ANO-ENT  LINE 12 COLUMN 46 PIC 9(02) USING ANO-AUX
                        BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-MES  LINE 12 COLUMN 43 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-ANO  LINE 12 COLUMN 46 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NTINUAR  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA DO MO
                   "VIMENTO INICIAL  -  [ESC] RETORNA ...      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DATA INCOERENTE !!!
                   "  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "CONFIRMA O ZERAMENT
                   "O ? [S/N]  [ ]  -  [ESC] RETORNA ...       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ZERAND
                   "O ACUMULADORES ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 45 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           OPEN     I-O      CADCTR
           READ     CADCTR
           DISPLAY  TLA-001  MEN-001
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-500-000.
           MOVE     DTI-CTR  TO  DTA-AUX
           ADD      1        TO  MES-AUX
           IF  MES-AUX   >   12
               MOVE      1   TO  MES-AUX
               ADD       1   TO  ANO-AUX.
           DISPLAY  TLA-002      MEN-002.

       ROT-000-020.
           DISPLAY  LPA-MES
           ACCEPT   (12 40)  DIA-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-500-000.
           IF  DIA-AUX   <   1  OR  > 31
               GO  TO        ROT-000-020.
           DISPLAY  DIA-ENT.

       ROT-000-030.
           DISPLAY  LPA-ANO
           ACCEPT   (12 43)  MES-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           IF  MES-AUX   <   1  OR  > 12
               GO  TO        ROT-000-030.
           DISPLAY  MES-ENT.

       ROT-000-040.
           DISPLAY  MEN-002
           ACCEPT   (12 46)  ANO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           DISPLAY  ANO-ENT  MEN-004
           MOVE     DTA-AUX  TO  DTI-CTR.

       ROT-000-050.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-040.
           IF  RSP-OPC   =   "N"
               GO  TO        ROT-500-000.
           IF  RSP-OPC  NOT  =   "S"
               GO  TO        ROT-000-050.

       ROT-100-000.
           DISPLAY  TLA-003  MEN-005
           OPEN     I-O      CADBCO
                    INPUT    CADLBC
           MOVE     ZEROS    TO  CHV-BCO
           START    CADBCO   KEY GREATER  CHV-BCO
                    INVALID  KEY GO  TO   ROT-100-040.

       ROT-100-010.
           READ     CADBCO   NEXT AT END
                    GO  TO   ROT-100-040.
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           START    CADLBC   KEY GREATER  CHV-LBC
                    INVALID  KEY GO  TO   ROT-100-040.

       ROT-100-020.
           READ     CADLBC   NEXT AT END
                    GO  TO   ROT-100-030.
           IF  BCO-LBC  NOT  =   CHV-BCO  OR
               DTA-LBC  NOT  <   DTI-CTR
               GO  TO        ROT-100-030.
           IF  FLG-LBC   =   1
               GO  TO        ROT-100-020.
           IF  COD-LBC   =   1 OR 3 OR 6
               COMPUTE       SDI-BCO   =
                             SDI-BCO   +  VAL-LBC
           ELSE
               COMPUTE       SDI-BCO   =
                             SDI-BCO   -  VAL-LBC.
           GO  TO   ROT-100-020.

       ROT-100-030.
           REWRITE  REG-BCO
           GO  TO   ROT-100-010.

       ROT-100-040.
           SORT     CADSRT   ON  ASCENDING  KEY  CHV-SRT
                    INPUT        PROCEDURE  IS   ROT-100-050
                    OUTPUT       PROCEDURE  IS   ROT-100-090
           DISPLAY  COL-001
           GO  TO   ROT-200-000.

       ROT-100-050  SECTION.

       ROT-100-060.
           MOVE     SPACES   TO  CHV-LBC
           START    CADLBC   KEY GREATER  CHV-LBC
                    INVALID  KEY GO  TO   ROT-100-080.

       ROT-100-070.
           READ     CADLBC   NEXT AT END
                    GO  TO   ROT-100-080.
           IF  FLG-LBC   =   0   AND
               DTA-LBC   <   DTI-CTR
               GO  TO        ROT-100-070.
           MOVE     REG-LBC  TO  REG-SRT
           RELEASE  REG-SRT
           GO  TO   ROT-100-070.

       ROT-100-080.
           CLOSE    CADLBC
           OPEN     OUTPUT   CADLBC.

       ROT-100-090  SECTION.

       ROT-100-100.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-100-110.
           MOVE     REG-SRT  TO  REG-LBC
           WRITE    REG-LBC
           GO  TO   ROT-100-100.

       ROT-100-110.
           CLOSE    CADBCO   CADLBC.

       ROT-100-120  SECTION.

       ROT-200-000.
           OPEN     I-O      CADCMB
                             CADEST
           MOVE     ZEROS    TO  CHV-CMB
           START    CADCMB   KEY GREATER  CHV-CMB.

       ROT-200-010.
           READ     CADCMB   NEXT AT END
                    GO  TO   ROT-200-020.
           MOVE     ZEROS    TO  PRX-EST
                                 VVM-CMB
           MOVE     CHV-CMB  TO  SUF-EST
           READ     CADEST
           COMPUTE  MIN-EST  =   QVM-EST  /  10
           MOVE     ZEROS    TO  QVM-EST
           IF  MES-AUX   =   01
               MOVE  ZEROS   TO  QVA-EST.
           REWRITE  REG-CMB
           REWRITE  REG-EST
           GO  TO   ROT-200-010.

       ROT-200-020.
           MOVE     1        TO  PRX-EST
           MOVE     ZEROS    TO  SUF-EST
           START    CADEST   KEY GREATER  CHV-EST
                    INVALID  KEY GO  TO   ROT-200-040.

       ROT-200-030.
           READ     CADEST   NEXT AT END
                    GO  TO   ROT-200-040.
           COMPUTE  MIN-EST  =   QVM-EST  /  02
           MOVE     ZEROS    TO  QVM-EST
           IF  MES-AUX   =   01
               MOVE  ZEROS   TO  QVA-EST.
           REWRITE  REG-EST
           GO  TO   ROT-200-030.

       ROT-200-040.
           CLOSE    CADCMB   CADEST
           OPEN     OUTPUT   CADLCP  CADLES
           CLOSE    CADLCP   CADLES
           DISPLAY  COL-002.

       ROT-300-000.
           OPEN     I-O      CADCXA
                    CADECR   CADPRS
           MOVE     SPACES   TO  CHV-CXA
           START    CADCXA   KEY GREATER  CHV-CXA.

       ROT-300-010.
           READ     CADCXA   NEXT AT END
                    GO  TO   ROT-300-020.
           MOVE     1        TO  POS-CXA
           MOVE     ZEROS    TO  DTA-CXA
           REWRITE  REG-CXA
           GO  TO   ROT-300-010.

       ROT-300-020.
           MOVE     SPACES   TO  CHV-PRS
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-300-030.
           READ     CADPRS   NEXT AT END
                    GO  TO   ROT-300-040.
           MOVE     ZEROS    TO  QTD-PRS  QTM-PRS
                                 CTD-PRS  CTM-PRS
                                 VDD-PRS  VDM-PRS
           REWRITE  REG-PRS
           GO  TO   ROT-300-030.

       ROT-300-040.
           DISPLAY  COL-003
           MOVE     ZEROS    TO  CHV-ECR
           START    CADECR   KEY GREATER  CHV-ECR.

       ROT-300-050.
           READ     CADECR   NEXT AT END
                    GO  TO   ROT-300-060.
           MOVE     ZEROS    TO  FCL-ECR  FLG-ECR
                                 ITL-ECR  FTL-ECR
                                 AFR-ECR  EXT-ECR
           MOVE     ICL-ECR  TO  IDL-ECR
           REWRITE  REG-ECR
           GO  TO   ROT-300-050.

       ROT-300-060.
           CLOSE    CADECR   CADCXA  CADPRS
           OPEN     OUTPUT   CADLCX  CADVDA
           MOVE     ZEROS      TO   DIA-CTR
                                    DTC-CTR
                                    DTD-CTR
           REWRITE  REG-CTR
           CLOSE    CADCTR   CADLCX  CADVDA
           DISPLAY  COL-004.

       ROT-400-000.
           SORT     CADSRT   ON  ASCENDING  KEY  CHV-SRT
                    INPUT        PROCEDURE  IS   ROT-400-010
                    OUTPUT       PROCEDURE  IS   ROT-400-050.
           GO  TO   ROT-500-000.

       ROT-400-010  SECTION.

       ROT-400-020.
           OPEN     INPUT    CADLDP
           MOVE     SPACES   TO  CHV-LDP
           START    CADLDP   KEY GREATER  CHV-LDP
                    INVALID  KEY GO  TO   ROT-400-040.

       ROT-400-030.
           READ     CADLDP   NEXT AT END
                    GO  TO   ROT-400-040.
           IF  FLG-LDP   =   2   AND
               PGT-LDP   <   DTI-CTR
               GO  TO        ROT-400-030.
           MOVE     REG-LDP  TO  REG-SRT
           RELEASE  REG-SRT
           GO  TO   ROT-400-030.

       ROT-400-040.
           CLOSE    CADLDP
           OPEN     OUTPUT   CADLDP.

       ROT-400-050  SECTION.

       ROT-400-060.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-400-070.
           MOVE     REG-SRT  TO  REG-LDP
           WRITE    REG-LDP
           GO  TO   ROT-400-060.

       ROT-400-070.
           CLOSE    CADLDP
           DISPLAY  COL-005.

       ROT-400-080  SECTION.

       ROT-500-000.
           MOVE     3   TO   PRM-001
           CHAIN   "DYN200"  USING  PRM-001.
