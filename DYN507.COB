       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN507.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCLI        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CLI.

           SELECT      CADCTR        ASSIGN  TO  DISK.

           SELECT      CADFAT        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-FAT.

           SELECT      CADNTA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTA.

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      CADSRT        ASSIGN  TO  DISK.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCLI      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCLI.DAT".
       01  REG-CLI.
           03  CHV-CLI          PIC  X(10).
           03  NOM-CLI          PIC  X(35).
           03  TEL-CLI          PIC  9(12).
           03  CNT-CLI          PIC  X(15).
           03  END-CLI          PIC  X(35).
           03  CID-CLI          PIC  X(20).
           03  CEP-CLI          PIC  9(08).
           03  EST-CLI          PIC  X(02).
           03  CGC-CLI          PIC  9(14).
           03  INS-CLI          PIC  X(15).
           03  CTG-CLI          PIC  9(01).

       FD  CADCTR      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCTR.DAT".
       01  REG-CTR.
           03  DIA-CTR          PIC  9(02).
           03  DTI-CTR          PIC  9(06).
           03  DTC-CTR          PIC  9(06).
           03  DTD-CTR          PIC  9(06).
           03  FAT-CTR          PIC  9(06).
           03  PCT-CTR          PIC  9(02)V99.

       FD  CADFAT      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADFAT.DAT".
       01  REG-FAT.
           03  CHV-FAT          PIC  9(06).
           03  CLI-FAT          PIC  X(10).
           03  DTA-FAT          PIC  9(06).
           03  VAL-FAT          PIC S9(10)V99.

       FD  CADNTA      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADNTA.DAT".
       01  REG-NTA.
           03  CHV-NTA.
               05  CLI-NTA      PIC  X(10).
               05  DTA-NTA      PIC  9(06).
               05  NUM-NTA      PIC  9(06).
           03  PCA-NTA          PIC  X(07).
           03  DES-NTA          PIC  X(20).
           03  VAL-NTA          PIC S9(09)V99.

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

       SD  CADSRT.

       01  REG-SRT.
           03  CHV-SRT.
               05  CLI-SRT      PIC  X(10).
               05  DTA-SRT      PIC  9(06).
               05  NUM-SRT      PIC  9(06).
           03  PCA-SRT          PIC  X(07).
           03  DES-SRT          PIC  X(20).
           03  VAL-SRT          PIC S9(09)V99.

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC  X(80).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ACM-LIN          PIC  9(02).
           03  ACM-QTD          PIC  9(03).
           03  TOT-QTD          PIC  9(04).
           03  ACM-VAL          PIC S9(10)V99.
           03  TOT-VAL          PIC S9(11)V99.

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-LIM.
               05  ANO-LIM      PIC  9(04).
               05  MES-LIM      PIC  9(02).
               05  DIA-LIM      PIC  9(02).

           03  DTA-LCT.
               05  ANO-LCT      PIC  9(04).
               05  MES-LCT      PIC  9(02).
               05  DIA-LCT      PIC  9(02).

           03  TEL-AUX.
               05  DDD-AUX      PIC  9(04).
               05  PRX-AUX      PIC  9(04).
               05  SUF-AUX      PIC  9(04).

       01  CAB-001.
           03  FILLER           PIC  X(22)  VALUE  "-DYNAMIC-".
           03  NOM-CB1          PIC  X(48).
           03  FILLER           PIC  X(08)  VALUE  "FOLHA :".
           03  PAG-CB1          PIC  9(02).

       01  CAB-002.
           03  TIP-CB2          PIC  X(72)  VALUE
               "RESUMO DO FATURAMENTO".
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "FATURA   CLIENTE          TELEFONE       NOME / CONTATO  NOT
           "AS   VALOR DA FATURA".

       01  DET-001.
           03  FAT-DT1          PIC  9(06).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  CLI-DT1          PIC  X(13).
           03  FILLER           PIC  X(01)  VALUE  "(".
           03  DDD-DT1          PIC  9(04).
           03  FILLER           PIC  X(02)  VALUE  ")".
           03  PRX-DT1          PIC  ZZZ9.
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DT1          PIC  9(04).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  CNT-DT1          PIC  X(17).
           03  QTD-DT1          PIC  ZZ9.
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  VAL-DT1          PIC  --.---.---.--9,99.

       01  TOT-001.
           03  FILLER           PIC  X(56)  VALUE  "
               "  - TOTAL DO FATURAMENTO .........".
           03  QTD-TT1          PIC  Z.ZZ9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  VAL-TT1          PIC  ---.---.---.--9,99.

       01  PRM-001              PIC  9(01).

       01  PRM-002.
           03  CLI-PR2          PIC  X(10).
           03  CTG-PR2          PIC  9(01).
           03  DTA-PR2.
               05  ANO-PR2      PIC  9(02).
               05  MES-PR2      PIC  9(02).
               05  DIA-PR2      PIC  9(02).

       SCREEN          SECTION.

       01  COL-001  LINE 14 COLUMN 21 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-002  LINE 14 COLUMN 31 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-003  LINE 14 COLUMN 41 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-004  LINE 14 COLUMN 51 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  07.
       01  COL-005  LINE 14 COLUMN 61 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  07.

       01  MEN-001  LINE 22 COLUMN 14 PIC X(67) FROM "LIBERACAO COMPLETA
                    "  -  [ESC] RETORNA ...                           "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001  PRM-002.

       ROT-000-000.
           OPEN     I-O      CADCTR
                    INPUT    CADNTA
           READ     CADCTR
           MOVE     ZEROS    TO  ACM-VAL
           MOVE     SPACES   TO  RSP-OPC
           MOVE     DIA-PR2  TO  DIA-LIM
           MOVE     MES-PR2  TO  MES-LIM
           MOVE     ANO-PR2  TO  ANO-LIM
           IF  ANO-PR2   <   10
               ADD     2000  TO  ANO-LIM
           ELSE
               ADD     1900  TO  ANO-LIM.
           GO  TO   ROT-100-000, ROT-200-000,
                    DEPENDING    ON  PRM-001.

       ROT-000-010.
           IF  PRM-001   =   1   CHAIN  "DYN505".
           IF  PRM-001   =   2   CHAIN  "DYN506".

       ROT-100-000.
           DISPLAY  COL-001
           MOVE     SPACES   TO  CHV-NTA
           START    CADNTA   KEY GREATER    CHV-NTA
           SORT     CADSRT   ON  ASCENDING  KEY CHV-SRT
                    INPUT        PROCEDURE  IS  ROT-100-010
                    OUTPUT       PROCEDURE  IS  ROT-100-040
           DISPLAY  COL-005      MEN-001
           ACCEPT   OPC-001
           GO  TO   ROT-000-010.

       ROT-100-010  SECTION.

       ROT-100-020.
           READ     CADNTA   NEXT AT END
                    GO  TO   ROT-100-030.
           MOVE     DTA-NTA  TO  DTA-SYS
           MOVE     DIA-SYS  TO  DIA-LCT
           MOVE     MES-SYS  TO  MES-LCT
           MOVE     ANO-SYS  TO  ANO-LCT
           IF  ANO-SYS   <   10
               ADD     2000  TO  ANO-LCT
           ELSE
               ADD     1900  TO  ANO-LCT.
           IF  CLI-NTA   =   CLI-PR2 AND
               DTA-LCT  NOT  >   DTA-LIM
               ADD  VAL-NTA  TO  ACM-VAL
               GO  TO        ROT-100-020.
           MOVE     REG-NTA  TO  REG-SRT
           RELEASE  REG-SRT
           GO  TO   ROT-100-020.

       ROT-100-030.
           DISPLAY  COL-002
           CLOSE    CADNTA
           OPEN     OUTPUT   CADNTA
           DISPLAY  COL-003.

       ROT-100-040  SECTION.

       ROT-100-050.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-100-060.
           MOVE     REG-SRT  TO  REG-NTA
           WRITE    REG-NTA
           GO  TO   ROT-100-050.

       ROT-100-060.
           DISPLAY  COL-004
           OPEN     I-O      CADFAT
           ADD      1        TO  FAT-CTR
           MOVE     FAT-CTR  TO  CHV-FAT
           MOVE     DTA-PR2  TO  DTA-FAT
           MOVE     CLI-PR2  TO  CLI-FAT
           MOVE     ACM-VAL  TO  VAL-FAT
           WRITE    REG-FAT
           REWRITE  REG-CTR
           CLOSE    CADCTR
                    CADFAT   CADNTA.

       ROT-100-070  SECTION.

       ROT-200-000.
           OPEN     I-O      CADFAT
                    OUTPUT   RELATO
                    INPUT    CADCLI  CADPRM
           READ     CADPRM
           MOVE     1   TO   IND1.

       ROT-200-010.
           MOVE     CLI-PRM (IND1 + 1)  TO IND2
           MOVE     LET-TAB (IND2)  TO  LET-NOM (IND1)
           ADD      1   TO   IND1
           IF  IND1  <  36
               GO  TO        ROT-200-010.
           MOVE     SPACES   TO  LET-NOM (IND1)
           ACCEPT   DTA-SYS  FROM   DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.
           MOVE     NOM-TAB  TO  NOM-CB1
           MOVE     DIA-SYS  TO  DIA-CB2
           MOVE     MES-SYS  TO  MES-CB2
           MOVE     ANO-SYS  TO  ANO-CB2
           DISPLAY  COL-001
           MOVE     1        TO  PAG-CB1
           MOVE     ZEROS    TO  ACM-QTD
                                 TOT-QTD  TOT-VAL
           WRITE    REG-REL FROM CAB-001  AFTER 0
           MOVE     SPACES   TO  CHV-CLI
           START    CADCLI   KEY GREATER  CHV-CLI.

       ROT-200-020.
           WRITE    REG-REL FROM CAB-002  AFTER 2
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-003
           MOVE     ZEROS    TO  ACM-LIN.

       ROT-200-030.
           READ     CADCLI  NEXT AT  END
                    GO  TO   ROT-200-070.
           IF  CTG-CLI  NOT  =   CTG-PR2
               GO  TO        ROT-200-030.
           MOVE     SPACES   TO  CHV-NTA
           MOVE     CHV-CLI  TO  CLI-NTA
           START    CADNTA   KEY GREATER  CHV-NTA
                    INVALID  KEY GO  TO   ROT-200-070.

       ROT-200-040.
           READ     CADNTA   NEXT AT END
                    GO  TO   ROT-200-050.
           MOVE     DTA-NTA  TO  DTA-SYS
           MOVE     DIA-SYS  TO  DIA-LCT
           MOVE     MES-SYS  TO  MES-LCT
           MOVE     ANO-SYS  TO  ANO-LCT
           IF  ANO-SYS   <   10
               ADD     2000  TO  ANO-LCT
           ELSE
               ADD     1900  TO  ANO-LCT.
           IF  CLI-NTA  NOT  =   CHV-CLI
               GO   TO       ROT-200-050.
           IF  DTA-LCT   >   DTA-LIM
               GO  TO        ROT-200-040.
           ADD      1        TO  ACM-QTD
           ADD      VAL-NTA  TO  ACM-VAL
           GO  TO   ROT-200-040.

       ROT-200-050.
           IF  ACM-QTD   =   0
               GO  TO        ROT-200-030.
           IF  ACM-LIN   <   52
               GO  TO        ROT-200-060.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           PERFORM  ROT-200-020.

       ROT-200-060.
           ADD      1        TO  FAT-CTR
           MOVE     FAT-CTR  TO  FAT-DT1  CHV-FAT
           MOVE     CHV-CLI  TO  CLI-DT1  CLI-FAT
           MOVE     TEL-CLI  TO  TEL-AUX
           MOVE     DDD-AUX  TO  DDD-DT1
           MOVE     PRX-AUX  TO  PRX-DT1
           MOVE     SUF-AUX  TO  SUF-DT1
           MOVE     CNT-CLI  TO  CNT-DT1
           MOVE     ACM-QTD  TO  QTD-DT1
           MOVE     ACM-VAL  TO  VAL-DT1  VAL-FAT
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  ACM-LIN
           ADD      ACM-QTD  TO  TOT-QTD
           ADD      ACM-VAL  TO  TOT-VAL
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           MOVE     DTA-PR2  TO  DTA-FAT
           WRITE    REG-FAT
           GO  TO   ROT-200-030.

       ROT-200-070.
           DISPLAY  COL-002
           MOVE     TOT-QTD  TO  QTD-TT1
           MOVE     TOT-VAL  TO  VAL-TT1
           WRITE    REG-REL FROM TOT-001    AFTER  2.

       ROT-200-080.
           CLOSE    CADFAT       RELATO
           MOVE     SPACES   TO  CHV-NTA
           START    CADNTA   KEY GREATER    CHV-NTA
           SORT     CADSRT   ON  ASCENDING  KEY CHV-SRT
                    INPUT        PROCEDURE  IS  ROT-200-090
                    OUTPUT       PROCEDURE  IS  ROT-200-120
           DISPLAY  COL-005      MEN-001
           ACCEPT   OPC-001
           GO  TO   ROT-000-010.

       ROT-200-090  SECTION.

       ROT-200-100.
           READ     CADNTA   NEXT AT END
                    GO  TO   ROT-200-110.
           MOVE     CLI-NTA  TO  CHV-CLI
           READ     CADCLI
           MOVE     DTA-NTA  TO  DTA-SYS
           MOVE     DIA-SYS  TO  DIA-LCT
           MOVE     MES-SYS  TO  MES-LCT
           MOVE     ANO-SYS  TO  ANO-LCT
           IF  ANO-SYS   <   10
               ADD     2000  TO  ANO-LCT
           ELSE
               ADD     1900  TO  ANO-LCT.
           IF  CTG-CLI   =   CTG-PR2 AND
               DTA-LCT  NOT  >   DTA-LIM
               GO  TO        ROT-200-100.
           MOVE     REG-NTA  TO  REG-SRT
           RELEASE  REG-SRT
           GO  TO   ROT-200-100.

       ROT-200-110.
           DISPLAY  COL-003
           CLOSE    CADNTA
           OPEN     OUTPUT   CADNTA.

       ROT-200-120  SECTION.

       ROT-200-130.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-200-140.
           MOVE     REG-SRT  TO  REG-NTA
           WRITE    REG-NTA
           GO  TO   ROT-200-130.

       ROT-200-140.
           DISPLAY  COL-004
           REWRITE  REG-CTR
           CLOSE    CADCLI  CADCTR
                    CADNTA  CADPRM.
