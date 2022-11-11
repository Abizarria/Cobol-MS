       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN509.
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

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST.

           SELECT      CADFAT        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-FAT.

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

       FD  CADFAT      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADFAT.DAT".
       01  REG-FAT.
           03  CHV-FAT          PIC  9(06).
           03  CLI-FAT          PIC  X(10).
           03  DTA-FAT          PIC  9(06).
           03  VAL-FAT          PIC S9(10)V99.

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC  X(80).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  INI-AUX          PIC  9(06).
           03  FIN-AUX          PIC  9(06).

           03  CEP-AUX.
               05  CP1-AUX      PIC  9(05).
               05  CP2-AUX      PIC  9(03).

           03  TEL-AUX.
               05  DDD-AUX      PIC  9(04).
               05  PRX-AUX      PIC  9(04).
               05  SUF-AUX      PIC  9(04).

           03  CGC-AUX.
               05  CG1-AUX      PIC  9(02).
               05  CG2-AUX      PIC  9(03).
               05  CG3-AUX      PIC  9(03).
               05  CG4-AUX      PIC  9(04).
               05  CG5-AUX      PIC  9(02).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

       01  IMP-001.
           03  FILLER           PIC  9(02) COMP-0  VALUE  27.
           03  FILLER           PIC  9(01) COMP    VALUE  4.

       01  IMP-002.
           03  FILLER           PIC  9(02) COMP-0  VALUE  27.
           03  FILLER           PIC  9(01) COMP    VALUE  5.

       01  NTA-001.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  FILLER           PIC  X(05)  VALUE  "VENDA".

       01  NTA-002.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  NOM-N02          PIC  X(43).
           03  CG1-N02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  ".".
           03  CG2-N02          PIC  9(03).
           03  FILLER           PIC  X(01)  VALUE  ".".
           03  CG3-N02          PIC  9(03).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  CG4-N02          PIC  9(04).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  CG5-N02          PIC  9(02).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  DIA-N02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-N02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-N02          PIC  9(02).

       01  NTA-003.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  END-N03          PIC  X(50).
           03  CP1-N03          PIC  9(05).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  CP2-N03          PIC  9(03).
           03  FILLER           PIC  X(05)  VALUE  SPACES.
           03  DIA-N03          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-N03          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-N03          PIC  9(02).

       01  NTA-004.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  CID-N04          PIC  X(26).
           03  PRX-N04          PIC  ZZZ9.
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-N04          PIC  9(04).
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  EST-N04          PIC  X(05).
           03  INS-N04          PIC  X(15).

       01  NTA-005.
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  FILLER           PIC  X(07)  VALUE  "A VISTA".

       01  NTA-006.
           03  FILLER           PIC  X(11)  VALUE  SPACES.
           03  DES-N06          PIC  X(29).
           03  FILLER           PIC  X(04)  VALUE  "LTS".
           03  QTD-N06          PIC  ZZ9,9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  VAL-N06          PIC  ZZ9,99.
           03  FILLER           PIC  X(02).
           03  TOT-N06          PIC  Z.ZZZ.ZZ9,99.

       01  NTA-007.
           03  FILLER           PIC  X(66)  VALUE  SPACES.
           03  VAL-N07          PIC  ZZZ.ZZZ.ZZ9,99.

       01  NTA-008.
           03  FILLER           PIC  X(12)  VALUE  SPACES.
           03  FILLER           PIC  X(10)  VALUE  "CONSUMIDOR".

       01  NTA-009              PIC  X(40)  VALUE
           "      IMPOSTO RECOLHIDO POR SUBSTITUICAO".

       01  NTA-010              PIC  X(44)  VALUE
           "      NOS TERMOS DO ARTIGO 392 DO R.I.C.M.S.".

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "EMISSAO DE NOTAS FISCAIS -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 27 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 54 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 56 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 56 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 56 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 58 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 56 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 56 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 56 PIC X(06) FROM SPACES
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
           03  LINE 08 COLUMN 22 PIC X(32) FROM
             " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"  BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 22 PIC X(32) FROM
             " ³                            ³"  BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 22 PIC X(32) FROM
             " ³  Fatura Inicial .:         ³"  BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 22 PIC X(32) FROM
             " ³                            ³"  BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 22 PIC X(32) FROM
             " ³  Fatura Final ...:         ³"  BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 22 PIC X(32) FROM
             " ³                            ³"  BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 22 PIC X(32) FROM
             " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"  BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 54 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 24 PIC X(32) FROM SPACES.

       01  ENT-001.
           03  INI-ENT  LINE 10 COLUMN 44 PIC 9(06) USING INI-AUX
                        BACKGROUND-COLOR  01.
           03  FIN-ENT  LINE 12 COLUMN 44 PIC 9(06) USING FIN-AUX
                        BACKGROUND-COLOR  01.
       01  LPA-001.
           03  LPA-FIN  LINE 12 COLUMN 44 PIC X(06) FROM SPACES
                        BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "FATURA INICIAL  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "FATURA FINAL  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO NOTAS FISCAIS ...                          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "FATURA INEXISTENTE
                   " -  [ESC] RETORNA ...                           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "SEQUENCIA DE FATURA
                   "S INCOERENTE  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     INPUT    CADFAT.

       ROT-000-010.
           MOVE     ZEROS    TO  INI-AUX  FIN-AUX.

       ROT-000-020.
           DISPLAY  LPA-FIN  MEN-001
           ACCEPT   (10 44)  INI-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           IF  INI-AUX   =   ZEROS
               GO  TO        ROT-000-020.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     INI-AUX  TO  CHV-FAT
           READ     CADFAT   INVALID KEY
                    DISPLAY  MEN-005
                    ACCEPT   OPC-001
                    GO  TO   ROT-000-020.
           DISPLAY  INI-ENT.

       ROT-000-030.
           DISPLAY  MEN-002
           ACCEPT   (12 44)  FIN-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           IF  FIN-AUX   =   ZEROS
               GO  TO        ROT-000-030.
           MOVE     SPACES   TO  RSP-OPC
           IF  FIN-AUX   <   INI-AUX
               DISPLAY       MEN-006
               ACCEPT        OPC-001
               GO  TO        ROT-000-020.
           MOVE     FIN-AUX  TO  CHV-FAT
           READ     CADFAT   INVALID KEY
                    DISPLAY  MEN-005
                    ACCEPT   OPC-001
                    GO  TO   ROT-000-030.
           DISPLAY  FIN-ENT  MEN-003
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.

       ROT-100-000.
           DISPLAY  MEN-004
           OPEN     OUTPUT       RELATO
                    INPUT        CADCLI  CADEST
           ACCEPT   DTA-SYS FROM DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.
           MOVE     DIA-SYS  TO  DIA-N03
           MOVE     MES-SYS  TO  MES-N03
           MOVE     ANO-SYS  TO  ANO-N03
           WRITE    REG-REL FROM IMP-001  AFTER  0
           MOVE     "0001"   TO  CHV-EST
           READ     CADEST
           MOVE     DES-EST  TO  DES-N06
           MOVE     VDA-EST  TO  VAL-N06
           MOVE     INI-AUX  TO  CHV-FAT
           START    CADFAT   KEY  EQUAL   CHV-FAT.

       ROT-100-010.
           READ     CADFAT   NEXT AT END
                    GO  TO   ROT-100-030.
           IF  CHV-FAT   >   FIN-AUX
               GO  TO        ROT-100-030.
           MOVE     CLI-FAT  TO  CHV-CLI
           READ     CADCLI
           MOVE     NOM-CLI  TO  NOM-N02
           MOVE     CGC-CLI  TO  CGC-AUX
           MOVE     CG1-AUX  TO  CG1-N02
           MOVE     CG2-AUX  TO  CG2-N02
           MOVE     CG3-AUX  TO  CG3-N02
           MOVE     CG4-AUX  TO  CG4-N02
           MOVE     CG5-AUX  TO  CG5-N02
           MOVE     END-CLI  TO  END-N03
           MOVE     CEP-CLI  TO  CEP-AUX
           MOVE     CP1-AUX  TO  CP1-N03
           MOVE     CP2-AUX  TO  CP2-N03
           MOVE     CID-CLI  TO  CID-N04
           MOVE     TEL-CLI  TO  TEL-AUX
           MOVE     PRX-AUX  TO  PRX-N04
           MOVE     SUF-AUX  TO  SUF-N04
           MOVE     EST-CLI  TO  EST-N04
           MOVE     INS-CLI  TO  INS-N04
           COMPUTE  QTD-N06  =   VAL-FAT  /  VDA-EST
           MOVE     VAL-FAT  TO  TOT-N06     VAL-N07
           MOVE     DTA-FAT  TO  DTA-AUX
           MOVE     DIA-AUX  TO  DIA-N02
           MOVE     MES-AUX  TO  MES-N02
           MOVE     ANO-AUX  TO  ANO-N02.

       ROT-100-020.
           WRITE    REG-REL FROM NTA-001  AFTER  12
           WRITE    REG-REL FROM NTA-002  AFTER  03
           WRITE    REG-REL FROM NTA-003  AFTER  02
           WRITE    REG-REL FROM NTA-004  AFTER  02
           WRITE    REG-REL FROM NTA-005  AFTER  03
           WRITE    REG-REL FROM NTA-006  AFTER  05
           WRITE    REG-REL FROM NTA-008  AFTER  14
           WRITE    REG-REL FROM NTA-007  AFTER  05
           WRITE    REG-REL FROM NTA-009  AFTER  11
           WRITE    REG-REL FROM NTA-010
           GO  TO   ROT-100-010.

       ROT-100-030.
           WRITE    REG-REL FROM IMP-002
           CLOSE    RELATO
                    CADCLI  CADEST
           DISPLAY  LPA-001
           GO  TO   ROT-000-010.

       ROT-200-000.
           MOVE     3   TO   PRM-001
           CLOSE    CADFAT
           CHAIN   "DYN500"  USING  PRM-001.
