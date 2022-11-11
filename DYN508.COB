       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN508.
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
           03  LOC-AUX          PIC  X(40).
           03  MG1-AUX          PIC  X(40).
           03  MG2-AUX          PIC  X(40).
           03  MG3-AUX          PIC  X(40).

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

           03  CEP-AUX.
               05  CP1-AUX      PIC  9(05).
               05  CP2-AUX      PIC  9(03).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

       01  IMP-001.
           03  FILLER           PIC  9(02) COMP-0  VALUE  27.
           03  FILLER           PIC  9(01) COMP    VALUE  0.

       01  IMP-002.
           03  FILLER           PIC  9(02) COMP-0  VALUE  27.
           03  FILLER           PIC  9(01) COMP    VALUE  2.

       01  BLQ-001.
           03  LOC-B01          PIC  X(54).
           03  DIA-B01          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-B01          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-B01          PIC  9(02).

       01  BLQ-002.
           03  DD1-B02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MM1-B02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AA1-B02          PIC  9(02).
           03  FILLER           PIC  X(07)  VALUE  SPACES.
           03  NUM-B02          PIC  9(06).
           03  FILLER           PIC  X(14)  VALUE  SPACES.
           03  DD2-B02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MM2-B02          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AA2-B02          PIC  9(02).

       01  BLQ-003.
           03  FILLER           PIC  X(45)  VALUE  SPACES.
           03  VAL-B03          PIC  --.---.---.--9,99.

       01  BLQ-004              PIC  X(45).

       01  BLQ-005              PIC  X(45).

       01  BLQ-006              PIC  X(45).

       01  BLQ-007.
           03  FILLER           PIC  X(06)  VALUE  SPACES.
           03  NOM-B07          PIC  X(40).

       01  BLQ-008.
           03  FILLER           PIC  X(06)  VALUE  SPACES.
           03  END-B08          PIC  X(37).
           03  CGC-B08          PIC  99.999.999/9999.99.

       01  BLQ-009.
           03  FILLER           PIC  X(06)  VALUE  SPACES.
           03  CP1-B09          PIC  9(05).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  CP2-B09          PIC  9(03).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  CID-B09          PIC  X(28).
           03  EST-B09          PIC  X(02).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "EMISSAO DE BOLETOS -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.

       01  TLA-002.
           03  LINE 06 COLUMN 08 PIC X(64) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 08 PIC X(64) FROM " ³ Fatura Inicial .:
               "              Fatura Final ...:        ³"
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 08 PIC X(64) FROM " ³ Vencimento .....:
               "/  /                                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 08 PIC X(64) FROM " ³ Local de Pagto .:
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 08 PIC X(64) FROM " ³ Instrucoes .....:
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 08 PIC X(64) FROM " ³
               "                                       ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 08 PIC X(64) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 72 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 10 PIC X(64) FROM SPACES.

       01  ENT-001.
           03  INI-ENT  LINE 08 COLUMN 29 PIC 9(06) USING INI-AUX
                        BACKGROUND-COLOR  01.
           03  FIN-ENT  LINE 08 COLUMN 63 PIC 9(06) USING FIN-AUX
                        BACKGROUND-COLOR  01.
           03  DIA-ENT  LINE 10 COLUMN 29 PIC 9(02) USING DIA-AUX
                        BACKGROUND-COLOR  01.
           03  MES-ENT  LINE 10 COLUMN 32 PIC 9(02) USING MES-AUX
                        BACKGROUND-COLOR  01.
           03  ANO-ENT  LINE 10 COLUMN 35 PIC 9(02) USING ANO-AUX
                        BACKGROUND-COLOR  01.
           03  LOC-ENT  LINE 12 COLUMN 29 PIC X(40) USING LOC-AUX
                        BACKGROUND-COLOR  01.
           03  MG1-ENT  LINE 14 COLUMN 29 PIC X(40) USING MG1-AUX
                        BACKGROUND-COLOR  01.
           03  MG2-ENT  LINE 15 COLUMN 29 PIC X(40) USING MG2-AUX
                        BACKGROUND-COLOR  01.
           03  MG3-ENT  LINE 16 COLUMN 29 PIC X(40) USING MG3-AUX
                        BACKGROUND-COLOR  01.
       01  LPA-001.
           03  LPA-FIN  LINE 08 COLUMN 63 PIC X(06) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-DIA  LINE 10 COLUMN 29 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-MES  LINE 10 COLUMN 32 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-ANO  LINE 10 COLUMN 35 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-LOC  LINE 12 COLUMN 29 PIC X(40) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-MG1  LINE 14 COLUMN 29 PIC X(40) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-MG2  LINE 15 COLUMN 29 PIC X(40) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-MG3  LINE 16 COLUMN 29 PIC X(40) FROM SPACES
                        BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "FATURA INICIAL  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "FATURA FINAL  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA DE VE
                   "NCIMENTO  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O LOCAL DE P
                   "AGAMENTO  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE AS INSTRUCOE
                   "S DA COBRANCA  -  [ESC] RETORNA ...             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO BOLETOS DE COBRANCA BANCARIA ...           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "FATURA INEXISTENTE
                   " -  [ESC] RETORNA ...                           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "SEQUENCIA DE FATURA
                   "S INCOERENTE  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           ACCEPT   DTA-SYS   FROM  DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.
           OPEN     INPUT    CADFAT.

       ROT-000-010.
           MOVE     ZEROS    TO  INI-AUX
                                 FIN-AUX  DTA-AUX
           MOVE     SPACES   TO  LOC-AUX  MG1-AUX
                                 MG2-AUX  MG3-AUX.
       ROT-000-020.
           DISPLAY  LPA-FIN  MEN-001
           ACCEPT   (08 29)  INI-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           IF  INI-AUX   =   ZEROS
               GO  TO        ROT-000-020.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     INI-AUX  TO  CHV-FAT
           READ     CADFAT   INVALID KEY
                    DISPLAY  MEN-008
                    ACCEPT   OPC-001
                    GO  TO   ROT-000-020.
           DISPLAY  INI-ENT.

       ROT-000-030.
           DISPLAY  LPA-DIA  MEN-002
           ACCEPT   (08 63)  FIN-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           IF  FIN-AUX   =   ZEROS
               GO  TO        ROT-000-030.
           MOVE     SPACES   TO  RSP-OPC
           IF  FIN-AUX   <   INI-AUX
               DISPLAY       MEN-009
               ACCEPT        OPC-001
               GO  TO        ROT-000-020.
           MOVE     FIN-AUX  TO  CHV-FAT
           READ     CADFAT   INVALID KEY
                    DISPLAY  MEN-008
                    ACCEPT   OPC-001
                    GO  TO   ROT-000-030.
           DISPLAY  FIN-ENT.

       ROT-000-040.
           DISPLAY  LPA-MES  MEN-003
           ACCEPT   (10 29)  DIA-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           IF  DIA-AUX   <   1  OR >  31
               GO  TO        ROT-000-040.
           DISPLAY  DIA-ENT.

       ROT-000-050.
           DISPLAY  LPA-ANO
           ACCEPT   (10 32)  MES-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-040.
           IF  MES-AUX   <   1  OR >  12
               GO  TO        ROT-000-050.
           DISPLAY  MES-ENT.

       ROT-000-060.
           DISPLAY  LPA-LOC  MEN-003
           ACCEPT   (10 35)  ANO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-050.
           DISPLAY  ANO-ENT.

       ROT-000-070.
           DISPLAY  LPA-MG1  MEN-004
           ACCEPT   (12 29)  LOC-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-060.
           DISPLAY  LOC-ENT.

       ROT-000-080.
           DISPLAY  LPA-MG2  MEN-005
           ACCEPT   (14 29)  MG1-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-070.
           DISPLAY  MG1-ENT.

       ROT-000-090.
           DISPLAY  LPA-MG3
           ACCEPT   (15 29)  MG2-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-080.
           DISPLAY  MG2-ENT.

       ROT-000-100.
           DISPLAY  MEN-005
           ACCEPT   (16 29)  MG3-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-090.
           DISPLAY  MG3-ENT  MEN-006
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-100.

       ROT-100-000.
           DISPLAY  MEN-007
           OPEN     INPUT    CADCLI
                    OUTPUT   RELATO
           MOVE     INI-AUX  TO  CHV-FAT
           START    CADFAT   KEY  EQUAL   CHV-FAT
           MOVE     LOC-AUX  TO  LOC-B01
           MOVE     DIA-AUX  TO  DIA-B01
           MOVE     MES-AUX  TO  MES-B01
           MOVE     ANO-AUX  TO  ANO-B01
           MOVE     DIA-SYS  TO  DD1-B02  DD2-B02
           MOVE     MES-SYS  TO  MM1-B02  MM2-B02
           MOVE     ANO-SYS  TO  AA1-B02  AA2-B02
           MOVE     MG1-AUX  TO  BLQ-004
           MOVE     MG2-AUX  TO  BLQ-005
           MOVE     MG3-AUX  TO  BLQ-006
           WRITE    REG-REL FROM IMP-001  AFTER 0.

       ROT-100-010.
           READ     CADFAT   NEXT AT END
                    GO  TO   ROT-100-020.
           IF  CHV-FAT   >   FIN-AUX
               GO  TO        ROT-100-020.
           MOVE     CLI-FAT  TO  CHV-CLI
           READ     CADCLI
           MOVE     CHV-FAT  TO  NUM-B02
           MOVE     VAL-FAT  TO  VAL-B03
           MOVE     NOM-CLI  TO  NOM-B07
           MOVE     END-CLI  TO  END-B08
           MOVE     CGC-CLI  TO  CGC-B08
           MOVE     CEP-CLI  TO  CEP-AUX
           MOVE     CP1-AUX  TO  CP1-B09
           MOVE     CP2-AUX  TO  CP2-B09
           MOVE     CID-CLI  TO  CID-B09
           MOVE     EST-CLI  TO  EST-B09
           WRITE    REG-REL FROM BLQ-001
           WRITE    REG-REL FROM BLQ-002  AFTER  5
           WRITE    REG-REL FROM BLQ-003  AFTER  2
           WRITE    REG-REL FROM BLQ-004  AFTER  3
           WRITE    REG-REL FROM BLQ-005  AFTER  2
           WRITE    REG-REL FROM BLQ-006  AFTER  2
           WRITE    REG-REL FROM BLQ-007  AFTER  5
           WRITE    REG-REL FROM BLQ-008
           WRITE    REG-REL FROM BLQ-009
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL               AFTER 10
           GO  TO   ROT-100-010.

       ROT-100-020.
           WRITE    REG-REL  FROM  IMP-002
           CLOSE    CADCLI
                    RELATO
           DISPLAY  LPA-001
           GO  TO   ROT-000-010.

       ROT-200-000.
           MOVE     3   TO   PRM-001
           CLOSE    CADFAT
           CHAIN   "DYN500"  USING  PRM-001.
