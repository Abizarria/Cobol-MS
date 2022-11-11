       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN115.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADATV        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ATV.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADATV      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADATV.DAT".
       01  REG-ATV.
           03  CHV-ATV          PIC  9(01).
           03  DES-ATV          PIC  X(15).

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

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC X(100).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  QTD-AUX          PIC  9(03).
           03  ACM-QTD          PIC  9(03).

           03  CTA-AUX          PIC  9(03)V99.
           03  RED-CTA          REDEFINES     CTA-AUX.
               05  INT-AUX      PIC  9(03).
               05  DEC-AUX      PIC  9(02).

           03  CHV-INI.
               05  PRX-INI      PIC  9(01).
               05  SUF-INI      PIC  9(03).

           03  CHV-FIN.
               05  PRX-FIN      PIC  9(01).
               05  SUF-FIN      PIC  9(03).

       01  IMP-001.
           03  FILLER           PIC  9(01)  COMP-0  VALUE 15.
           03  FILLER           PIC  9(01)  COMP-0  VALUE 27.
           03  FILLER           PIC  9(01)  COMP    VALUE  4.

       01  IMP-002.
           03  FILLER           PIC  9(01)  COMP-0  VALUE 18.
           03  FILLER           PIC  9(01)  COMP-0  VALUE 27.
           03  FILLER           PIC  9(01)  COMP    VALUE  5.

       01  ETQ-001.
           03  FILLER           PIC  X(10)  VALUE  "CODIGO .:".
           03  PR1-ET1          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SF1-ET1          PIC  9(03).
           03  FILLER           PIC  X(15)  VALUE  "     CODIGO .:".
           03  PR2-ET1          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SF2-ET1          PIC  9(03).
           03  FILLER           PIC  X(16)  VALUE  "      CODIGO .:".
           03  PR3-ET1          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SF3-ET1          PIC  9(03).
           03  FILLER           PIC  X(15)  VALUE  "     CODIGO .:".
           03  PR4-ET1          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SF4-ET1          PIC  9(03).
           03  FILLER           PIC  X(15)  VALUE  "     CODIGO .:".
           03  PR5-ET1          PIC  9(01).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SF5-ET1          PIC  9(03).

       01  ETQ-002.
           03  FILLER           PIC  X(03)  VALUE  "R$".
           03  PC1-ET2          PIC  Z.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(08)  VALUE  "     R$".
           03  PC2-ET2          PIC  Z.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(09)  VALUE  "      R$".
           03  PC3-ET2          PIC  Z.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(08)  VALUE  "     R$".
           03  PC4-ET2          PIC  Z.ZZZ.ZZ9,99.
           03  FILLER           PIC  X(08)  VALUE  "     R$".
           03  PC5-ET2          PIC  Z.ZZZ.ZZ9,99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "EMISSAO DE ETIQUETAS P/ PRODUTOS  -  Ver. 7.11"
               BACKGROUND-COLOR  02    FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 55 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 55 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 55 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 55 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 55 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 55 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 55 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 55 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 55 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 34 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 36 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 07 COLUMN 22 PIC X(31) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄ¿"  BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 22 PIC X(31) FROM " ³
               "      ³"  BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 22 PIC X(31) FROM " ³  Codigo Inicial .:
               "-     ³"  BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 22 PIC X(31) FROM " ³
               "      ³"  BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 22 PIC X(31) FROM " ³  Codigo Final ...:
               "-     ³"  BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 22 PIC X(31) FROM " ³
               "      ³"  BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 22 PIC X(31) FROM " ³  Quantidade .....:
               "      ³"  BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 22 PIC X(31) FROM " ³
               "      ³"  BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 22 PIC X(31) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÙ"  BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 24 PIC X(31) FROM SPACES.

       01  ENT-001.
           03  PRI-ENT LINE 09 COLUMN 44 PIC 9(01) USING PRX-INI
                       BACKGROUND-COLOR  01.
           03  SFI-ENT LINE 09 COLUMN 46 PIC 9(03) USING SUF-INI
                       BACKGROUND-COLOR  01.
           03  PRF-ENT LINE 11 COLUMN 44 PIC 9(01) USING PRX-FIN
                       BACKGROUND-COLOR  01.
           03  SFF-ENT LINE 11 COLUMN 46 PIC 9(03) USING SUF-FIN
                       BACKGROUND-COLOR  01.
           03  QTD-ENT LINE 13 COLUMN 44 PIC 9(03) USING QTD-AUX
                       BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-SFI LINE 09 COLUMN 46 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-PRF LINE 11 COLUMN 44 PIC X(01) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SFF LINE 11 COLUMN 46 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-QTD LINE 13 COLUMN 44 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO INICIAL  -  [ESC] RETORNA ...             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "ITEM INICIAL  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO FINAL  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "ITEM FINAL  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO INEXISTENTE
                   " -  [ESC] RETORNA ...                           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "SEQUENCIA DE CODIGO
                   "S INCOERENTE  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A QUANTIDADE
                   " P/ PRODUTO  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "A QUANTIDADE DEVE S
                   "ER MULTIPLO DE 5  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR !!!  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO ETIQUETAS P/ PRODUTOS ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     INPUT    CADATV   CADEST.

       ROT-000-010.
           DISPLAY  MEN-001  LPA-SFI
           MOVE     0   TO   PRX-INI
           ACCEPT   (09 44)  PRX-INI  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-000.
           MOVE     PRX-INI  TO  CHV-ATV
           READ     CADATV   INVALID KEY
                    GO  TO   ROT-000-010.
           DISPLAY  PRI-ENT.

       ROT-000-020.
           DISPLAY  MEN-002  LPA-PRF
           MOVE     0   TO   SUF-INI
           ACCEPT   (09 46)  SUF-INI  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           IF  SUF-INI   =   ZEROS
               GO  TO        ROT-000-020.
           DISPLAY  SFI-ENT
           MOVE     PRX-INI  TO  PRX-EST
           MOVE     SUF-INI  TO  SUF-EST
           MOVE     SPACES   TO  RSP-OPC
           READ     CADEST   INVALID KEY
                    DISPLAY  MEN-005
                    ACCEPT   OPC-001
                    GO  TO   ROT-000-020.

       ROT-000-030.
           DISPLAY  MEN-003  LPA-SFF
           MOVE     0   TO   PRX-FIN
           ACCEPT   (11 44)  PRX-FIN  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           MOVE     PRX-FIN  TO  CHV-ATV
           READ     CADATV   INVALID KEY
                    GO  TO   ROT-000-030.
           DISPLAY  PRF-ENT.

       ROT-000-040.
           DISPLAY  LPA-QTD  MEN-004
           MOVE     0   TO   SUF-FIN
           ACCEPT   (11 46)  SUF-FIN  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           IF  SUF-FIN   =   ZEROS
               GO  TO        ROT-000-040.
           DISPLAY  SFF-ENT
           MOVE     SPACES   TO  RSP-OPC
           IF  CHV-FIN   <   CHV-INI
               DISPLAY       MEN-006
               ACCEPT        OPC-001
               GO TO         ROT-000-040.
           MOVE     PRX-FIN  TO  PRX-EST
           MOVE     SUF-FIN  TO  SUF-EST
           READ     CADEST   INVALID KEY
                    DISPLAY  MEN-005
                    ACCEPT   OPC-001
                    GO  TO   ROT-000-040.

       ROT-000-050.
           DISPLAY  MEN-007
           MOVE     0   TO   QTD-AUX
           ACCEPT   (13 44)  QTD-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-040.
           IF  QTD-AUX   =   ZEROS
               GO  TO        ROT-000-050.
           DISPLAY  QTD-ENT
           MOVE     SPACES   TO  RSP-OPC
           COMPUTE  CTA-AUX  =   QTD-AUX  /  5
           IF  DEC-AUX   >   0
               DISPLAY       MEN-008
               ACCEPT        OPC-001
               GO  TO        ROT-000-050.
           DISPLAY  MEN-009
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-050.
           DISPLAY  MEN-010
           OPEN     OUTPUT       RELATO
           WRITE    REG-REL FROM IMP-001  AFTER 0
           MOVE     CHV-INI  TO  CHV-EST
           START    CADEST   KEY  EQUAL   CHV-EST.

       ROT-100-000.
           READ     CADEST   NEXT AT END
                    GO  TO   ROT-100-020.
           IF  CHV-EST   >   CHV-FIN
               GO  TO        ROT-100-020.
           MOVE     PRX-EST  TO  PR1-ET1  PR2-ET1
                                 PR3-ET1  PR4-ET1  PR5-ET1
           MOVE     SUF-EST  TO  SF1-ET1  SF2-ET1
                                 SF3-ET1  SF4-ET1  SF5-ET1
           MOVE     VDA-EST  TO  PC1-ET2  PC2-ET2
                                 PC3-ET2  PC4-ET2  PC5-ET2
           MOVE     5        TO  ACM-QTD.

       ROT-100-010.
           WRITE    REG-REL FROM ETQ-001  AFTER  0
           WRITE    REG-REL FROM ETQ-002  AFTER  2
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL      AFTER 2
           ADD      5        TO  ACM-QTD
           IF  ACM-QTD   >   QTD-AUX
               GO  TO        ROT-100-000.
           GO  TO   ROT-100-010.

       ROT-100-020.
           WRITE    REG-REL FROM IMP-002
           CLOSE    RELATO
           DISPLAY  LPA-001
           GO  TO   ROT-000-010.

       ROT-300-000.
           CLOSE    CADATV   CADEST
           MOVE     6   TO   PRM-001
           CHAIN   "DYN100"  USING  PRM-001.
