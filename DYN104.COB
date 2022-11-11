       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN104.
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

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  FTR-AUX          PIC  9(03).
           03  PCT-AUX          PIC  9(03)V99.

           03  CHV-INI.
               05  PRX-INI      PIC  9(01).
               05  SUF-INI      PIC  9(03).

           03  CHV-FIN.
               05  PRX-FIN      PIC  9(01).
               05  SUF-FIN      PIC  9(03).

           03  VAL-AUX          PIC  9(07)V99.
           03  RED-VAL          REDEFINES  VAL-AUX.
               05  INT-AUX      PIC  9(07).
               05  RST-AUX      PIC  9(02).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "REAJUSTE DE PRECOS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 17 COLUMN 58 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 58 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 42 PIC X(21) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 23 PIC X(33) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 23 PIC X(33) FROM " ³
               "        ³" BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 23 PIC X(33) FROM " ³  Codigo Inicial :  -
               "        ³" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 23 PIC X(33) FROM " ³
               "        ³" BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 23 PIC X(33) FROM " ³  Codigo Final ..:  -
               "        ³" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 23 PIC X(33) FROM " ³
               "        ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 58 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 23 PIC X(33) FROM " ³
               "        ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 58 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 23 PIC X(33) FROM " ³  Percentual ....:
               "     %  ³" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 58 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 23 PIC X(33) FROM " ³
               "        ³" BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 58 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 23 PIC X(33) FROM " ³  Arredondamento :
               "        ³" BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 58 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 23 PIC X(33) FROM " ³
               "        ³" BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 58 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 23 PIC X(33) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 25 PIC X(33) FROM SPACES.

       01  ENT-001.
           03  PRI-ENT LINE 08 COLUMN 44 PIC 9(01)  USING PRX-INI
                       BACKGROUND-COLOR  01.
           03  SFI-ENT LINE 08 COLUMN 46 PIC 9(03)  USING SUF-INI
                       BACKGROUND-COLOR  01.
           03  PRF-ENT LINE 10 COLUMN 44 PIC 9(01)  USING PRX-FIN
                       BACKGROUND-COLOR  01.
           03  SFF-ENT LINE 10 COLUMN 46 PIC 9(03)  USING SUF-FIN
                       BACKGROUND-COLOR  01.
           03  PCT-ENT LINE 13 COLUMN 44 PIC ZZ9,99 USING PCT-AUX
                       BACKGROUND-COLOR  01.
           03  FTR-ENT LINE 15 COLUMN 44 PIC ZZ9    USING FTR-AUX
                       BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-PRI LINE 08 COLUMN 44 PIC X(01) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SFI LINE 08 COLUMN 46 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-PRF LINE 10 COLUMN 44 PIC X(01) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SFF LINE 10 COLUMN 46 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-PCT LINE 13 COLUMN 44 PIC X(06) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-FTR LINE 15 COLUMN 44 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO INICIAL  -  [ESC] RETORNA ...             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "ITEM INICIAL  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO FINAL  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "ITEM FINAL  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O PERCENTUAL
                   " P/ REAJUSTE  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE UM MULTIPLO
                   "P/ ARREDONDAMENTO  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ RE
                   "AJUSTAR OS PRECOS  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  REAJUS
                   "TANDO OS PRECOS DE VENDA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "SEQUENCIA DE CODIGO
                   "S INCOERENTE !!!  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001
           OPEN     INPUT    CADATV
                    I-O      CADEST.

       ROT-100-000.
           DISPLAY  LPA-SFI  MEN-001
           MOVE     0   TO   CHV-ATV
           ACCEPT   (08 44)  CHV-ATV  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           READ     CADATV   INVALID KEY
                    GO  TO   ROT-100-000.
           MOVE     CHV-ATV  TO  PRX-INI
           DISPLAY  PRI-ENT.

       ROT-100-010.
           DISPLAY  LPA-PRF  MEN-002
           MOVE     0   TO   SUF-EST
           ACCEPT   (08 46)  SUF-EST  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-000.
           IF  SUF-EST   =   ZEROS
               GO  TO        ROT-100-010.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     PRX-INI  TO  PRX-EST
           READ     CADEST   INVALID KEY
                    DISPLAY  MEN-003
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-010.
           MOVE     SUF-EST  TO  SUF-INI
           DISPLAY  SFI-ENT.

       ROT-100-020.
           DISPLAY  LPA-SFF  MEN-004
           MOVE     0   TO   CHV-ATV
           ACCEPT   (10 44)  CHV-ATV  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           READ     CADATV   INVALID KEY
                    GO  TO   ROT-100-020.
           MOVE     CHV-ATV  TO  PRX-FIN
           DISPLAY  PRF-ENT.

       ROT-100-030.
           DISPLAY  LPA-PCT  MEN-005
           MOVE     0   TO   SUF-EST
           ACCEPT   (10 46)  SUF-EST  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           IF  SUF-EST   =   ZEROS
               GO  TO        ROT-100-030.
           MOVE     SUF-EST  TO  SUF-FIN
           MOVE     SPACES   TO  RSP-OPC
           IF  CHV-FIN   <   CHV-INI
               DISPLAY       MEN-010
               ACCEPT        OPC-001
               GO  TO        ROT-100-030.
           MOVE     PRX-FIN  TO  PRX-EST
           READ     CADEST   INVALID KEY
                    DISPLAY  MEN-003
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-030.
           DISPLAY  SFF-ENT
           MOVE     ZEROS    TO  PCT-AUX
                                 FTR-AUX.
       ROT-100-040.
           DISPLAY  LPA-FTR  MEN-006
           ACCEPT   (13 44)  PCT-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           IF  PCT-AUX   =   ZEROS
               GO  TO        ROT-100-040.
           DISPLAY  PCT-ENT.

       ROT-100-050.
           DISPLAY  MEN-007
           ACCEPT   (15 44)  FTR-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.
           DISPLAY  FTR-ENT  MEN-008
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-050.
           DISPLAY  MEN-009
           MOVE     CHV-INI  TO  CHV-EST
           START    CADEST   KEY  EQUAL   CHV-EST.

       ROT-100-060.
           READ     CADEST   NEXT AT END
                    GO  TO   ROT-100-080.
           IF  CHV-EST   >   CHV-FIN
               GO  TO        ROT-100-080.
           COMPUTE  VDA-EST  =   VDA-EST  *  PCT-AUX
                                 /   100  +  VDA-EST
           COMPUTE  VAL-AUX  =   VDA-EST  /  FTR-AUX
           IF  FTR-AUX   =   0   OR  RST-AUX   =   0
               GO  TO        ROT-100-070.
           COMPUTE  VDA-EST  =   INT-AUX  *  FTR-AUX  +  FTR-AUX.

       ROT-100-070.
           COMPUTE  PCT-EST  =  (VDA-EST  -  CTM-EST)
                                 *   100  /  VDA-EST
           REWRITE  REG-EST
           GO  TO   ROT-100-060.

       ROT-100-080.
           DISPLAY  LPA-001
           GO  TO   ROT-100-000.

       ROT-200-000.
           MOVE     2    TO  PRM-001
           CLOSE    CADATV   CADEST
           CHAIN   "DYN100"  USING  PRM-001.
