       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN202.
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

           SELECT      CADECR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ECR.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCMB      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCMB.DAT".
       01  REG-CMB.
           03  CHV-CMB          PIC  9(01).
           03  DES-CMB          PIC  X(15).
           03  VVM-CMB          PIC  9(11)V99.
           03  LUC-CMB          PIC  9(04)V9999.

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
           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ACM-LTS          PIC  9(05)V9.

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 06 COLUMN 69 PIC X(07) FROM " ÚÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 07 COLUMN 69 PIC X(07) FROM " ³ E ³"
               BACKGROUND-COLOR  04.
           03  LINE 07 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 69 PIC X(07) FROM " ³ N ³"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 69 PIC X(07) FROM " ³ C ³"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 69 PIC X(07) FROM " ³ E ³"
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 69 PIC X(07) FROM " ³ R ³"
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 69 PIC X(07) FROM " ³ R ³"
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 69 PIC X(07) FROM " ³ A ³"
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 69 PIC X(07) FROM " ³ N ³"
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 69 PIC X(07) FROM " ³ T ³"
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 69 PIC X(07) FROM " ³ E ³"
               BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 69 PIC X(07) FROM " ³ S ³"
               BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 69 PIC X(07) FROM " ÀÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 18 COLUMN 76 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 71 PIC X(07) FROM SPACES.

       01  TLA-002.
           03  LINE 05 COLUMN 04 PIC X(61) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
             "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 06 PIC X(57) FROM
             " N§  COMBUSTIVEL     ABERTURA  FECHAMENTO  AFER   LITROS"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 06 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 07 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 08 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 09 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 15 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 16 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 17 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 04 PIC X(61) FROM " ³
             "                                    ³" BACKGROUND-COLOR 1.
           03  LINE 18 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 04 PIC X(61) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
             "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 1.
           03  LINE 19 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 06 PIC X(61) FROM SPACES.

       01  LIN-001.
           03  LINE 08 COLUMN 07 PIC 9(02)    USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 11 PIC X(15)    USING DES-CMB
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 27 PIC 999999,9 USING ICL-ECR
               BACKGROUND-COLOR  01.

       01  LIN-002.
           03  LINE 09 COLUMN 07 PIC 9(02)    USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 11 PIC X(15)    USING DES-CMB
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 27 PIC 999999,9 USING ICL-ECR
               BACKGROUND-COLOR  01.

       01  LIN-003.
           03  LINE 10 COLUMN 07 PIC 9(02)    USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 11 PIC X(15)    USING DES-CMB
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 27 PIC 999999,9 USING ICL-ECR
               BACKGROUND-COLOR  01.

       01  LIN-004.
           03  LINE 11 COLUMN 07 PIC 9(02)    USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 11 PIC X(15)    USING DES-CMB
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 27 PIC 999999,9 USING ICL-ECR
               BACKGROUND-COLOR  01.

       01  LIN-005.
           03  LINE 12 COLUMN 07 PIC 9(02)    USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 11 PIC X(15)    USING DES-CMB
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 27 PIC 999999,9 USING ICL-ECR
               BACKGROUND-COLOR  01.

       01  LIN-006.
           03  LINE 13 COLUMN 07 PIC 9(02)    USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 11 PIC X(15)    USING DES-CMB
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 27 PIC 999999,9 USING ICL-ECR
               BACKGROUND-COLOR  01.

       01  LIN-007.
           03  LINE 14 COLUMN 07 PIC 9(02)    USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 11 PIC X(15)    USING DES-CMB
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 27 PIC 999999,9 USING ICL-ECR
               BACKGROUND-COLOR  01.

       01  LIN-008.
           03  LINE 15 COLUMN 07 PIC 9(02)    USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 11 PIC X(15)    USING DES-CMB
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 27 PIC 999999,9 USING ICL-ECR
               BACKGROUND-COLOR  01.

       01  LIN-009.
           03  LINE 16 COLUMN 07 PIC 9(02)    USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 11 PIC X(15)    USING DES-CMB
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 27 PIC 999999,9 USING ICL-ECR
               BACKGROUND-COLOR  01.

       01  LIN-010.
           03  LINE 17 COLUMN 07 PIC 9(02)    USING CHV-ECR
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 11 PIC X(15)    USING DES-CMB
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 27 PIC 999999,9 USING ICL-ECR
               BACKGROUND-COLOR  01.

       01  ENT-001.
           03  FCL-001 LINE 08 COLUMN 38 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-001 LINE 08 COLUMN 49 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-001 LINE 08 COLUMN 54 PIC ZZ.ZZ9,9 USING ACM-LTS
                       BACKGROUND-COLOR  01.
           03  FCL-002 LINE 09 COLUMN 38 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-002 LINE 09 COLUMN 49 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-002 LINE 09 COLUMN 54 PIC ZZ.ZZ9,9 USING ACM-LTS
                       BACKGROUND-COLOR  01.
           03  FCL-003 LINE 10 COLUMN 38 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-003 LINE 10 COLUMN 49 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-003 LINE 10 COLUMN 54 PIC ZZ.ZZ9,9 USING ACM-LTS
                       BACKGROUND-COLOR  01.
           03  FCL-004 LINE 11 COLUMN 38 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-004 LINE 11 COLUMN 49 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-004 LINE 11 COLUMN 54 PIC ZZ.ZZ9,9 USING ACM-LTS
                       BACKGROUND-COLOR  01.
           03  FCL-005 LINE 12 COLUMN 38 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-005 LINE 12 COLUMN 49 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-005 LINE 12 COLUMN 54 PIC ZZ.ZZ9,9 USING ACM-LTS
                       BACKGROUND-COLOR  01.
           03  FCL-006 LINE 13 COLUMN 38 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-006 LINE 13 COLUMN 49 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-006 LINE 13 COLUMN 54 PIC ZZ.ZZ9,9 USING ACM-LTS
                       BACKGROUND-COLOR  01.
           03  FCL-007 LINE 14 COLUMN 38 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-007 LINE 14 COLUMN 49 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-007 LINE 14 COLUMN 54 PIC ZZ.ZZ9,9 USING ACM-LTS
                       BACKGROUND-COLOR  01.
           03  FCL-008 LINE 15 COLUMN 38 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-008 LINE 15 COLUMN 49 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-008 LINE 15 COLUMN 54 PIC ZZ.ZZ9,9 USING ACM-LTS
                       BACKGROUND-COLOR  01.
           03  FCL-009 LINE 16 COLUMN 38 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-009 LINE 16 COLUMN 49 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-009 LINE 16 COLUMN 54 PIC ZZ.ZZ9,9 USING ACM-LTS
                       BACKGROUND-COLOR  01.
           03  FCL-010 LINE 17 COLUMN 38 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-010 LINE 17 COLUMN 49 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-010 LINE 17 COLUMN 54 PIC ZZ.ZZ9,9 USING ACM-LTS
                       BACKGROUND-COLOR  01.

       01  LPA-LIN.
           03  LPA-001 LINE 08 COLUMN 07 PIC X(55) FROM SPACES
                       BACKGROUND-COLOR 01.
           03  LPA-002 LINE 09 COLUMN 07 PIC X(55) FROM SPACES
                       BACKGROUND-COLOR 01.
           03  LPA-003 LINE 10 COLUMN 07 PIC X(55) FROM SPACES
                       BACKGROUND-COLOR 01.
           03  LPA-004 LINE 11 COLUMN 07 PIC X(55) FROM SPACES
                       BACKGROUND-COLOR 01.
           03  LPA-005 LINE 12 COLUMN 07 PIC X(55) FROM SPACES
                       BACKGROUND-COLOR 01.
           03  LPA-006 LINE 13 COLUMN 07 PIC X(55) FROM SPACES
                       BACKGROUND-COLOR 01.
           03  LPA-007 LINE 14 COLUMN 07 PIC X(55) FROM SPACES
                       BACKGROUND-COLOR 01.
           03  LPA-008 LINE 15 COLUMN 07 PIC X(55) FROM SPACES
                       BACKGROUND-COLOR 01.
           03  LPA-009 LINE 16 COLUMN 07 PIC X(55) FROM SPACES
                       BACKGROUND-COLOR 01.
           03  LPA-010 LINE 17 COLUMN 07 PIC X(55) FROM SPACES
                       BACKGROUND-COLOR 01.

       01  LPA-TLA.
           03  LINE 05 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO COMBUSTIVEIS ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O ENCERRANTE
                   " DE FECHAMENTO  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O TOTAL EM L
                   "ITROS DA AFERICAO  -  [ESC] RETORNA ...     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "ENCERRANTE OU AFERI
                   "CAO INCOERENTE !!!  -  [ESC] RETORNA ...    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS ENCERRANTES  -  [ESC] RETORNA ..."
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-002.

       ROT-000-000.
           DISPLAY  LPA-TLA  TLA-001
                    TLA-002  MEN-001
           OPEN     I-O      CADECR
                    INPUT    CADCMB
                             CADPRS
           MOVE     8        TO  LIN
           MOVE     1        TO  IND1
           MOVE     ZEROS    TO  CHV-ECR
           START    CADECR   KEY GREATER  CHV-ECR.

       ROT-100-000.
           READ     CADECR   NEXT AT END
                    GO  TO   ROT-200-000.
           IF  CXA-PR2  NOT  =   CX1-ECR
                        AND      CX2-ECR
               GO  TO        ROT-100-000.
           MOVE     CMB-ECR  TO  CHV-CMB
           READ     CADCMB
           IF  LIN  >   17
               DISPLAY       LPA-LIN
               MOVE      08  TO  LIN.
           IF  IND1   =  01  OR  11  OR  21  OR  31  DISPLAY  LIN-001.
           IF  IND1   =  02  OR  12  OR  22  OR  32  DISPLAY  LIN-002.
           IF  IND1   =  03  OR  13  OR  23  OR  33  DISPLAY  LIN-003.
           IF  IND1   =  04  OR  14  OR  24  OR  34  DISPLAY  LIN-004.
           IF  IND1   =  05  OR  15  OR  25  OR  35  DISPLAY  LIN-005.
           IF  IND1   =  06  OR  16  OR  26  OR  36  DISPLAY  LIN-006.
           IF  IND1   =  07  OR  17  OR  27  OR  37  DISPLAY  LIN-007.
           IF  IND1   =  08  OR  18  OR  28  OR  38  DISPLAY  LIN-008.
           IF  IND1   =  09  OR  19  OR  29  OR  39  DISPLAY  LIN-009.
           IF  IND1   =  10  OR  20  OR  30  OR  40  DISPLAY  LIN-010.

       ROT-100-010.
           DISPLAY  MEN-002
           ACCEPT  (LIN, 38) FCL-ECR  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           IF  IND1   =  01  OR  11  OR  21  OR  31  DISPLAY  FCL-001.
           IF  IND1   =  02  OR  12  OR  22  OR  32  DISPLAY  FCL-002.
           IF  IND1   =  03  OR  13  OR  23  OR  33  DISPLAY  FCL-003.
           IF  IND1   =  04  OR  14  OR  24  OR  34  DISPLAY  FCL-004.
           IF  IND1   =  05  OR  15  OR  25  OR  35  DISPLAY  FCL-005.
           IF  IND1   =  06  OR  16  OR  26  OR  36  DISPLAY  FCL-006.
           IF  IND1   =  07  OR  17  OR  27  OR  37  DISPLAY  FCL-007.
           IF  IND1   =  08  OR  18  OR  28  OR  38  DISPLAY  FCL-008.
           IF  IND1   =  09  OR  19  OR  29  OR  39  DISPLAY  FCL-009.
           IF  IND1   =  10  OR  20  OR  30  OR  40  DISPLAY  FCL-010.

       ROT-100-020.
           DISPLAY  MEN-003
           ACCEPT  (LIN, 49) AFR-ECR  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-070.
           IF  FLG-ECR   =   0
               COMPUTE       ACM-LTS  =  FCL-ECR + 1000000 - ICL-ECR
           ELSE
               COMPUTE       ACM-LTS  = (FTL-ECR + 1000000 - ICL-ECR)
                                      + (FCL-ECR + 1000000 - ITL-ECR).
           MOVE    SPACES    TO  RSP-OPC
           IF  AFR-ECR   >   ACM-LTS
               DISPLAY       MEN-004
               ACCEPT        OPC-001
               GO  TO        ROT-100-020.
           COMPUTE  ACM-LTS  =   ACM-LTS  -  AFR-ECR
           IF  IND1   =  01  OR  11  OR  21  OR  31
               DISPLAY       AFR-001         VDA-001.
           IF  IND1   =  02  OR  12  OR  22  OR  32
               DISPLAY       AFR-002         VDA-002.
           IF  IND1   =  03  OR  13  OR  23  OR  33
               DISPLAY       AFR-003         VDA-003.
           IF  IND1   =  04  OR  14  OR  24  OR  34
               DISPLAY       AFR-004         VDA-004.
           IF  IND1   =  05  OR  15  OR  25  OR  35
               DISPLAY       AFR-005         VDA-005.
           IF  IND1   =  06  OR  16  OR  26  OR  36
               DISPLAY       AFR-006         VDA-006.
           IF  IND1   =  07  OR  17  OR  27  OR  37
               DISPLAY       AFR-007         VDA-007.
           IF  IND1   =  08  OR  18  OR  28  OR  38
               DISPLAY       AFR-008         VDA-008.
           IF  IND1   =  09  OR  19  OR  29  OR  39
               DISPLAY       AFR-009         VDA-009.
           IF  IND1   =  10  OR  20  OR  30  OR  40
               DISPLAY       AFR-010         VDA-010.
           REWRITE  REG-ECR
           IF  LIN  <    17
               ADD        1  TO  LIN
               ADD        1  TO  IND1
               GO  TO        ROT-100-000.
           DISPLAY  MEN-005
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           ADD      1  TO  LIN
           ADD      1  TO  IND1
           GO  TO   ROT-100-000.

       ROT-100-030.
           IF  IND1   =  01  OR  11  OR  21  OR  31  DISPLAY  LPA-001.
           IF  IND1   =  02  OR  12  OR  22  OR  32  DISPLAY  LPA-002.
           IF  IND1   =  03  OR  13  OR  23  OR  33  DISPLAY  LPA-003.
           IF  IND1   =  04  OR  14  OR  24  OR  34  DISPLAY  LPA-004.
           IF  IND1   =  05  OR  15  OR  25  OR  35  DISPLAY  LPA-005.
           IF  IND1   =  06  OR  16  OR  26  OR  36  DISPLAY  LPA-006.
           IF  IND1   =  07  OR  17  OR  27  OR  37  DISPLAY  LPA-007.
           IF  IND1   =  08  OR  18  OR  28  OR  38  DISPLAY  LPA-008.
           IF  IND1   =  09  OR  19  OR  29  OR  39  DISPLAY  LPA-009.
           IF  IND1   =  10  OR  20  OR  30  OR  40  DISPLAY  LPA-010.
           IF  LIN    >  08
               GO  TO    ROT-100-060.
           IF  IND1   =  01
               GO  TO    ROT-200-020.
           MOVE      17      TO  LIN
           COMPUTE   IND1    =   IND1  -  10
           MOVE      ZEROS   TO  CHV-ECR  IND2
           START     CADECR  KEY GREATER  CHV-ECR.

       ROT-100-040.
           READ     CADECR   NEXT
           ADD      1   TO   IND2
           IF  IND2     <    IND1
               GO  TO        ROT-100-040.
           DISPLAY  LPA-LIN.

       ROT-100-050.
           MOVE     CMB-ECR  TO  CHV-CMB
           READ     CADCMB
           IF  FLG-ECR   =   0
               COMPUTE       ACM-LTS  = (FCL-ECR + 1000000 - ICL-ECR)
                                      -  AFR-ECR
           ELSE
               COMPUTE       ACM-LTS  = (FTL-ECR + 1000000 - ICL-ECR)
                                      + (FCL-ECR + 1000000 - ITL-ECR)
                                      -  AFR-ECR.
           IF  IND1   =   01  OR  11  OR  21  OR  31
               DISPLAY    LIN-001 FCL-001 AFR-001 VDA-001.
           IF  IND1   =   02  OR  12  OR  22  OR  32
               DISPLAY    LIN-002 FCL-002 AFR-002 VDA-002.
           IF  IND1   =   03  OR  13  OR  23  OR  33
               DISPLAY    LIN-003 FCL-003 AFR-003 VDA-003.
           IF  IND1   =   04  OR  14  OR  24  OR  34
               DISPLAY    LIN-004 FCL-004 AFR-004 VDA-004.
           IF  IND1   =   05  OR  15  OR  25  OR  35
               DISPLAY    LIN-005 FCL-005 AFR-005 VDA-005.
           IF  IND1   =   06  OR  16  OR  26  OR  36
               DISPLAY    LIN-006 FCL-006 AFR-006 VDA-006.
           IF  IND1   =   07  OR  17  OR  27  OR  37
               DISPLAY    LIN-007 FCL-007 AFR-007 VDA-007.
           IF  IND1   =   08  OR  18  OR  28  OR  38
               DISPLAY    LIN-008 FCL-008 AFR-008 VDA-008.
           IF  IND1   =   09  OR  19  OR  29  OR  39
               DISPLAY    LIN-009 FCL-009 AFR-009 VDA-009.
           IF  IND1   =   10  OR  20  OR  30
               DISPLAY    LIN-010 FCL-010 AFR-010 VDA-010
               GO  TO     ROT-100-020.
           ADD      1  TO  IND1
           READ     CADECR NEXT
           GO  TO   ROT-100-050.

       ROT-100-060.
           COMPUTE   CHV-ECR  =   CHV-ECR  -  1
           READ      CADECR
           IF  CXA-PR2  NOT   =   CX1-ECR
                        AND       CX2-ECR
               GO  TO         ROT-100-060.
           COMPUTE   LIN   =  LIN  -  1
           COMPUTE   IND1  =  IND1 -  1
           GO  TO    ROT-100-020.

       ROT-100-070.
           IF  IND1   =  01  OR  11  OR  21  OR  31  DISPLAY  AFR-001.
           IF  IND1   =  02  OR  12  OR  22  OR  32  DISPLAY  AFR-002.
           IF  IND1   =  03  OR  13  OR  23  OR  33  DISPLAY  AFR-003.
           IF  IND1   =  04  OR  14  OR  24  OR  34  DISPLAY  AFR-004.
           IF  IND1   =  05  OR  15  OR  25  OR  35  DISPLAY  AFR-005.
           IF  IND1   =  06  OR  16  OR  26  OR  36  DISPLAY  AFR-006.
           IF  IND1   =  07  OR  17  OR  27  OR  37  DISPLAY  AFR-007.
           IF  IND1   =  08  OR  18  OR  28  OR  38  DISPLAY  AFR-008.
           IF  IND1   =  09  OR  19  OR  29  OR  39  DISPLAY  AFR-009.
           IF  IND1   =  10  OR  20  OR  30  OR  40  DISPLAY  AFR-010.
           GO  TO   ROT-100-010.

       ROT-200-000.
           IF  LIN   >   17
               GO  TO        ROT-200-010.
           DISPLAY  MEN-005
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               SUBTRACT  1   FROM  LIN
               SUBTRACT  1   FROM  IND1
               GO  TO        ROT-100-020.

       ROT-200-010.
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     1        TO  ATV-PRS
           MOVE     999      TO  LIN-PRS
           START    CADPRS   KEY GREATER CHV-PRS
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   2   CHAIN  "DYN203"  USING  PRM-002.
           IF  ATV-PRS   =   3   CHAIN  "DYN204"  USING  PRM-002.
           IF  ATV-PRS   =   4   CHAIN  "DYN205"  USING  PRM-002.
           IF  ATV-PRS   =   5   CHAIN  "DYN206"  USING  PRM-002.
           IF  ATV-PRS   =   6   CHAIN  "DYN207"  USING  PRM-002.
           IF  ATV-PRS   =   7   CHAIN  "DYN208"  USING  PRM-002.
           IF  ATV-PRS   =   8   CHAIN  "DYN209"  USING  PRM-002.

       ROT-200-020.
           CLOSE    CADCMB
                    CADECR  CADPRS
           CHAIN   "DYN201".