       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN210.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

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
           03  IND3             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  NUM-OPC          PIC  9(01).
           03  NUM-BMB          PIC  9(02).
           03  TECLADO          PIC  9(02).
           03  AFR-AUX          PIC  9(04).
           03  FCL-AUX          PIC  9(06)V9.
           03  ACM-QTD          PIC  9(05)V9.
           03  TOT-QTD          PIC  9(06)V9.
           03  ACM-VAL          PIC  9(10)V99.
           03  TOT-VAL          PIC  9(11)V99.

           03  DET-001.
               05  NUM-DT1      PIC  9(02).
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  ATV-DT1      PIC  X(17).
               05  QTD-DT1      PIC  ZZZ.ZZ9,9.
               05  FILLER       PIC  X(02)  VALUE  SPACES.
               05  VAL-DT1      PIC  Z.ZZZ.ZZZ.ZZ9,99.

           03  DET-002.
               05  NUM-DT2      PIC  9(02).
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  CMB-DT2      PIC  X(18).
               05  ICL-DT2      PIC  999999,9.
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  FCL-DT2      PIC  999999,9.
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  AFR-DT2      PIC  9999.
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  LTS-DT2      PIC  ZZ.ZZ9,9.

           03  TAB-REG          OCCURS  09.
               05  PRX-TAB      PIC  9(01).
               05  CMB-TAB      PIC  X(15).
               05  REG-TAB      PIC  X(48).
               05  QTD-TAB      PIC  9(06)V9.
               05  VAL-TAB      PIC  9(10)V99.

           03  TAB-BMB          PIC  X(60)  OCCURS  30.

           03  ENT-AUX.
               05  LIN-001      PIC  X(48).
               05  LIN-002      PIC  X(48).
               05  LIN-003      PIC  X(48).
               05  LIN-004      PIC  X(48).
               05  LIN-005      PIC  X(48).
               05  LIN-006      PIC  X(48).
               05  LIN-007      PIC  X(48).
               05  LIN-008      PIC  X(48).
               05  LIN-009      PIC  X(48).

           03  BMB-AUX.
               05  BMB-001      PIC  X(60).
               05  BMB-002      PIC  X(60).
               05  BMB-003      PIC  X(60).
               05  BMB-004      PIC  X(60).
               05  BMB-005      PIC  X(60).
               05  BMB-006      PIC  X(60).
               05  BMB-007      PIC  X(60).
               05  BMB-008      PIC  X(60).
               05  BMB-009      PIC  X(60).
               05  BMB-010      PIC  X(60).

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 05 COLUMN 13 PIC X(56) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 15 PIC X(52) FROM
               "  N§  COMBUSTIVEL     QUANTIDADE    VALOR DA VENDA"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 06 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 07 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 08 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 09 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 15 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 16 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 13 PIC X(56) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" BACKGROUND-COLOR 1.
           03  LINE 17 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 13 PIC X(56) FROM " ³
               "                               ³" BACKGROUND-COLOR 1.
           03  LINE 18 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 13 PIC X(56) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 1.
           03  LINE 19 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 15 PIC X(56) FROM SPACES.

       01  TLA-002.
           03  LINE 05 COLUMN 06 PIC X(68) FROM
               " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 08 PIC X(64) FROM "  N§   COMBUSTIVEL
               "  ABERTURA  FECHAMENTO  AFER     LITROS"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR 14.
           03  LINE 06 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 07 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 08 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 09 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 15 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 16 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 17 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 06 PIC X(68) FROM
               " ³
               "          ³" BACKGROUND-COLOR 1.
           03  LINE 18 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 06 PIC X(68) FROM
               " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 1.
           03  LINE 19 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 08 PIC X(68) FROM SPACES.

       01  ENT-001.
           03  LINE 08 COLUMN 17 PIC X(48) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 17 PIC X(48) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 17 PIC X(48) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 17 PIC X(48) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 17 PIC X(48) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 17 PIC X(48) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 17 PIC X(48) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 17 PIC X(48) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 17 PIC X(48) USING LIN-009
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 17 PIC X(20) FROM "Sub-total .........:"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.
           03  LINE 18 COLUMN 38 PIC ZZZ.ZZ9,9  USING TOT-QTD
               BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.
           03  LINE 18 COLUMN 48 PIC ZZ.ZZZ.ZZZ.ZZ9,99 USING TOT-VAL
               BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.

       01  ENT-002.
           03  LINE 08 COLUMN 10 PIC X(60) USING BMB-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 10 PIC X(60) USING BMB-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 10 PIC X(60) USING BMB-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 10 PIC X(60) USING BMB-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 10 PIC X(60) USING BMB-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 10 PIC X(60) USING BMB-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 10 PIC X(60) USING BMB-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 10 PIC X(60) USING BMB-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 10 PIC X(60) USING BMB-009
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 10 PIC X(60) USING BMB-010
               BACKGROUND-COLOR  01.

       01  ENT-003.
           03  FCL-001 LINE 08 COLUMN 44 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-001 LINE 08 COLUMN 55 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-001 LINE 08 COLUMN 62 PIC ZZ.ZZ9,9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  FCL-002 LINE 09 COLUMN 44 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-002 LINE 09 COLUMN 55 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-002 LINE 09 COLUMN 62 PIC ZZ.ZZ9,9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  FCL-003 LINE 10 COLUMN 44 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-003 LINE 10 COLUMN 55 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-003 LINE 10 COLUMN 62 PIC ZZ.ZZ9,9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  FCL-004 LINE 11 COLUMN 44 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-004 LINE 11 COLUMN 55 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-004 LINE 11 COLUMN 62 PIC ZZ.ZZ9,9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  FCL-005 LINE 12 COLUMN 44 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-005 LINE 12 COLUMN 55 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-005 LINE 12 COLUMN 62 PIC ZZ.ZZ9,9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  FCL-006 LINE 13 COLUMN 44 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-006 LINE 13 COLUMN 55 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-006 LINE 13 COLUMN 62 PIC ZZ.ZZ9,9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  FCL-007 LINE 14 COLUMN 44 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-007 LINE 14 COLUMN 55 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-007 LINE 14 COLUMN 62 PIC ZZ.ZZ9,9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  FCL-008 LINE 15 COLUMN 44 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-008 LINE 15 COLUMN 55 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-008 LINE 15 COLUMN 62 PIC ZZ.ZZ9,9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  FCL-009 LINE 16 COLUMN 44 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-009 LINE 16 COLUMN 55 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-009 LINE 16 COLUMN 62 PIC ZZ.ZZ9,9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  FCL-010 LINE 17 COLUMN 44 PIC 999999,9 USING FCL-ECR
                       BACKGROUND-COLOR  01.
           03  AFR-010 LINE 17 COLUMN 55 PIC 9(04)    USING AFR-ECR
                       BACKGROUND-COLOR  01.
           03  VDA-010 LINE 17 COLUMN 62 PIC ZZ.ZZ9,9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
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

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  CALCUL
                   "ANDO VENDA DE COMBUSTIVEIS ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [F1] P/ CORRI
                   "GIR  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DO
                   "COMBUSTIVEL ?  [ ]  -  [ESC] RETORNA ...      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO BOMBAS ...                              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA, [F1] P/ ALTERAR - [ESC] RETORNA ..."
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "BOMBA ?  [  ]  -  [ESC] RETORNAR ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O ENCERRANTE
                   " DE FECHAMENTO  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O TOTAL EM L
                   "ITROS DA AFERICAO  -  [ESC] RETORNA ...       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "VENDA INCOERENTE !!
                   "!  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002  LINE 22 COLUMN 49 PIC 9(01) USING NUM-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-003  LINE 22 COLUMN 43 PIC 9(02) USING NUM-BMB AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-002.

       ROT-000-000.
           OPEN     I-O      CADECR
                             CADPRS.
       ROT-000-010.
           DISPLAY  TLA-001
                    MEN-001
           MOVE     1   TO   IND1.

       ROT-000-020.
           MOVE     SPACES   TO  CMB-TAB (IND1)
                                 REG-TAB (IND1)
           MOVE     ZEROS    TO  PRX-TAB (IND1)
                                 QTD-TAB (IND1)
                                 VAL-TAB (IND1)
           ADD      1        TO  IND1
           IF  IND1  <  10
               GO  TO        ROT-000-020.
           MOVE     ZEROS    TO  CHV-ECR
           START    CADECR   KEY GREATER  CHV-ECR.

       ROT-000-030.
           READ     CADECR   NEXT AT END
                    GO  TO   ROT-000-040.
           IF  CXA-PR2  NOT  =   CX1-ECR
                        AND      CX2-ECR
               GO  TO        ROT-000-030.
           IF  FLG-ECR   =   0
               COMPUTE       ACM-QTD  =  (FCL-ECR + 1000000 - ICL-ECR)
                                      -   AFR-ECR
           ELSE
               COMPUTE       ACM-QTD  =  (FTL-ECR + 1000000 - ICL-ECR)
                                      +  (FCL-ECR + 1000000 - ITL-ECR)
                                      -   AFR-ECR.
           COMPUTE  ACM-VAL  =   ACM-QTD  *   VAL-ECR
           ADD      ACM-QTD  TO  QTD-TAB     (CMB-ECR)
           ADD      ACM-VAL  TO  VAL-TAB     (CMB-ECR)
           GO  TO   ROT-000-030.

       ROT-000-040.
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     ZEROS    TO  TOT-QTD
                                 TOT-VAL  IND1
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-000-050.
           READ     CADPRS   NEXT
           IF  ATV-PRS  NOT  =  1
               GO  TO        ROT-000-060.
           MOVE     QTD-TAB (PRX-PRS) TO  QTD-PRS
           MOVE     VAL-TAB (PRX-PRS) TO  VDD-PRS
           REWRITE  REG-PRS
           ADD      1        TO  IND1
           MOVE     IND1     TO  NUM-DT1
           MOVE     DES-PRS  TO  ATV-DT1
           MOVE     QTD-PRS  TO  QTD-DT1
           MOVE     VDD-PRS  TO  VAL-DT1
           MOVE     PRX-PRS  TO  PRX-TAB (IND1)
           MOVE     DES-PRS  TO  CMB-TAB (IND1)
           MOVE     DET-001  TO  REG-TAB (IND1)
           ADD      QTD-PRS  TO  TOT-QTD
           ADD      VDD-PRS  TO  TOT-VAL
           GO  TO   ROT-000-050.

       ROT-000-060.
           MOVE     REG-TAB (01) TO  LIN-001
           MOVE     REG-TAB (02) TO  LIN-002
           MOVE     REG-TAB (03) TO  LIN-003
           MOVE     REG-TAB (04) TO  LIN-004
           MOVE     REG-TAB (05) TO  LIN-005
           MOVE     REG-TAB (06) TO  LIN-006
           MOVE     REG-TAB (07) TO  LIN-007
           MOVE     REG-TAB (08) TO  LIN-008
           MOVE     REG-TAB (09) TO  LIN-009
           DISPLAY  ENT-001.

       ROT-000-070.
           DISPLAY  MEN-002
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-080.
           IF  TECLADO   =   02
               GO  TO        ROT-100-000.
           IF  TECLADO  NOT  =   0
               GO  TO        ROT-000-070.
           MOVE     TUR-PR2  TO  TUR-PRS
           MOVE     CXA-PR2  TO  CXA-PRS
           MOVE     2        TO  ATV-PRS
           MOVE     ZEROS    TO  LIN-PRS
           START    CADPRS   KEY GREATER  CHV-PRS
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   2   CHAIN   "DYN211"  USING  PRM-002.
           IF  ATV-PRS   =   3   CHAIN   "DYN212"  USING  PRM-002.
           IF  ATV-PRS   =   4   CHAIN   "DYN213"  USING  PRM-002.
           IF  ATV-PRS   =   5   CHAIN   "DYN214"  USING  PRM-002.
           IF  ATV-PRS   >   5   CHAIN   "DYN215"  USING  PRM-002.

       ROT-000-080.
           CLOSE    CADECR   CADPRS
           CHAIN   "DYN209"  USING  PRM-002.

       ROT-100-000.
           MOVE     ZEROS    TO  NUM-OPC
           DISPLAY  MEN-003
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-070.
           IF  NUM-OPC   <   1 OR > IND1
               GO  TO        ROT-100-000.
           DISPLAY  TLA-002      MEN-004
           MOVE     ZEROS    TO  CHV-ECR  IND1
           START    CADECR   KEY GREATER  CHV-ECR.

       ROT-100-010.
           READ     CADECR   NEXT AT END
                    GO  TO   ROT-100-020.
           IF  CXA-PR2  NOT  =   CX1-ECR
                        AND      CX2-ECR
               GO  TO        ROT-100-010.
           IF  CMB-ECR  NOT  =   PRX-TAB (NUM-OPC)
               GO  TO        ROT-100-010.
           ADD      1        TO  IND1
           MOVE     CMB-TAB (NUM-OPC) TO  CMB-DT2
           MOVE     CHV-ECR  TO  NUM-DT2
           MOVE     ICL-ECR  TO  ICL-DT2
           MOVE     FCL-ECR  TO  FCL-DT2
           MOVE     AFR-ECR  TO  AFR-DT2
           IF  FLG-ECR   =   0
               COMPUTE       ACM-QTD  =  (FCL-ECR + 1000000 - ICL-ECR)
                                      -   AFR-ECR
           ELSE
               COMPUTE       ACM-QTD  =  (FTL-ECR + 1000000 - ICL-ECR)
                                      +  (FCL-ECR + 1000000 - ITL-ECR)
                                      -   AFR-ECR.
           MOVE     ACM-QTD  TO  LTS-DT2
           MOVE     DET-002  TO  TAB-BMB     (IND1)
           MOVE     IND1     TO  IND2
           GO  TO   ROT-100-010.

       ROT-100-020.
           IF  IND1  >  29
               MOVE     0    TO  IND1
               GO  TO        ROT-100-030.
           ADD      1        TO  IND1
           MOVE     SPACES   TO  TAB-BMB (IND1)
           GO  TO   ROT-100-020.

       ROT-100-030.
           MOVE     TAB-BMB (IND1 + 01)  TO  BMB-001
           MOVE     TAB-BMB (IND1 + 02)  TO  BMB-002
           MOVE     TAB-BMB (IND1 + 03)  TO  BMB-003
           MOVE     TAB-BMB (IND1 + 04)  TO  BMB-004
           MOVE     TAB-BMB (IND1 + 05)  TO  BMB-005
           MOVE     TAB-BMB (IND1 + 06)  TO  BMB-006
           MOVE     TAB-BMB (IND1 + 07)  TO  BMB-007
           MOVE     TAB-BMB (IND1 + 08)  TO  BMB-008
           MOVE     TAB-BMB (IND1 + 09)  TO  BMB-009
           MOVE     TAB-BMB (IND1 + 10)  TO  BMB-010
           ADD      10   TO  IND1
           DISPLAY  ENT-002.

       ROT-100-040.
           DISPLAY  MEN-005
           MOVE     " "  TO  RSP-OPC
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-007
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-100-050.
           IF  TECLADO   =   01
               DISPLAY       LPA-001
               GO  TO        ROT-000-010.
           IF  TECLADO   =   02
               GO  TO        ROT-100-060.
           IF  TECLADO  NOT  =  00
               GO  TO        ROT-100-040.
           IF  IND1     NOT  <   IND2
               GO  TO        ROT-100-040.
           GO  TO   ROT-100-030.

       ROT-100-050.
           IF  IND1  <  11
               GO  TO        ROT-100-040.
           COMPUTE  IND1  =  IND1  -  20
           GO  TO   ROT-100-030.

       ROT-100-060.
           MOVE     ZEROS    TO  NUM-BMB
           DISPLAY  MEN-006
           ACCEPT   OPC-003
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-001
               GO  TO        ROT-000-010.
           MOVE     ZEROS    TO  CHV-ECR  IND3
           START    CADECR   KEY GREATER  CHV-ECR.

       ROT-100-070.
           READ     CADECR   NEXT AT END
                    GO  TO   ROT-100-060.
           IF  CXA-PR2  NOT  =   CX1-ECR
                        AND      CX2-ECR  OR
               CMB-ECR  NOT  =   PRX-TAB (NUM-OPC)
               GO  TO        ROT-100-070.
           ADD      1        TO  IND3
           IF  CHV-ECR  NOT  =   NUM-BMB
               GO  TO        ROT-100-070.
           IF  IND3      <   (IND1 - 09)
               OR  IND3  >    IND1
               GO  TO        ROT-100-060.
           IF  IND3  >  20
               COMPUTE  LIN  = IND3 - 13.
           IF  IND3  >  10
               COMPUTE  LIN  = IND3 - 03.
           IF  IND3  NOT  >  10
               COMPUTE  LIN  = IND3 + 07.
           MOVE     FCL-ECR  TO  FCL-AUX
           MOVE     AFR-ECR  TO  AFR-AUX.

       ROT-100-080.
           DISPLAY  MEN-008
           ACCEPT  (LIN, 44) FCL-ECR  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO  NOT  =   01
               GO  TO        ROT-100-090.
           MOVE     FCL-AUX  TO  FCL-ECR
           PERFORM  ROT-100-090
           GO  TO   ROT-100-060.

       ROT-100-090.
           IF  IND3   =  01  OR  11  OR  21  DISPLAY  FCL-001.
           IF  IND3   =  02  OR  12  OR  22  DISPLAY  FCL-002.
           IF  IND3   =  03  OR  13  OR  23  DISPLAY  FCL-003.
           IF  IND3   =  04  OR  14  OR  24  DISPLAY  FCL-004.
           IF  IND3   =  05  OR  15  OR  25  DISPLAY  FCL-005.
           IF  IND3   =  06  OR  16  OR  26  DISPLAY  FCL-006.
           IF  IND3   =  07  OR  17  OR  27  DISPLAY  FCL-007.
           IF  IND3   =  08  OR  18  OR  28  DISPLAY  FCL-008.
           IF  IND3   =  09  OR  19  OR  29  DISPLAY  FCL-009.
           IF  IND3   =  10  OR  20  OR  30  DISPLAY  FCL-010.

       ROT-100-100.
           DISPLAY  MEN-009
           ACCEPT  (LIN, 55) AFR-ECR  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-130.
           IF  FLG-ECR   =   0
               COMPUTE       ACM-QTD  =  (FCL-ECR + 1000000 - ICL-ECR)
           ELSE
               COMPUTE       ACM-QTD  =  (FTL-ECR + 1000000 - ICL-ECR)
                                      +  (FCL-ECR + 1000000 - ITL-ECR).
           MOVE    SPACES    TO  RSP-OPC
           IF  AFR-ECR   >   ACM-QTD
               DISPLAY       MEN-010
               ACCEPT        OPC-001
               GO  TO        ROT-100-100.
           COMPUTE  ACM-QTD  =   ACM-QTD  -   AFR-ECR.

       ROT-100-110.
           IF  IND3   =  01  OR  11  OR  21  DISPLAY  AFR-001  VDA-001.
           IF  IND3   =  02  OR  12  OR  22  DISPLAY  AFR-002  VDA-002.
           IF  IND3   =  03  OR  13  OR  23  DISPLAY  AFR-003  VDA-003.
           IF  IND3   =  04  OR  14  OR  24  DISPLAY  AFR-004  VDA-004.
           IF  IND3   =  05  OR  15  OR  25  DISPLAY  AFR-005  VDA-005.
           IF  IND3   =  06  OR  16  OR  26  DISPLAY  AFR-006  VDA-006.
           IF  IND3   =  07  OR  17  OR  27  DISPLAY  AFR-007  VDA-007.
           IF  IND3   =  08  OR  18  OR  28  DISPLAY  AFR-008  VDA-008.
           IF  IND3   =  09  OR  19  OR  29  DISPLAY  AFR-009  VDA-009.
           IF  IND3   =  10  OR  20  OR  30  DISPLAY  AFR-010  VDA-010.

       ROT-100-120.
           REWRITE  REG-ECR
           GO  TO   ROT-100-060.

       ROT-100-130.
           MOVE     AFR-AUX  TO  AFR-ECR
           IF  IND3   =  01  OR  11  OR  21  DISPLAY  AFR-001.
           IF  IND3   =  02  OR  12  OR  22  DISPLAY  AFR-002.
           IF  IND3   =  03  OR  13  OR  23  DISPLAY  AFR-003.
           IF  IND3   =  04  OR  14  OR  24  DISPLAY  AFR-004.
           IF  IND3   =  05  OR  15  OR  25  DISPLAY  AFR-005.
           IF  IND3   =  06  OR  16  OR  26  DISPLAY  AFR-006.
           IF  IND3   =  07  OR  17  OR  27  DISPLAY  AFR-007.
           IF  IND3   =  08  OR  18  OR  28  DISPLAY  AFR-008.
           IF  IND3   =  09  OR  19  OR  29  DISPLAY  AFR-009.
           IF  IND3   =  10  OR  20  OR  30  DISPLAY  AFR-010.
           GO  TO   ROT-100-080.