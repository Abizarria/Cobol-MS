       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN221.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCTR        ASSIGN  TO  DISK.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS.

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
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  QTD-CMB          PIC  9(07)V9.
           03  QTD-PRD          PIC  9(07)V9.
           03  QTD-SRV          PIC  9(07)V9.
           03  VAL-CMB          PIC  9(11)V99.
           03  VAL-PRD          PIC  9(11)V99.
           03  VAL-SRV          PIC  9(11)V99.
           03  VAL-TOT          PIC  9(12)V99.

           03  DTA-INI.
               05  ANO-INI      PIC  9(02).
               05  MES-INI      PIC  9(02).
               05  DIA-INI      PIC  9(02).

           03  DTA-FIN.
               05  ANO-FIN      PIC  9(02).
               05  MES-FIN      PIC  9(02).
               05  DIA-FIN      PIC  9(02).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 01 PIC X(67) FROM
               " POSICAO ATUAL DE CAIXAS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.

       01  TLA-002.
           03  LINE 05 COLUMN 10 PIC X(60) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 10 PIC X(60) FROM " ³     CAIXAS FECHADOS
              "NO PERIODO DE   /  /   A   /  /    ³" BACKGROUND-COLOR 1.
           03  LINE 06 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 10 PIC X(60) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" BACKGROUND-COLOR 1.
           03  LINE 07 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 10 PIC X(60) FROM " ³
              "                                   ³" BACKGROUND-COLOR 1.
           03  LINE 08 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 10 PIC X(60) FROM " ³
              "                                   ³" BACKGROUND-COLOR 1.
           03  LINE 09 COLUMN 12 PIC X(56) FROM
               "  ORIGEM / ATIVIDADE     QUANTIDADE   VALOR DAS VENDAS"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 09 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 10 PIC X(60) FROM " ³
              "                                   ³" BACKGROUND-COLOR 1.
           03  LINE 10 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 10 PIC X(60) FROM " ³  Combustiveis .....:
              "                                   ³" BACKGROUND-COLOR 1.
           03  LINE 11 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 10 PIC X(60) FROM " ³
              "                                   ³" BACKGROUND-COLOR 1.
           03  LINE 12 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 10 PIC X(60) FROM " ³  Outros Produtos ..:
              "                                   ³" BACKGROUND-COLOR 1.
           03  LINE 13 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 10 PIC X(60) FROM " ³
              "                                   ³" BACKGROUND-COLOR 1.
           03  LINE 14 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 10 PIC X(60) FROM " ³  Servicos .........:
              "                                   ³" BACKGROUND-COLOR 1.
           03  LINE 15 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 10 PIC X(60) FROM " ³
              "                                   ³" BACKGROUND-COLOR 1.
           03  LINE 16 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 10 PIC X(60) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" BACKGROUND-COLOR 1.
           03  LINE 17 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 10 PIC X(60) FROM " ³
              "                                   ³" BACKGROUND-COLOR 1.
           03  LINE 18 COLUMN 14 PIC X(32) FROM "Valor Total das Vendas
              ".........."    BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.
           03  LINE 18 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 10 PIC X(60) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
              "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 1.
           03  LINE 19 COLUMN 70 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 12 PIC X(60) FROM SPACES.

       01  ENT-001.
           03  LINE 06 COLUMN 14 PIC 9(02) USING DIA-CTR
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 47 PIC 9(02) USING DIA-INI
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 50 PIC 9(02) USING MES-INI
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 53 PIC 9(02) USING ANO-INI
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 58 PIC 9(02) USING DIA-FIN
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 61 PIC 9(02) USING MES-FIN
               BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 64 PIC 9(02) USING ANO-FIN
               BACKGROUND-COLOR  01.

       01  ENT-002.
           03  LINE 11 COLUMN 35 PIC Z.ZZZ.ZZ9,9        USING QTD-CMB
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 49 PIC ZZ.ZZZ.ZZZ.ZZ9,99  USING VAL-CMB
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 35 PIC Z.ZZZ.ZZ9,9        USING QTD-PRD
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 49 PIC ZZ.ZZZ.ZZZ.ZZ9,99  USING VAL-PRD
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 35 PIC Z.ZZZ.ZZ9,9        USING QTD-SRV
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 49 PIC ZZ.ZZZ.ZZZ.ZZ9,99  USING VAL-SRV
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 48 PIC ZZZ.ZZZ.ZZZ.ZZ9,99 USING VAL-TOT
               BACKGROUND-COLOR  01  FOREGROUND-COLOR   15.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ACUMUL
                   "ANDO VENDAS ...                            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ESC] P/ RETO
                   "RNAR ...                                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001
           OPEN     INPUT    CADCTR
                             CADPRS
           READ     CADCTR
           DISPLAY  TLA-002
           MOVE     DTI-CTR  TO  DTA-INI
           MOVE     DTD-CTR  TO  DTA-FIN
           DISPLAY  ENT-001      MEN-001
           MOVE     ZEROS    TO  VAL-TOT
                    QTD-CMB      QTD-PRD  QTD-SRV
                    VAL-CMB      VAL-PRD  VAL-SRV
           MOVE     SPACES   TO  CHV-PRS
           START    CADPRS   KEY GREATER  CHV-PRS.

       ROT-000-010.
           READ     CADPRS   NEXT AT END
                    GO  TO   ROT-000-020.
           IF  ATV-PRS   >   3
               GO  TO        ROT-000-010.
           IF  ATV-PRS   =   1
               ADD  QTM-PRS  TO  QTD-CMB
               ADD  VDM-PRS  TO  VAL-CMB.
           IF  ATV-PRS   =   2
               ADD  QTM-PRS  TO  QTD-PRD
               ADD  VDM-PRS  TO  VAL-PRD.
           IF  ATV-PRS   =   3
               ADD  QTM-PRS  TO  QTD-SRV
               ADD  VDM-PRS  TO  VAL-SRV.
           ADD      VDM-PRS  TO  VAL-TOT
           GO  TO   ROT-000-010.

       ROT-000-020.
           DISPLAY  ENT-002      MEN-002.

       ROT-000-030.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO  NOT  =  1
               GO  TO        ROT-000-030.
           CLOSE    CADCTR   CADPRS
           MOVE     4   TO   PRM-001
           CHAIN   "DYN200"  USING  PRM-001.
