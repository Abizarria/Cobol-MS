       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN511.
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

           SELECT      CADCPD        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CPD.

           SELECT      CADFAT        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-FAT.

           SELECT      CADNTA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTA.

           SELECT      CADNTE        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTE.

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

       FD  CADCPD      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCPD.DAT".
       01  REG-CPD.
           03  CHV-CPD.
               05  DTA-CPD      PIC  9(06).
               05  NUM-CPD      PIC  9(06).
               05  LCT-CPD      PIC  9(03).
           03  BCO-CPD          PIC  9(03).
           03  NOM-CPD          PIC  X(30).
           03  VAL-CPD          PIC  9(09)V99.

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

       FD  CADNTE      LABEL         RECORD  IS  STANDARD
                       VALUE     OF  FILE-ID     "CADNTE.DAT".
       01  REG-NTE.
           03  CHV-NTE          PIC  9(06).
           03  DTA-NTE          PIC  9(06).
           03  CLI-NTE          PIC  X(10).
           03  PCA-NTE          PIC  X(07).
           03  DES-NTE          PIC  X(20).
           03  VAL-NTE          PIC S9(09)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ACM-QTD          PIC  9(04).
           03  ACM-VAL          PIC S9(11)V99.
           03  ACM-TOT          PIC S9(12)V99.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "POSICAO ATUAL DE CLIENTES  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 27 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 27 PIC X(29) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 29 PIC X(33) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 31 PIC X(31) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 37 PIC X(25) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 07 COLUMN 16 PIC X(48) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"  BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 16 PIC X(48) FROM " ³ Cliente ....:
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 16 PIC X(48) FROM " ³ Requisicoes :
               " R$                    ³"  BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 16 PIC X(48) FROM " ³ Faturas ....:
               " R$                    ³"  BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 16 PIC X(48) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"  BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 19 PIC X(23) FROM "Total do Cliente ......
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 16 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 16 PIC X(48) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"  BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 18 PIC X(48) FROM SPACES.

       01  TLA-003.
           03  LINE 09 COLUMN 18 PIC X(45) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"     BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 18 PIC X(45) FROM " ³
               "                    ³"     BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 18 PIC X(45) FROM " ³  Data ....:   /  /
               "  Cheques .:        ³"     BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 18 PIC X(45) FROM " ³
               "                    ³"     BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 18 PIC X(45) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"     BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 18 PIC X(45) FROM " ³
               "                    ³"     BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 22 PIC X(19) FROM "Total do Dia ......"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 18 PIC X(45) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"     BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 20 PIC X(45) FROM SPACES.

       01  TLA-004.
           03  LINE 06 COLUMN 16 PIC X(48) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"  BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 16 PIC X(48) FROM " ³ Requisicoes :
               " R$                    ³"  BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 16 PIC X(48) FROM " ³ Eventuais ..:
               " R$                    ³"  BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 16 PIC X(48) FROM " ³ Faturas ....:
               " R$                    ³"  BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 16 PIC X(48) FROM " ³ Cheques-pre :
               " R$                    ³"  BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 16 PIC X(48) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"  BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 16 PIC X(48) FROM " ³
               "                       ³"  BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 19 PIC X(23) FROM "Total a Receber .......
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 16 PIC X(48) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"  BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 64 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 18 PIC X(48) FROM SPACES.

       01  ENT-001.
           03  CLI-001  LINE 09 COLUMN 33 PIC X(10) USING CHV-CLI
                        BACKGROUND-COLOR  01.
           03  QNT-001  LINE 11 COLUMN 33 PIC Z.ZZ9 USING ACM-QTD
                        BACKGROUND-COLOR  01.
           03  VNT-001  LINE 11 COLUMN 44 PIC --.---.---.--9,99   USING
               ACM-VAL  BACKGROUND-COLOR  01.
           03  QFT-001  LINE 13 COLUMN 33 PIC Z.ZZ9 USING ACM-QTD
                        BACKGROUND-COLOR  01.
           03  VFT-001  LINE 13 COLUMN 43 PIC ---.---.---.--9,99  USING
               ACM-VAL  BACKGROUND-COLOR  01.
           03  TOT-001  LINE 16 COLUMN 42 PIC ----.---.---.--9,99 USING
               ACM-TOT  BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.

       01  ENT-002.
           03  QNT-002  LINE 08 COLUMN 33 PIC Z.ZZ9 USING ACM-QTD
                        BACKGROUND-COLOR  01.
           03  VNT-002  LINE 08 COLUMN 43 PIC ---.---.---.--9,99  USING
               ACM-VAL  BACKGROUND-COLOR  01.
           03  QNE-002  LINE 10 COLUMN 33 PIC Z.ZZ9 USING ACM-QTD
                        BACKGROUND-COLOR  01.
           03  VNE-002  LINE 10 COLUMN 43 PIC ---.---.---.--9,99  USING
               ACM-VAL  BACKGROUND-COLOR  01.
           03  QFT-002  LINE 12 COLUMN 33 PIC Z.ZZ9 USING ACM-QTD
                        BACKGROUND-COLOR  01.
           03  VFT-002  LINE 12 COLUMN 43 PIC ---.---.---.--9,99  USING
               ACM-VAL  BACKGROUND-COLOR  01.
           03  QCP-002  LINE 14 COLUMN 33 PIC Z.ZZ9 USING ACM-QTD
                        BACKGROUND-COLOR  01.
           03  VCP-002  LINE 14 COLUMN 43 PIC ---.---.---.--9,99  USING
               ACM-VAL  BACKGROUND-COLOR  01.
           03  TOT-002  LINE 17 COLUMN 42 PIC ----.---.---.--9,99 USING
               ACM-TOT  BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.

       01  ENT-003.
           03  DIA-003  LINE 11 COLUMN 33 PIC 9(02) USING DIA-AUX
                        BACKGROUND-COLOR  01.
           03  MES-003  LINE 11 COLUMN 36 PIC 9(02) USING MES-AUX
                        BACKGROUND-COLOR  01.
           03  ANO-003  LINE 11 COLUMN 39 PIC 9(02) USING ANO-AUX
                        BACKGROUND-COLOR  01.
           03  QTD-003  LINE 11 COLUMN 54 PIC Z.ZZ9 USING ACM-QTD
                        BACKGROUND-COLOR  01.
           03  VAL-003  LINE 14 COLUMN 42 PIC ZZ.ZZZ.ZZZ.ZZ9,99 USING
               ACM-VAL  BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
       01  LPA-001.
           03  LPA-QN1  LINE 11 COLUMN 33 PIC X(05) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-VN1  LINE 11 COLUMN 44 PIC X(17) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-QF1  LINE 13 COLUMN 33 PIC X(05) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-VF1  LINE 13 COLUMN 43 PIC X(18) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-TT1  LINE 16 COLUMN 42 PIC X(19) FROM SPACES
                        BACKGROUND-COLOR  01.

       01  LPA-002.
           03  LPA-MES  LINE 11 COLUMN 36 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-ANO  LINE 11 COLUMN 39 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-QTD  LINE 11 COLUMN 54 PIC X(05) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-VAL  LINE 14 COLUMN 42 PIC X(17) FROM SPACES
                        BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "CLIENTE  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "CLIENTE INEXISTENTE
                   "  -  [ESC] RETORNA ...                          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ACUMUL
                   "ANDO MOVIMENTO DO CLIENTE ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NSULTAR OUTRO CLIENTE  -  [ESC] RETORNA ...     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ACUMUL
                   "ANDO MOVIMENTO A RECEBER ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA DA CO
                   "NSULTA  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ESC] P/ RETO
                   "RNAR ...                                        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ACUMUL
                   "ANDO CHEQUES PRE-DATADOS ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NSULTAR OUTRA DATA  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001.

       ROT-000-000.
           DISPLAY  TLA-001
           IF  PRM-001   =   1
               DISPLAY       TLA-002.
           IF  PRM-001   =   2
               DISPLAY       TLA-003.
           IF  PRM-001   =   3
               DISPLAY       TLA-004.
           OPEN     INPUT    CADCLI   CADCPD
                    CADFAT   CADNTA   CADNTE
           GO  TO   ROT-100-000, ROT-200-000,
                    ROT-300-000, DEPENDING  ON  PRM-001.

       ROT-100-000.
           DISPLAY  MEN-001
           MOVE     " "  TO  CHV-CLI
           ACCEPT   (09 33)  CHV-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADCLI   INVALID KEY
                    DISPLAY  MEN-002
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-000.
           DISPLAY  CLI-001  MEN-003
           MOVE     ZEROS    TO  CHV-FAT  ACM-QTD
                                 ACM-VAL  ACM-TOT
           IF  CTG-CLI   =   5
               GO  TO        ROT-100-100.
           MOVE     SPACES   TO  CHV-NTA
           MOVE     CHV-CLI  TO  CLI-NTA
           START    CADNTA   KEY GREATER  CHV-NTA
                    INVALID  KEY GO  TO   ROT-100-020.

       ROT-100-010.
           READ     CADNTA   NEXT AT END
                    GO  TO   ROT-100-020.
           IF  CLI-NTA  NOT  =   CHV-CLI
               GO  TO        ROT-100-020.
           ADD      1        TO  ACM-QTD
           ADD      VAL-NTA  TO  ACM-VAL
           DISPLAY  QNT-001      VNT-001
           GO  TO   ROT-100-010.

       ROT-100-020.
           DISPLAY  QNT-001      VNT-001
           ADD      ACM-VAL  TO  ACM-TOT
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           START    CADFAT   KEY GREATER  CHV-FAT
                    INVALID  KEY GO  TO   ROT-100-040.

       ROT-100-030.
           READ     CADFAT   NEXT AT END
                    GO  TO   ROT-100-040.
           IF  CLI-FAT  NOT  =   CHV-CLI
               GO  TO        ROT-100-030.
           ADD      1        TO  ACM-QTD
           ADD      VAL-FAT  TO  ACM-VAL
           DISPLAY  QFT-001      VFT-001
           GO  TO   ROT-100-030.

       ROT-100-040.
           ADD      ACM-VAL  TO  ACM-TOT
           DISPLAY  QFT-001      VFT-001
                    TOT-001      MEN-004
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           DISPLAY  LPA-001
           GO  TO   ROT-100-000.

       ROT-100-100.
           MOVE     ZEROS    TO  CHV-NTE
           START    CADNTE   KEY GREATER  CHV-NTE
                    INVALID  KEY GO  TO   ROT-100-120.

       ROT-100-110.
           READ     CADNTE   NEXT AT END
                    GO  TO   ROT-100-120.
           IF  CLI-NTE  NOT  =   CHV-CLI
               GO  TO        ROT-100-110.
           ADD      1        TO  ACM-QTD
           ADD      VAL-NTE  TO  ACM-VAL
           DISPLAY  QNT-001      VNT-001
           GO  TO   ROT-100-110.

       ROT-100-120.
           DISPLAY  QNT-001      VNT-001
           ADD      ACM-VAL  TO  ACM-TOT
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           GO  TO   ROT-100-040.

       ROT-200-000.
           DISPLAY  LPA-002
           MOVE     ZEROS    TO  DTA-AUX
                    ACM-QTD      ACM-VAL.

       ROT-200-010.
           DISPLAY  LPA-MES  MEN-006
           ACCEPT   (11 33)  DIA-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           IF  DIA-AUX   <   1  OR  > 31
               GO  TO        ROT-200-010.
           DISPLAY  DIA-003.

       ROT-200-020.
           DISPLAY  LPA-ANO
           ACCEPT   (11 36)  MES-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-010.
           IF  MES-AUX   <   1  OR  > 12
               GO  TO        ROT-200-020.
           DISPLAY  MES-003.

       ROT-200-030.
           ACCEPT   (11 39)  ANO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-020.
           DISPLAY  ANO-003  MEN-008
           MOVE     SPACES   TO  CHV-CPD
           MOVE     DTA-AUX  TO  DTA-CPD
           START    CADCPD   KEY GREATER  CHV-CPD
                    INVALID  KEY GO  TO   ROT-200-050.

       ROT-200-040.
           READ     CADCPD   NEXT AT END
                    GO  TO   ROT-200-050.
           IF  DTA-CPD  NOT  =   DTA-AUX
               GO  TO        ROT-200-050.
           ADD      1        TO  ACM-QTD
           ADD      VAL-CPD  TO  ACM-VAL
           DISPLAY  QTD-003      VAL-003
           GO  TO   ROT-200-040.

       ROT-200-050.
           DISPLAY  QTD-003
                    VAL-003  MEN-009
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           GO  TO   ROT-200-000.

       ROT-300-000.
           DISPLAY  MEN-005
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL  ACM-TOT
                                 CHV-CPD  CHV-FAT  CHV-NTE
           MOVE     SPACES   TO  CHV-NTA
           START    CADNTA   KEY GREATER  CHV-NTA
                    INVALID  KEY GO  TO   ROT-300-020.

       ROT-300-010.
           READ     CADNTA   NEXT AT END
                    GO  TO   ROT-300-020.
           ADD      1        TO  ACM-QTD
           ADD      VAL-NTA  TO  ACM-VAL
           DISPLAY  QNT-002      VNT-002
           GO  TO   ROT-300-010.

       ROT-300-020.
           DISPLAY  QNT-002      VNT-002
           ADD      ACM-VAL  TO  ACM-TOT
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           START    CADNTE   KEY GREATER  CHV-NTE
                    INVALID  KEY GO  TO   ROT-300-040.

       ROT-300-030.
           READ     CADNTE   NEXT AT END
                    GO  TO   ROT-300-040.
           ADD      1        TO  ACM-QTD
           ADD      VAL-NTE  TO  ACM-VAL
           DISPLAY  QNE-002      VNE-002
           GO  TO   ROT-300-030.

       ROT-300-040.
           DISPLAY  QNE-002      VNE-002
           ADD      ACM-VAL  TO  ACM-TOT
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           START    CADFAT   KEY GREATER  CHV-FAT
                    INVALID  KEY GO  TO   ROT-300-060.

       ROT-300-050.
           READ     CADFAT   NEXT AT END
                    GO  TO   ROT-300-060.
           ADD      1        TO  ACM-QTD
           ADD      VAL-FAT  TO  ACM-VAL
           DISPLAY  QFT-002      VFT-002
           GO  TO   ROT-300-050.

       ROT-300-060.
           DISPLAY  QFT-002      VFT-002
           ADD      ACM-VAL  TO  ACM-TOT
           MOVE     ZEROS    TO  ACM-QTD  ACM-VAL
           START    CADCPD   KEY GREATER  CHV-CPD
                    INVALID  KEY GO  TO   ROT-300-080.

       ROT-300-070.
           READ     CADCPD   NEXT AT END
                    GO  TO   ROT-300-080.
           ADD      1        TO  ACM-QTD
           ADD      VAL-CPD  TO  ACM-VAL
           DISPLAY  QCP-002      VCP-002
           GO  TO   ROT-300-070.

       ROT-300-080.
           ADD      ACM-VAL  TO  ACM-TOT
           DISPLAY  QCP-002      VCP-002
                    TOT-002      MEN-007.

       ROT-300-090.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO  NOT   =   01
               GO  TO        ROT-300-090.

       ROT-400-000.
           CLOSE    CADCLI   CADCPD
                    CADFAT   CADNTA  CADNTE
           MOVE     4   TO   PRM-001
           CHAIN   "DYN500"  USING  PRM-001.
