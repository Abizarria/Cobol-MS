       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN224.
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

           SELECT      CADTMD        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMD.

           SELECT      CADTNQ        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TNQ.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCMB      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCMB.DAT".
       01  REG-CMB.
           03  CHV-CMB          PIC  9(01).
           03  DES-CMB          PIC  X(15).
           03  VVM-CMB          PIC  9(11)V99.
           03  LUC-CMB          PIC  9(04)V9999.

       FD  CADTMD      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADTMD.DAT".
       01  REG-TMD.
           03  CHV-TMD.
               05  TNQ-TMD      PIC  9(02).
               05  LTR-TMD      PIC  9(03)V9.
           03  QTD-TMD          PIC  9(05).
           03  LTM-TMD          PIC  9(02)V9.

       FD  CADTNQ      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADTNQ.DAT".
       01  REG-TNQ.
           03  CHV-TNQ          PIC  9(02).
           03  CMB-TNQ          PIC  9(01).
           03  SDI-TNQ          PIC S9(05)V9.
           03  SDA-TNQ          PIC S9(05)V9.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(02).
           03  NUM-OPC          PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  LTR-AUX          PIC  9(03)V9.
           03  DNS-AUX          PIC  9(01)V999.
           03  ACM-SDO          PIC S9(06)V9.
           03  ACM-CMP          PIC S9(11)V99.
           03  CTA-AUX          PIC  9(03)V999999.
           03  RED-CTA          REDEFINES  CTA-AUX.
               05  INT-AUX      PIC  9(03).
               05  DEC-AUX      PIC  9(06).

       01  PRM-001              PIC  9(01).

       01  PRM-002.
           03  BCO-PR2          PIC  9(03).
           03  VAL-PR2          PIC  9(10)V99.

       01  PRM-003.
           03  CMP-PR3          OCCURS  10.
               05  CMB-PR3      PIC  9(01).
               05  NTF-PR3      PIC  9(06).
               05  VCT-PR3      PIC  9(06).
               05  TQ1-PR3      PIC  9(02).
               05  TQ2-PR3      PIC  9(02).
               05  TQ3-PR3      PIC  9(02).
               05  TQ4-PR3      PIC  9(02).
               05  TQ5-PR3      PIC  9(02).
               05  QTD-PR3      PIC S9(05)V9.
               05  CST-PR3      PIC  9(04)V9999.
               05  VAL-PR3      PIC S9(10)V99.

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 08 COLUMN 24 PIC X(33) FROM
               " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 24 PIC X(33) FROM
               " ³  CONCILIACAO DOS ESTOQUES   ³" BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 24 PIC X(33) FROM
               " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 24 PIC X(33) FROM
               " ³             ³               ³" BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 24 PIC X(33) FROM
               " ³  1 - Regua  ³  2 - Telemed  ³" BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 24 PIC X(33) FROM
               " ³             ³               ³" BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 24 PIC X(33) FROM
               " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 26 PIC X(33) FROM SPACES.

       01  TLA-002.
           03  LINE 07 COLUMN 20 PIC X(40) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 20 PIC X(40) FROM " ³  CONCILIACAO DOS EST
               "OQUES / REGUA  ³"    BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 24 PIC X(32) FROM
               "CONCILIACAO DOS ESTOQUES / REGUA"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 08 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 20 PIC X(40) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"    BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 20 PIC X(40) FROM " ³
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 20 PIC X(40) FROM " ³  Tanque .:     -
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 20 PIC X(40) FROM " ³
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 20 PIC X(40) FROM " ³  Volume .:
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 20 PIC X(40) FROM " ³
               "               ³"    BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 20 PIC X(40) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 22 PIC X(40) FROM SPACES.

       01  TLA-003.
           03  LINE 06 COLUMN 16 PIC X(47) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 16 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 22 PIC X(35) FROM
               "CONCILIACAO DOS ESTOQUES / TELEMED"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR 14.
           03  LINE 07 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 16 PIC X(47) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"   BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 16 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 16 PIC X(47) FROM " ³  N§ do Tanque ..:
               "  -                   ³"   BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 16 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 16 PIC X(47) FROM " ³  Leitura .......:
               "                      ³"   BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 16 PIC X(47) FROM " ³  Densidade .....:
               "                      ³"   BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 16 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 16 PIC X(47) FROM " ³  Volume Atual ..:
               "                      ³"   BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 16 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 16 PIC X(47) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 18 PIC X(47) FROM SPACES.

       01  TLA-004.
           03  LINE 07 COLUMN 11 PIC X(58) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 11 PIC X(58) FROM " ³  Deposito do Saldo A
               "nterior .. R$                    ³"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³"
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 11 PIC X(58) FROM " ³  Valor Total das Com
               "pras ..... R$                    ³"
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³"
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 11 PIC X(58) FROM " ³  Volume Total dos Ta
               "nques ......:                    ³"
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 11 PIC X(58) FROM " ³
               "                                 ³"
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 11 PIC X(58) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 69 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 13 PIC X(58) FROM SPACES.

       01  ENT-001.
           03  LINE 11 COLUMN 34 PIC 9(02) USING CHV-TNQ
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 41 PIC X(15) USING DES-CMB
               BACKGROUND-COLOR  01.

       01  ENT-002  LINE 13 COLUMN 33 PIC ---.--9,9 USING SDA-TNQ
                    BACKGROUND-COLOR  01.

       01  ENT-003.
           03  LINE 10 COLUMN 37 PIC 9(02) USING CHV-TNQ
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 44 PIC X(15) USING DES-CMB
               BACKGROUND-COLOR  01.

       01  ENT-004  LINE 12 COLUMN 37 PIC ZZ9,9    USING LTR-AUX
                    BACKGROUND-COLOR  01.

       01  ENT-005  LINE 13 COLUMN 37 PIC   9,999  USING DNS-AUX
                    BACKGROUND-COLOR  01.

       01  ENT-006  LINE 15 COLUMN 36 PIC ---.--9,9 USING SDA-TNQ
                    BACKGROUND-COLOR  01.

       01  ENT-007.
           03  LINE 09 COLUMN 49 PIC Z.ZZZ.ZZZ.ZZ9,99   USING VAL-PR2
               BACKGROUND-COLOR  04  FOREGROUND-COLOR 15.
           03  LINE 11 COLUMN 47 PIC ---.---.---.--9,99 USING ACM-CMP
               BACKGROUND-COLOR  04  FOREGROUND-COLOR 15.
           03  LINE 13 COLUMN 55 PIC ----.--9,9         USING ACM-SDO
               BACKGROUND-COLOR  04  FOREGROUND-COLOR 15.

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

       01  LPA-002  LINE 13 COLUMN 41 PIC X(01) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  LPA-003.
           03  LPA-DNS LINE 13 COLUMN 37 PIC X(05) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-LTS LINE 15 COLUMN 36 PIC X(09) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DES
                   "EJADA ? [ ]  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VOLUME DE
                   "FECHAMENTO DO TANQUE  -  [ESC] RETORNA ...      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "LEITURA  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DENSIDADE
                   "DO PRODUTO  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "LEITURA INCOERENTE
                   "OU TABELA INCOMPLETA  -  [ESC] RETORNA ...      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CA
                   "LCULAR OUTRO TANQUE  -  [ESC] RETORNA ...       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "CONFIRMA TODOS OS L
                   "ANCAMENTOS ? [S/N]  [ ]  -  [ESC] RETORNA ...   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-003  LINE 22 COLUMN 54 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001  PRM-002  PRM-003.

       ROT-000-000.
           OPEN      I-O      CADTNQ
                     INPUT    CADCMB
                              CADTMD
           DISPLAY   LPA-001  TLA-001.

       ROT-000-010.
           MOVE      ZEROS    TO  NUM-OPC
           DISPLAY   MEN-001
           ACCEPT    OPC-001
           ACCEPT    TECLADO  FROM ESCAPE KEY
           IF  TECLADO    =   01
               GO  TO         ROT-000-020.
           MOVE      ZEROS    TO  ACM-SDO
                                  ACM-CMP
           IF  NUM-OPC    =   1
               DISPLAY        TLA-002
               GO  TO         ROT-100-000.
           IF  NUM-OPC    =   2
               DISPLAY        TLA-003
               GO  TO         ROT-200-000.
           GO  TO    ROT-000-010.

       ROT-000-020.
           MOVE      2   TO   PRM-001
           CLOSE     CADCMB
                     CADTMD   CADTNQ
           CHAIN    "DYN223"  USING  PRM-001  PRM-002.

       ROT-100-000.
           MOVE     ZEROS    TO  CHV-TNQ
           START    CADTNQ   KEY GREATER  CHV-TNQ.

       ROT-100-010.
           READ     CADTNQ   NEXT AT END
                    GO  TO   ROT-300-000.
           MOVE     CMB-TNQ  TO  CHV-CMB
           READ     CADCMB
           DISPLAY  ENT-001.

       ROT-100-020.
           DISPLAY  LPA-002  MEN-002
           ACCEPT   (13 33)  SDA-TNQ  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-001
                             TLA-001
               GO  TO        ROT-000-010.
           DISPLAY  ENT-002  MEN-003
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           ADD      SDA-TNQ  TO  ACM-SDO
           REWRITE  REG-TNQ
           GO  TO   ROT-100-010.

       ROT-200-000.
           MOVE     ZEROS    TO  CHV-TNQ
           START    CADTNQ   KEY GREATER  CHV-TNQ.

       ROT-200-010.
           READ     CADTNQ   NEXT AT END
                    GO  TO   ROT-300-000.
           MOVE     CMB-TNQ  TO  CHV-CMB
           READ     CADCMB
           DISPLAY  ENT-003
           MOVE     ZEROS    TO  LTR-AUX  DNS-AUX.

       ROT-200-020.
           DISPLAY  LPA-DNS  MEN-004
           ACCEPT   (12 37)  LTR-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-001
                             TLA-001
               GO  TO        ROT-000-010.
           IF  LTR-AUX   =   ZEROS
               GO  TO        ROT-200-040.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  ENT-004.

       ROT-200-030.
           DISPLAY  LPA-LTS  MEN-005
           ACCEPT   (13 37)  DNS-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   "01"
               GO  TO        ROT-200-020.
           IF  DNS-AUX   =   ZEROS
               GO  TO        ROT-200-030.
           DISPLAY  ENT-005
           MOVE     SPACES   TO  RSP-OPC
           MOVE     CHV-TNQ  TO  TNQ-TMD
           MOVE     ZEROS    TO  LTR-TMD
           START    CADTMD   KEY GREATER  CHV-TMD
                    INVALID  KEY GO  TO   ROT-200-050.
           READ     CADTMD   NEXT
           IF  TNQ-TMD  NOT  =   CHV-TNQ
               GO  TO            ROT-200-050.
           COMPUTE  CTA-AUX  = ((LTR-AUX  -   LTR-TMD)
                             /   DNS-AUX) +   LTR-TMD
           MOVE     CHV-TNQ  TO  TNQ-TMD
           MOVE     INT-AUX  TO  LTR-TMD
           READ     CADTMD   INVALID KEY
                    DISPLAY  MEN-006
                    ACCEPT   OPC-002
                    GO  TO   ROT-200-030.
           MOVE     ZEROS    TO  INT-AUX
           COMPUTE  SDA-TNQ  =   QTD-TMD  +   CTA-AUX
                             *   LTM-TMD  *   10
           DISPLAY  ENT-006  MEN-007
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-030.
           REWRITE  REG-TNQ
           DISPLAY  LPA-003.

       ROT-200-040.
           ADD      SDA-TNQ  TO  ACM-SDO
           GO  TO   ROT-200-010.

       ROT-200-050.
           DISPLAY  MEN-006
           ACCEPT   OPC-002
           GO  TO   ROT-200-030.

       ROT-300-000.
           MOVE     ZEROS    TO  IND1.

       ROT-300-010.
           ADD      1        TO  IND1
           IF  IND1  >  10   OR
               CMB-PR3 (IND1) =  0
               GO  TO        ROT-300-020.
           COMPUTE  ACM-CMP  =   ACM-CMP  +  VAL-PR3 (IND1)
           GO  TO   ROT-300-010.

       ROT-300-020.
           DISPLAY  LPA-001  TLA-004
                    ENT-007  MEN-008.

       ROT-300-030.
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-003
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-001
                             TLA-001
               GO  TO        ROT-000-010.
           IF  RSP-OPC   =   "S"
               GO  TO        ROT-400-000.
           IF  RSP-OPC  NOT  =   "N"
               GO  TO        ROT-300-030.
           MOVE     1        TO  PRM-001
           CHAIN   "DYN223"  USING   PRM-001.

       ROT-400-000.
           CHAIN    "DYN225" USING   PRM-001
                             PRM-002 PRM-003.