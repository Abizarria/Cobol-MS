       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN510.
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

           SELECT      CADPRM        ASSIGN  TO  DISK.

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

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC  X(80).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  INI-AUX          PIC  9(06).
           03  FIN-AUX          PIC  9(06).

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  END-TAB          PIC  X(35).
           03  RED-END          REDEFINES   END-TAB.
               05  LET-END      PIC  X(01)  OCCURS  35.

           03  CID-TAB          PIC  X(20).
           03  RED-CID          REDEFINES   CID-TAB.
               05  LET-CID      PIC  X(01)  OCCURS  20.

           03  EST-TAB          PIC  X(02).
           03  RED-EST          REDEFINES   EST-TAB.
               05  LET-EST      PIC  X(01)  OCCURS  02.

           03  INS-TAB          PIC  X(15).
           03  RED-INS          REDEFINES   INS-TAB.
               05  LET-INS      PIC  X(01)  OCCURS  15.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

           03  CEP-AUX.
               05  CP1-AUX      PIC  9(05).
               05  CP2-AUX      PIC  9(03).

           03  CGC-AUX.
               05  CG1-AUX      PIC  9(02).
               05  CG2-AUX      PIC  9(03).
               05  CG3-AUX      PIC  9(03).
               05  CG4-AUX      PIC  9(04).
               05  CG5-AUX      PIC  9(02).

           03  DTA-INI.
               05  ANO-INI      PIC  9(02).
               05  MES-INI      PIC  9(02).
               05  DIA-INI      PIC  9(02).

           03  DTA-FIN.
               05  ANO-FIN      PIC  9(02).
               05  MES-FIN      PIC  9(02).
               05  DIA-FIN      PIC  9(02).

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

       01  TRC-001              PIC  X(80)  VALUE  ALL  "-".

       01  TRC-002              PIC  X(80)  VALUE  ALL  "- ".

       01  REC-001.
           03  FILLER           PIC  X(17)  VALUE  "-DYNAMIC-".
           03  NOM-R01          PIC  X(55).
           03  DIA-R01          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-R01          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-R01          PIC  9(02).

       01  REC-002.
           03  END-R02          PIC  X(39).
           03  FILLER           PIC  X(05)  VALUE  "CEP:".
           03  CP1-R02          PIC  9(05).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  CP2-R02          PIC  9(03).
           03  FILLER           PIC  X(03)  VALUE  SPACES.
           03  CID-R02          PIC  X(22).
           03  EST-R02          PIC  X(02).

       01  REC-003              PIC  X(80)  VALUE  "
           "         R   E   C   I   B   O".

       01  REC-004.
           03  FILLER           PIC  X(07)  VALUE  SPACES.
           03  FILLER           PIC  X(13)  VALUE  "RECEBEMOS DE".
           03  CLI-R04          PIC  X(35).
           03  FILLER           PIC  X(18)  VALUE  ", A IMPORTANCIA DE".

       01  REC-005.
           03  FILLER           PIC  X(11)  VALUE  "       R$".
           03  VAL-R05          PIC  --.---.---.--9,99.
           03  FILLER           PIC  X(45)  VALUE
               ", REFERENTE AO FORNECIMENTO DE COMBUSTIVEIS E".

       01  REC-006.
           03  FILLER           PIC  X(37)  VALUE
               "       OUTROS SERVICOS NO PERIODO DE".
           03  DDI-R06          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MMI-R06          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AAI-R06          PIC  9(02).
           03  FILLER           PIC  X(03)  VALUE  " A".
           03  DDF-R06          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MMF-R06          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  AAF-R06          PIC  9(02).
           03  FILLER           PIC  X(17)  VALUE  ". CONFORME FATURA".

       01  REC-007.
           03  FILLER           PIC  X(14)  VALUE  "       NUMERO".
           03  FAT-R07          PIC  999.999.
           03  FILLER           PIC  X(01)  VALUE  ".".

       01  REC-008              PIC  X(80)  VALUE  "
           "              /    /".

       01  REC-009              PIC  X(80)  VALUE
           "--------------------------,   --------------   -------------
           "--------------------".

       01  REC-010              PIC  X(80)  VALUE
           "          LOCAL                    DATA              CARIMBO
           " E ASSINATURA       ".

       01  REC-011.
           03  FILLER           PIC  X(08)  VALUE  "C.G.C.:".
           03  CG1-R11          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  ".".
           03  CG2-R11          PIC  9(03).
           03  FILLER           PIC  X(01)  VALUE  ".".
           03  CG3-R11          PIC  9(03).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  CG4-R11          PIC  9(04).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  CG5-R11          PIC  9(02).
           03  FILLER           PIC  X(39)  VALUE
               "                  INSCRICAO ESTADUAL.:".
           03  INS-R11          PIC  X(15).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "EMISSAO DE RECIBOS -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 59 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 59 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 59 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 59 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 59 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 59 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 34 PIC X(28) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 36 PIC X(26) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 41 PIC X(21) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 07 COLUMN 23 PIC X(34) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 23 PIC X(34) FROM " ³
               "         ³" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 23 PIC X(34) FROM " ³  Fatura Inicial .:
               "         ³" BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 23 PIC X(34) FROM " ³
               "         ³" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 23 PIC X(34) FROM " ³  Fatura Final ...:
               "         ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 23 PIC X(34) FROM " ³
               "         ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 23 PIC X(34) FROM " ³  Data Inicial ...:
               " /  /    ³" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 23 PIC X(34) FROM " ³
               "         ³" BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 23 PIC X(34) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 57 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 25 PIC X(34) FROM SPACES.

       01  ENT-001.
           03  INI-ENT  LINE 09 COLUMN 45 PIC 9(06) USING INI-AUX
                        BACKGROUND-COLOR  01.
           03  FIN-ENT  LINE 11 COLUMN 45 PIC 9(06) USING FIN-AUX
                        BACKGROUND-COLOR  01.
           03  DIA-ENT  LINE 13 COLUMN 45 PIC 9(02) USING DIA-INI
                        BACKGROUND-COLOR  01.
           03  MES-ENT  LINE 13 COLUMN 48 PIC 9(02) USING MES-INI
                        BACKGROUND-COLOR  01.
           03  ANO-ENT  LINE 13 COLUMN 51 PIC 9(02) USING ANO-INI
                        BACKGROUND-COLOR  01.
       01  LPA-001.
           03  LPA-FIN  LINE 11 COLUMN 45 PIC X(06) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-DIA  LINE 13 COLUMN 45 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-MES  LINE 13 COLUMN 48 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.
           03  LPA-ANO  LINE 13 COLUMN 51 PIC X(02) FROM SPACES
                        BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "FATURA INICIAL  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "FATURA FINAL  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA INICI
                   "AL DO FATURAMENTO  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO RECIBOS DO FATURAMENTO ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "FATURA INEXISTENTE
                   " -  [ESC] RETORNA ...                           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "SEQUENCIA DE FATURA
                   "S INCOERENTE  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     INPUT    CADFAT
           ACCEPT   DTA-SYS FROM DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.
           MOVE     DIA-SYS  TO  DIA-R01
           MOVE     MES-SYS  TO  MES-R01
           MOVE     ANO-SYS  TO  ANO-R01.

       ROT-000-010.
           MOVE     ZEROS    TO  INI-AUX
                                 FIN-AUX  DTA-INI.
       ROT-000-020.
           DISPLAY  LPA-FIN  MEN-001
           ACCEPT   (09 45)  INI-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           IF  INI-AUX   =   ZEROS
               GO  TO        ROT-000-020.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     INI-AUX  TO  CHV-FAT
           READ     CADFAT   INVALID KEY
                    DISPLAY  MEN-006
                    ACCEPT   OPC-001
                    GO  TO   ROT-000-020.
           DISPLAY  INI-ENT.

       ROT-000-030.
           DISPLAY  LPA-DIA  MEN-002
           ACCEPT   (11 45)  FIN-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           IF  FIN-AUX   =   ZEROS
               GO  TO        ROT-000-030.
           MOVE     SPACES   TO  RSP-OPC
           IF  FIN-AUX   <   INI-AUX
               DISPLAY       MEN-007
               ACCEPT        OPC-001
               GO  TO        ROT-000-020.
           MOVE     FIN-AUX  TO  CHV-FAT
           READ     CADFAT   INVALID KEY
                    DISPLAY  MEN-006
                    ACCEPT   OPC-001
                    GO  TO   ROT-000-030.
           DISPLAY  FIN-ENT.

       ROT-000-040.
           DISPLAY  LPA-MES  MEN-003
           ACCEPT   (13 45)  DIA-INI  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           IF  DIA-INI   <   1  OR >  31
               GO  TO        ROT-000-040.
           DISPLAY  DIA-ENT.

       ROT-000-050.
           DISPLAY  LPA-ANO
           ACCEPT   (13 48)  MES-INI  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-040.
           IF  MES-INI   <   1  OR >  12
               GO  TO        ROT-000-050.
           DISPLAY  MES-ENT.

       ROT-000-060.
           DISPLAY  MEN-003
           ACCEPT   (13 51)  ANO-INI  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-050.
           DISPLAY  ANO-ENT  MEN-004
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-060.

       ROT-100-000.
           DISPLAY  MEN-005
           OPEN     OUTPUT   RELATO
                    INPUT    CADCLI  CADPRM
           READ     CADPRM
           MOVE     1   TO   IND2.

       ROT-100-010.
           MOVE     CLI-PRM (IND2 + 1)  TO IND1
           MOVE     LET-TAB (IND1)  TO  LET-NOM (IND2)
           ADD      1   TO   IND2
           IF  IND2  <  36
               GO  TO        ROT-100-010.
           MOVE     SPACES   TO  LET-NOM (IND2)
           MOVE     NOM-TAB  TO  NOM-R01
           MOVE     1        TO  IND2.

       ROT-100-020.
           MOVE     END-PRM (IND2)  TO  IND1
           MOVE     LET-TAB (IND1)  TO  LET-END (IND2)
           ADD      1   TO   IND2
           IF  IND2  <  36
               GO  TO        ROT-100-020.
           MOVE     END-TAB  TO  END-R02
           MOVE     1        TO  IND2.

       ROT-100-030.
           MOVE     CID-PRM (IND2)  TO  IND1
           MOVE     LET-TAB (IND1)  TO  LET-CID (IND2)
           ADD      1   TO   IND2
           IF  IND2  <  21
               GO  TO        ROT-100-030.
           MOVE     CID-TAB  TO  CID-R02
           MOVE     1        TO  IND2.

       ROT-100-040.
           MOVE     EST-PRM (IND2)  TO  IND1
           MOVE     LET-TAB (IND1)  TO  LET-EST (IND2)
           ADD      1   TO   IND2
           IF  IND2  <  03
               GO  TO        ROT-100-040.
           MOVE     EST-TAB  TO  EST-R02
           MOVE     1        TO  IND2.

       ROT-100-050.
           MOVE     INS-PRM (IND2)  TO  IND1
           MOVE     LET-TAB (IND1)  TO  LET-INS (IND2)
           ADD      1   TO   IND2
           IF  IND2  <  16
               GO  TO        ROT-100-050.
           MOVE     INS-TAB  TO  INS-R11
           MOVE     CEP-PRM  TO  CEP-AUX
           MOVE     CP1-AUX  TO  CP1-R02
           MOVE     CP2-AUX  TO  CP2-R02
           MOVE     CGC-PRM  TO  CGC-AUX
           MOVE     CG1-AUX  TO  CG1-R11
           MOVE     CG2-AUX  TO  CG2-R11
           MOVE     CG3-AUX  TO  CG3-R11
           MOVE     CG4-AUX  TO  CG4-R11
           MOVE     CG5-AUX  TO  CG5-R11
           MOVE     DIA-SYS  TO  DIA-R01
           MOVE     MES-SYS  TO  MES-R01
           MOVE     ANO-SYS  TO  ANO-R01
           MOVE     DIA-INI  TO  DDI-R06
           MOVE     MES-INI  TO  MMI-R06
           MOVE     ANO-INI  TO  AAI-R06
           MOVE     INI-AUX  TO  CHV-FAT
           START    CADFAT   KEY  EQUAL   CHV-FAT
           WRITE    REG-REL FROM IMP-001  AFTER 0.

       ROT-100-060.
           READ     CADFAT   NEXT AT END
                    GO  TO   ROT-100-070.
           IF  CHV-FAT   >   FIN-AUX
               GO  TO        ROT-100-070.
           MOVE     CLI-FAT  TO  CHV-CLI
           READ     CADCLI
           MOVE     NOM-CLI  TO  CLI-R04
           MOVE     VAL-FAT  TO  VAL-R05
           MOVE     DTA-FAT  TO  DTA-FIN
           MOVE     DIA-FIN  TO  DDF-R06
           MOVE     MES-FIN  TO  MMF-R06
           MOVE     ANO-FIN  TO  AAF-R06
           MOVE     CHV-FAT  TO  FAT-R07
           WRITE    REG-REL FROM TRC-001  AFTER  0
           WRITE    REG-REL FROM REC-001
           WRITE    REG-REL FROM REC-002  AFTER  2
           WRITE    REG-REL FROM TRC-001
           WRITE    REG-REL FROM REC-003  AFTER  4
           WRITE    REG-REL FROM REC-003  AFTER  0
           WRITE    REG-REL FROM REC-004  AFTER  4
           WRITE    REG-REL FROM REC-005  AFTER  2
           WRITE    REG-REL FROM REC-006  AFTER  2
           WRITE    REG-REL FROM REC-007  AFTER  2
           WRITE    REG-REL FROM REC-008  AFTER  4
           WRITE    REG-REL FROM REC-009
           WRITE    REG-REL FROM REC-010
           WRITE    REG-REL FROM TRC-001  AFTER  2
           WRITE    REG-REL FROM REC-011
           WRITE    REG-REL FROM TRC-001
           WRITE    REG-REL FROM TRC-002  AFTER  3
           MOVE     SPACES   TO  REG-REL
           WRITE    REG-REL               AFTER  2
           GO  TO   ROT-100-060.

       ROT-100-070.
           WRITE    REG-REL  FROM  IMP-002
           CLOSE    CADCLI
                    CADPRM  RELATO
           DISPLAY  LPA-001
           GO  TO   ROT-000-010.

       ROT-200-000.
           MOVE     3   TO   PRM-001
           CLOSE    CADFAT
           CHAIN   "DYN500"  USING  PRM-001.
