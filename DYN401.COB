       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN401.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADBCO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-BCO.

           SELECT      CADLBC        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LBC.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADBCO      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADBCO.DAT".
       01  REG-BCO.
           03  CHV-BCO          PIC  9(03).
           03  NOM-BCO          PIC  X(15).
           03  SDI-BCO          PIC S9(11)V99.

       FD  CADLBC      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLBC.DAT".
       01  REG-LBC.
           03  CHV-LBC.
               05  BCO-LBC      PIC  9(03).
               05  DTA-LBC      PIC  9(06).
               05  LCT-LBC      PIC  9(03).
           03  FLG-LBC          PIC  9(01).
           03  DOC-LBC          PIC  9(06).
           03  HST-LBC          PIC  X(20).
           03  COD-LBC          PIC  9(01).
           03  VAL-LBC          PIC  9(10)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  NUM-OPC          PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  OPC-TLA          PIC  X(09).
           03  SDA-AUX          PIC S9(11)V99.
           03  CHS-AUX          PIC S9(11)V99.
           03  DSP-AUX          PIC S9(11)V99.

       01  TAB-OPC.
           03  FILLER           PIC  X(09)  VALUE  "INCLUSAO ".
           03  FILLER           PIC  X(09)  VALUE  "ALTERACAO".
           03  FILLER           PIC  X(09)  VALUE  "CONSULTA ".
           03  FILLER           PIC  X(09)  VALUE  "EXCLUSAO ".
       01  RED-OPC              REDEFINES   TAB-OPC.
           03  OPC-TAB          PIC  X(09)  OCCURS  04.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "CADASTRO DE BANCOS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 54 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 54 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 54 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 54 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 54 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 54 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 54 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 54 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 54 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 54 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 54 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 07 COLUMN 08 PIC X(44) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"      BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 08 PIC X(44) FROM " ³
               "                   ³"      BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 08 PIC X(44) FROM " ³  Codigo ..:      -
               "                   ³"      BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 08 PIC X(44) FROM " ³
               "                   ³"      BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 08 PIC X(44) FROM " ³  Saldo Atual ....:
               "                   ³"      BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 08 PIC X(44) FROM " ³
               "                   ³"      BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 08 PIC X(44) FROM " ³  Chs Pendentes ..:
               "                   ³"      BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 08 PIC X(44) FROM " ³
               "                   ³"      BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 08 PIC X(44) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"      BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 08 PIC X(44) FROM " ³
               "                   ³"      BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 12 PIC X(17) FROM "Disponivel .....:"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 08 PIC X(44) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"      BACKGROUND-COLOR 01.
           03  LINE 17 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 10 PIC X(44) FROM SPACES.

       01  TLA-003.
           03  LINE 08 COLUMN 58 PIC X(15) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 58 PIC X(17) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 58 PIC X(17) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 60 PIC X(15) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 21 PIC X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 21 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 23 PIC X(17) FROM "     Bancos"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 01.
           03  LINE 09 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 21 PIC X(21) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 21 PIC X(21) FROM " ³  1 - Incluir    ³
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 21 PIC X(21) FROM " ³  2 - Alterar    ³
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 21 PIC X(21) FROM " ³  3 - Consultar  ³
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 21 PIC X(21) FROM " ³  4 - Excluir    ³
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 21 PIC X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 23 PIC X(21) FROM SPACES.

       01  TLA-004.
           03  LINE 08 COLUMN 58 PIC X(15) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 58 PIC X(03) FROM " ³"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 61 PIC X(09) USING  OPC-TLA
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 70 PIC X(03) FROM " ³"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 58 PIC X(15) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 60 PIC X(15) FROM SPACES.

       01  TLA-005 LINE 09 COLUMN 23 PIC 9(03) USING CHV-BCO
                   BACKGROUND-COLOR  01.

       01  TLA-006.
           03  NOM-ENT LINE 09 COLUMN 31 PIC X(15) USING NOM-BCO
                       BACKGROUND-COLOR  01.
           03  SDI-ENT LINE 11 COLUMN 30 PIC ---.---.---.--9,99 USING
               SDA-AUX BACKGROUND-COLOR  01.
           03  CHS-ENT LINE 13 COLUMN 30 PIC ---.---.---.--9,99 USING
               CHS-AUX BACKGROUND-COLOR  01.
           03  SDA-ENT LINE 16 COLUMN 30 PIC ---.---.---.--9,99 USING
               DSP-AUX BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.

       01  TLA-007 LINE 09 COLUMN 23 PIC X(03) FROM SPACES
                   BACKGROUND-COLOR  01.

       01  TLA-008.
           03  LPA-NOM LINE 09 COLUMN 31 PIC X(15) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SDI LINE 11 COLUMN 30 PIC X(18) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CHS LINE 13 COLUMN 30 PIC X(18) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SDA LINE 16 COLUMN 30 PIC X(18) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DES
                   "EJADA ? [ ]  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "BANCO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO JA CADASTRAD
                   "O !!!  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NOME DO BA
                   "NCO  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O SALDO INIC
                   "IAL DO BANCO  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "BANCO COM MOVIMENTO
                   " NO PERIODO  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTOS ...                            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR A EXCLUSAO  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NSULTAR OUTRO BANCO ...                         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           OPEN     I-O      CADBCO
                    INPUT    CADLBC
           DISPLAY  TLA-001  TLA-002.

       ROT-000-010.
           DISPLAY  TLA-007  TLA-003
                             MEN-001.
       ROT-000-020.
           MOVE     0   TO   NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-500-000.
           IF  NUM-OPC   <   1  OR  >  4
               GO  TO        ROT-000-020.
           MOVE     OPC-TAB (NUM-OPC) TO  OPC-TLA
           DISPLAY  TLA-002  TLA-004.

       ROT-000-030.
           DISPLAY  LPA-NOM  MEN-002
           MOVE     0   TO   CHV-BCO
           ACCEPT   (09 23)  CHV-BCO  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           IF  CHV-BCO   =   ZEROS
               GO  TO        ROT-000-030.
           DISPLAY  TLA-005
           MOVE     SPACES   TO  RSP-OPC
           READ     CADBCO   INVALID KEY
                    GO  TO   ROT-100-000.
           IF  NUM-OPC   =   1
               GO  TO        ROT-000-070.
           DISPLAY  MEN-009
           MOVE     ZEROS    TO  CHS-AUX
           MOVE     SDI-BCO  TO  SDA-AUX  DSP-AUX
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           START    CADLBC   KEY GREATER  CHV-LBC
                    INVALID  KEY GO  TO   ROT-000-040.

       ROT-000-040.
           READ     CADLBC   NEXT AT END
                    GO  TO   ROT-000-050.
           IF  BCO-LBC  NOT  =   CHV-BCO
               GO  TO        ROT-000-050.
           IF  FLG-LBC   =   1
               ADD  VAL-LBC  TO  CHS-AUX
               GO  TO        ROT-000-040.
           IF  COD-LBC   =   2  OR  4  OR  5
               COMPUTE       SDA-AUX   =  SDA-AUX  -  VAL-LBC
           ELSE
               COMPUTE       SDA-AUX   =  SDA-AUX  +  VAL-LBC.
           GO  TO   ROT-000-040.

       ROT-000-050.
           COMPUTE  DSP-AUX  =   SDA-AUX  -   CHS-AUX
           DISPLAY  TLA-006
           GO  TO   ROT-100-000, ROT-200-000, ROT-300-000,
                    ROT-400-000, DEPENDING    ON  NUM-OPC.

       ROT-000-060.
           DISPLAY  TLA-008
           GO  TO   ROT-000-030.

       ROT-000-070.
           DISPLAY  MEN-003
           ACCEPT   OPC-002
           GO  TO   ROT-000-030.

       ROT-100-000.
           IF  NUM-OPC   >   1
               GO  TO        ROT-100-030.
           MOVE     SPACES   TO  NOM-BCO
           MOVE     ZEROS    TO  SDI-BCO  CHS-AUX.

       ROT-100-010.
           DISPLAY  LPA-SDI  MEN-005
           ACCEPT   (09 31)  NOM-BCO  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           DISPLAY  NOM-ENT.

       ROT-100-020.
           DISPLAY  LPA-CHS  LPA-SDA
                    LPA-SDI  MEN-006
           ACCEPT   (11 30)  SDI-BCO  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           MOVE     SDI-BCO  TO  SDA-AUX
                                 DSP-AUX
           DISPLAY  SDI-ENT  CHS-ENT
                    SDA-ENT  MEN-007
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           WRITE    REG-BCO
           GO  TO   ROT-000-060.

       ROT-100-030.
           DISPLAY  MEN-004
           ACCEPT   OPC-002
           GO  TO   ROT-000-060.

       ROT-200-000.
           DISPLAY  MEN-005
           ACCEPT   (09 31)  NOM-BCO  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  NOM-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-000-060.

       ROT-200-010.
           DISPLAY  MEN-007
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.
           REWRITE  REG-BCO
           GO  TO   ROT-000-060.

       ROT-300-000.
           DISPLAY  MEN-011
           ACCEPT   OPC-002
           GO  TO   ROT-000-060.

       ROT-400-000.
           DISPLAY  MEN-009
           IF  SDI-BCO  NOT  =   0
               GO  TO        ROT-400-010.
           MOVE     SPACES   TO  CHV-LBC
           MOVE     CHV-BCO  TO  BCO-LBC
           START    CADLBC   KEY GREATER  CHV-LBC
                    INVALID  KEY GO  TO   ROT-400-020.
           READ     CADLBC   NEXT
           IF  BCO-LBC  NOT  =   CHV-BCO
               GO  TO        ROT-400-020.

       ROT-400-010.
           DISPLAY  MEN-008
           ACCEPT   OPC-002
           GO  TO   ROT-000-060.

       ROT-400-020.
           DISPLAY  MEN-010
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM  ESCAPE KEY
           IF  TECLADO   =   00
               DELETE    CADBCO.
           GO  TO   ROT-000-060.

       ROT-500-000.
           MOVE     1   TO  PRM-001
           CLOSE    CADBCO  CADLBC
           CHAIN   "DYN400"  USING  PRM-001.
