       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN101.
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

           SELECT      CADCMB        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CMB.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST.

           SELECT      CADLCP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LCP.

           SELECT      CADLES        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LES.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADATV      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADATV.DAT".
       01  REG-ATV.
           03  CHV-ATV          PIC  9(01).
           03  DES-ATV          PIC  X(15).

       FD  CADCMB      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCMB.DAT".
       01  REG-CMB.
           03  CHV-CMB          PIC  9(01).
           03  DES-CMB          PIC  X(15).
           03  VVM-CMB          PIC  9(11)V99.
           03  LUC-CMB          PIC  9(04)V9999.

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

       FD  CADLCP      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLCP.DAT".
       01  REG-LCP.
           03  CHV-LCP.
               05  PRX-LCP      PIC  9(01).
               05  SUF-LCP      PIC  9(03).
               05  DTA-LCP      PIC  9(06).
               05  LCT-LCP      PIC  9(03).
           03  NTF-LCP          PIC  9(06).
           03  QTD-LCP          PIC S9(06).
           03  CST-LCP          PIC  9(07)V9999.

       FD  CADLES      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLES.DAT".
       01  REG-LES.
           03  CHV-LES.
               05  PRX-LES      PIC  9(01).
               05  SUF-LES      PIC  9(03).
               05  DTA-LES      PIC  9(06).
               05  LCT-LES      PIC  9(03).
           03  SDA-LES          PIC S9(06)V9.
           03  SDC-LES          PIC  9(06)V9.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  NUM-OPC          PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  OPC-TLA          PIC  X(09).
           03  VDA-AUX          PIC  9(07)V99.
           03  CST-AUX          PIC  9(07)V9999.

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
               "CADASTRO DE PRODUTOS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.

       01  TLA-002.
           03  LINE 05 COLUMN 03 PIC X(55) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 01.
           03  LINE 06 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 06 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 03 PIC X(55) FROM " ³ Codigo .......:  -
               "   -                          ³" BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 03 PIC X(55) FROM " ³ Descricao ....:
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 03 PIC X(55) FROM " ³ Saldo Atual ..:
               "           Critico .:         ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 03 PIC X(55) FROM " ³ Custo Atual ..:
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 03 PIC X(55) FROM " ³ Custo Medio ..:
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 40 PIC X(12) FROM  "   Vendas"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 15 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 03 PIC X(55) FROM " ³ Venda Atual ..:
               "           Mes :              ³" BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 03 PIC X(55) FROM " ³ Margem Lucro .:
               "   %       Ano :              ³" BACKGROUND-COLOR 01.
           03  LINE 17 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 18 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 03 PIC X(55) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 01.
           03  LINE 19 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 05 PIC X(55) FROM SPACES.

       01  TLA-003.
           03  LINE 06 COLUMN 62 PIC  X(15) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 62 PIC  X(17) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 62 PIC  X(17) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 64 PIC  X(15) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 21 PIC  X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 21 PIC  X(21) FROM " ³                 ³
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 23 PIC  X(17) FROM "     Estoque"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR 01.
           03  LINE 08 COLUMN 42 PIC  X(02) FROM SPACES.
           03  LINE 09 COLUMN 21 PIC  X(21) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 42 PIC  X(02) FROM SPACES.
           03  LINE 10 COLUMN 21 PIC  X(21) FROM " ³  1 - Incluir    ³
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 42 PIC  X(02) FROM SPACES.
           03  LINE 11 COLUMN 21 PIC  X(21) FROM " ³  2 - Alterar    ³
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 42 PIC  X(02) FROM SPACES.
           03  LINE 12 COLUMN 21 PIC  X(21) FROM " ³  3 - Consultar  ³
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 42 PIC  X(02) FROM SPACES.
           03  LINE 13 COLUMN 21 PIC  X(21) FROM " ³  4 - Excluir    ³
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 42 PIC  X(02) FROM SPACES.
           03  LINE 14 COLUMN 21 PIC  X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 42 PIC  X(02) FROM SPACES.
           03  LINE 15 COLUMN 23 PIC  X(21) FROM SPACES.

       01  TLA-004.
           03  LINE 06 COLUMN 62 PIC X(15) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 07 COLUMN 62 PIC X(03) FROM " ³"
               BACKGROUND-COLOR  04.
           03  LINE 07 COLUMN 65 PIC X(09) USING OPC-TLA
               BACKGROUND-COLOR  04.
           03  LINE 07 COLUMN 74 PIC X(03) FROM " ³"
               BACKGROUND-COLOR  04.
           03  LINE 07 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 62 PIC X(15) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 77 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 64 PIC X(15) FROM SPACES.

       01  ENT-001.
           03  PRX-ENT LINE 07 COLUMN 22 PIC 9(01) USING CHV-ATV
                       BACKGROUND-COLOR  01.
           03  SUF-ENT LINE 07 COLUMN 24 PIC 9(03) USING SUF-EST
                       BACKGROUND-COLOR  01.
           03  ATV-ENT LINE 07 COLUMN 32 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.

       01  ENT-002.
           03  DES-ENT LINE 09 COLUMN 22 PIC X(25)        USING DES-EST
                       BACKGROUND-COLOR  01.
           03  SDO-ENT LINE 11 COLUMN 21 PIC ------9,9    USING SDO-EST
                       BACKGROUND-COLOR  01.
           03  MIN-ENT LINE 11 COLUMN 48 PIC ZZZ.ZZ9      USING MIN-EST
                       BACKGROUND-COLOR  01.
           03  CST-ENT LINE 13 COLUMN 22 PIC ZZZZZZ9,9999 USING CST-EST
                       BACKGROUND-COLOR  01.
           03  CTM-ENT LINE 14 COLUMN 22 PIC ZZZZZZ9,9999 USING CTM-EST
                       BACKGROUND-COLOR  01.
           03  VDA-ENT LINE 16 COLUMN 22 PIC ZZZZZZ9,99   USING VDA-EST
                       BACKGROUND-COLOR  01.
           03  QVM-ENT LINE 16 COLUMN 44 PIC Z.ZZZ.ZZ9,9  USING QVM-EST
                       BACKGROUND-COLOR  01.
           03  PCT-ENT LINE 17 COLUMN 21 PIC ---9,99      USING PCT-EST
                       BACKGROUND-COLOR  01.
           03  QVA-ENT LINE 17 COLUMN 43 PIC ZZ.ZZZ.ZZ9,9 USING QVA-EST
                       BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-SUF LINE 07 COLUMN 24 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-ATV LINE 07 COLUMN 32 PIC X(15) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  LPA-002.
           03  LPA-DES LINE 09 COLUMN 22 PIC X(25) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SDO LINE 11 COLUMN 21 PIC X(09) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-MIN LINE 11 COLUMN 48 PIC X(07) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CST LINE 13 COLUMN 22 PIC X(12) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CTM LINE 14 COLUMN 22 PIC X(12) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-VDA LINE 16 COLUMN 22 PIC X(10) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-QVM LINE 16 COLUMN 44 PIC X(11) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-PCT LINE 17 COLUMN 21 PIC X(07) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-QVA LINE 17 COLUMN 43 PIC X(12) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DES
                   "EJADA ? [ ]  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "GRUPO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO JA CADASTRAD
                   "O !!!  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "COMBUSTIVEL INEXIST
                   "ENTE !!!  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "ITEM  -  [ESC] RETORNA ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DESCRICAO
                   "DO ITEM  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O SALDO DO E
                   "STOQUE EM UNIDADES  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O PRECO DE C
                   "USTO UNITARIO  -  [ESC] RETORNA ...             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O PRECO DE V
                   "ENDA UNITARIO  -  [ESC] RETORNA ...             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A MARGEM DE
                   "LUCRO DESEJADA  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-012 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-013 LINE 22 COLUMN 14 PIC X(67) FROM "ITEM COM MOVIMENTO
                   "NO PERIODO  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-014 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTOS ...                            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-015 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR A EXCLUSAO  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-016 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CUSTO MEDI
                   "O UNITARIO  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-017 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NSULTAR OUTRO ITEM ...                          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     I-O      CADEST
                    INPUT    CADATV  CADCMB.

       ROT-000-010.
           DISPLAY  TLA-003  MEN-001.

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
           DISPLAY  LPA-001  MEN-002
           MOVE     0   TO   CHV-ATV
           ACCEPT   (07 22)  CHV-ATV  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           READ     CADATV   INVALID KEY
                    GO  TO   ROT-000-030.
           MOVE     0   TO   SUF-EST
           DISPLAY  PRX-ENT  ATV-ENT.

       ROT-000-040.
           DISPLAY  MEN-006
           ACCEPT   (07 24)  SUF-EST  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           IF  SUF-EST   =   ZEROS
               GO  TO        ROT-000-040.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     CHV-ATV  TO  PRX-EST
           READ     CADEST   INVALID KEY
                    GO  TO   ROT-100-000.
           IF  NUM-OPC   =   1
               GO  TO        ROT-000-060.
           DISPLAY  SUF-ENT
           MOVE     CST-EST  TO  CST-AUX
           MOVE     VDA-EST  TO  VDA-AUX
           DISPLAY  ENT-002
           GO  TO   ROT-100-000, ROT-200-000, ROT-300-000,
                    ROT-400-000, DEPENDING    ON  NUM-OPC.

       ROT-000-050.
           DISPLAY  LPA-002
           ADD      1   TO  SUF-EST
           GO  TO   ROT-000-040.

       ROT-000-060.
           DISPLAY  MEN-003
           ACCEPT   OPC-002
           GO  TO   ROT-000-050.

       ROT-100-000.
           IF  NUM-OPC   >   1
               GO  TO        ROT-100-070.
           DISPLAY  SUF-ENT
           MOVE     SPACES   TO  DES-EST
           MOVE     ZEROS    TO  SDO-EST  MIN-EST  CST-EST  CTM-EST
                                 VDA-EST  PCT-EST  QVM-EST  QVA-EST
           IF  CHV-ATV   >   0
               GO  TO        ROT-100-010.
           MOVE     SUF-EST  TO  CHV-CMB
           READ     CADCMB   INVALID KEY
                    DISPLAY  MEN-005
                    ACCEPT   OPC-002
                    GO  TO   ROT-000-040.

       ROT-100-010.
           DISPLAY  LPA-SDO
                    LPA-MIN  MEN-007
           ACCEPT   (09 22)  DES-EST  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-DES
               GO  TO        ROT-000-040.
           DISPLAY  DES-ENT.

       ROT-100-020.
           DISPLAY  LPA-CST  MEN-008
           ACCEPT   (11 21)  SDO-EST  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           DISPLAY  SDO-ENT  MIN-ENT.

       ROT-100-030.
           DISPLAY  LPA-CTM
                    LPA-VDA  MEN-009
           ACCEPT   (13 22)  CST-EST  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           MOVE     CST-EST  TO  CTM-EST
           DISPLAY  CST-ENT      CTM-ENT.

       ROT-100-040.
           DISPLAY  LPA-QVA  LPA-QVM
                    LPA-PCT  MEN-010
           ACCEPT   (16 22)  VDA-EST  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           IF  VDA-EST   =   ZEROS
               GO  TO        ROT-100-050.
           COMPUTE  PCT-EST  =  (VDA-EST -  CTM-EST)
                             *   100     /  VDA-EST
           GO  TO   ROT-100-060.

       ROT-100-050.
           DISPLAY  LPA-VDA  MEN-011
           ACCEPT   (17 21)  PCT-EST  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.
           COMPUTE  VDA-EST  =   CTM-EST  *  100
                             /  (100  -  PCT-EST).

       ROT-100-060.
           DISPLAY  VDA-ENT  PCT-ENT
                    QVM-ENT  QVA-ENT MEN-012
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.
           WRITE    REG-EST
           GO  TO   ROT-000-050.

       ROT-100-070.
           DISPLAY  MEN-004
           ACCEPT   OPC-002
           MOVE     0   TO  SUF-EST
           GO  TO   ROT-000-040.

       ROT-200-000.
           DISPLAY  MEN-007
           ACCEPT   (09 22)  DES-EST  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  DES-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-000-050.

       ROT-200-010.
           DISPLAY  MEN-009
           ACCEPT   (13 22)  CST-EST  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  CST-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.

       ROT-200-020.
           DISPLAY  MEN-016
           ACCEPT   (14 22)  CTM-EST  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  CTM-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-010.

       ROT-200-030.
           DISPLAY  MEN-010
           ACCEPT   (16 22)  VDA-EST  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  VDA-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-020.
           IF  VDA-EST   =   ZEROS
               GO  TO        ROT-200-040.
           COMPUTE  PCT-EST  =  (VDA-EST -  CTM-EST)
                             *   100     /  VDA-EST
           GO  TO   ROT-200-050.

       ROT-200-040.
           DISPLAY  MEN-011
           ACCEPT   (17 21)  PCT-EST  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  PCT-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-030.
           COMPUTE  VDA-EST  =   CTM-EST  *  100
                             /  (100  -  PCT-EST).

       ROT-200-050.
           DISPLAY  VDA-ENT  PCT-ENT
                             MEN-012
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM  ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-030.
           REWRITE  REG-EST
           GO  TO   ROT-000-050.

       ROT-300-000.
           DISPLAY  MEN-017
           ACCEPT   OPC-002
           GO  TO   ROT-000-050.

       ROT-400-000.
           IF  SDO-EST  NOT  =  0
               GO  TO        ROT-400-020.
           DISPLAY  MEN-014
           OPEN     INPUT    CADLCP
           MOVE     SPACES   TO  CHV-LCP
           MOVE     PRX-EST  TO  PRX-LCP
           MOVE     SUF-EST  TO  SUF-LCP
           START    CADLCP   KEY GREATER  CHV-LCP
                    INVALID  KEY GO  TO   ROT-400-010.
           READ     CADLCP   NEXT
           IF  PRX-LCP  NOT  =   PRX-EST  OR
               SUF-LCP  NOT  =   SUF-EST
               GO  TO        ROT-400-010.
           CLOSE    CADLCP
           GO  TO   ROT-400-020.

       ROT-400-010.
           OPEN     INPUT    CADLES
           MOVE     SPACES   TO  CHV-LES
           MOVE     PRX-EST  TO  PRX-LES
           MOVE     SUF-EST  TO  SUF-LES
           START    CADLES   KEY GREATER  CHV-LES
                    INVALID  KEY GO  TO   ROT-400-030.
           READ     CADLES   NEXT
           IF  PRX-LES  NOT  =   PRX-EST  OR
               SUF-LES  NOT  =   SUF-EST
               GO  TO        ROT-400-030.
           CLOSE    CADLCP   CADLES.

       ROT-400-020.
           DISPLAY  MEN-013
           ACCEPT   OPC-002
           GO  TO   ROT-000-050.

       ROT-400-030.
           CLOSE    CADLCP   CADLES
           DISPLAY  MEN-015
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM  ESCAPE KEY
           IF  TECLADO   =   00
               DELETE    CADEST.
           GO  TO   ROT-000-050.

       ROT-500-000.
           MOVE     1   TO   PRM-001
           CLOSE    CADATV
                    CADCMB   CADEST
           CHAIN   "DYN100"  USING  PRM-001.
