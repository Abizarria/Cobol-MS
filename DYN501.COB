       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN501.
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
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-FAT.

           SELECT      CADNTA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-NTA.

           SELECT      CADNTE        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
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
                       VALUE    OF   FILE-ID     "CADNTE.DAT".
       01  REG-NTE.
           03  CHV-NTE          PIC  9(06).
           03  DTA-NTE          PIC  9(06).
           03  CLI-NTE          PIC  X(10).
           03  PCA-NTE          PIC  X(07).
           03  DES-NTE          PIC  X(20).
           03  VAL-NTE          PIC S9(09)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  NUM-OPC          PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  OPC-TLA          PIC  X(09).
           03  CTG-TLA          PIC  X(09).

       01  TEL-AUX.
           03  DDD-AUX          PIC  9(04).
           03  PRX-AUX          PIC  9(04).
           03  SUF-AUX          PIC  9(04).

       01  CEP-AUX.
           03  PCP-AUX          PIC  9(05).
           03  CCP-AUX          PIC  9(03).

       01  CGC-AUX.
           03  CG1-AUX          PIC  9(02).
           03  CG2-AUX          PIC  9(03).
           03  CG3-AUX          PIC  9(03).
           03  CG4-AUX          PIC  9(04).
           03  CG5-AUX          PIC  9(02).

       01  TAB-OPC.
           03  FILLER           PIC  X(09)  VALUE  "INCLUSAO ".
           03  FILLER           PIC  X(09)  VALUE  "ALTERACAO".
           03  FILLER           PIC  X(09)  VALUE  "CONSULTA ".
           03  FILLER           PIC  X(09)  VALUE  "EXCLUSAO ".
       01  RED-OPC              REDEFINES   TAB-OPC.
           03  OPC-TAB          PIC  X(09)  OCCURS  04.

       01  TAB-CTG.
           03  FILLER           PIC  X(09)  VALUE  "SEMANAL  ".
           03  FILLER           PIC  X(09)  VALUE  "DEZENAL  ".
           03  FILLER           PIC  X(09)  VALUE  "QUINZENAL".
           03  FILLER           PIC  X(09)  VALUE  "MENSAL   ".
           03  FILLER           PIC  X(09)  VALUE  "EVENTUAL ".
       01  RED-CTG              REDEFINES   TAB-CTG.
           03  CTG-TAB          PIC  X(09)  OCCURS  05.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "CADASTRO DE CLIENTES  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.

       01  TLA-002.
           03  LINE 05 COLUMN 03 PIC X(55) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" BACKGROUND-COLOR 01.
           03  LINE 06 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 06 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 03 PIC X(55) FROM " ³ Codigo .....:
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 03 PIC X(55) FROM " ³ Razao Social:
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 03 PIC X(55) FROM " ³ Telefone ...: (    )
               "     -      ¯                 ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 03 PIC X(55) FROM " ³ Endereco ...:
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 03 PIC X(55) FROM " ³                    -
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 03 PIC X(55) FROM " ³ C.G.C. .....:   .
               ".   /    -                    ³" BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 03 PIC X(55) FROM " ³ Ins. Est. ..:
               "            Pagto :           ³" BACKGROUND-COLOR 01.
           03  LINE 17 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 03 PIC X(55) FROM " ³
               "                              ³" BACKGROUND-COLOR 01.
           03  LINE 18 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 03 PIC X(55) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" BACKGROUND-COLOR 01.
           03  LINE 19 COLUMN 58 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 05 PIC X(55) FROM SPACES.

       01  TLA-003.
           03  LINE 06 COLUMN 62 PIC X(15) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 62 PIC X(17) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 62 PIC X(17) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 64 PIC X(15) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 21 PIC X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 21 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR  04.
           03  LINE 08 COLUMN 23 PIC X(17) FROM "    Clientes"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 01.
           03  LINE 08 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 21 PIC X(21) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 21 PIC X(21) FROM " ³  1 - Incluir    ³
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 21 PIC X(21) FROM " ³  2 - Alterar    ³
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 21 PIC X(21) FROM " ³  3 - Consultar  ³
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 21 PIC X(21) FROM " ³  4 - Excluir    ³
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 21 PIC X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 23 PIC X(21) FROM SPACES.

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

       01  TLA-005.
           03  LINE 07 COLUMN 21 PIC X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR   04.
           03  LINE 08 COLUMN 21 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR   04.
           03  LINE 08 COLUMN 23 PIC X(17) FROM "    Pagamento"
               BACKGROUND-COLOR   07 FOREGROUND-COLOR 01.
           03  LINE 08 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 21 PIC X(21) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR   04.
           03  LINE 09 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 21 PIC X(21) FROM " ³  1 - Semanal    ³
               BACKGROUND-COLOR   04.
           03  LINE 10 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 21 PIC X(21) FROM " ³  2 - Dezenal    ³
               BACKGROUND-COLOR   04.
           03  LINE 11 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 21 PIC X(21) FROM " ³  3 - Quinzenal  ³
               BACKGROUND-COLOR   04.
           03  LINE 12 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 21 PIC X(21) FROM " ³  4 - Mensal     ³
               BACKGROUND-COLOR   04.
           03  LINE 13 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 21 PIC X(21) FROM " ³  5 - Eventual   ³
               BACKGROUND-COLOR   04.
           03  LINE 14 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 21 PIC X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR   04.
           03  LINE 15 COLUMN 42 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 23 PIC X(21) FROM SPACES.

       01  ENT-001  LINE 07 COLUMN 20 PIC X(10) USING CHV-CLI
                    BACKGROUND-COLOR  01.

       01  ENT-002.
           03  NOM-ENT LINE 09 COLUMN 20 PIC X(35) USING NOM-CLI
                       BACKGROUND-COLOR  01.
           03  DDD-ENT LINE 11 COLUMN 21 PIC 9(04) USING DDD-AUX
                       BACKGROUND-COLOR  01.
           03  PRX-ENT LINE 11 COLUMN 27 PIC ZZZ9  USING PRX-AUX
                       BACKGROUND-COLOR  01.
           03  SUF-ENT LINE 11 COLUMN 32 PIC 9(04) USING SUF-AUX
                       BACKGROUND-COLOR  01.
           03  CNT-ENT LINE 11 COLUMN 40 PIC X(15) USING CNT-CLI
                       BACKGROUND-COLOR  01.
           03  END-ENT LINE 13 COLUMN 20 PIC X(35) USING END-CLI
                       BACKGROUND-COLOR  01.
           03  PCP-ENT LINE 14 COLUMN 20 PIC 9(05) USING PCP-AUX
                       BACKGROUND-COLOR  01.
           03  CCP-ENT LINE 14 COLUMN 26 PIC 9(03) USING CCP-AUX
                       BACKGROUND-COLOR  01.
           03  CID-ENT LINE 14 COLUMN 31 PIC X(20) USING CID-CLI
                       BACKGROUND-COLOR  01.
           03  EST-ENT LINE 14 COLUMN 53 PIC X(02) USING EST-CLI
                       BACKGROUND-COLOR  01.
           03  CG1-ENT LINE 16 COLUMN 20 PIC 9(02) USING CG1-AUX
                       BACKGROUND-COLOR  01.
           03  CG2-ENT LINE 16 COLUMN 23 PIC 9(03) USING CG2-AUX
                       BACKGROUND-COLOR  01.
           03  CG3-ENT LINE 16 COLUMN 27 PIC 9(03) USING CG3-AUX
                       BACKGROUND-COLOR  01.
           03  CG4-ENT LINE 16 COLUMN 31 PIC 9(04) USING CG4-AUX
                       BACKGROUND-COLOR  01.
           03  CG5-ENT LINE 16 COLUMN 36 PIC 9(02) USING CG5-AUX
                       BACKGROUND-COLOR  01.
           03  INS-ENT LINE 17 COLUMN 20 PIC X(15) USING INS-CLI
                       BACKGROUND-COLOR  01.
           03  CTG-ENT LINE 17 COLUMN 46 PIC X(09) USING CTG-TLA
                       BACKGROUND-COLOR  01.

       01  LPA-001  LINE 07 COLUMN 20 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  LPA-002.
           03  LPA-NOM LINE 09 COLUMN 20 PIC X(35) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-DDD LINE 11 COLUMN 21 PIC X(04) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-PRX LINE 11 COLUMN 27 PIC X(04) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-SUF LINE 11 COLUMN 32 PIC X(04) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CNT LINE 11 COLUMN 40 PIC X(15) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-END LINE 13 COLUMN 20 PIC X(35) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-PCP LINE 14 COLUMN 20 PIC X(05) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CCP LINE 14 COLUMN 26 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CID LINE 14 COLUMN 31 PIC X(20) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-EST LINE 14 COLUMN 53 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CG1 LINE 16 COLUMN 20 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CG2 LINE 16 COLUMN 23 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CG3 LINE 16 COLUMN 27 PIC X(03) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CG4 LINE 16 COLUMN 31 PIC X(04) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CG5 LINE 16 COLUMN 36 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-INS LINE 17 COLUMN 20 PIC X(15) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-CTG LINE 17 COLUMN 46 PIC X(09) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DES
                   "EJADA ? [ ]  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "CLIENTE  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO JA CADASTRAD
                   "O !!!  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "CODIGO INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NOME DO CL
                   "IENTE  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DE
                   "AREA  -  [ESC] RETORNA ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O PREFIXO DO
                   " TELEFONE  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DO
                   "TELEFONE  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NOME P/ CO
                   "NTATO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O ENDERECO D
                   "O CLIENTE  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "C.E.P  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-012 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O COMPLEMENT
                   "O C.E.P  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-013 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NOME DO MU
                   "NICIPIO  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-014 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A SIGLA DO E
                   "STADO  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-015 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "C.G.C  -  [ESC] RETORNA ...                     "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-016 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DA
                   "INSCRICAO ESTADUAL  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-017 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-018 LINE 22 COLUMN 14 PIC X(67) FROM "CLIENTE COM MOVIMEN
                   "TO PENDENTE  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-019 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTO ...                             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-020 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR A EXCLUSAO  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-021 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NSULTAR OUTRO CLIENTE ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-022 LINE 22 COLUMN 14 PIC X(67) FROM "FORMA DE PAGAMENTO
                   "DO CLIENTE ? [ ]  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-003 LINE 22 COLUMN 47 PIC 9(01) USING CTG-CLI AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     I-O      CADCLI.

       ROT-000-010.
           DISPLAY  LPA-001  TLA-003
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
           MOVE     " "  TO  CHV-CLI
           ACCEPT   (07 20)  CHV-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-010.
           IF  CHV-CLI   =   SPACES
               GO  TO        ROT-000-030.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADCLI   INVALID KEY
                    GO  TO   ROT-100-000.
           IF  NUM-OPC   =   1
               GO  TO        ROT-000-050.
           DISPLAY  ENT-001
           MOVE     TEL-CLI  TO  TEL-AUX
           MOVE     CEP-CLI  TO  CEP-AUX
           MOVE     CGC-CLI  TO  CGC-AUX
           MOVE     CTG-TAB (CTG-CLI) TO  CTG-TLA
           DISPLAY  ENT-002
           GO  TO   ROT-100-000, ROT-200-000, ROT-300-000,
                    ROT-400-000, DEPENDING    ON  NUM-OPC.

       ROT-000-040.
           DISPLAY  LPA-002
           GO  TO   ROT-000-030.

       ROT-000-050.
           DISPLAY  MEN-003
           ACCEPT   OPC-002
           GO  TO   ROT-000-030.

       ROT-100-000.
           IF  NUM-OPC   >   1
               GO  TO        ROT-100-210.
           DISPLAY  ENT-001
           MOVE     ZEROS    TO  TEL-AUX  CEP-AUX  CGC-AUX
           MOVE     SPACES   TO  NOM-CLI  END-CLI  CNT-CLI
                                 CID-CLI  EST-CLI  INS-CLI.
       ROT-100-010.
           DISPLAY  LPA-DDD  MEN-005
           ACCEPT   (09 20)  NOM-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           DISPLAY  NOM-ENT.

       ROT-100-020.
           DISPLAY  LPA-PRX  MEN-006
           ACCEPT   (11 21)  DDD-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-010.
           DISPLAY  DDD-ENT.

       ROT-100-030.
           DISPLAY  LPA-SUF  MEN-007
           ACCEPT   (11 27)  PRX-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           DISPLAY  PRX-ENT.

       ROT-100-040.
           DISPLAY  LPA-CNT  MEN-008
           ACCEPT   (11 32)  SUF-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           DISPLAY  SUF-ENT.

       ROT-100-050.
           DISPLAY  LPA-END  MEN-009
           ACCEPT   (11 40)  CNT-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-040.
           DISPLAY  CNT-ENT.

       ROT-100-060.
           DISPLAY  LPA-PCP  MEN-010
           ACCEPT   (13 20)  END-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-050.
           DISPLAY  END-ENT.

       ROT-100-070.
           DISPLAY  LPA-CCP  MEN-011
           ACCEPT   (14 20)  PCP-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-060.
           DISPLAY  PCP-ENT.

       ROT-100-080.
           DISPLAY  LPA-CID  MEN-012
           ACCEPT   (14 26)  CCP-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-070.
           DISPLAY  CCP-ENT.

       ROT-100-090.
           DISPLAY  LPA-EST  MEN-013
           ACCEPT   (14 31)  CID-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-080.
           DISPLAY  CID-ENT.

       ROT-100-100.
           DISPLAY  LPA-CG1  MEN-014
           ACCEPT   (14 53)  EST-CLI  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-090.
           DISPLAY  EST-ENT  MEN-015.

       ROT-100-110.
           DISPLAY  LPA-CG2
           ACCEPT   (16 20)  CG1-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-100.
           DISPLAY  CG1-ENT.

       ROT-100-120.
           DISPLAY  LPA-CG3
           ACCEPT   (16 23)  CG2-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-110.
           DISPLAY  CG2-ENT.

       ROT-100-130.
           DISPLAY  LPA-CG4
           ACCEPT   (16 27)  CG3-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-120.
           DISPLAY  CG3-ENT.

       ROT-100-140.
           DISPLAY  LPA-CG5
           ACCEPT   (16 31)  CG4-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-130.
           DISPLAY  CG4-ENT.

       ROT-100-150.
           DISPLAY  LPA-INS  MEN-015
           ACCEPT   (16 36)  CG5-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-140.
           DISPLAY  CG5-ENT.

       ROT-100-160.
           DISPLAY  LPA-CTG  MEN-016
           ACCEPT   (17 20)  INS-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-150.
           DISPLAY  INS-ENT.

       ROT-100-170.
           DISPLAY  TLA-005  MEN-022
                             LPA-CTG.
       ROT-100-180.
           MOVE     0   TO   CTG-CLI
           ACCEPT   OPC-003
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               PERFORM       ROT-100-190
               GO  TO        ROT-100-160.
           IF  CTG-CLI   <   1  OR  >  5
               GO  TO        ROT-100-180.
           MOVE     CTG-TAB (CTG-CLI) TO  CTG-TLA.

       ROT-100-190.
           DISPLAY  TLA-002  ENT-001
                             ENT-002.
       ROT-100-200.
           DISPLAY  MEN-017
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-170.
           MOVE     TEL-AUX  TO  TEL-CLI
           MOVE     CEP-AUX  TO  CEP-CLI
           MOVE     CGC-AUX  TO  CGC-CLI
           WRITE    REG-CLI
           GO  TO   ROT-000-040.

       ROT-100-210.
           DISPLAY  MEN-004
           ACCEPT   OPC-002
           GO  TO   ROT-000-040.

       ROT-200-000.
           DISPLAY  MEN-005
           ACCEPT   (09 20)  NOM-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  NOM-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-000-040.

       ROT-200-010.
           DISPLAY  MEN-006
           ACCEPT   (11 21)  DDD-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  DDD-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-000.

       ROT-200-020.
           DISPLAY  MEN-007
           ACCEPT   (11 27)  PRX-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  PRX-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-010.

       ROT-200-030.
           DISPLAY  MEN-008
           ACCEPT   (11 32)  SUF-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  SUF-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-020.

       ROT-200-040.
           DISPLAY  MEN-009
           ACCEPT   (11 40)  CNT-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  CNT-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-030.

       ROT-200-050.
           DISPLAY  MEN-010
           ACCEPT   (13 20)  END-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  END-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-040.

       ROT-200-060.
           DISPLAY  MEN-011
           ACCEPT   (14 20)  PCP-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  PCP-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-050.

       ROT-200-070.
           DISPLAY  MEN-012
           ACCEPT   (14 26)  CCP-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  CCP-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-060.

       ROT-200-080.
           DISPLAY  MEN-013
           ACCEPT   (14 31)  CID-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  CID-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-070.

       ROT-200-090.
           DISPLAY  MEN-014
           ACCEPT   (14 53)  EST-CLI  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  EST-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-080.

       ROT-200-100.
           DISPLAY  MEN-015
           ACCEPT   (16 20)  CG1-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  CG1-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-090.

       ROT-200-110.
           ACCEPT   (16 23)  CG2-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  CG2-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-100.

       ROT-200-120.
           ACCEPT   (16 27)  CG3-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  CG3-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-110.

       ROT-200-130.
           ACCEPT   (16 31)  CG4-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  CG4-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-120.

       ROT-200-140.
           DISPLAY  MEN-015
           ACCEPT   (16 36)  CG5-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  CG5-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-130.

       ROT-200-150.
           DISPLAY  MEN-016
           ACCEPT   (17 20)  INS-CLI  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           DISPLAY  INS-ENT
           IF  TECLADO   =   01
               GO  TO        ROT-200-140.
           DISPLAY  TLA-005.

       ROT-200-160.
           DISPLAY  MEN-022
           ACCEPT   OPC-003
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               PERFORM       ROT-200-170
               GO  TO        ROT-200-150.
           IF  CTG-CLI   <   1  OR  >  5
               MOVE      0   TO  CTG-CLI
               GO  TO        ROT-100-160.
           MOVE     CTG-TAB (CTG-CLI) TO  CTG-TLA.

       ROT-200-170.
           DISPLAY  TLA-002  ENT-001
                             ENT-002.
       ROT-200-180.
           DISPLAY  MEN-017
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               DISPLAY       TLA-005
               GO  TO        ROT-200-160.
           MOVE     TEL-AUX  TO  TEL-CLI
           MOVE     CEP-AUX  TO  CEP-CLI
           MOVE     CGC-AUX  TO  CGC-CLI
           REWRITE  REG-CLI
           GO  TO   ROT-000-040.

       ROT-300-000.
           DISPLAY  MEN-021
           ACCEPT   OPC-002
           GO  TO   ROT-000-040.

       ROT-400-000.
           DISPLAY  MEN-019
           OPEN     INPUT    CADFAT
                    CADNTA   CADNTE
           IF  CTG-CLI   =   5
               GO  TO        ROT-400-020.
           MOVE     SPACES   TO  CHV-NTA
           MOVE     CHV-CLI  TO  CLI-NTA
           START    CADNTA   KEY GREATER  CHV-NTA
                    INVALID  KEY GO  TO   ROT-400-010.
           READ     CADNTA   NEXT
           IF  CLI-NTA   =   CHV-CLI
               GO  TO        ROT-400-030.

       ROT-400-010.
           READ     CADFAT   AT  END
                    GO  TO   ROT-400-040.
           IF  CLI-FAT  NOT  =   CHV-CLI
               GO  TO        ROT-400-010.
           GO  TO   ROT-400-030.

       ROT-400-020.
           READ     CADNTE   AT  END
                    GO  TO   ROT-400-040.
           IF  CLI-NTE  NOT  =   CHV-CLI
               GO  TO        ROT-400-020.

       ROT-400-030.
           CLOSE    CADFAT
                    CADNTA  CADNTE
           DISPLAY  MEN-018
           ACCEPT   OPC-002
           GO  TO   ROT-000-040.

       ROT-400-040.
           CLOSE    CADFAT
                    CADNTA  CADNTE
           DISPLAY  MEN-020
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   00
               DELETE    CADCLI.
           GO  TO   ROT-000-040.

       ROT-500-000.
           MOVE     1   TO   PRM-001
           CLOSE    CADCLI
           CHAIN   "DYN500"  USING  PRM-001.
