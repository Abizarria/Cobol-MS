       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN400.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.

       DATA            DIVISION.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  NUM-OPC          PIC  9(01).
           03  TECLADO          PIC  9(02).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 01 PIC X(67) FROM
               " SISTEMA AUTO-POSTO  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 27 PIC X(22) FROM " ³                  ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 29 PIC X(18) FROM "  Menu Principal"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 07 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 27 PIC X(22) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 27 PIC X(22) FROM " ³   1 - Estoque    ³"
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 27 PIC X(22) FROM " ³   2 - Caixas     ³"
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 27 PIC X(22) FROM " ³   3 - Despesas   ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 27 PIC X(22) FROM " ³   4 - Bancos     ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 27 PIC X(22) FROM " ³   5 - Clientes   ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 27 PIC X(22) FROM " ³   6 - Posicao    ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 27 PIC X(22) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 29 PIC X(22) FROM SPACES.

       01  TLA-002.
           03  LINE 12 COLUMN 31 PIC X(03) FROM " 4"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  01.
           03  LINE 08 COLUMN 34 PIC X(22) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 34 PIC X(22) FROM " ³                  ³
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 36 PIC X(18) FROM "      Bancos"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  01.
           03  LINE 09 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 34 PIC X(22) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 34 PIC X(22) FROM " ³  1 - Cadastro    ³
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 34 PIC X(22) FROM " ³  2 - Movimento   ³
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 34 PIC X(22) FROM " ³  3 - Conciliacao ³
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 34 PIC X(22) FROM " ³  4 - Consultas   ³
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 34 PIC X(22) FROM " ³  5 - Posicao     ³
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 34 PIC X(22) FROM " ³  6 - Relatorios  ³
               BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 34 PIC X(22) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 36 PIC X(22) FROM SPACES.

       01  TLA-003.
           03  LINE 14 COLUMN 37 PIC X(03)  FROM  " 4"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 12 COLUMN 40 PIC X(20) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 40 PIC X(20) FROM " ³                ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 42 PIC X(16) FROM "   Consultas"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 13 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 40 PIC X(20) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 40 PIC X(20) FROM " ³  1 - Extratos  ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 40 PIC X(20) FROM " ³  2 - Debitos   ³"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 40 PIC X(20) FROM " ³  3 - Creditos  ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 40 PIC X(20) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 42 PIC X(20) FROM SPACES.

       01  TLA-004.
           03  LINE 16 COLUMN 37 PIC X(03)  FROM  " 6"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 12 COLUMN 40 PIC X(20) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 40 PIC X(20) FROM " ³                ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 42 PIC X(16) FROM "   Relatorios"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 13 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 40 PIC X(20) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 40 PIC X(20) FROM " ³  1 - Bancos    ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 40 PIC X(20) FROM " ³  2 - Extratos  ³"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 40 PIC X(20) FROM " ³  3 - Cheques   ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 40 PIC X(20) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 42 PIC X(20) FROM SPACES.

       01  TLA-005.
           03  LINE 17 COLUMN 43 PIC X(03) FROM " 3"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  01.
           03  LINE 14 COLUMN 46 PIC X(22) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 46 PIC X(22) FROM " ³                  ³
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 48 PIC X(18) FROM "     Cheques"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  01.
           03  LINE 15 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 46 PIC X(22) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 46 PIC X(22) FROM " ³  1 - Pendentes   ³
               BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 46 PIC X(22) FROM " ³  2 - Devolvidos  ³
               BACKGROUND-COLOR  04.
           03  LINE 18 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 46 PIC X(22) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 19 COLUMN 68 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 48 PIC X(22) FROM SPACES.

       01  LPA-001.
           03  LINE 04 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 05 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 21 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.

       01  LPA-002.
           03  LINE 12 COLUMN 58 PIC X(02) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 58 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 58 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 58 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 58 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 58 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 58 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 42 PIC X(20) FROM SPACES
               BACKGROUND-COLOR  07.

       01  LPA-003.
           03  LINE 14 COLUMN 62 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 62 PIC X(08) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 62 PIC X(08) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 62 PIC X(08) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 62 PIC X(08) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 62 PIC X(08) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 48 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DES
                   "EJADA ? [ ]  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001.

       ROT-000-000.
           IF  PRM-001   =   0
               GO  TO        ROT-000-010.
           DISPLAY  LPA-001      TLA-001
           IF  PRM-001   =   1   OR   2
                         OR  3   OR   5
               GO  TO        ROT-000-010.
           DISPLAY  TLA-002      MEN-001
           IF  PRM-001   =   4
               GO  TO        ROT-400-000.
           IF  PRM-001   =   6
               GO  TO        ROT-600-000.

       ROT-000-010.
           DISPLAY  TLA-002  MEN-001.

       ROT-000-020.
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-700-000.
           IF  NUM-OPC   =   4
               GO  TO        ROT-400-000.
           IF  NUM-OPC   =   6
               GO  TO        ROT-600-000.
           IF  NUM-OPC   =   1   CHAIN  "DYN401".
           IF  NUM-OPC   =   2   CHAIN  "DYN402".
           IF  NUM-OPC   =   3   CHAIN  "DYN404".
           IF  NUM-OPC   =   5   CHAIN  "DYN408".
           GO  TO   ROT-000-020.

       ROT-400-000.
           DISPLAY  TLA-003.

       ROT-400-010.
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =    01
               DISPLAY       LPA-002
               GO  TO        ROT-000-010.
           IF  NUM-OPC   =   1   CHAIN  "DYN405".
           IF  NUM-OPC   =   2   CHAIN  "DYN406".
           IF  NUM-OPC   =   3   CHAIN  "DYN407".
           GO  TO   ROT-400-010.

       ROT-600-000.
           DISPLAY  TLA-004.

       ROT-600-010.
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =    01
               DISPLAY       LPA-002
               GO  TO        ROT-000-010.
           IF  NUM-OPC   =   3
               GO  TO        ROT-600-100.
           IF  NUM-OPC   =   1   CHAIN  "DYN409".
           IF  NUM-OPC   =   2   CHAIN  "DYN410".
           GO  TO   ROT-600-010.

       ROT-600-100.
           DISPLAY  TLA-005.

       ROT-600-110.
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =    01
               DISPLAY       LPA-003
               GO  TO        ROT-600-000.
           IF  NUM-OPC   =   1   CHAIN  "DYN411".
           IF  NUM-OPC   =   2   CHAIN  "DYN412".
           GO  TO   ROT-600-110.

       ROT-700-000.
           MOVE     ZEROS     TO    PRM-001
           CHAIN   "DYN001"  USING  PRM-001.
