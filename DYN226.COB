       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN226.
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

           SELECT      CADCTR        ASSIGN  TO  DISK.

           SELECT      CADECR        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-ECR.

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      CADTNQ        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TNQ.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCMB      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCMB.DAT".
       01  REG-CMB.
           03  CHV-CMB          PIC  9(01).
           03  DES-CMB          PIC  X(15).
           03  VVM-CMB          PIC  9(11)V99.
           03  LUC-CMB          PIC  9(04)V9999.

       FD  CADCTR      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCTR.DAT".
       01  REG-CTR.
           03  DIA-CTR          PIC  9(02).
           03  DTI-CTR          PIC  9(06).
           03  DTC-CTR          PIC  9(06).
           03  DTD-CTR          PIC  9(06).
           03  FAT-CTR          PIC  9(06).
           03  PCT-CTR          PIC  9(02)V99.

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

       FD  CADPRM      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADPRM.DAT".
       01  REG-PRM.
           05  COD-PRM          PIC  9(05).
           05  CLI-PRM          PIC  9(02)  OCCURS  36.
           05  END-PRM          PIC  9(02)  OCCURS  35.
           05  CID-PRM          PIC  9(02)  OCCURS  20.
           05  EST-PRM          PIC  9(02)  OCCURS  02.
           05  CEP-PRM          PIC  9(08).
           05  CGC-PRM          PIC  9(14).
           05  INS-PRM          PIC  9(02)  OCCURS  15.

       FD  CADTNQ      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADTNQ.DAT".
       01  REG-TNQ.
           03  CHV-TNQ          PIC  9(02).
           03  CMB-TNQ          PIC  9(01).
           03  SDI-TNQ          PIC S9(05)V9.
           03  SDA-TNQ          PIC S9(05)V9.

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC X(137).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  IND-CMB          PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  X(02).
           03  VDA-AUX          PIC  9(05)V9.

           03  ACM-SDI          PIC S9(06)V9.
           03  ACM-SDA          PIC S9(06)V9.
           03  ACM-CMP          PIC S9(06)V9.
           03  ACM-VDA          PIC  9(06)V9.
           03  ACM-VAL          PIC S9(10)V99.

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

       01  IMP-001.
           03  FILLER           PIC  9(01)  COMP-0  VALUE  15.

       01  IMP-002.
           03  FILLER           PIC  9(01)  COMP-0  VALUE  18.

       01  CAB-001.
           03  FILLER           PIC  X(22)  VALUE  "-DYNAMIC-".
           03  NOM-CB1          PIC  X(48).
           03  FILLER           PIC  X(08)  VALUE  "FOLHA :".
           03  PAG-CB1          PIC  9(02).

       01  CAB-002              PIC  X(80)  VALUE
           "L I V R O  D E  M O V I M E N T A C A O  D E  C O M B U S T
           "I V E I S  ( L M C )".

       01  CAB-003              PIC X(137)  VALUE  ALL  "-".

       01  CAB-004.
           03  FILLER           PIC  X(19)  VALUE  "| 1 | PRODUTO ...:".
           03  CMB-CB4          PIC  X(91).
           03  FILLER           PIC  X(16)  VALUE  "| 2 | DATA ...:".
           03  DIA-CB4          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB4          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB4          PIC  9(02).
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  CAB-005.
           03  FILLER           PIC  X(01)  VALUE  "|".
           03  FILLER           PIC X(135)  VALUE  ALL  "-".
           03  FILLER           PIC  X(01)  VALUE  "|".

       01  CAB-006.
           03  FILLER           PIC X(136)  VALUE  "| 3 | ESTOQUE DE ABE
               "RTURA (MEDICAO FISICA NO INICIO DO DIA)".
           03  FILLER           PIC  X(01)  VALUE  "|".

       01  CAB-007.
           03  FILLER           PIC  X(77)  VALUE
               "| 4 | VOLUME RECEBIDO NO DIA (EM LITROS)".
           03  FILLER           PIC  X(60)  VALUE  "| 4.1 |   TANQUES DE
               " DESCARGA    | 4.2 |  VOLUME RECEBIDO  |".

       01  CAB-008.
           03  FILLER           PIC  X(77)  VALUE
               "| 5 | VOLUME VENDIDO NO DIA (EM LITROS)".
           03  FILLER           PIC  X(47)  VALUE
               "| 4.4 | VOLUME DISP. (3.1 + 4.3) |".
           03  QTD-CB8          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  CAB-009.
           03  FILLER           PIC  X(77)  VALUE
               "| 5.1 | TANQUES | 5.2 |  BICO  | 5.3 | FECHAMENTO (+) |
               "5.4 |  ABERTURA (-)".
           03  FILLER           PIC  X(60)  VALUE  "| 5.5 |      AFERICO
               "ES (-)       | 5.6 | VENDAS / BICO (=) |".

       01  CAB-010.
           03  FILLER           PIC X(136)  VALUE  "| 9 | FECHAMENTO FIS
               "ICO (MEDICAO FISICA NO FIM DO DIA)".
           03  FILLER           PIC  X(01)  VALUE  "|".

       01  DET-001.
           03  FILLER           PIC  X(07)  VALUE  "| TQ |".
           03  T01-DT1          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T02-DT1          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T03-DT1          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T04-DT1          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T05-DT1          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T06-DT1          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T07-DT1          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T08-DT1          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T09-DT1          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T10-DT1          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(29)  VALUE
               "  | 3.1 |  ESTOQUE INICIAL  |".

       01  DET-002.
           03  FILLER           PIC  X(01)  VALUE  "|".
           03  Q01-DT2          PIC  ---.--9,9      BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q02-DT2          PIC  ---.--9,9      BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q03-DT2          PIC  ---.--9,9      BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q04-DT2          PIC  ---.--9,9      BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q05-DT2          PIC  ---.--9,9      BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q06-DT2          PIC  ---.--9,9      BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q07-DT2          PIC  ---.--9,9      BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q08-DT2          PIC  ---.--9,9      BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q09-DT2          PIC  ---.--9,9      BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q10-DT2          PIC  ---.--9,9      BLANK WHEN ZEROS.
           03  FILLER           PIC  X(15)  VALUE  " |".
           03  TOT-DT2          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  DET-003.
           03  FILLER           PIC  X(54)  VALUE
               "|  NOTA FISCAL NUMERO .............................:".
           03  NTF-DT3          PIC  999.999.
           03  FILLER           PIC  X(06)  VALUE  "  DE".
           03  DIA-DT3          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-DT3          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-DT3          PIC  9(02).
           03  FILLER           PIC  X(06)  VALUE  "  |".
           03  TQ1-DT3          PIC  9(02).
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  TQ2-DT3          PIC  9(02).
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  TQ3-DT3          PIC  9(02).
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  TQ4-DT3          PIC  9(02).
           03  FILLER           PIC  X(04)  VALUE  SPACES.
           03  TQ5-DT3          PIC  9(02).
           03  FILLER           PIC  X(17)  VALUE  "   |".
           03  QTD-DT3          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  DET-004.
           03  FILLER           PIC  X(04)  VALUE  "|".
           03  TQ1-DT4          PIC  9(02).
           03  FILLER           PIC  X(05)  VALUE  "  /".
           03  TQ2-DT4          PIC  9(02).
           03  FILLER           PIC  X(10)  VALUE  "   |".
           03  BMB-DT4          PIC  9(02).
           03  FILLER           PIC  X(14)  VALUE  "      |".
           03  FIN-DT4          PIC  999999,9.
           03  FILLER           PIC  X(15)  VALUE  "       |".
           03  INI-DT4          PIC  999999,9.
           03  FILLER           PIC  X(22)  VALUE  "       |".
           03  AFR-DT4          PIC  Z.ZZ9.
           03  FILLER           PIC  X(29)  VALUE  "             |".
           03  VDA-DT4          PIC  ZZ.ZZ9,9.
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  DET-005.
           03  FILLER           PIC  X(07)  VALUE  "| TQ |".
           03  T01-DT5          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T02-DT5          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T03-DT5          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T04-DT5          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T05-DT5          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T06-DT5          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T07-DT5          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T08-DT5          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T09-DT5          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(09)  VALUE  "  | TQ |".
           03  T10-DT5          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(29)  VALUE
               "  | 9.1 |       TOTAL       |".

       01  DET-006.
           03  FILLER           PIC  X(01)  VALUE  "|".
           03  Q01-DT6          PIC  ---.--9,9     BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q02-DT6          PIC  ---.--9,9     BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q03-DT6          PIC  ---.--9,9     BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q04-DT6          PIC  ---.--9,9     BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q05-DT6          PIC  ---.--9,9     BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q06-DT6          PIC  ---.--9,9     BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q07-DT6          PIC  ---.--9,9     BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q08-DT6          PIC  ---.--9,9     BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q09-DT6          PIC  ---.--9,9     BLANK WHEN ZEROS.
           03  FILLER           PIC  X(02)  VALUE  " |".
           03  Q10-DT6          PIC  ---.--9,9     BLANK WHEN ZEROS.
           03  FILLER           PIC  X(15)  VALUE  " |".
           03  TOT-DT6          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  DET-007.
           03  FILLER           PIC  X(61)  VALUE
               "|  NOTA FISCAL NUMERO .............................:".
           03  FILLER           PIC  X(08)  VALUE  "  DE".
           03  FILLER           PIC  X(03)  VALUE  "/".
           03  FILLER           PIC  X(03)  VALUE  "/".
           03  FILLER           PIC  X(35)  VALUE  "  |".
           03  FILLER           PIC  X(21)  VALUE  "|".
           03  FILLER           PIC  X(06)  VALUE  "0,0  |".

       01  DET-008.
           03  FILLER           PIC  X(13)  VALUE  "|".
           03  FILLER           PIC  X(12)  VALUE  "   |".
           03  FILLER           PIC  X(22)  VALUE  "      |".
           03  FILLER           PIC  X(23)  VALUE  "       |".
           03  FILLER           PIC  X(27)  VALUE  "       |".
           03  FILLER           PIC  X(37)  VALUE  "             |".
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  TOT-001.
           03  FILLER           PIC  X(77)  VALUE  "|".
           03  FILLER           PIC  X(47)  VALUE
               "| 4.3 |      TOTAL RECEBIDO      |".
           03  QTD-TT1          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  TOT-002.
           03  FILLER           PIC  X(77)  VALUE
               "| 10 |                       VALOR DAS VENDAS (EM R$)".
           03  FILLER           PIC  X(48)  VALUE
               "| 5.7 |      VENDAS NO DIA       |".
           03  QTD-TT2          PIC  ZZZ.ZZ9,9.
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  TOT-003.
           03  FILLER           PIC  X(58)  VALUE  "| 10.1 | VALOR NO DI
               "A (5.7 x PRECO DE VENDA)           |".
           03  VAL-TT3          PIC  --.---.---.--9,99.
           03  FILLER           PIC  X(49)  VALUE
               "  | 6 | EST.ESCRITURAL (4.4 - 5.7) |".
           03  QTD-TT3          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  TOT-004.
           03  FILLER           PIC  X(57)  VALUE  "| 10.2 | VALOR ACUMU
               "LADO NO MES                        |".
           03  VAL-TT4          PIC  ---.---.---.--9,99.
           03  FILLER           PIC  X(49)  VALUE
               "  | 7 | VOLUME DE FECHAMENTO (9.1) |".
           03  QTD-TT4          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  TOT-005.
           03  FILLER           PIC  X(77)  VALUE
               "| 11 |                         PARA USO DO REVENDEDOR".
           03  FILLER           PIC  X(47)  VALUE
               "| 8 | PERDAS / SOBRAS (7 - 6)    |".
           03  QTD-TT5          PIC  ----.--9,9.
           03  FILLER           PIC  X(03)  VALUE  "  |".

       01  TOT-006.
           03  FILLER           PIC  X(77)  VALUE  "|".
           03  FILLER           PIC  X(60)  VALUE  "| 12 |
               "DESTINADO A FISCALIZACAO               |".

       01  OBS-001.
           03  FILLER           PIC  X(77)  VALUE
               "| 13 | OBSERVACOES |".
           03  FILLER           PIC  X(59)  VALUE  "| A.N.P.".
           03  FILLER           PIC  X(01)  VALUE  "|".

       01  OBS-002.
           03  FILLER           PIC  X(77)  VALUE
               "|-------------------".
           03  FILLER           PIC  X(59)  VALUE  "|".
           03  FILLER           PIC  X(01)  VALUE  "|".

       01  OBS-003.
           03  FILLER           PIC  X(77)  VALUE  "|".
           03  FILLER           PIC  X(59)  VALUE  "|".
           03  FILLER           PIC  X(01)  VALUE  "|".

       01  OBS-004.
           03  FILLER           PIC  X(77)  VALUE  "|".
           03  FILLER           PIC  X(01)  VALUE  "|".
           03  FILLER           PIC  X(58)  VALUE  ALL  "-".
           03  FILLER           PIC  X(01)  VALUE  "|".

       01  OBS-005.
           03  FILLER           PIC  X(77)  VALUE  "|".
           03  FILLER           PIC  X(59)  VALUE
               "| OUTROS ORGAOS FISCAIS".
           03  FILLER           PIC  X(01)  VALUE  "|".

       01  OBS-006.
           03  FILLER           PIC  X(68)  VALUE  "|                (*)
               " ATENCAO - SE O RESULTADO FOR NEGATIVO, PODE EST".
           03  FILLER           PIC  X(69)  VALUE  "AR HAVENDO VAZAMENTO
               " DE PRODUTO PARA O MEIO AMBIENTE                |".

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

       01  PRM-004.
           03  SDO-PR4          PIC S9(11)V99  OCCURS  05.

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 10 COLUMN 28 PIC X(23) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 28 PIC X(23) FROM " ³                   ³
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 51 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 28 PIC X(23) FROM " ³  FOLHA DO L.M.C.  ³
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 51 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 28 PIC X(23) FROM " ³                   ³
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 51 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 28 PIC X(23) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 51 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 30 PIC X(23) FROM SPACES.

       01  LPA-001.
           03  LINE 09 COLUMN 11 PIC X(58) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 11 PIC X(60) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 13 PIC X(58) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001  LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA EMITIR A FO
                    "LHA DO L.M.C. ? [S/N]  [ ]  -  [ESC] RETORNA ... "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002  LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRI
                    "MINDO FOLHA DO L.M.C. ...                        "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 56 PIC X(01) USING RSP-OPC  AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001  PRM-002
                                           PRM-003  PRM-004.
       ROT-000-000.
           DISPLAY  LPA-001  TLA-001.

       ROT-000-010.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-001
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO   =   01
               GO  TO        ROT-400-000.
           IF  RSP-OPC   =   "N"
               GO  TO        ROT-300-000.
           IF  RSP-OPC  NOT  =   "S"
               GO  TO        ROT-000-010.

       ROT-100-000.
           DISPLAY  MEN-002
           OPEN     INPUT    CADCTR
                             CADPRM
           READ     CADCTR
           READ     CADPRM
           MOVE     1        TO  IND1
           ADD      1        TO  DIA-CTR
           MOVE     DTC-CTR  TO  DTA-AUX
           MOVE     DIA-CTR  TO  PAG-CB1
           MOVE     DIA-AUX  TO  DIA-CB4
           MOVE     MES-AUX  TO  MES-CB4
           MOVE     ANO-AUX  TO  ANO-CB4.

       ROT-100-010.
           MOVE     CLI-PRM (IND1 + 1)  TO IND2
           MOVE     LET-TAB (IND2)  TO  LET-NOM (IND1)
           ADD      1   TO   IND1
           IF  IND1  <  36
               GO  TO        ROT-100-010.
           MOVE     SPACES   TO  LET-NOM (IND1)
           MOVE     NOM-TAB  TO  NOM-CB1
           CLOSE    CADCTR       CADPRM
           OPEN     INPUT        CADCMB
                                 CADECR   CADTNQ
           MOVE     ZEROS    TO  CHV-CMB  IND-CMB
           START    CADCMB   KEY GREATER  CHV-CMB.

       ROT-100-020.
           READ     CADCMB  NEXT AT  END
                    GO  TO       ROT-100-030.
           ADD      1        TO  IND-CMB
           GO  TO   ROT-100-020.

       ROT-100-030.
           MOVE     ZEROS    TO  CHV-CMB
           START    CADCMB   KEY GREATER  CHV-CMB.

       ROT-200-000.
           READ     CADCMB  NEXT AT  END
                    GO  TO       ROT-200-900.
           OPEN     OUTPUT       RELATO
           MOVE     DES-CMB  TO  CMB-CB4
           WRITE    REG-REL FROM CAB-001  AFTER  0
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM IMP-001  AFTER  0
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM CAB-004
           WRITE    REG-REL FROM CAB-005
           WRITE    REG-REL FROM CAB-006
           WRITE    REG-REL FROM CAB-005
           MOVE     ZEROS    TO  ACM-SDI  ACM-SDA  CHV-TNQ  IND1
                    T01-DT1      T02-DT1  T03-DT1  T04-DT1  T05-DT1
                    T06-DT1      T07-DT1  T08-DT1  T09-DT1  T10-DT1
                    Q01-DT2      Q02-DT2  Q03-DT2  Q04-DT2  Q05-DT2
                    Q06-DT2      Q07-DT2  Q08-DT2  Q09-DT2  Q10-DT2
                    T01-DT5      T02-DT5  T03-DT5  T04-DT5  T05-DT5
                    T06-DT5      T07-DT5  T08-DT5  T09-DT5  T10-DT5
                    Q01-DT6      Q02-DT6  Q03-DT6  Q04-DT6  Q05-DT6
                    Q06-DT6      Q07-DT6  Q08-DT6  Q09-DT6  Q10-DT6
           START    CADTNQ   KEY GREATER  CHV-TNQ.

       ROT-200-010.
           READ     CADTNQ   NEXT AT END
                    GO  TO   ROT-200-020.
           IF  CMB-TNQ  NOT  =   CHV-CMB
               GO  TO        ROT-200-010.
           ADD      1        TO  IND1
           IF  IND1  =  01
               MOVE          SDI-TNQ  TO  Q01-DT2
               MOVE          SDA-TNQ  TO  Q01-DT6
               MOVE          CHV-TNQ  TO  T01-DT1  T01-DT5.
           IF  IND1  =  02
               MOVE          SDI-TNQ  TO  Q02-DT2
               MOVE          SDA-TNQ  TO  Q02-DT6
               MOVE          CHV-TNQ  TO  T02-DT1  T02-DT5.
           IF  IND1  =  03
               MOVE          SDI-TNQ  TO  Q03-DT2
               MOVE          SDA-TNQ  TO  Q03-DT6
               MOVE          CHV-TNQ  TO  T03-DT1  T03-DT5.
           IF  IND1  =  04
               MOVE          SDI-TNQ  TO  Q04-DT2
               MOVE          SDA-TNQ  TO  Q04-DT6
               MOVE          CHV-TNQ  TO  T04-DT1  T04-DT5.
           IF  IND1  =  05
               MOVE          SDI-TNQ  TO  Q05-DT2
               MOVE          SDA-TNQ  TO  Q05-DT6
               MOVE          CHV-TNQ  TO  T05-DT1  T05-DT5.
           IF  IND1  =  06
               MOVE          SDI-TNQ  TO  Q06-DT2
               MOVE          SDA-TNQ  TO  Q06-DT6
               MOVE          CHV-TNQ  TO  T06-DT1  T06-DT5.
           IF  IND1  =  07
               MOVE          SDI-TNQ  TO  Q07-DT2
               MOVE          SDA-TNQ  TO  Q07-DT6
               MOVE          CHV-TNQ  TO  T07-DT1  T07-DT5.
           IF  IND1  =  08
               MOVE          SDI-TNQ  TO  Q08-DT2
               MOVE          SDA-TNQ  TO  Q08-DT6
               MOVE          CHV-TNQ  TO  T08-DT1  T08-DT5.
           IF  IND1  =  09
               MOVE          SDI-TNQ  TO  Q09-DT2
               MOVE          SDA-TNQ  TO  Q09-DT6
               MOVE          CHV-TNQ  TO  T09-DT1  T09-DT5.
           IF  IND1  =  10
               MOVE          SDI-TNQ  TO  Q10-DT2
               MOVE          SDA-TNQ  TO  Q10-DT6
               MOVE          CHV-TNQ  TO  T10-DT1  T10-DT5.
           ADD      SDI-TNQ  TO  ACM-SDI
           ADD      SDA-TNQ  TO  ACM-SDA
           GO  TO   ROT-200-010.

       ROT-200-020.
           MOVE     ACM-SDI  TO  TOT-DT2
           MOVE     ACM-SDA  TO  TOT-DT6
           WRITE    REG-REL FROM DET-001
           WRITE    REG-REL FROM CAB-005
           WRITE    REG-REL FROM DET-002
           WRITE    REG-REL FROM CAB-005
           WRITE    REG-REL FROM CAB-007
           WRITE    REG-REL FROM CAB-005
           MOVE     ZEROS    TO  ACM-CMP  IND1  IND2.

       ROT-200-030.
           ADD      1        TO  IND1
           IF  IND1  >   10  OR  CMB-PR3 (IND1)  =  0
               GO  TO        ROT-200-040.
           IF  CHV-CMB  NOT  =   CMB-PR3 (IND1)
               GO  TO        ROT-200-030.
           MOVE     NTF-PR3 (IND1)   TO  NTF-DT3
           MOVE     TQ1-PR3 (IND1)   TO  TQ1-DT3
           MOVE     TQ2-PR3 (IND1)   TO  TQ2-DT3
           MOVE     TQ3-PR3 (IND1)   TO  TQ3-DT3
           MOVE     TQ4-PR3 (IND1)   TO  TQ4-DT3
           MOVE     TQ5-PR3 (IND1)   TO  TQ5-DT3
           MOVE     QTD-PR3 (IND1)   TO  QTD-DT3
           ADD      QTD-PR3 (IND1)   TO  ACM-CMP
           MOVE     DIA-AUX  TO  DIA-DT3
           MOVE     MES-AUX  TO  MES-DT3
           MOVE     ANO-AUX  TO  ANO-DT3
           WRITE    REG-REL FROM DET-003
           ADD      1        TO  IND2
           GO  TO   ROT-200-030.

       ROT-200-040.
           IF  IND2   >   1
               GO  TO        ROT-200-050.
           WRITE    REG-REL FROM DET-007
           ADD      1        TO  IND2
           GO  TO   ROT-200-040.

       ROT-200-050.
           WRITE    REG-REL FROM CAB-005
           MOVE     ACM-CMP  TO  QTD-TT1
           WRITE    REG-REL FROM TOT-001
           WRITE    REG-REL FROM CAB-005
           COMPUTE  QTD-CB8  =   ACM-SDI  +  ACM-CMP
           WRITE    REG-REL FROM CAB-008
           WRITE    REG-REL FROM CAB-005
           WRITE    REG-REL FROM CAB-009
           WRITE    REG-REL FROM CAB-005
           MOVE     ZEROS    TO  CHV-ECR  IND1
                                 ACM-VDA  ACM-VAL
           START    CADECR   KEY GREATER  CHV-ECR.

       ROT-200-060.
           READ     CADECR   NEXT AT END
                    GO  TO   ROT-200-070.
           IF  CMB-ECR  NOT  =   CHV-CMB
               GO  TO        ROT-200-060.
           MOVE     TQ1-ECR  TO  TQ1-DT4
           MOVE     TQ2-ECR  TO  TQ2-DT4
           MOVE     CHV-ECR  TO  BMB-DT4
           MOVE     ICL-ECR  TO  FIN-DT4
           MOVE     IDL-ECR  TO  INI-DT4
           MOVE     EXT-ECR  TO  AFR-DT4
           COMPUTE  VDA-AUX  =  (FTL-ECR  +  1000000  -  IDL-ECR)
                             +  (ICL-ECR  +  1000000  -  ITL-ECR)
                             -   EXT-ECR
           COMPUTE  ACM-VAL  =   ACM-VAL
                 +  VDA-AUX  *   VAL-ECR
           MOVE     VDA-AUX  TO  VDA-DT4
           WRITE    REG-REL FROM DET-004
           ADD      VDA-AUX  TO  ACM-VDA
           ADD      1        TO  IND1
           GO  TO   ROT-200-060.

       ROT-200-070.
           IF  IND1   >  07
               GO  TO        ROT-200-080.
           WRITE    REG-REL FROM DET-008
           ADD      1        TO  IND1
           GO  TO   ROT-200-070.

       ROT-200-080.
           WRITE    REG-REL FROM CAB-005
           MOVE     ACM-VDA  TO  QTD-TT2
           WRITE    REG-REL FROM TOT-002
           WRITE    REG-REL FROM CAB-005
           MOVE     ACM-VAL  TO  VAL-TT3
           COMPUTE  QTD-TT3  =   ACM-SDI
                 +  ACM-CMP  -   ACM-VDA
           WRITE    REG-REL FROM TOT-003
           WRITE    REG-REL FROM CAB-005
           COMPUTE  VAL-TT4  =   VVM-CMB
                             +   ACM-VAL
           MOVE     ACM-SDA  TO  QTD-TT4
           WRITE    REG-REL FROM TOT-004
           WRITE    REG-REL FROM CAB-005
           COMPUTE  QTD-TT5  =   ACM-SDA  -  (ACM-SDI
                             +   ACM-CMP  -   ACM-VDA)
           WRITE    REG-REL FROM TOT-005
           WRITE    REG-REL FROM CAB-005
           WRITE    REG-REL FROM TOT-006
           WRITE    REG-REL FROM CAB-005
           WRITE    REG-REL FROM OBS-001
           WRITE    REG-REL FROM OBS-002
           WRITE    REG-REL FROM OBS-003
           WRITE    REG-REL FROM OBS-003
           WRITE    REG-REL FROM OBS-004
           WRITE    REG-REL FROM OBS-005
           WRITE    REG-REL FROM OBS-003
           WRITE    REG-REL FROM OBS-003
           WRITE    REG-REL FROM OBS-003
           WRITE    REG-REL FROM CAB-005
           WRITE    REG-REL FROM CAB-010
           WRITE    REG-REL FROM CAB-005
           WRITE    REG-REL FROM DET-005
           WRITE    REG-REL FROM CAB-005
           WRITE    REG-REL FROM DET-006
           WRITE    REG-REL FROM CAB-005
           WRITE    REG-REL FROM OBS-006
           WRITE    REG-REL FROM CAB-003
           WRITE    REG-REL FROM IMP-002  AFTER  0
           CLOSE    RELATO
           GO  TO   ROT-200-000.

       ROT-200-900.
           CLOSE    CADCMB
                    CADECR   CADTNQ.

       ROT-300-000.
           CHAIN   "DYN227"  USING  PRM-001  PRM-002
                                    PRM-003  PRM-004.
       ROT-400-000.
           CHAIN   "DYN225"  USING  PRM-001  PRM-002
                                    PRM-003  PRM-004.
