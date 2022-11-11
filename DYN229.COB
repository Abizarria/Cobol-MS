       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN229.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCPD        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-CPD.

           SELECT      CADFAT        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-FAT.

           SELECT      CADNTA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-NTA.

           SELECT      CADNTE        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-NTE.

       DATA            DIVISION.
       FILE            SECTION.

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
                       VALUE    OF   FILE-ID     "CADNTE.DAT".
       01  REG-NTE.
           03  CHV-NTE          PIC  9(06).
           03  DTA-NTE          PIC  9(06).
           03  CLI-NTE          PIC  X(10).
           03  PCA-NTE          PIC  X(07).
           03  DES-NTE          PIC  X(20).
           03  VAL-NTE          PIC S9(09)V99.

       WORKING-STORAGE SECTION.

       LINKAGE         SECTION.

       01  ACM-SDO              PIC S9(12)V99.

       PROCEDURE       DIVISION  USING  ACM-SDO.

       ROT-000-000.
           OPEN     INPUT    CADCPD   CADFAT
                             CADNTA   CADNTE.

       ROT-000-010.
           READ     CADCPD   AT  END
                    GO  TO   ROT-000-020.
           ADD      VAL-CPD  TO  ACM-SDO
           GO  TO   ROT-000-010.

       ROT-000-020.
           READ     CADFAT   AT  END
                    GO  TO   ROT-000-030.
           ADD      VAL-FAT  TO  ACM-SDO
           GO  TO   ROT-000-020.

       ROT-000-030.
           READ     CADNTA   AT  END
                    GO  TO   ROT-000-040.
           ADD      VAL-NTA  TO  ACM-SDO
           GO  TO   ROT-000-030.

       ROT-000-040.
           READ     CADNTE   AT  END
                    GO  TO   ROT-000-050.
           ADD      VAL-NTE  TO  ACM-SDO
           GO  TO   ROT-000-040.

       ROT-000-050.
           CLOSE    CADCPD       CADFAT
                    CADNTA       CADNTE
           EXIT     PROGRAM.