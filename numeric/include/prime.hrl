-define( GENPRIME, {global, genprime} ).
-define( DETS_AUTOSAVE, 100000 ).     % 10 seconds
-define( DETS_FILE_BUCKET, 100000000 ). % 100 million
-define( DETS_SLOT_BUCKET, 1000 ).
-define( DETS_MAX_SLOTS, ?DETS_FILE_BUCKET ).
-define( DETS_MIN_SLOTS, ?DETS_FILE_BUCKET ).

-record( gpstate, { detsdir, tbls, nfiles } ).
-record( entry, {key, count, primes} ). % Will not be used.
