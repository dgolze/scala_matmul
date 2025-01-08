MODULE kinds

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: sp, dp, default_string_length

   INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 30)
   INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(14, 200)
   INTEGER, PARAMETER :: default_string_length = 100

END MODULE kinds

