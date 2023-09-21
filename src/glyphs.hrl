% See https://www.fileformat.info/info/unicode/char/search.htm
-define(UNICODE_HEAVY_CHECK_MARK, <<16#2714/utf8>>).
-define(UNICODE_BALLOT_X, <<16#2717/utf8>>).
-define(UNICODE_HEAVY_BALLOT_X, <<16#2718/utf8>>).

-define(TEST_PASSED_GLYPH, ?UNICODE_HEAVY_CHECK_MARK).
-define(TEST_FAILED_GLYPH, ?UNICODE_HEAVY_BALLOT_X).
-define(TEST_SKIPPED_GLYPH, ?UNICODE_BALLOT_X).
