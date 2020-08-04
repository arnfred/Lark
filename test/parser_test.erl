-module(parser_test).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").

local_type_test_() ->
    Module = 
      "import Boolean/_\n"
      "type Boolean -> True | False\n"
      "def xor a b\n"
      " | True False -> True\n"
      " | False True -> True\n"
      " | _ _ -> False",
	{timeout,3600, 
	 [?_assertMatch({ok, _}, parser:parse(text, [Module]))]}.
