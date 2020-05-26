-module(domain_util).
-export([get_arity/1, mapfun/2, mapfun/3]).

get_arity(Fun) ->
    proplists:get_value(arity, erlang:fun_info(Fun)).

mapfun(Mapper, Fun) -> 
    case get_arity(Fun) of
        0  -> Mapper(Fun());
        1  -> fun(Arg) -> Mapper(Fun(Arg)) end;
        2  -> fun(A1, A2) -> Mapper(Fun(A1, A2)) end;
        3  -> fun(A1, A2, A3) -> Mapper(Fun(A1, A2, A3)) end;
        4  -> fun(A1, A2, A3, A4) -> Mapper(Fun(A1, A2, A3, A4)) end;
        5  -> fun(A1, A2, A3, A4, A5) -> Mapper(Fun(A1, A2, A3, A4, A5)) end;
        6  -> fun(A1, A2, A3, A4, A5, A6) -> Mapper(Fun(A1, A2, A3, A4, A5, A6)) end;
        7  -> fun(A1, A2, A3, A4, A5, A6, A7) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7)) end;
        8  -> fun(A1, A2, A3, A4, A5, A6, A7, A8) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8)) end;
        9  -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8, A9)) end;
        10 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) end;
        11 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)) end;
        12 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)) end;
        13 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) -> Mapper(Fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)) end
    end.

mapfun(Mapper, Fun1, Fun2) -> 
    case {get_arity(Fun1), get_arity(Fun2)} of
        {0, 0}   -> Mapper(Fun1(), Fun2());
        {1, 1}   -> fun(Arg) -> Mapper(Fun1(Arg), Fun2(Arg)) end;
        {2, 2}   -> fun(A1, A2) -> Mapper(Fun1(A1, A2), Fun2(A1, A2)) end;
        {3, 3}   -> fun(A1, A2, A3) -> Mapper(Fun1(A1, A2, A3), Fun2(A1, A2, A3)) end;
        {4, 4}   -> fun(A1, A2, A3, A4) -> Mapper(Fun1(A1, A2, A3, A4), Fun2(A1, A2, A3, A4)) end;
        {5, 5}   -> fun(A1, A2, A3, A4, A5) -> Mapper(Fun1(A1, A2, A3, A4, A5), Fun2(A1, A2, A3, A4, A5)) end;
        {6, 6}   -> fun(A1, A2, A3, A4, A5, A6) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6), Fun2(A1, A2, A3, A4, A5, A6)) end;
        {7, 7}   -> fun(A1, A2, A3, A4, A5, A6, A7) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7), Fun2(A1, A2, A3, A4, A5, A6, A7)) end;
        {8, 8}   -> fun(A1, A2, A3, A4, A5, A6, A7, A8) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8), Fun2(A1, A2, A3, A4, A5, A6, A7, A8)) end;
        {9, 9}   -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9)) end;
        {10, 10} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) end;
        {11, 11} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)) end;
        {12, 12} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)) end;
        {13, 13} -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) -> Mapper(Fun1(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13), Fun2(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)) end
    end.

