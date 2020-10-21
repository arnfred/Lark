-module(syntax).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 328).

unpack_tuple([T]) -> T;
unpack_tuple([T | _] = Terms) -> {tuple, ctx(T), Terms}.
unpack_sum([T]) -> T;
unpack_sum([T | _] = Terms) -> {sum, ctx(T), Terms}.

name([{symbol, _, _, S} | _]) -> S.
args([_ | Args]) -> Args.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V;
unwrap({_,_,_,V}) -> V.

make_symbol({var_symbol, L, S}) -> {symbol, L, variable, S};
make_symbol({type_symbol, L, S}) -> {symbol, L, type, S}.
make_symbol({_, L, S}, Type) -> {symbol, L, Type, S}.

ctx({_, Ctx})         -> Ctx;
ctx({_, Ctx, _})      -> Ctx;
ctx({_, Ctx, _, _})   -> Ctx;
ctx([Head|_]) 		  -> ctx(Head).

-file("/usr/local/Cellar/erlang/23.0.4/lib/erlang/lib/parsetools-2.2/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) -> [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("/Users/arnfred/workspace/kind/src/syntax.erl", 198).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
yeccpars2_0(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_all(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_2(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccgoto_statements(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_statement(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_statement(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_statement(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_statement(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_7/7}).
yeccpars2_7(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_8(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
yeccpars2_cont_8(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_8(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_8(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_8(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_8(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_8(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_8(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_8(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_9: see yeccpars2_8

yeccpars2_10(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 225, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_11/7}).
yeccpars2_11(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_12: see yeccpars2_8

-dialyzer({nowarn_function, yeccpars2_13/7}).
yeccpars2_13(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, right_arrow, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_14(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccgoto_symbols(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_symbol(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_operator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccgoto_operator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_operator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_operator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccgoto_operator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_operator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_operator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_operator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_27_(Stack),
 yeccgoto_symbols(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_28(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_30/7}).
yeccpars2_30(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_31(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_implies(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_32_(Stack),
 yeccgoto_implies(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_clause_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_34/7}).
yeccpars2_34(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_newtype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_36(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_type_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_37(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_infix(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_42/7}).
yeccpars2_42(S, right_arrow, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_patterns(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_45(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_collection(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_infix(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_application(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_collection(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_53(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_55(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_literal(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_60(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_61/7}).
yeccpars2_61(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_64(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_66/7}).
yeccpars2_66(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_67(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_67_(Stack),
 yeccgoto_expressions(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_68(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_70(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_71(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_72: see yeccpars2_29

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_lambda(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_75(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_76/7}).
yeccpars2_76(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_77/7}).
yeccpars2_77(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_78(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_81(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_83(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_84/7}).
yeccpars2_84(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_85/7}).
yeccpars2_85(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_86(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_88(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_89_(Stack),
 yeccgoto_sequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_90(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_91: see yeccpars2_70

-dialyzer({nowarn_function, yeccpars2_92/7}).
yeccpars2_92(S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_93: see yeccpars2_70

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_assignment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_95/7}).
yeccpars2_95(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_97/7}).
yeccpars2_97(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_sequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_99_(Stack),
 yeccgoto_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_100: see yeccpars2_90

yeccpars2_101(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_102(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), val, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_104_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_105(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_sequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_107(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_107_\'$end\''(Stack),
 yeccgoto_expression(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_assign(Stack),
 yeccgoto_expression(hd(Nss), assign, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_close(Stack),
 yeccgoto_expression(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_comma(Stack),
 yeccgoto_expression(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_def(Stack),
 yeccgoto_expression(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_import_keyword(Stack),
 yeccgoto_expression(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_module_keyword(Stack),
 yeccgoto_expression(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_newlines(Stack),
 yeccgoto_expression(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_square_close(Stack),
 yeccgoto_expression(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_type(Stack),
 yeccgoto_expression(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_108(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_109(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_110(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_secondary_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_112_(Stack),
 yeccgoto_secondary_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_113(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_115(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_116(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_118(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_119(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_120(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_123: see yeccpars2_90

yeccpars2_124(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_124_\'$end\''(Stack),
 yeccgoto_noun(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_assign(Stack),
 yeccgoto_noun(hd(Nss), assign, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_close(Stack),
 yeccgoto_noun(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_colon(Stack),
 yeccgoto_noun(hd(Nss), colon, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_comma(Stack),
 yeccgoto_noun(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_curly_close(Stack),
 yeccgoto_noun(hd(Nss), curly_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_def(Stack),
 yeccgoto_noun(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_import_keyword(Stack),
 yeccgoto_noun(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_module_keyword(Stack),
 yeccgoto_noun(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_newlines(Stack),
 yeccgoto_noun(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_pipe(Stack),
 yeccgoto_noun(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_square_close(Stack),
 yeccgoto_noun(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_type(Stack),
 yeccgoto_noun(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_125(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_125_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_126(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_127(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_127_(Stack),
 yeccgoto_qualified_symbol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_128_(Stack),
 yeccgoto_qualified_symbol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_(Stack),
 yeccgoto_qualified_symbol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_(Stack),
 yeccgoto_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_131/7}).
yeccpars2_131(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_132_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_133: see yeccpars2_70

yeccpars2_134(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_134_(Stack),
 yeccgoto_expressions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_(Stack),
 yeccgoto_expressions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_137: see yeccpars2_108

yeccpars2_138(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_139: see yeccpars2_108

yeccpars2_140(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_141(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_142(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_rightbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_143(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_144(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_(Stack),
 yeccgoto_rightbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_146: see yeccpars2_108

yeccpars2_147(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_147_(Stack),
 yeccgoto_leftbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_148(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_148_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_149(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_152: see yeccpars2_70

-dialyzer({nowarn_function, yeccpars2_153/7}).
yeccpars2_153(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_154_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_156(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_157/7}).
yeccpars2_157(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_158_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_159: see yeccpars2_70

-dialyzer({nowarn_function, yeccpars2_160/7}).
yeccpars2_160(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_161_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_162_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_163(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_163_(Stack),
 yeccgoto_leftbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_164(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_165(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_167(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_168_(Stack),
 yeccgoto_pair(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_169(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_170(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_171(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_172: see yeccpars2_71

yeccpars2_173(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_174(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_174_\'$end\''(Stack),
 yeccgoto_pair_val(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_assign(Stack),
 yeccgoto_pair_val(hd(Nss), assign, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_close(Stack),
 yeccgoto_pair_val(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_comma(Stack),
 yeccgoto_pair_val(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_curly_close(Stack),
 yeccgoto_pair_val(hd(Nss), curly_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_def(Stack),
 yeccgoto_pair_val(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_import_keyword(Stack),
 yeccgoto_pair_val(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_module_keyword(Stack),
 yeccgoto_pair_val(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_newlines(Stack),
 yeccgoto_pair_val(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_pipe(Stack),
 yeccgoto_pair_val(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_square_close(Stack),
 yeccgoto_pair_val(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_type(Stack),
 yeccgoto_pair_val(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_175(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_176(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_177(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_179(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_180(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_181(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_182/7}).
yeccpars2_182(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_183(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_184: see yeccpars2_54

yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_185_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_186(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_187(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dict_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_189(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dict_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_190(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_191/7}).
yeccpars2_191(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_192(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_192_(Stack),
 yeccgoto_dict_elements(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_193(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_194(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_195_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_196: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_197/7}).
yeccpars2_197(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_198_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_199: see yeccpars2_108

yeccpars2_200(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_200_(Stack),
 yeccgoto_dict_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_201_(Stack),
 yeccgoto_dict_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_203(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 206, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_8(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_204(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_205_(Stack),
 yeccgoto_pattern_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_206(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_apply(Stack),
 yeccgoto_symbol(hd(Ss), apply, Ss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_colon(Stack),
 yeccgoto_symbol(hd(Ss), colon, Ss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_comma(Stack),
 yeccgoto_symbol(hd(Ss), comma, Ss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_newlines(Stack),
 yeccgoto_symbol(hd(Ss), newlines, Ss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_pipe(Stack),
 yeccgoto_symbol(hd(Ss), pipe, Ss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, slash, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_slash(Stack),
 yeccgoto_symbol(hd(Ss), slash, Ss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_(Stack),
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_207_(Stack),
 yeccgoto_pattern_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_208_(Stack),
 yeccgoto_patterns(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_209: see yeccpars2_28

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_type_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_or_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_212(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_212_(Stack),
 yeccgoto_flat_sum_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_213_(Stack),
 yeccgoto_sum_or_expression(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_214: see yeccpars2_71

%% yeccpars2_215: see yeccpars2_108

yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_216_(Stack),
 yeccgoto_flat_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_217: see yeccpars2_29

yeccpars2_218(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_218_(Stack),
 yeccgoto_type_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_(Stack),
 yeccgoto_type_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_220_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_newtype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_222_(Stack),
 yeccgoto_all(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_223/7}).
yeccpars2_223(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_224/7}).
yeccpars2_224(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_225(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_225_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_226_(Stack),
 yeccgoto_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_228(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_228_(Stack),
 yeccgoto_import(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_229_(Stack),
 yeccgoto_import(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_230: see yeccpars2_13

%% yeccpars2_231: see yeccpars2_70

%% yeccpars2_232: see yeccpars2_29

%% yeccpars2_233: see yeccpars2_42

yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_234_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_235(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_235_(Stack),
 yeccgoto_def_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_236: see yeccpars2_29

yeccpars2_237(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_237_(Stack),
 yeccgoto_def_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_238_(Stack),
 yeccgoto_def_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_239: see yeccpars2_70

yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_240_(Stack),
 yeccgoto_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_242_(Stack),
 yeccgoto_statements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_243_(Stack),
 yeccgoto_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_all/7}).
yeccgoto_all(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_application/7}).
yeccgoto_application(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_arguments/7}).
yeccgoto_arguments(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arguments(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arguments(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_assignment/7}).
yeccgoto_assignment(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_braced_pattern/7}).
yeccgoto_braced_pattern(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(182, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_braced_pattern(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(182, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clause/7}).
yeccgoto_clause(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clause_separator/7}).
yeccgoto_clause_separator(13, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(217, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(230, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(232, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(236, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_collection/7}).
yeccgoto_collection(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_def_clauses/7}).
yeccgoto_def_clauses(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_def_clauses(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict/7}).
yeccgoto_dict(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(126=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(225=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict_element/7}).
yeccgoto_dict_element(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(192, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_element(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(192, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_element(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(192, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict_elements/7}).
yeccgoto_dict_elements(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(191, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_elements(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(197, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_elements(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_element/7}).
yeccgoto_element(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(105, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_elements/7}).
yeccgoto_elements(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expression/7}).
yeccgoto_expression(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(133, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(153, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(159, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expressions/7}).
yeccgoto_expressions(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(157, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(159, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(160, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_flat_sum_list/7}).
yeccgoto_flat_sum_list(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flat_sum_list(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flat_sum_list(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_function/7}).
yeccgoto_function(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_implies/7}).
yeccgoto_implies(13, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(209, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(230, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(231, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(233, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(239, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_import/7}).
yeccgoto_import(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_infix/7}).
yeccgoto_infix(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(133, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(159, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_lambda/7}).
yeccgoto_lambda(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_leftbias_infix/7}).
yeccgoto_leftbias_infix(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_list/7}).
yeccgoto_list(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_literal/7}).
yeccgoto_literal(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_module/7}).
yeccgoto_module(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_module(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_module(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_newtype/7}).
yeccgoto_newtype(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_noun/7}).
yeccgoto_noun(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(133, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(163, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(159, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operator/7}).
yeccgoto_operator(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(137, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(146, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(137, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(126=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(142, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(137, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(146, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(137, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(163, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(137, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(137, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair/7}).
yeccgoto_pair(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair_key/7}).
yeccgoto_pair_key(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(133, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(159, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair_val/7}).
yeccgoto_pair_val(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern/7}).
yeccgoto_pattern(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_application/7}).
yeccgoto_pattern_application(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_verb/7}).
yeccgoto_pattern_verb(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_patterns/7}).
yeccgoto_patterns(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(233, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(233, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_qualified_symbol/7}).
yeccgoto_qualified_symbol(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(224, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(126=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_rightbias_infix/7}).
yeccgoto_rightbias_infix(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_secondary_separator/7}).
yeccgoto_secondary_separator(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(108, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondary_separator(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(108, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_separator/7}).
yeccgoto_separator(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(133, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(100, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(100, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(199, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sequence/7}).
yeccgoto_sequence(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_statement/7}).
yeccgoto_statement(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_statements/7}).
yeccgoto_statements(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statements(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statements(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_elem/7}).
yeccgoto_sum_elem(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_list/7}).
yeccgoto_sum_list(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(159=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_or_expression/7}).
yeccgoto_sum_or_expression(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_or_expression(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_terms/7}).
yeccgoto_sum_terms(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(95, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_symbol/7}).
yeccgoto_symbol(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(14, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(223, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(14, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(14, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(186, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(175, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(126, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(127, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(133, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(159, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(175, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(186, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(186, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_symbols/7}).
yeccgoto_symbols(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(230, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbols(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbols(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_clause/7}).
yeccgoto_type_clause(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_clause(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_clause(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_clauses/7}).
yeccgoto_type_clauses(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_clauses(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_clauses(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_verb/7}).
yeccgoto_verb(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(133, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(159, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(184, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_2_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 61).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_14_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 98).
yeccpars2_14_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 89).
yeccpars2_16_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 94).
yeccpars2_17_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 91).
yeccpars2_18_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 95).
yeccpars2_19_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 93).
yeccpars2_20_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 90).
yeccpars2_21_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 96).
yeccpars2_22_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 92).
yeccpars2_23_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_24_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 84).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 85).
yeccpars2_26_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 99).
yeccpars2_27_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_32_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 71).
yeccpars2_32_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_33_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 77).
yeccpars2_35_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type_def , ctx ( __1 ) , name ( __2 ) , args ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 213).
yeccpars2_36_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 224).
yeccpars2_44_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 132).
yeccpars2_67_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 255).
yeccpars2_73_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 217).
yeccpars2_74_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { lambda , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_89_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 278).
yeccpars2_89_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 73).
yeccpars2_94_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { val , ctx ( __1 ) , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 280).
yeccpars2_98_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 304).
yeccpars2_99_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sum , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 284).
yeccpars2_102_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 270).
yeccpars2_103_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 283).
yeccpars2_104_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 282).
yeccpars2_105_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 279).
yeccpars2_106_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,'yeccpars2_107_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 121).
'yeccpars2_107_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_assign/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 121).
yeccpars2_107_assign(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 121).
yeccpars2_107_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 121).
yeccpars2_107_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 121).
yeccpars2_107_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 121).
yeccpars2_107_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 121).
yeccpars2_107_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 121).
yeccpars2_107_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 121).
yeccpars2_107_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 121).
yeccpars2_107_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_107_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_107_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_107_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_107_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_107_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_107_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_107_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_107_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_107_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_107_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 156).
yeccpars2_107_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 318).
yeccpars2_111_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_112_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 319).
yeccpars2_112_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 308).
yeccpars2_114_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_115_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 306).
yeccpars2_115_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,'yeccpars2_124_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
'yeccpars2_124_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_assign/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_assign(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_colon/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_colon(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_curly_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_curly_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_124_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 156).
yeccpars2_124_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_125_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 307).
yeccpars2_125_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_127_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 101).
yeccpars2_127_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_128_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 103).
yeccpars2_128_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 | unwrap ( __3 ) ] }
  end | __Stack].

-compile({inline,yeccpars2_129_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 102).
yeccpars2_129_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_130_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 303).
yeccpars2_130_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sum , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_132_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 257).
yeccpars2_132_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_134_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 133).
yeccpars2_134_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_135_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 134).
yeccpars2_135_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_136_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 256).
yeccpars2_136_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 174).
yeccpars2_142_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , make_symbol ( __2 , operator ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_144_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 175).
yeccpars2_144_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , make_symbol ( __2 , operator ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_147_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 173).
yeccpars2_147_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , __2 , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_148_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 162).
yeccpars2_148_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 ] }
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 156).
yeccpars2_154_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 163).
yeccpars2_155_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 | __4 ] }
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 166).
yeccpars2_158_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,yeccpars2_161_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 168).
yeccpars2_161_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,yeccpars2_162_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 167).
yeccpars2_162_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_163_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 172).
yeccpars2_163_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , __2 , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_168_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 183).
yeccpars2_168_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { pair , ctx ( __1 ) , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_174_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
'yeccpars2_174_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_assign/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
yeccpars2_174_assign(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
yeccpars2_174_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
yeccpars2_174_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_curly_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
yeccpars2_174_curly_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
yeccpars2_174_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
yeccpars2_174_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
yeccpars2_174_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
yeccpars2_174_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
yeccpars2_174_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
yeccpars2_174_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 197).
yeccpars2_174_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_174_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_174_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_174_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_174_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_174_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_174_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_174_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_174_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_174_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 147).
yeccpars2_174_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_174_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 156).
yeccpars2_174_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_185_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 227).
yeccpars2_185_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_192_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 265).
yeccpars2_192_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_195_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 258).
yeccpars2_195_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_198_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 260).
yeccpars2_198_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_200_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 266).
yeccpars2_200_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_201_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 267).
yeccpars2_201_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_202_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 259).
yeccpars2_202_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_205_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 239).
yeccpars2_205_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 ] }
  end | __Stack].

-compile({inline,yeccpars2_206_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 84).
yeccpars2_206_apply(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_206_colon/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 84).
yeccpars2_206_colon(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_206_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 84).
yeccpars2_206_comma(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_206_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 84).
yeccpars2_206_newlines(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_206_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 84).
yeccpars2_206_pipe(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_206_slash/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 84).
yeccpars2_206_slash(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_206_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 243).
yeccpars2_206_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_207_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 240).
yeccpars2_207_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 | __4 ] }
  end | __Stack].

-compile({inline,yeccpars2_208_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 225).
yeccpars2_208_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_210_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 207).
yeccpars2_210_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { clause , ctx ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_212_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 300).
yeccpars2_212_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_213_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 296).
yeccpars2_213_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unpack_sum ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_216_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 301).
yeccpars2_216_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_218_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 214).
yeccpars2_218_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_219_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 215).
yeccpars2_219_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_220_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 161).
yeccpars2_220_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_221_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 76).
yeccpars2_221_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type_def , ctx ( __1 ) , name ( __2 ) , args ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_222_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 52).
yeccpars2_222_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_225_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 85).
yeccpars2_225_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_226_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 110).
yeccpars2_226_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { module , ctx ( __1 ) , [ make_symbol ( __2 ) ] , unwrap ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_227_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 111).
yeccpars2_227_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { module , ctx ( __1 ) , unwrap ( __2 ) , unwrap ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_228_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 113).
yeccpars2_228_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { import , ctx ( __1 ) , [ __2 ] }
  end | __Stack].

-compile({inline,yeccpars2_229_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 114).
yeccpars2_229_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { import , ctx ( __1 ) , unwrap ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_234_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 75).
yeccpars2_234_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { def , ctx ( __1 ) , name ( __2 ) , args ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_235_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 209).
yeccpars2_235_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_237_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 210).
yeccpars2_237_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_238_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 211).
yeccpars2_238_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_240_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 205).
yeccpars2_240_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { clause , ctx ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_241_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 74).
yeccpars2_241_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { def , ctx ( __1 ) , name ( __2 ) , args ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_242_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 62).
yeccpars2_242_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_243_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 64).
yeccpars2_243_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].


-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 350).
