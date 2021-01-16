-module(syntax).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 334).

unpack_sum([T]) -> T;
unpack_sum([T | _] = Terms) -> {sum, ctx(T), Terms}.

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

-file("/usr/local/Cellar/erlang/23.1.1/lib/erlang/lib/parsetools-2.2/include/yeccpre.hrl", 0).
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



-file("/Users/arnfred/workspace/kind/src/syntax.erl", 193).

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
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%%  yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_0/7}).
yeccpars2_cont_0(S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_all(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_2(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
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

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_statement(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
yeccpars2_8(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_9(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_9/7}).
yeccpars2_cont_9(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_10: see yeccpars2_9

%% yeccpars2_11: see yeccpars2_9

yeccpars2_12(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_14: see yeccpars2_9

yeccpars2_15(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, right_arrow, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_symbol(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
 yeccgoto_operator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_28/7}).
yeccpars2_28(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_29(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, right_arrow, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), right_arrow, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), space_curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), space_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), space_square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_infix(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, right_arrow, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), right_arrow, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), space_curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), space_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), space_square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_34/7}).
yeccpars2_34(S, right_arrow, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_35: see yeccpars2_28

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_patterns(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_38(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_39(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_collection(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_infix(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_application(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_collection(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_newtype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_46(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_def_fun(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 252, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_52/7}).
yeccpars2_52(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_53(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_implies(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_55(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 235, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 236, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_literal(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_60(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
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

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_62(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_64/7}).
yeccpars2_64(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_66(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_67(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_69/7}).
yeccpars2_69(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_70(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_expressions(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_71(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_72(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_73: see yeccpars2_43

%% yeccpars2_74: see yeccpars2_53

%% yeccpars2_75: see yeccpars2_48

yeccpars2_76(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_78(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
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

-dialyzer({nowarn_function, yeccpars2_79/7}).
yeccpars2_79(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_80/7}).
yeccpars2_80(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_, _, _, _, T, _, _) ->
 yeccerror(T).

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
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_82(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_83(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_85(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_87(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_88/7}).
yeccpars2_88(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_89/7}).
yeccpars2_89(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_90(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_92(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_sequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_94: see yeccpars2_9

yeccpars2_95(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_96: see yeccpars2_53

%% yeccpars2_97: see yeccpars2_76

%% yeccpars2_98: see yeccpars2_48

-dialyzer({nowarn_function, yeccpars2_99/7}).
yeccpars2_99(S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_100: see yeccpars2_43

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_assignment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_102(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_103(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_close(Stack),
 yeccgoto_expression(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_comma(Stack),
 yeccgoto_expression(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_sum_elem(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_104/7}).
yeccpars2_104(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_105/7}).
yeccpars2_105(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_106(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_107(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_108/7}).
yeccpars2_108(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_109(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_110(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_112(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
yeccpars2_113(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_114(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_115: see yeccpars2_43

yeccpars2_116(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_117(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_118(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_120/7}).
yeccpars2_120(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_121_(Stack),
 yeccgoto_sequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_122(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_122(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_122(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_122(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_122(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_122(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_122(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_122(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_122(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_122(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_(Stack),
 yeccgoto_sum_elem(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_123/7}).
yeccpars2_123(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_124(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_close(Stack),
 yeccgoto_sum_elem(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_newlines(Stack),
 yeccgoto_sum_elem(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_pipe(Stack),
 yeccgoto_sum_elem(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_125_(Stack),
 yeccgoto_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_126(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

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

yeccpars2_130(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_close(Stack),
 yeccgoto_expression(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_comma(Stack),
 yeccgoto_expression(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_newlines(Stack),
 yeccgoto_sum_elem(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_pipe(Stack),
 yeccgoto_sum_elem(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_(Stack),
 yeccgoto_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_132/7}).
yeccpars2_132(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_133_(Stack),
 yeccgoto_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_134: see yeccpars2_34

%% yeccpars2_135: see yeccpars2_43

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_137: see yeccpars2_118

yeccpars2_138(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_139(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), space_curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), space_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), space_square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), val, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_139_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_140_(Stack),
 yeccgoto_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_141_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_142(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_143_(Stack),
 yeccgoto_sequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_144(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_144_\'$end\''(Stack),
 yeccgoto_expression(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_144(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_close(Stack),
 yeccgoto_expression(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_144(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_comma(Stack),
 yeccgoto_expression(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_144(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_def(Stack),
 yeccgoto_expression(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_144(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_import_keyword(Stack),
 yeccgoto_expression(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_144(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_macro(Stack),
 yeccgoto_expression(hd(Nss), macro, Nss, NewStack, T, Ts, Tzr);
yeccpars2_144(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_module_keyword(Stack),
 yeccgoto_expression(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_144(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_newlines(Stack),
 yeccgoto_expression(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_144(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_square_close(Stack),
 yeccgoto_expression(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_144(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_type(Stack),
 yeccgoto_expression(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_(Stack),
 yeccgoto_noun(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_145: see yeccpars2_106

yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_147(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_148_(Stack),
 yeccgoto_secondary_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_149_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_150(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_150_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_151(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), space_curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), space_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), space_square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_151_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_152_(Stack),
 yeccgoto_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_153_(Stack),
 'yeccgoto_\'fun\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_154/7}).
yeccpars2_154(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_155(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_155_\'$end\''(Stack),
 yeccgoto_expression(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_close(Stack),
 yeccgoto_expression(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_comma(Stack),
 yeccgoto_expression(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_def(Stack),
 yeccgoto_expression(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_import_keyword(Stack),
 yeccgoto_expression(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_macro(Stack),
 yeccgoto_expression(hd(Nss), macro, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_module_keyword(Stack),
 yeccgoto_expression(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_newlines(Stack),
 yeccgoto_expression(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_square_close(Stack),
 yeccgoto_expression(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_type(Stack),
 yeccgoto_expression(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_156/7}).
yeccpars2_156(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_157_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_158: see yeccpars2_43

yeccpars2_159(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), space_curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), space_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), space_square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_159_(Stack),
 yeccgoto_expressions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_160_(Stack),
 yeccgoto_expressions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_161_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_162(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_163(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_164: see yeccpars2_162

yeccpars2_165(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_166(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_167(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_167_(Stack),
 yeccgoto_rightbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_168(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_169(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_(Stack),
 yeccgoto_rightbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_171: see yeccpars2_43

%% yeccpars2_172: see yeccpars2_116

yeccpars2_173(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_(Stack),
 yeccgoto_noun(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_175/7}).
yeccpars2_175(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_176(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_176_\'$end\''(Stack),
 yeccgoto_noun(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_close(Stack),
 yeccgoto_noun(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_colon(Stack),
 yeccgoto_noun(hd(Nss), colon, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_comma(Stack),
 yeccgoto_noun(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_curly_close(Stack),
 yeccgoto_noun(hd(Nss), curly_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_def(Stack),
 yeccgoto_noun(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_import_keyword(Stack),
 yeccgoto_noun(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_macro(Stack),
 yeccgoto_noun(hd(Nss), macro, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_module_keyword(Stack),
 yeccgoto_noun(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_newlines(Stack),
 yeccgoto_noun(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_pipe(Stack),
 yeccgoto_noun(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_square_close(Stack),
 yeccgoto_noun(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_type(Stack),
 yeccgoto_noun(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_177: see yeccpars2_162

yeccpars2_178(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_178_(Stack),
 yeccgoto_leftbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_179(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_179_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_180(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_183: see yeccpars2_43

-dialyzer({nowarn_function, yeccpars2_184/7}).
yeccpars2_184(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_185_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_186_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_187(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_188/7}).
yeccpars2_188(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_189_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_190: see yeccpars2_43

-dialyzer({nowarn_function, yeccpars2_191/7}).
yeccpars2_191(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_192_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_193_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_194(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_(Stack),
 yeccgoto_leftbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_195(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_196(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_198(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_199_(Stack),
 yeccgoto_pair(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_200(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_201(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_202(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_203: see yeccpars2_53

%% yeccpars2_204: see yeccpars2_76

yeccpars2_205(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 206, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_206(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_206_\'$end\''(Stack),
 yeccgoto_pair_val(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_close(Stack),
 yeccgoto_pair_val(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_comma(Stack),
 yeccgoto_pair_val(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_curly_close(Stack),
 yeccgoto_pair_val(hd(Nss), curly_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_def(Stack),
 yeccgoto_pair_val(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_import_keyword(Stack),
 yeccgoto_pair_val(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_macro(Stack),
 yeccgoto_pair_val(hd(Nss), macro, Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_module_keyword(Stack),
 yeccgoto_pair_val(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_newlines(Stack),
 yeccgoto_pair_val(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_pipe(Stack),
 yeccgoto_pair_val(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_square_close(Stack),
 yeccgoto_pair_val(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_type(Stack),
 yeccgoto_pair_val(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_(Stack),
 yeccgoto_noun(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_207/7}).
yeccpars2_207(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_208(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_208_\'$end\''(Stack),
 yeccgoto_pair_val(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_close(Stack),
 yeccgoto_pair_val(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_comma(Stack),
 yeccgoto_pair_val(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_curly_close(Stack),
 yeccgoto_pair_val(hd(Nss), curly_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_def(Stack),
 yeccgoto_pair_val(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_import_keyword(Stack),
 yeccgoto_pair_val(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_macro(Stack),
 yeccgoto_pair_val(hd(Nss), macro, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_module_keyword(Stack),
 yeccgoto_pair_val(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_newlines(Stack),
 yeccgoto_pair_val(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_pipe(Stack),
 yeccgoto_pair_val(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_square_close(Stack),
 yeccgoto_pair_val(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_type(Stack),
 yeccgoto_pair_val(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_209/7}).
yeccpars2_209(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_210: see yeccpars2_43

yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_211_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_212/7}).
yeccpars2_212(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_213_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_214_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_215(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_216(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_217(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_219(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_220(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_221(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_222/7}).
yeccpars2_222(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 225, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_223(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_224: see yeccpars2_56

yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_225_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_226(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_227(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dict_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_229(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dict_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_230(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_231/7}).
yeccpars2_231(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_231(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_232(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_232_(Stack),
 yeccgoto_dict_elements(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_233(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_234(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_235_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_236: see yeccpars2_162

-dialyzer({nowarn_function, yeccpars2_237/7}).
yeccpars2_237(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 238, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_238_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_239: see yeccpars2_162

yeccpars2_240(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), space_curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, space_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), space_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, space_square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), space_square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_240_(Stack),
 yeccgoto_dict_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_dict_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_242_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_243_(Stack),
 yeccgoto_implies(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_244_(Stack),
 yeccgoto_clause_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_245/7}).
yeccpars2_245(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_246_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_247: see yeccpars2_162

-dialyzer({nowarn_function, yeccpars2_248/7}).
yeccpars2_248(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr);
yeccpars2_248(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_249_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_250_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_251: see yeccpars2_48

yeccpars2_252(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_252_(Stack),
 yeccgoto_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_253_(Stack),
 yeccgoto_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_254_(Stack),
 yeccgoto_def_fun(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_255_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_256: see yeccpars2_163

yeccpars2_257(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_258(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_259_(Stack),
 yeccgoto_pattern_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_260_(Stack),
 yeccgoto_pattern_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_261_(Stack),
 yeccgoto_patterns(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_262_(Stack),
 yeccgoto_pattern_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_263: see yeccpars2_43

yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_264_(Stack),
 yeccgoto_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_265_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_266_(Stack),
 yeccgoto_all(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_267/7}).
yeccpars2_267(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_267(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_268/7}).
yeccpars2_268(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_269(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(S, space_curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_269_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_270_(Stack),
 yeccgoto_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_271_(Stack),
 yeccgoto_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_272: see yeccpars2_15

yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_273_(Stack),
 yeccgoto_newmacro(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_274(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_274_(Stack),
 yeccgoto_import(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_275(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_275_(Stack),
 yeccgoto_import(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_276: see yeccpars2_15

%% yeccpars2_277: see yeccpars2_43

yeccpars2_278(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_278_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_279_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_280_(Stack),
 yeccgoto_statements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_281_(Stack),
 yeccgoto_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_all/7}).
yeccgoto_all(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_application/7}).
yeccgoto_application(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_arguments/7}).
yeccgoto_arguments(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arguments(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arguments(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arguments(259=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_assignment/7}).
yeccgoto_assignment(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_braced_pattern/7}).
yeccgoto_braced_pattern(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(222, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_braced_pattern(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(222, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clause/7}).
yeccgoto_clause(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(272, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clause_separator/7}).
yeccgoto_clause_separator(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(251, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(272, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clauses/7}).
yeccgoto_clauses(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clauses(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clauses(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clauses(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clauses(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clauses(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_collection/7}).
yeccgoto_collection(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_def_fun/7}).
yeccgoto_def_fun(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_def_fun(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_def_fun(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict/7}).
yeccgoto_dict(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(126=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict_element/7}).
yeccgoto_dict_element(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_element(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_element(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_element(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_element(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict_elements/7}).
yeccgoto_dict_elements(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(245, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_elements(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(231, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_elements(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(237, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_elements(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_elements(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(248, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_element/7}).
yeccgoto_element(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_elements/7}).
yeccgoto_elements(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expression/7}).
yeccgoto_expression(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(175, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(105, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(175, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(184, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(207, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expressions/7}).
yeccgoto_expressions(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(209, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(191, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'fun\''/7}).
'yeccgoto_\'fun\''(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_function/7}).
yeccgoto_function(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_implies/7}).
yeccgoto_implies(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(263, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(135, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(272, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(135, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(277, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_import/7}).
yeccgoto_import(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_infix/7}).
yeccgoto_infix(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(169, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(177, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(263, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(272, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(277, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_leftbias_infix/7}).
yeccgoto_leftbias_infix(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_list/7}).
yeccgoto_list(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_literal/7}).
yeccgoto_literal(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_module/7}).
yeccgoto_module(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_module(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_module(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_newmacro/7}).
yeccgoto_newmacro(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newmacro(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newmacro(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_newtype/7}).
yeccgoto_newtype(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_noun/7}).
yeccgoto_noun(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(194, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(167, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(177, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(263, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(272, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(277, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operator/7}).
yeccgoto_operator(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(162, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(177, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(162, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(94=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(126=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(162, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(177, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(162, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(162, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(229, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(162, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair/7}).
yeccgoto_pair(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair_key/7}).
yeccgoto_pair_key(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(263, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(277, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair_val/7}).
yeccgoto_pair_val(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern/7}).
yeccgoto_pattern(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(99, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(272, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_application/7}).
yeccgoto_pattern_application(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_verb/7}).
yeccgoto_pattern_verb(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_verb(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_verb(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_verb(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_verb(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_verb(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_verb(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_verb(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_verb(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_verb(272, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_verb(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(35, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_patterns/7}).
yeccgoto_patterns(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(272, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_qualified_symbol/7}).
yeccgoto_qualified_symbol(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(268, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(126=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_rightbias_infix/7}).
yeccgoto_rightbias_infix(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_secondary_separator/7}).
yeccgoto_secondary_separator(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(145, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondary_separator(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(145, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_separator/7}).
yeccgoto_separator(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(89, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(137, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(142, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(137, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(239, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sequence/7}).
yeccgoto_sequence(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_statement/7}).
yeccgoto_statement(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(13, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_statements/7}).
yeccgoto_statements(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statements(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statements(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_elem/7}).
yeccgoto_sum_elem(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_list/7}).
yeccgoto_sum_list(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(100=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(210=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_terms/7}).
yeccgoto_sum_terms(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(108, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_symbol/7}).
yeccgoto_symbol(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(276, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(274, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(272, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(267, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(226, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(226, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(215, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(94, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(126, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(127, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(163, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(180, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(177, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(196, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(215, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(226, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(226, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(226, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(257, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(263, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(272, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(277, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_verb/7}).
yeccgoto_verb(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(163, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(177, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(263, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(272, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(277, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_2_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 62).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 93).
yeccpars2_17_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 98).
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
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 99).
yeccpars2_20_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 97).
yeccpars2_21_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 94).
yeccpars2_22_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 100).
yeccpars2_23_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 96).
yeccpars2_24_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 91).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 88).
yeccpars2_26_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 89).
yeccpars2_27_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 225).
yeccpars2_37_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 78).
yeccpars2_45_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type_def , ctx ( __1 ) , unwrap ( __2 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 216).
yeccpars2_47_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { 'fun' , ctx ( __1 ) , __1 }
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 213).
yeccpars2_49_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 137).
yeccpars2_70_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_77_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 257).
yeccpars2_77_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_93_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 287).
yeccpars2_93_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 75).
yeccpars2_101_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { val , ctx ( __1 ) , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_103_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_103_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_103_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_103_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_103_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_103_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_103_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_103_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_103_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_103_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_103_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
yeccpars2_103_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
yeccpars2_103_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 315).
yeccpars2_103_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_121_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 289).
yeccpars2_121_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_122_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_122_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_122_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_122_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_122_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_122_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_122_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_122_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_122_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_122_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_122_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_122_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_122_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_122_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_122_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_122_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_122_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_122_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_122_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_122_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_122_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 315).
yeccpars2_122_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_124_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_124_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_124_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_124_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_124_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_124_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_124_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_124_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_124_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_124_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 314).
yeccpars2_124_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 314).
yeccpars2_124_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 314).
yeccpars2_124_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_124_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 162).
yeccpars2_124_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_125_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 308).
yeccpars2_125_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sum , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_127_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 105).
yeccpars2_127_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_128_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 107).
yeccpars2_128_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 | unwrap ( __3 ) ] }
  end | __Stack].

-compile({inline,yeccpars2_129_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 106).
yeccpars2_129_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_130_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_130_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_130_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_130_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_130_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_130_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_130_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_130_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_130_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_130_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_130_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
yeccpars2_130_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
yeccpars2_130_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 314).
yeccpars2_130_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 314).
yeccpars2_130_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_130_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 162).
yeccpars2_130_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 306).
yeccpars2_131_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sum , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_133_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 307).
yeccpars2_133_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sum , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_136_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 81).
yeccpars2_136_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , ctx ( __1 ) , unwrap ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_139_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 293).
yeccpars2_139_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_140_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 279).
yeccpars2_140_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_141_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 292).
yeccpars2_141_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 291).
yeccpars2_142_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_143_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 288).
yeccpars2_143_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,'yeccpars2_144_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
'yeccpars2_144_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_144_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
yeccpars2_144_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_144_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
yeccpars2_144_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_144_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
yeccpars2_144_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_144_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
yeccpars2_144_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_144_macro/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
yeccpars2_144_macro(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_144_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
yeccpars2_144_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_144_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
yeccpars2_144_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_144_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
yeccpars2_144_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_144_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 126).
yeccpars2_144_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_144_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_144_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_148_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 325).
yeccpars2_148_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_149_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 312).
yeccpars2_149_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_150_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 310).
yeccpars2_150_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 311).
yeccpars2_151_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_152_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 305).
yeccpars2_152_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sum , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_153_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 218).
yeccpars2_153_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { 'fun' , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,'yeccpars2_155_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
'yeccpars2_155_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
yeccpars2_155_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
yeccpars2_155_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
yeccpars2_155_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
yeccpars2_155_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_macro/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
yeccpars2_155_macro(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
yeccpars2_155_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
yeccpars2_155_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
yeccpars2_155_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 125).
yeccpars2_155_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_155_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_155_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_155_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_155_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_155_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_155_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_155_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_155_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_155_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_155_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 162).
yeccpars2_155_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 259).
yeccpars2_157_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_159_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 138).
yeccpars2_159_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_160_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 139).
yeccpars2_160_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_161_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 258).
yeccpars2_161_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_167_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 180).
yeccpars2_167_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , make_symbol ( __2 , operator ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_169_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 181).
yeccpars2_169_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , make_symbol ( __2 , operator ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_174_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_174_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,'yeccpars2_176_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
'yeccpars2_176_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_colon/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_colon(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_curly_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_curly_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_macro/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_macro(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_176_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 162).
yeccpars2_176_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_178_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 179).
yeccpars2_178_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , __2 , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_179_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 168).
yeccpars2_179_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 ] }
  end | __Stack].

-compile({inline,yeccpars2_185_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 162).
yeccpars2_185_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_186_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 169).
yeccpars2_186_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 | __4 ] }
  end | __Stack].

-compile({inline,yeccpars2_189_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 172).
yeccpars2_189_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,yeccpars2_192_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 174).
yeccpars2_192_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,yeccpars2_193_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 173).
yeccpars2_193_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_194_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 178).
yeccpars2_194_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , __2 , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_199_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 189).
yeccpars2_199_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { pair , ctx ( __1 ) , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_206_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
'yeccpars2_206_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_206_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_206_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_curly_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_206_curly_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_206_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_206_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_macro/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_206_macro(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_206_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_206_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_206_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_206_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 204).
yeccpars2_206_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_206_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 153).
yeccpars2_206_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,'yeccpars2_208_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
'yeccpars2_208_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
yeccpars2_208_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
yeccpars2_208_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_curly_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
yeccpars2_208_curly_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
yeccpars2_208_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
yeccpars2_208_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_macro/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
yeccpars2_208_macro(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
yeccpars2_208_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
yeccpars2_208_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
yeccpars2_208_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
yeccpars2_208_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 203).
yeccpars2_208_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_208_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_208_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_208_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_208_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_208_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_208_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_208_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_208_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_208_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 152).
yeccpars2_208_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_208_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 162).
yeccpars2_208_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_211_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 264).
yeccpars2_211_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_213_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 266).
yeccpars2_213_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_214_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 265).
yeccpars2_214_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_225_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 230).
yeccpars2_225_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_232_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 274).
yeccpars2_232_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_235_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 267).
yeccpars2_235_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_238_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 269).
yeccpars2_238_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_240_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 275).
yeccpars2_240_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_241_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 276).
yeccpars2_241_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_242_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 268).
yeccpars2_242_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_243_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 73).
yeccpars2_243_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_244_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 211).
yeccpars2_244_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_246_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 260).
yeccpars2_246_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_249_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 262).
yeccpars2_249_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_250_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 261).
yeccpars2_250_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_252_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 214).
yeccpars2_252_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_253_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 215).
yeccpars2_253_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_254_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 217).
yeccpars2_254_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { 'fun' , ctx ( __2 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_255_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 79).
yeccpars2_255_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type_def , ctx ( __1 ) , unwrap ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_259_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 241).
yeccpars2_259_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 ] }
  end | __Stack].

-compile({inline,yeccpars2_260_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 242).
yeccpars2_260_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 | __4 ] }
  end | __Stack].

-compile({inline,yeccpars2_261_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 226).
yeccpars2_261_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_262_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 240).
yeccpars2_262_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_264_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 212).
yeccpars2_264_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { clause , ctx ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_265_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 167).
yeccpars2_265_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_266_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 53).
yeccpars2_266_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_269_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 89).
yeccpars2_269_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_270_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 114).
yeccpars2_270_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { module , ctx ( __1 ) , [ make_symbol ( __2 ) ] , unwrap ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_271_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 115).
yeccpars2_271_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { module , ctx ( __1 ) , unwrap ( __2 ) , unwrap ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_273_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 80).
yeccpars2_273_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , ctx ( __1 ) , unwrap ( __2 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_274_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 117).
yeccpars2_274_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { import , ctx ( __1 ) , [ __2 ] }
  end | __Stack].

-compile({inline,yeccpars2_275_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 118).
yeccpars2_275_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { import , ctx ( __1 ) , unwrap ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_278_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 76).
yeccpars2_278_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { def , ctx ( __1 ) , unwrap ( __2 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_279_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 77).
yeccpars2_279_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { def , ctx ( __1 ) , unwrap ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_280_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 63).
yeccpars2_280_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_281_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 65).
yeccpars2_281_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].


-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 351).
