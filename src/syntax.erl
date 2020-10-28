-module(syntax).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 319).

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
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 232, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, right_arrow, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_29(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_infix(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_34/7}).
yeccpars2_34(S, right_arrow, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_patterns(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_37(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_collection(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_infix(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_application(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_collection(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_newtype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_45(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_def_fun(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_47(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_48(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_51/7}).
yeccpars2_51(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_52(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_53(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_implies(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_literal(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_60/7}).
yeccpars2_60(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_62(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

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

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_65/7}).
yeccpars2_65(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_66(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 yeccgoto_expressions(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

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
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_68(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
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

%% yeccpars2_69: see yeccpars2_42

yeccpars2_70(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_71: see yeccpars2_47

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_72_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 'yeccgoto_\'fun\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_74(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_75/7}).
yeccpars2_75(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_76/7}).
yeccpars2_76(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_77(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_84/7}).
yeccpars2_84(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_85/7}).
yeccpars2_85(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
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

%% yeccpars2_90: see yeccpars2_9

yeccpars2_91(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_92: see yeccpars2_70

%% yeccpars2_93: see yeccpars2_47

-dialyzer({nowarn_function, yeccpars2_94/7}).
yeccpars2_94(S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_95: see yeccpars2_42

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_assignment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_97(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_98(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_close(Stack),
 yeccgoto_expression(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_comma(Stack),
 yeccgoto_expression(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_newlines(Stack),
 yeccgoto_sum_elem(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_pipe(Stack),
 yeccgoto_sum_elem(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_99/7}).
yeccpars2_99(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_101/7}).
yeccpars2_101(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_sequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_104: see yeccpars2_34

%% yeccpars2_105: see yeccpars2_42

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_107(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_108(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_109(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), val, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_109_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_110_(Stack),
 yeccgoto_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_112_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_sequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_114(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_114_\'$end\''(Stack),
 yeccgoto_expression(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_close(Stack),
 yeccgoto_expression(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_comma(Stack),
 yeccgoto_expression(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_def(Stack),
 yeccgoto_expression(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_import_keyword(Stack),
 yeccgoto_expression(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_macro(Stack),
 yeccgoto_expression(hd(Nss), macro, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_module_keyword(Stack),
 yeccgoto_expression(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_newlines(Stack),
 yeccgoto_expression(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_square_close(Stack),
 yeccgoto_expression(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_type(Stack),
 yeccgoto_expression(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_115(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_117(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_118_(Stack),
 yeccgoto_secondary_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_119(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_119(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_120_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_121(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_121_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_122(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_123(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_125(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_127(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_128(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_129(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_130: see yeccpars2_107

yeccpars2_131(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_close(Stack),
 yeccgoto_sum_elem(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_newlines(Stack),
 yeccgoto_sum_elem(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_pipe(Stack),
 yeccgoto_sum_elem(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_132(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_132_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_133(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_134(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_(Stack),
 yeccgoto_qualified_symbol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_(Stack),
 yeccgoto_qualified_symbol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_qualified_symbol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_137_(Stack),
 yeccgoto_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_138/7}).
yeccpars2_138(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_139_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_140: see yeccpars2_42

yeccpars2_141(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_141_(Stack),
 yeccgoto_expressions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_expressions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_143_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_144(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_145(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_146: see yeccpars2_144

yeccpars2_147(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_148(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_149(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_149_(Stack),
 yeccgoto_rightbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_150(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_151(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_151_(Stack),
 yeccgoto_rightbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_153: see yeccpars2_128

yeccpars2_154(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_155(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_155_\'$end\''(Stack),
 yeccgoto_noun(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_close(Stack),
 yeccgoto_noun(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_colon(Stack),
 yeccgoto_noun(hd(Nss), colon, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_comma(Stack),
 yeccgoto_noun(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_curly_close(Stack),
 yeccgoto_noun(hd(Nss), curly_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_def(Stack),
 yeccgoto_noun(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_import_keyword(Stack),
 yeccgoto_noun(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_macro(Stack),
 yeccgoto_noun(hd(Nss), macro, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_module_keyword(Stack),
 yeccgoto_noun(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_newlines(Stack),
 yeccgoto_noun(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_pipe(Stack),
 yeccgoto_noun(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_square_close(Stack),
 yeccgoto_noun(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_type(Stack),
 yeccgoto_noun(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_156: see yeccpars2_144

yeccpars2_157(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_157_(Stack),
 yeccgoto_leftbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_158(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_158_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_159(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_162: see yeccpars2_42

-dialyzer({nowarn_function, yeccpars2_163/7}).
yeccpars2_163(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_164_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_165_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_166(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_167/7}).
yeccpars2_167(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_168_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_169: see yeccpars2_42

-dialyzer({nowarn_function, yeccpars2_170/7}).
yeccpars2_170(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_171_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_173(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_(Stack),
 yeccgoto_leftbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_174(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_175(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_177(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_178_(Stack),
 yeccgoto_pair(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_179(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_180(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_181(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_182: see yeccpars2_70

yeccpars2_183(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_184(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_184_\'$end\''(Stack),
 yeccgoto_pair_val(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_close(Stack),
 yeccgoto_pair_val(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_comma(Stack),
 yeccgoto_pair_val(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_curly_close(Stack),
 yeccgoto_pair_val(hd(Nss), curly_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_def(Stack),
 yeccgoto_pair_val(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_import_keyword(Stack),
 yeccgoto_pair_val(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_macro(Stack),
 yeccgoto_pair_val(hd(Nss), macro, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_module_keyword(Stack),
 yeccgoto_pair_val(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_newlines(Stack),
 yeccgoto_pair_val(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_pipe(Stack),
 yeccgoto_pair_val(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_square_close(Stack),
 yeccgoto_pair_val(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_type(Stack),
 yeccgoto_pair_val(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_185_(Stack),
 yeccgoto_implies(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_186(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_187(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_188(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_190(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_191(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_192(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_193/7}).
yeccpars2_193(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_194(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_195: see yeccpars2_52

yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_196_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_197_(Stack),
 yeccgoto_clause_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_198(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_199(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dict_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_201(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dict_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_202(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_203/7}).
yeccpars2_203(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_204(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_204_(Stack),
 yeccgoto_dict_elements(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_205(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_206(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_207_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_208: see yeccpars2_144

-dialyzer({nowarn_function, yeccpars2_209/7}).
yeccpars2_209(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_211: see yeccpars2_144

yeccpars2_212(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_212_(Stack),
 yeccgoto_dict_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_213_(Stack),
 yeccgoto_dict_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_214_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_215: see yeccpars2_47

yeccpars2_216(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_216_(Stack),
 yeccgoto_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_217_(Stack),
 yeccgoto_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_218_(Stack),
 yeccgoto_def_fun(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_219_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_220(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 223, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_221(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_(Stack),
 yeccgoto_pattern_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_223(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_223_apply(Stack),
 yeccgoto_symbol(hd(Ss), apply, Ss, NewStack, T, Ts, Tzr);
yeccpars2_223(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_223_colon(Stack),
 yeccgoto_symbol(hd(Ss), colon, Ss, NewStack, T, Ts, Tzr);
yeccpars2_223(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_223_comma(Stack),
 yeccgoto_symbol(hd(Ss), comma, Ss, NewStack, T, Ts, Tzr);
yeccpars2_223(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_223_newlines(Stack),
 yeccgoto_symbol(hd(Ss), newlines, Ss, NewStack, T, Ts, Tzr);
yeccpars2_223(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_223_pipe(Stack),
 yeccgoto_symbol(hd(Ss), pipe, Ss, NewStack, T, Ts, Tzr);
yeccpars2_223(_S, slash, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_223_slash(Stack),
 yeccgoto_symbol(hd(Ss), slash, Ss, NewStack, T, Ts, Tzr);
yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_223_(Stack),
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_pattern_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_225_(Stack),
 yeccgoto_patterns(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_226: see yeccpars2_42

yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_228_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_229_(Stack),
 yeccgoto_all(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_230/7}).
yeccpars2_230(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_231/7}).
yeccpars2_231(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_231(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_232(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_232_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_233_(Stack),
 yeccgoto_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_234_(Stack),
 yeccgoto_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_235: see yeccpars2_15

yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_236_(Stack),
 yeccgoto_newmacro(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_237(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_237_(Stack),
 yeccgoto_import(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_238_(Stack),
 yeccgoto_import(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_239: see yeccpars2_15

%% yeccpars2_240: see yeccpars2_42

yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_242_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_243_(Stack),
 yeccgoto_statements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_244_(Stack),
 yeccgoto_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_all/7}).
yeccgoto_all(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_application/7}).
yeccgoto_application(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_arguments/7}).
yeccgoto_arguments(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arguments(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arguments(222=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_assignment/7}).
yeccgoto_assignment(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_braced_pattern/7}).
yeccgoto_braced_pattern(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(193, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_braced_pattern(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(193, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clause/7}).
yeccgoto_clause(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clause_separator/7}).
yeccgoto_clause_separator(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(215, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clauses/7}).
yeccgoto_clauses(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clauses(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clauses(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clauses(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clauses(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clauses(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_collection/7}).
yeccgoto_collection(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_def_fun/7}).
yeccgoto_def_fun(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_def_fun(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_def_fun(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict/7}).
yeccgoto_dict(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict_element/7}).
yeccgoto_dict_element(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(204, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_element(208, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(204, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_element(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(204, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict_elements/7}).
yeccgoto_dict_elements(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(203, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_elements(208, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(209, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_elements(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_element/7}).
yeccgoto_element(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_elements/7}).
yeccgoto_elements(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(101, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(101, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expression/7}).
yeccgoto_expression(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(163, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(183, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expressions/7}).
yeccgoto_expressions(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(167, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(170, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'fun\''/7}).
'yeccgoto_\'fun\''(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'fun\''(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_function/7}).
yeccgoto_function(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_implies/7}).
yeccgoto_implies(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(226, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(105, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(105, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(240, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_import/7}).
yeccgoto_import(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_infix/7}).
yeccgoto_infix(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(208, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_leftbias_infix/7}).
yeccgoto_leftbias_infix(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_list/7}).
yeccgoto_list(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_literal/7}).
yeccgoto_literal(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr).

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
yeccgoto_newtype(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_noun/7}).
yeccgoto_noun(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(201, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(157, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(208, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(201, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(201, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr).

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
yeccgoto_operator(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(156, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair/7}).
yeccgoto_pair(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair_key/7}).
yeccgoto_pair_key(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(208, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair_val/7}).
yeccgoto_pair_val(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern/7}).
yeccgoto_pattern(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_application/7}).
yeccgoto_pattern_application(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_verb/7}).
yeccgoto_pattern_verb(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_patterns/7}).
yeccgoto_patterns(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_qualified_symbol/7}).
yeccgoto_qualified_symbol(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(231, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_rightbias_infix/7}).
yeccgoto_rightbias_infix(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_secondary_separator/7}).
yeccgoto_secondary_separator(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondary_separator(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_separator/7}).
yeccgoto_separator(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(211, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sequence/7}).
yeccgoto_sequence(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr).

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
 yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statements(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_elem/7}).
yeccgoto_sum_elem(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(121, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_list/7}).
yeccgoto_sum_list(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(70=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(235=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_terms/7}).
yeccgoto_sum_terms(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(99, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_symbol/7}).
yeccgoto_symbol(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(239, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(237, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(235, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(230, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(186, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(133, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(159, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(175, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(186, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(208, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(159, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_verb/7}).
yeccgoto_verb(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(195, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(208, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(158, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_2_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 61).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 92).
yeccpars2_17_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 97).
yeccpars2_18_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 94).
yeccpars2_19_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 98).
yeccpars2_20_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 96).
yeccpars2_21_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 93).
yeccpars2_22_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 99).
yeccpars2_23_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 95).
yeccpars2_24_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 90).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_26_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 88).
yeccpars2_27_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 221).
yeccpars2_36_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 77).
yeccpars2_44_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type_def , ctx ( __1 ) , unwrap ( __2 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 212).
yeccpars2_46_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { 'fun' , ctx ( __1 ) , __1 }
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 209).
yeccpars2_48_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_66_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 135).
yeccpars2_66_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_72_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 252).
yeccpars2_72_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 214).
yeccpars2_73_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { 'fun' , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_89_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 275).
yeccpars2_89_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 74).
yeccpars2_96_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { val , ctx ( __1 ) , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_98_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_98_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_98_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_98_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_98_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_98_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_98_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_98_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_98_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_98_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_98_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_98_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_98_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 300).
yeccpars2_98_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 300).
yeccpars2_98_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 159).
yeccpars2_98_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 277).
yeccpars2_102_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 294).
yeccpars2_103_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sum , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 80).
yeccpars2_106_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , ctx ( __1 ) , unwrap ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 281).
yeccpars2_109_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_110_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 267).
yeccpars2_110_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 280).
yeccpars2_111_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_112_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 279).
yeccpars2_112_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 276).
yeccpars2_113_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,'yeccpars2_114_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
'yeccpars2_114_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_114_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_114_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_114_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_114_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_macro/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_114_macro(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_114_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_114_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_114_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_114_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_114_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_114_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_114_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_114_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_114_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_114_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_114_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_114_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_114_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_114_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 159).
yeccpars2_114_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_118_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 310).
yeccpars2_118_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_120_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 298).
yeccpars2_120_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_121_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 296).
yeccpars2_121_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_131_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_131_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_131_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_131_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_131_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_131_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_131_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_131_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_131_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_131_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_131_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 300).
yeccpars2_131_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 300).
yeccpars2_131_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 300).
yeccpars2_131_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_131_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 159).
yeccpars2_131_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_132_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 297).
yeccpars2_132_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_134_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 104).
yeccpars2_134_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_135_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 106).
yeccpars2_135_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 | unwrap ( __3 ) ] }
  end | __Stack].

-compile({inline,yeccpars2_136_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 105).
yeccpars2_136_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_137_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 293).
yeccpars2_137_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sum , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_139_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 254).
yeccpars2_139_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_141_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 136).
yeccpars2_141_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 137).
yeccpars2_142_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_143_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 253).
yeccpars2_143_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_149_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 177).
yeccpars2_149_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , make_symbol ( __2 , operator ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 178).
yeccpars2_151_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , make_symbol ( __2 , operator ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,'yeccpars2_155_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
'yeccpars2_155_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_colon/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_colon(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_curly_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_curly_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_macro/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_macro(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_155_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 159).
yeccpars2_155_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 176).
yeccpars2_157_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , __2 , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 165).
yeccpars2_158_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 ] }
  end | __Stack].

-compile({inline,yeccpars2_164_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 159).
yeccpars2_164_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 166).
yeccpars2_165_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 | __4 ] }
  end | __Stack].

-compile({inline,yeccpars2_168_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 169).
yeccpars2_168_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,yeccpars2_171_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 171).
yeccpars2_171_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,yeccpars2_172_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 170).
yeccpars2_172_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_173_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 175).
yeccpars2_173_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , __2 , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_178_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 186).
yeccpars2_178_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { pair , ctx ( __1 ) , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_184_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
'yeccpars2_184_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_184_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_184_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_curly_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_184_curly_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_184_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_184_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_macro/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_184_macro(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_184_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_184_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_184_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_184_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_184_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_184_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_184_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_184_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_184_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_184_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_184_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_184_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_184_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_184_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_184_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_184_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 159).
yeccpars2_184_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_185_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 72).
yeccpars2_185_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_196_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 224).
yeccpars2_196_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_197_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 207).
yeccpars2_197_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_204_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 262).
yeccpars2_204_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_207_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 255).
yeccpars2_207_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_210_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 257).
yeccpars2_210_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_212_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 263).
yeccpars2_212_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_213_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 264).
yeccpars2_213_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_214_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 256).
yeccpars2_214_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_216_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 210).
yeccpars2_216_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_217_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 211).
yeccpars2_217_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_218_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 213).
yeccpars2_218_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { 'fun' , ctx ( __2 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_219_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 78).
yeccpars2_219_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type_def , ctx ( __1 ) , unwrap ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_222_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 236).
yeccpars2_222_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 ] }
  end | __Stack].

-compile({inline,yeccpars2_223_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_223_apply(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_223_colon/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_223_colon(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_223_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_223_comma(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_223_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_223_newlines(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_223_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_223_pipe(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_223_slash/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_223_slash(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_223_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 240).
yeccpars2_223_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_224_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 237).
yeccpars2_224_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 | __4 ] }
  end | __Stack].

-compile({inline,yeccpars2_225_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 222).
yeccpars2_225_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_227_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 208).
yeccpars2_227_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { clause , ctx ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_228_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 164).
yeccpars2_228_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_229_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 52).
yeccpars2_229_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_232_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 88).
yeccpars2_232_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_233_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 113).
yeccpars2_233_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { module , ctx ( __1 ) , [ make_symbol ( __2 ) ] , unwrap ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_234_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 114).
yeccpars2_234_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { module , ctx ( __1 ) , unwrap ( __2 ) , unwrap ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_236_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 79).
yeccpars2_236_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , ctx ( __1 ) , unwrap ( __2 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_237_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 116).
yeccpars2_237_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { import , ctx ( __1 ) , [ __2 ] }
  end | __Stack].

-compile({inline,yeccpars2_238_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 117).
yeccpars2_238_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { import , ctx ( __1 ) , unwrap ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_241_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 75).
yeccpars2_241_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { def , ctx ( __1 ) , unwrap ( __2 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_242_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 76).
yeccpars2_242_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { def , ctx ( __1 ) , unwrap ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_243_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 62).
yeccpars2_243_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_244_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 64).
yeccpars2_244_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].


-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 336).
