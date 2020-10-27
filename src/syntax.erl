-module(syntax).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 331).

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



-file("/Users/arnfred/workspace/kind/src/syntax.erl", 196).

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
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_9/7}).
yeccpars2_cont_9(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_9(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_10: see yeccpars2_9

%% yeccpars2_11: see yeccpars2_9

yeccpars2_12(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 227, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_14: see yeccpars2_9

-dialyzer({nowarn_function, yeccpars2_15/7}).
yeccpars2_15(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, right_arrow, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_16(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_symbols(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_symbol(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
 yeccgoto_operator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_symbols(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_30(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
yeccpars2_32(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_33(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_implies(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_implies(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_clause_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_36/7}).
yeccpars2_36(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_newtype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_38(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_type_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_infix(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_44/7}).
yeccpars2_44(S, right_arrow, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_patterns(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_47(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 205, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_48(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_collection(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_infix(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_application(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_collection(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_53(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_55(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_literal(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
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

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_61(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_62(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_62(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_63/7}).
yeccpars2_63(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_66(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_68/7}).
yeccpars2_68(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_69(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 yeccgoto_expressions(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_71(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
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

yeccpars2_72(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_74: see yeccpars2_31

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_76_(Stack),
 yeccgoto_lambda(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_77(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_77(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_78/7}).
yeccpars2_78(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_79/7}).
yeccpars2_79(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_80(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_81(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_83(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_85(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_86/7}).
yeccpars2_86(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_87/7}).
yeccpars2_87(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_, _, _, _, T, _, _) ->
 yeccerror(T).

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
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
 [_|Nss] = Ss,
 NewStack = yeccpars2_91_(Stack),
 yeccgoto_sequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_92(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_93: see yeccpars2_72

-dialyzer({nowarn_function, yeccpars2_94/7}).
yeccpars2_94(S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_95: see yeccpars2_72

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_assignment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_97/7}).
yeccpars2_97(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_99/7}).
yeccpars2_99(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_sequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_102: see yeccpars2_92

yeccpars2_103(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_104(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), val, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_104_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_107(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_108_(Stack),
 yeccgoto_sequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_109_\'$end\''(Stack),
 yeccgoto_expression(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_assign(Stack),
 yeccgoto_expression(hd(Nss), assign, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_close(Stack),
 yeccgoto_expression(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_comma(Stack),
 yeccgoto_expression(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_def(Stack),
 yeccgoto_expression(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_import_keyword(Stack),
 yeccgoto_expression(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_macro(Stack),
 yeccgoto_expression(hd(Nss), macro, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_module_keyword(Stack),
 yeccgoto_expression(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_newlines(Stack),
 yeccgoto_expression(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_square_close(Stack),
 yeccgoto_expression(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_type(Stack),
 yeccgoto_expression(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_110(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_111(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_112(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_secondary_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_secondary_separator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_115(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_116_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_117(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_117_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

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
yeccpars2_118(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
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

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
yeccpars2_120(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_120(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
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
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_123(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, val, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_124(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_125: see yeccpars2_92

yeccpars2_126(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_126_\'$end\''(Stack),
 yeccgoto_noun(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_assign(Stack),
 yeccgoto_noun(hd(Nss), assign, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_close(Stack),
 yeccgoto_noun(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_colon(Stack),
 yeccgoto_noun(hd(Nss), colon, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_comma(Stack),
 yeccgoto_noun(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_curly_close(Stack),
 yeccgoto_noun(hd(Nss), curly_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_def(Stack),
 yeccgoto_noun(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_import_keyword(Stack),
 yeccgoto_noun(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_macro(Stack),
 yeccgoto_noun(hd(Nss), macro, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_module_keyword(Stack),
 yeccgoto_noun(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_newlines(Stack),
 yeccgoto_noun(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_pipe(Stack),
 yeccgoto_noun(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_square_close(Stack),
 yeccgoto_noun(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_type(Stack),
 yeccgoto_noun(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_127(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondary_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_127_(Stack),
 yeccgoto_sum_terms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_128(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_129(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_(Stack),
 yeccgoto_qualified_symbol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_(Stack),
 yeccgoto_qualified_symbol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_(Stack),
 yeccgoto_qualified_symbol(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_(Stack),
 yeccgoto_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_133/7}).
yeccpars2_133(S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_134_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_135: see yeccpars2_72

yeccpars2_136(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_expressions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_137_(Stack),
 yeccgoto_expressions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_138_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_139: see yeccpars2_110

yeccpars2_140(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_141: see yeccpars2_110

yeccpars2_142(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
yeccpars2_143(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
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

yeccpars2_144(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_(Stack),
 yeccgoto_rightbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_145(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_146(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_146_(Stack),
 yeccgoto_rightbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_148: see yeccpars2_110

yeccpars2_149(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_149_(Stack),
 yeccgoto_leftbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_150(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_150_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_151(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_154: see yeccpars2_72

-dialyzer({nowarn_function, yeccpars2_155/7}).
yeccpars2_155(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_156_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_157_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_158(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_159/7}).
yeccpars2_159(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_160_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_161: see yeccpars2_72

-dialyzer({nowarn_function, yeccpars2_162/7}).
yeccpars2_162(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_163_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_164_(Stack),
 yeccgoto_arguments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_165(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_165_(Stack),
 yeccgoto_leftbias_infix(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_166(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_167(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_167(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
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
 yeccgoto_pair_val(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
yeccpars2_169(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
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

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_170_(Stack),
 yeccgoto_pair(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

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
yeccpars2_171(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_171(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_172(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_173(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), assign, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, def, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), def, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), import_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), macro, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), module_keyword, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), square_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, type, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_val(hd(Ss), type, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_174: see yeccpars2_73

yeccpars2_175(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_176(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_176_\'$end\''(Stack),
 yeccgoto_pair_val(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, assign, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_assign(Stack),
 yeccgoto_pair_val(hd(Nss), assign, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_close(Stack),
 yeccgoto_pair_val(hd(Nss), close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_comma(Stack),
 yeccgoto_pair_val(hd(Nss), comma, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_curly_close(Stack),
 yeccgoto_pair_val(hd(Nss), curly_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, def, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_def(Stack),
 yeccgoto_pair_val(hd(Nss), def, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, import_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_import_keyword(Stack),
 yeccgoto_pair_val(hd(Nss), import_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, macro, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_macro(Stack),
 yeccgoto_pair_val(hd(Nss), macro, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, module_keyword, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_module_keyword(Stack),
 yeccgoto_pair_val(hd(Nss), module_keyword, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_newlines(Stack),
 yeccgoto_pair_val(hd(Nss), newlines, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_pipe(Stack),
 yeccgoto_pair_val(hd(Nss), pipe, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, square_close, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_square_close(Stack),
 yeccgoto_pair_val(hd(Nss), square_close, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, type, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_type(Stack),
 yeccgoto_pair_val(hd(Nss), type, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_apply(Stack),
 yeccgoto_noun(hd(Nss), apply, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_caret_operator(Stack),
 yeccgoto_noun(hd(Nss), caret_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_comp_operator(Stack),
 yeccgoto_noun(hd(Nss), comp_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_div_operator(Stack),
 yeccgoto_noun(hd(Nss), div_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_eq_operator(Stack),
 yeccgoto_noun(hd(Nss), eq_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_minus_operator(Stack),
 yeccgoto_noun(hd(Nss), minus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_mult_operator(Stack),
 yeccgoto_noun(hd(Nss), mult_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_other_operator(Stack),
 yeccgoto_noun(hd(Nss), other_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_plus_operator(Stack),
 yeccgoto_noun(hd(Nss), plus_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_rightbias_operator(Stack),
 yeccgoto_noun(hd(Nss), rightbias_operator, Nss, NewStack, T, Ts, Tzr);
yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_(Stack),
 yeccgoto_verb(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_177(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
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

yeccpars2_178(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
yeccpars2_179(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_181(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_182(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
 yeccgoto_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_184/7}).
yeccpars2_184(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_185(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_braced_pattern(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_186: see yeccpars2_56

yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_187_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_188(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_189(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dict_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_191(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_dict_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_192(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_193/7}).
yeccpars2_193(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_194(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_194_(Stack),
 yeccgoto_dict_elements(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_195(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_196(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), curly_close, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_noun(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pair_key(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_197_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_198: see yeccpars2_110

-dialyzer({nowarn_function, yeccpars2_199/7}).
yeccpars2_199(S, curly_close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_200_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_201: see yeccpars2_110

yeccpars2_202(_S, caret_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), caret_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, comp_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), comp_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), curly_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, div_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), div_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, eq_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), eq_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, minus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), minus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, mult_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), mult_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), open, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, other_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), other_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, plus_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), plus_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), rightbias_operator, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, square_open, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), square_open, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), type_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), value, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_separator(hd(Ss), var_symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_202_(Stack),
 yeccgoto_dict_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_203_(Stack),
 yeccgoto_dict_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_204_(Stack),
 yeccgoto_dict(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_205(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, rightbias_operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, type_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, var_symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_9(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_206(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), apply, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), comma, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), newlines, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_verb(hd(Ss), pipe, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_207_(Stack),
 yeccgoto_pattern_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_208(_S, apply, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_apply(Stack),
 yeccgoto_symbol(hd(Ss), apply, Ss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_colon(Stack),
 yeccgoto_symbol(hd(Ss), colon, Ss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, comma, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_comma(Stack),
 yeccgoto_symbol(hd(Ss), comma, Ss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, newlines, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_newlines(Stack),
 yeccgoto_symbol(hd(Ss), newlines, Ss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, pipe, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_pipe(Stack),
 yeccgoto_symbol(hd(Ss), pipe, Ss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, slash, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_slash(Stack),
 yeccgoto_symbol(hd(Ss), slash, Ss, NewStack, T, Ts, Tzr);
yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_(Stack),
 yeccgoto_pattern_verb(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_209_(Stack),
 yeccgoto_pattern_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_patterns(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_211: see yeccpars2_30

yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_212_(Stack),
 yeccgoto_type_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_sum_or_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_214(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_214_(Stack),
 yeccgoto_flat_sum_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_215_(Stack),
 yeccgoto_sum_or_expression(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_216: see yeccpars2_73

%% yeccpars2_217: see yeccpars2_110

yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_218_(Stack),
 yeccgoto_flat_sum_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_219: see yeccpars2_31

yeccpars2_220(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_220_(Stack),
 yeccgoto_type_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_type_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_222_(Stack),
 yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_223_(Stack),
 yeccgoto_newtype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_all(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_225/7}).
yeccpars2_225(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_226/7}).
yeccpars2_226(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_227(S, curly_open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_symbol(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_228_(Stack),
 yeccgoto_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_229_(Stack),
 yeccgoto_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_230: see yeccpars2_15

%% yeccpars2_231: see yeccpars2_72

%% yeccpars2_232: see yeccpars2_31

%% yeccpars2_233: see yeccpars2_44

yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_234_(Stack),
 yeccgoto_newmacro(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_235(S, newlines, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_235_(Stack),
 yeccgoto_def_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_236: see yeccpars2_31

yeccpars2_237(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_237_(Stack),
 yeccgoto_def_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_238_(Stack),
 yeccgoto_def_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_239: see yeccpars2_72

yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_240_(Stack),
 yeccgoto_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_newmacro(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_242(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_242_(Stack),
 yeccgoto_import(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_243_(Stack),
 yeccgoto_import(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_244: see yeccpars2_15

%% yeccpars2_245: see yeccpars2_72

%% yeccpars2_246: see yeccpars2_31

yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_247_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_248_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_249_(Stack),
 yeccgoto_statements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_250_(Stack),
 yeccgoto_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_all/7}).
yeccgoto_all(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_application/7}).
yeccgoto_application(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_application(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_arguments/7}).
yeccgoto_arguments(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arguments(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arguments(207=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_assignment/7}).
yeccgoto_assignment(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_braced_pattern/7}).
yeccgoto_braced_pattern(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(184, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_braced_pattern(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(184, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clause/7}).
yeccgoto_clause(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clause_separator/7}).
yeccgoto_clause_separator(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(219, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(230, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(232, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(236, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_separator(244, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(246, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_collection/7}).
yeccgoto_collection(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_collection(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_def_clauses/7}).
yeccgoto_def_clauses(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_def_clauses(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_def_clauses(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict/7}).
yeccgoto_dict(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict_element/7}).
yeccgoto_dict_element(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(194, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_element(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(194, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_element(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(194, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_dict_elements/7}).
yeccgoto_dict_elements(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(193, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_elements(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(199, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_dict_elements(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_element/7}).
yeccgoto_element(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_elements/7}).
yeccgoto_elements(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(99, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(99, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expression/7}).
yeccgoto_expression(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(154, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(155, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(161, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(175, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expressions/7}).
yeccgoto_expressions(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(68, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(133, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(159, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(161, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(162, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_flat_sum_list/7}).
yeccgoto_flat_sum_list(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flat_sum_list(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flat_sum_list(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_function/7}).
yeccgoto_function(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_implies/7}).
yeccgoto_implies(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(211, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(230, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(231, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(233, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(239, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(244, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(245, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_import/7}).
yeccgoto_import(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_infix/7}).
yeccgoto_infix(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(146, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(148, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(154, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(161, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(219, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_infix(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_lambda/7}).
yeccgoto_lambda(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lambda(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_leftbias_infix/7}).
yeccgoto_leftbias_infix(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_leftbias_infix(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_list/7}).
yeccgoto_list(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_literal/7}).
yeccgoto_literal(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr).

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
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_noun/7}).
yeccgoto_noun(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(191, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(148, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(154, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(161, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(191, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(191, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(219, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operator/7}).
yeccgoto_operator(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(139, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(148, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(139, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(139, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(148, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(139, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(139, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(139, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operator(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair/7}).
yeccgoto_pair(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair_key/7}).
yeccgoto_pair_key(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(154, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(161, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pair_key(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pair_val/7}).
yeccgoto_pair_val(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern/7}).
yeccgoto_pattern(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(219, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_application/7}).
yeccgoto_pattern_application(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_application(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_verb/7}).
yeccgoto_pattern_verb(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_patterns/7}).
yeccgoto_patterns(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(219, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(233, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(233, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(233, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_qualified_symbol/7}).
yeccgoto_qualified_symbol(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(226, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualified_symbol(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_rightbias_infix/7}).
yeccgoto_rightbias_infix(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rightbias_infix(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_secondary_separator/7}).
yeccgoto_secondary_separator(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondary_separator(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_separator/7}).
yeccgoto_separator(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(135, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_separator(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(201, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sequence/7}).
yeccgoto_sequence(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sequence(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr).

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
 yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statements(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_elem/7}).
yeccgoto_sum_elem(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_elem(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_list/7}).
yeccgoto_sum_list(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(239=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_list(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_or_expression/7}).
yeccgoto_sum_or_expression(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_or_expression(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sum_terms/7}).
yeccgoto_sum_terms(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sum_terms(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_symbol/7}).
yeccgoto_symbol(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(242, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(225, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(148, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(154, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(161, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(167, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(205, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(219, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbol(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_symbols/7}).
yeccgoto_symbols(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(244, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbols(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(230, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbols(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbols(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_clause/7}).
yeccgoto_type_clause(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_clause(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_clause(219, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_clauses/7}).
yeccgoto_type_clauses(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_clauses(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_clauses(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_verb/7}).
yeccgoto_verb(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(55, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(148, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(154, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(161, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(201, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(205, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(150, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(219, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(239, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_2_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 61).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 101).
yeccpars2_16_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 92).
yeccpars2_18_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 97).
yeccpars2_19_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 94).
yeccpars2_20_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 98).
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
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 93).
yeccpars2_23_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 99).
yeccpars2_24_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 95).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 90).
yeccpars2_26_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 , operator )
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_27_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 88).
yeccpars2_28_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 102).
yeccpars2_29_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_34_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 72).
yeccpars2_34_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 207).
yeccpars2_35_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 78).
yeccpars2_37_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type_def , ctx ( __1 ) , name ( __2 ) , args ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 216).
yeccpars2_38_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 227).
yeccpars2_46_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 135).
yeccpars2_69_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_75_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 258).
yeccpars2_75_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 220).
yeccpars2_76_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { lambda , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 281).
yeccpars2_91_(__Stack0) ->
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

-compile({inline,yeccpars2_100_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 283).
yeccpars2_100_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 307).
yeccpars2_101_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sum , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 287).
yeccpars2_104_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 273).
yeccpars2_105_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 286).
yeccpars2_106_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 285).
yeccpars2_107_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_108_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 282).
yeccpars2_108_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,'yeccpars2_109_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
'yeccpars2_109_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_assign/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_109_assign(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_109_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_109_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_109_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_109_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_macro/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_109_macro(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_109_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_109_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_109_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 124).
yeccpars2_109_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_109_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_109_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_109_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_109_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_109_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_109_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_109_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_109_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_109_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_109_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 159).
yeccpars2_109_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 321).
yeccpars2_113_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 322).
yeccpars2_114_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_116_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 311).
yeccpars2_116_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_117_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 309).
yeccpars2_117_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,'yeccpars2_126_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
'yeccpars2_126_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_assign/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_assign(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_colon/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_colon(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_curly_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_curly_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_macro/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_macro(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_126_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_126_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 159).
yeccpars2_126_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_127_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 310).
yeccpars2_127_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_129_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 104).
yeccpars2_129_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_130_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 106).
yeccpars2_130_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 | unwrap ( __3 ) ] }
  end | __Stack].

-compile({inline,yeccpars2_131_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 105).
yeccpars2_131_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { qualified_symbol , ctx ( __1 ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_132_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 306).
yeccpars2_132_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { sum , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_134_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 260).
yeccpars2_134_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_136_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 136).
yeccpars2_136_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_137_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 137).
yeccpars2_137_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_138_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 259).
yeccpars2_138_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { list , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_144_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 177).
yeccpars2_144_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , make_symbol ( __2 , operator ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_146_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 178).
yeccpars2_146_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , make_symbol ( __2 , operator ) , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_149_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 176).
yeccpars2_149_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , __2 , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_150_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 165).
yeccpars2_150_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 ] }
  end | __Stack].

-compile({inline,yeccpars2_156_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 159).
yeccpars2_156_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 166).
yeccpars2_157_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 | __4 ] }
  end | __Stack].

-compile({inline,yeccpars2_160_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 169).
yeccpars2_160_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,yeccpars2_163_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 171).
yeccpars2_163_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,yeccpars2_164_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 170).
yeccpars2_164_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 175).
yeccpars2_165_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __2 ) , __2 , [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_170_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 186).
yeccpars2_170_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { pair , ctx ( __1 ) , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_176_\'$end\''/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
'yeccpars2_176_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_assign/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_assign(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_comma(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_curly_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_curly_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_def/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_def(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_import_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_import_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_macro/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_macro(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_module_keyword/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_module_keyword(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_newlines(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_pipe(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_square_close/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_square_close(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_type/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 200).
yeccpars2_176_type(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_176_apply(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_caret_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_176_caret_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_comp_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_176_comp_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_div_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_176_div_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_eq_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_176_eq_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_minus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_176_minus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_mult_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_176_mult_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_other_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_176_other_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_plus_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_176_plus_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_rightbias_operator/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 150).
yeccpars2_176_rightbias_operator(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_176_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 159).
yeccpars2_176_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_187_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 230).
yeccpars2_187_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_194_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 268).
yeccpars2_194_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_197_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 261).
yeccpars2_197_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_200_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 263).
yeccpars2_200_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_202_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 269).
yeccpars2_202_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_203_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 270).
yeccpars2_203_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_204_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 262).
yeccpars2_204_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { dict , ctx ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_207_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 242).
yeccpars2_207_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 ] }
  end | __Stack].

-compile({inline,yeccpars2_208_apply/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_208_apply(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_208_colon/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_208_colon(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_208_comma/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_208_comma(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_208_newlines/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_208_newlines(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_208_pipe/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_208_pipe(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_208_slash/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 87).
yeccpars2_208_slash(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_208_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 246).
yeccpars2_208_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_209_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 243).
yeccpars2_209_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __3 , [ __1 | __4 ] }
  end | __Stack].

-compile({inline,yeccpars2_210_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 228).
yeccpars2_210_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_212_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 210).
yeccpars2_212_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { clause , ctx ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_214_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 303).
yeccpars2_214_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_215_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 299).
yeccpars2_215_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unpack_sum ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_218_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 304).
yeccpars2_218_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_220_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 217).
yeccpars2_220_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_221_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 218).
yeccpars2_221_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_222_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 164).
yeccpars2_222_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { application , ctx ( __1 ) , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_223_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 77).
yeccpars2_223_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type_def , ctx ( __1 ) , name ( __2 ) , args ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_224_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 52).
yeccpars2_224_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_227_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 88).
yeccpars2_227_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_symbol ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_228_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 113).
yeccpars2_228_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { module , ctx ( __1 ) , [ make_symbol ( __2 ) ] , unwrap ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_229_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 114).
yeccpars2_229_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { module , ctx ( __1 ) , unwrap ( __2 ) , unwrap ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_234_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 80).
yeccpars2_234_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , ctx ( __1 ) , name ( __2 ) , args ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_235_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 212).
yeccpars2_235_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_237_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 213).
yeccpars2_237_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_238_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 214).
yeccpars2_238_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_240_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 208).
yeccpars2_240_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { clause , ctx ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_241_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 79).
yeccpars2_241_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , ctx ( __1 ) , name ( __2 ) , args ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_242_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 116).
yeccpars2_242_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { import , ctx ( __1 ) , [ __2 ] }
  end | __Stack].

-compile({inline,yeccpars2_243_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 117).
yeccpars2_243_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { import , ctx ( __1 ) , unwrap ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_247_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 76).
yeccpars2_247_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { def , ctx ( __1 ) , name ( __2 ) , args ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_248_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 75).
yeccpars2_248_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { def , ctx ( __1 ) , name ( __2 ) , args ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_249_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 62).
yeccpars2_249_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_250_/1}).
-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 64).
yeccpars2_250_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].


-file("/Users/arnfred/workspace/kind/src/syntax.yrl", 351).
