-module(part1).
-compile(export_all).
-compile(nowarn_export_all).
% Util
file_as_string(Filename) ->
  case file:read_file(Filename) of
    {ok, Binary} ->
      {ok, unicode:characters_to_list(Binary)};
    {error, Reason} ->
      io:format("Error: ~p~n", [Reason]),
      {error}
  end.

%%%%%%%%%%%%%%% Parsing %%%%%%%%%%%%%%%%%%
%% Transpose taken from
%% https://stackoverflow.com/questions/5389254/transposing-a-2-dimensional-matrix-in-erlang
transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

% Parse Crates
parse_crate_line(L) -> parse_crate_line(L, 0).
parse_crate_line([], _) -> [];
parse_crate_line([H|T], Idx) when ((Idx-1) rem 4)==0 ->
  case H of
    32 -> [empty|parse_crate_line(T, Idx+1)];
    _ -> [H|parse_crate_line(T, Idx+1)]
  end;
parse_crate_line([_|T], Idx) ->
  parse_crate_line(T, Idx+1).
remove_empty(L) ->
  lists:filter(fun(X) -> X =/= empty end, L).
parse_crates(CS) ->
  Lines = string:split(CS, "\n", all),
  CT = lists:map(fun parse_crate_line/1, lists:droplast(Lines)),
  array:from_list(lists:map(fun remove_empty/1, transpose(CT))).

% Parse Actions
parse_actions(AS) ->
  lists:map(fun parse_action_line/1, string:split(AS, "\n", all)).
parse_action_line(ALS) ->
  [_, CountStr, _, FromStr, _, ToStr] = string:tokens(ALS, " "),
  {Count,_} = string:to_integer(CountStr),
  {From,_} = string:to_integer(FromStr),
  {To,_} = string:to_integer(ToStr),
  {Count, From-1, To-1}.

parse_input(S) ->
  [CS, AS] = string:split(S, "\n\n"),
  {parse_crates(CS), parse_actions(AS)}.

%%%%%%%%%%%%% Simulation %%%%%%%%%%%%%%%%

simulate_sub_action(C, From, To) ->
  [Crate|FromC] = array:get(From, C),
  C1 = array:set(From, FromC, C),
  array:set(To, [Crate|array:get(To, C1)], C1).

simulate_action(C, {0, _, _}) -> C;
simulate_action(C, {Count, From, To}) ->
  {Crates, FromC} = lists:split(Count, array:get(From, C)),
  C1 = array:set(From, FromC, C),
  array:set(To, Crates++array:get(To, C1), C1).
simulate(C, []) -> C;
simulate(C, [H|T]) ->
  simulate(simulate_action(C, H), T).

%%%%%%%%%%%%%%% Display %%%%%%%%%%%%%%%%%%
display(C) ->
  io:format("~p~n", [array:to_list(C)]).


%% Main
main() ->
  case file_as_string("input_small.txt") of
    {ok, S} ->
      {C, A} = parse_input(S),
      display(simulate(C, A));
    {error} ->
      ok
  end.