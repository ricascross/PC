-module(server).
-export([start/0,server/1]).
-import(login_manager,[start_login/0]).
-import(user_manager,[userAuth/1]).
-import(score_manager,[scoreBoard/2]).


start() -> server(1234).

server(Port)->
   register(match_manager, spawn(fun() -> roomMatch([], [], 3) end)),
   register(login_manager, start_login()),
   register(score_manager, spawn(fun() -> scoreBoard([], []) end)),
   {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
   io:format("Servidor pronto.~n", []),
   acceptor(LSock).

acceptor(LSock) ->
   {ok, Sock} = gen_tcp:accept(LSock),
   spawn(fun() -> acceptor(LSock) end),
   io:format("Alguem entrou.~n", []),
   userAuth(Sock).

roomMatch(ActivePlayers, WaitingQueue, MaxPlayers) ->
   receive
      {newPlayer, User, Pid} ->
         WaitingQueue1 = [{Pid, User} | WaitingQueue],
         if
            length(ActivePlayers) < MaxPlayers ->
               io:format("User ~s wants to play. Waiting for opponent...~n", [User]),
               Player = lists:nth(0,WaitingQueue1),
               WaitingQueue2 = lists:delete(Player, WaitingQueue1),
               ActivePlayers1 = [ Player | ActivePlayers],
               roomMatch(ActivePlayers1, WaitingQueue2, MaxPlayers);
            true ->
               io:format("Opponents found~n", []),
               spawn(fun()-> matchInitialize(ActivePlayers, maps:new()) end),
               roomMatch(ActivePlayers, WaitingQueue, MaxPlayers)
         end;
      {leaveWaitMatch, User, Pid} ->
         io:format("User ~s left~n", [User]),
         roomMatch(ActivePlayers -- [{Pid, User}], WaitingQueue, MaxPlayers);
      {matchOver, Match} ->
         io:format("Match ~p is over~n", [Match]),
         roomMatch([], WaitingQueue, MaxPlayers)
   end.

%Função para garantir que não há sobreposição de spawns de criaturas/jogadores ao começar

checkPosition(_, _, _, []) ->
   true;
checkPosition(X, Y, Radius, [{_,Player} | Players]) ->
   {ok, X2} = maps:find(x, Player),
   {ok, Y2} = maps:find(y, Player),
   {ok, Radius2} = maps:find(radius, Player),
   Dist = math:sqrt(math:pow(X2 - X, 2) + math:pow(Y2 - Y, 2)),
   if
      Dist < Radius2 + Radius ->
         false;
      true ->
         checkPosition(X, Y, Radius, Players)
   end.

% Função que cria os objetos iniciais
createCreature(_, 0, _, Res) ->
   Res;
createCreature(Type, N, Players, Res) ->
   X = rand:uniform(1367)-1,
   Y = rand:uniform(769)-1,
   Radius = rand:uniform(15)+3,
   Bool = checkPosition(X, Y, Radius, Players),
   if
      not Bool ->
         createCreature(Type, N, Players, Res);
      true ->
         Creature = maps:new(),
         Creature2 = maps:put(type, Type, Creature),
         Creature3 = maps:put(x, X, Creature2),
         Creature4 = maps:put(y, Y, Creature3),
         Creature5 = maps:put(radius, Radius, Creature4),
         NewRes = [Creature5 | Res],
         createCreature(Type, N-1, Players, NewRes)
   end.

% Função que cria um jogador
createPlayer(Username, Players, PressedKeys, BestScore) ->
   X = rand:uniform(1367)-1,
   Y = rand:uniform(769)-1,
   Radius = 20,
   Bool = checkPosition(X, Y, Radius, Players),
   if
      BestScore == 0 ->
         BestScore1 = 5 * math:pi() * Radius * Radius;
      true ->
         BestScore1 = BestScore
   end,
   if
      not Bool ->
         createPlayer(Username, Players, PressedKeys, BestScore1);
      true ->
         Player = maps:new(),
         Player2 = maps:put(username, Username, Player),
         Player3 = maps:put(x, X, Player2),
         Player4 = maps:put(y, Y, Player3),
         Player5 = maps:put(radius, Radius, Player4),
         Player6 = maps:put(score, 5 * math:pi() * Radius * Radius, Player5),
         Player7 = maps:put(pressedKeys, PressedKeys, Player6),
         Player8 = maps:put(bestScore, BestScore1, Player7)
   end.