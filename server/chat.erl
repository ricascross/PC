-module(chat).
-export([start/1, stop/0]).

start(Port) ->
   register(?MODULE, spawn(fun() -> server(Port) end)),
   login_manager:start().


stop() ->
   login_manager:stop(),
   self() ! stop.


room(Pids) ->
   receive
      {enter, Pid} -> 
         io:format("user_entered\n",[]),
         room([Pid | Pids]);

      {line, Data} = Msg ->
         Info = string:split(Data,",",all),
         io:format("received: ~s",[Info]),
         
         case Info of
            [<<"login">>, User, Pass] -> self() ! login_manager:login(User, Pass,self());
            [<<"logout">>, User] -> io:format("Entrou no logout ~n");
            [<<"online\r\n">>] -> self() ! {lista , Pids}
         end,
         
         [Pid ! Msg || Pid <- Pids],
         room(Pids);

      {leave, Pid} ->
         io:format("user_left~n", []),
         room(Pids -- [Pid]);
      {lista, Info} ->
         io:format("received: ~s",[Info]);

      invalid ->
         io:format("Login invalido~n")
   end.

user(Sock, Room) ->
   receive
      {line, Data} ->
         gen_tcp:send(Sock, Data),
         user(Sock, Room);
      {tcp, _, Data} -> 
         Room ! {line, Data},
         user(Sock, Room);
      {tcp_closed, _} ->
         Room ! {leave, self()};
      {tcp_error, _, _} ->
         Room ! {leave, self()}
   end.

server(Port)->
   {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
   Room = spawn(fun() -> room([]) end),
   spawn(fun() -> acceptor(LSock, Room) end),
   receive stop -> ok end.

acceptor(LSock, Room) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Room) end),
    Room ! {enter, self()},
    user(Sock, Room).

