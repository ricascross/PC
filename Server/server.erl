-module(server).
-export([start/0,start/1, stop/0]).

start() -> start(1234).

start(Port) ->
   register(?MODULE, spawn(fun() -> server(Port) end)),
   login_manager:start().


stop() ->
      self() ! login_manager:stop(),
      self() ! stop.


room(Pids) ->
   receive
      {enter, Pid} ->
         io:format("user_entered~n",[]),
         room([Pid | Pids]);

      {line, Data} = Msg ->
         Info_without_newline = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
         Info = string:split(Info_without_newline,",",all),

         io:format("received: ~p~n",[Info]),

         case Info of
            ["create_account", User, Pass] -> self() ! login_manager:create_account(User, Pass);
            ["close_account", User, Pass] -> self() ! login_manager:close_account(User, Pass);
            ["login", User, Pass] -> self() ! login_manager:login(User, Pass);
            ["logout", User] -> self() ! login_manager:logout(User);
            ["online"] -> self() ! login_manager:online()
         end,

         [Pid ! Msg || Pid <- Pids],
         room(Pids);

      {leave, Pid} ->
         io:format("user_left~n", []),
         room(Pids -- [Pid]);



      % pattern matching do Info
      account_created -> io:format("Account created~n");
      user_exists -> io:format("User exists~n");

      account_closed -> io:format("Account closed~n");
      user_not_exists -> io:format("User not exists~n");

      login_done -> io:format("Login done~n");
      already_logged_in -> io:format("Already logged in~n");
      login_invalid -> io:format("Login invalid~n");

      logout_done -> io:format("Logout done~n");
      logout_invalid -> io:format("Logout invalid~n");

      {online, User_list} -> io:format("Users online: ~p~n", [User_list])

   end,
   room(Pids).

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