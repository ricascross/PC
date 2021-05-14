-module(login_manager).
-export([start/0, create_account/3, close_account/3, login/3, logout/2, online/0, stop/0]).

start() ->
    register(?MODULE, spawn(fun() -> loop(dict:new())  end)).

call(Request) ->
    ?MODULE ! {Request, self()},
    receive {Res, ?MODULE} -> Res end.

create_account(User, Pass,Pid) -> call({create_account, User, Pass,Pid}).
close_account(User, Pass,Pid) -> call({close_account, User, Pass,Pid}).
login(User, Pass,Pid) -> call({login, User, Pass,Pid}).
logout(User,Pid) -> call({logout, User,Pid}).
online() -> call(online).
stop() -> call(stop).

loop(Map) ->
    receive
    %From é o pid de quem enviou o pedido
        {{create_account, User, Pass,Pid}, From} ->
            % verificar se este pid já está no dicionário ou não
            case dict:find(User, Map) of
                error ->
                    % enviar a mensagem a quem fez o pedido(FROm)
                    From ! {ok, ?MODULE},
                    % criar a conta, adicionando ao dict o username e um tuplo passwd e logged in
                    loop(dict:store(User, {Pass, false}, Map));

                % como já existe, envia a quem pediu a mensagem a dizer que existe
                _ ->
                    From ! {user_exists, ?MODULE},
                    loop(Map)
            end;

        {{close_account, User, Pass,Pid}, From} ->
            case dict:find(User, Map) of
                % usa o pattern matching se a Pass no argumento for igual ao que está no dict, senão dá erro
                {ok, {Pass, _}} ->
                    From ! {ok, ?MODULE},
                    loop(dict:erase(User, Map));

                _ ->
                    From ! {user_not_exists, ?MODULE},
                    loop(Map)
            end;

        {{login, User, Pass,Pid}, From} ->
            io:format("Entrou~n"),
            case dict: find(User, Map) of
                {ok, {Pass, _}} ->
                    From ! {ok, ?MODULE};
                    %loop(dict:store(User, {Pass, true}, Map));
                _ ->
                    From ! {invalid, ?MODULE}
                    %loop(Map)
            end;

        {{logout, User, Pass,Pid}, From} ->
            case dict: find(User, Map) of
                {ok, {Pass, _}} ->
                    From ! {ok, ?MODULE},
                    loop(dict:store(User, {Pass, false}, Map));
                _ ->
                    From ! {invalid, ?MODULE},
                    loop(Map)
            end;

        {online, From} ->
            From ! {[User || {User, {_, true}} <- dict:to_list(Map)],?MODULE},
            loop(Map);

        {stop, From} ->
            From ! {ok, ?MODULE}

    end.

