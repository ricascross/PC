-module(login_manager).
-export([start/0, create_account/2, close_account/2, login/2, logout/1, online/0, stop/0]).

start() ->
    register(?MODULE, spawn(fun() -> loop(dict:new())  end)).

call(Request) ->
    ?MODULE ! {Request, self()},
    receive {Res, ?MODULE} -> Res end.

create_account(User, Pass) -> call({create_account, User, Pass}).
close_account(User, Pass) -> call({close_account, User, Pass}).
login(User, Pass) -> call({login, User, Pass}).
logout(User) -> call({logout, User}).
online() -> call(online).
stop() -> call(stop).

loop(Map) ->
    receive
    %From é o pid de quem enviou o pedido

        {{create_account, User, Pass}, From} ->
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

        {{close_account, User, Pass}, From} ->
            case dict:find(User, Map) of
                % usa o pattern matching se a Pass no argumento for igual ao que está no dict, senão dá erro
                {ok, {Pass, _}} ->
                    From ! {ok, ?MODULE},
                    loop(dict:erase(User, Map));

                _ ->
                    From ! {user_not_exists, ?MODULE},
                    loop(Map)
            end;

        {{login, User, Pass}, From} ->
            io:format("Entrou~n"),
            case dict: find(User, Map) of
                {ok, {Pass, _}} ->
                    From ! {ok, ?MODULE},
                    loop(dict:store(User, {Pass, true}, Map));
                _ ->
                    From ! {invalid, ?MODULE},
                    loop(Map)
            end;

        {{logout, User, Pass}, From} ->
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

