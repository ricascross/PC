-module(login_manager).
-export([start/0, create_account/2, close_account/2, login/2, logout/1, online/0, stop/0]).

start() ->
    register(?MODULE, spawn(fun() -> loop(maps:new())  end)).

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
            case maps:find(User, Map) of
                error ->
                    % enviar a mensagem a quem fez o pedido(FROm)
                    From ! {account_created, ?MODULE},
                    % criar a conta, adicionando ao dict o username e um tuplo passwd e logged in
                    loop(maps:put(User, {Pass, false}, Map));

                % como já existe, envia a quem pediu a mensagem a dizer que existe
                _ ->
                    From ! {user_exists, ?MODULE},
                    loop(Map)
            end;

        {{close_account, User, Pass}, From} ->
            case maps:find(User, Map) of
                % usa o pattern matching se a Pass no argumento for igual ao que está no dict, senão dá erro
                {ok, {Pass, _}} ->
                    From ! {account_closed, ?MODULE},
                    loop(dict:remove(User, Map));

                _ ->
                    From ! {user_not_exists, ?MODULE},
                    loop(Map)
            end;

        {{login, User, Pass}, From} ->
            case maps:find(User, Map) of
                {ok, {Pass, false}} ->
                    From ! {login_done, ?MODULE},
                    loop(maps:put(User, {Pass, true}, Map));
                {ok, {Pass, true}} ->
                    From ! {already_logged_in, ?MODULE},
                    loop(Map);
                _ ->
                    From ! {login_invalid, ?MODULE},
                    loop(Map)
            end;

        {{logout, User}, From} ->
            case maps:find(User, Map) of
                {ok, {Pass, _}} ->
                    From ! {logout_done, ?MODULE},
                    loop(maps:put(User, {Pass, false}, Map));
                _ ->
                    From ! {logout_invalid, ?MODULE},
                    loop(Map)
            end;

        {online, From} ->
            User_list = [User || {User, {_, true}} <- maps:to_list(Map)],
            From ! {{online, User_list},?MODULE},
            loop(Map);

        {stop, From} ->
            From ! {stopped, ?MODULE}

    end.

