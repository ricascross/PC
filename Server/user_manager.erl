-module(user_manager).
-export([userAuth/1]).
-import(login_manager,[login/2,logout/1,create_account/2,close_account/2]).

%Tratar da autenticacao
userAuth(Sock) ->
  receive
    {tcp, _, Data} ->
      Info_without_newline = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
      Info = string:split(Info_without_newline,",",all),

      case Info of
        ["create_account", User, Pass] ->
          Res = create_account(User, Pass),
          case Res of
            account_created ->
              gen_tcp:send(Sock, io_lib:format("Registered~n", [])),
              userInGame(Sock, User, newGame(Sock, User));
            user_exists ->
              gen_tcp:send(Sock, io_lib:format("UserExists~n", [])),
              userAuth(Sock)
          end;

        ["close_account", User, Pass] ->
          Res = close_account(User,Pass),
          case Res of
            account_closed ->
              gen_tcp:send(Sock, io_lib:format("AccountClosed~n", []));
            _ ->
              gen_tcp:send(Sock, io_lib:format("CloseAccountGoneWrong~n", []))
          end;

        ["login", User, Pass] ->
          Res = login(User, Pass),
          case Res of
            login_done ->
              gen_tcp:send(Sock, io_lib:format("LoginDone~n", [])),
              userInGame(Sock, User, newGame(Sock, User));
            already_logged_in ->
              gen_tcp:send(Sock, io_lib:format("AlreadyLoggedIn~n", [])),
              userAuth(Sock);
            login_invalid ->
              gen_tcp:send(Sock, io_lib:format("LoginInvalid~n", [])),
              userAuth(Sock)
          end;

        ["logout", User] ->
          Res = logout(User),
          case Res of
            logout_done ->
              gen_tcp:send(Sock, io_lib:format("LogoutDone~n", []));
            logout_invalid ->
              gen_tcp:send(Sock, io_lib:format("LogoutInvalid~n", []))
          end;

        _ ->
          self() ! gen_tcp:send(Sock, io_lib:format("InvalidCommand~n", []))
      end
  end.

% Função que devolve uma partida nova
newGame(Sock, Username) ->
  matchManager ! {newPlayer, Username, self()},
  receive
    {initialMatch, MatchInfo, Match, matchManager} ->
      initialInfo(Sock, MatchInfo),
      Match;
    {tcp_closed, _} ->
      io:format("User ~s disconnected~n", [Username]),
      matchManager ! {leaveWaitMatch, Username, self()},
      logout(Username);
    {tcp_error, _, _} ->
      io:format("User ~s disconnected with error~n", [Username]),
      matchManager ! {leaveWaitMatch, Username, self()},
      logout(Username)
  end.

%Funcao para gerir o jogo apos login validado
userInGame(Sock, Username, Match)->
  receive
    {matchOver, ScoreBoard, Match} ->
      matchOver(Sock,Username,ScoreBoard)

    after
      0 ->
        receive
          {matchOver, ScoreBoard, Match} ->
            matchOver(Sock,Username,ScoreBoard);

          {updateMatch, UpdateInfo, Match} ->
            sendUpdateInfo(Sock, UpdateInfo),
            userInGame(Sock,Username,Match);

          {tcp, _, Data} ->
            Info_without_newline = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
            Info = string:split(Info_without_newline,",",all),
            case Info of
              ["KeyChanged", Key, "True"] -> Match ! {keyChanged, Key, true, self()};
              ["KeyChanged", Key, "False"] -> Match ! {keyChanged, Key, false, self()}
            end,
            userInGame(Sock,Username,Match);

          {tcp_closed, _} ->
            io:format("~s has disconnected~n", [Username]),
            Match ! {leave, self()},
            logout(Username);

          {tcp_error, _, _} ->
            io:format("~s left due to error~n", [Username]),
            Match ! {leave, self()},
            logout(Username)
        end
  end.

matchOver(Sock, Username, Scoreboard) ->
  gen_tcp:send(Sock, io_lib:format("MatchOverBegin~n", [])), %for debugging
  Scores = maps:new(),
  maps:put(scores,Scoreboard, Scores),
  sendScores(Sock, Scores),
  gen_tcp:send(Sock, io_lib:format("MatchOverEnd~n", [])), %for debugging
  matchOverUserResponse(Sock, Username).

matchOverUserResponse(Sock, Username) ->
  receive
    {tcp, _, Data} ->
      Info_without_newline = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
      Info = string:split(Info_without_newline,",",all),
      case Info of
        ["Continue"] ->
          userInGame(Sock, Username, newGame(Sock, Username));
        ["Quit"] ->
          logout(Username),
          userAuth(Sock);
        _ ->
          gen_tcp:send(Sock,io_lib:format("UnknownResponse~n", [])),
          matchOverUserResponse(Sock,Username)
      end;

    {tcp_close, _} ->
      logout(Username);

    {tcp_error, _, _} ->
      logout(Username)
  end.

initialInfo(Sock, MatchInfo) ->
  gen_tcp:send(Sock, io_lib:format("StartInitialMatchInfo~n", [])), %for debugging
  sendPlayersInfo(Sock, MatchInfo),
  sendBlobsInfo(Sock, MatchInfo),
  sendScores(Sock, MatchInfo),
  gen_tcp:send(Sock, io_lib:format("EndInitialMatchInfo~n", [])). %for debugging

sendUpdateInfo(Sock, UpdateInfo) ->
  gen_tcp:send(Sock, io_lib:format("StartMatchInfo~n", [])), %for debugging
  sendPlayersInfo(Sock, UpdateInfo),
  sendBlobsInfo(Sock, UpdateInfo),
  sendScores(Sock, UpdateInfo),
  gen_tcp:send(Sock, io_lib:format("EndMatchInfo~n", [])). %for debugging

sendPlayersInfo(Sock, MatchInfo) ->
  case maps:find(players, MatchInfo) of
    {ok, Players} ->
      PlayersList = [Player || {_, Player} <- maps:to_list(Players)],
      sendPlayerInfo(Sock, PlayersList);
    error ->
      nothingToSend
  end.

sendPlayerInfo(_, []) ->
  playerDone;
sendPlayerInfo(Sock, [H|T]) ->
  {ok, Username} = maps:find(username, H),
  {ok, X} = maps:find(x, H),
  {ok, Y} = maps:find(y, H),
  {ok, Radius} = maps:find(radius, H),
  {ok, Score} = maps:find(score, H),
  {ok, Speed} = maps:find(speedBonus, H),
  gen_tcp:send(Sock, io_lib:format("P,~s,~w,~w,~w,~w,~w~n", [Username, X, Y, Radius, Score, Speed])),
  sendPlayerInfo(Sock, T).

sendBlobsInfo(Sock, MatchInfo) ->
  case maps:find(blobs, MatchInfo) of
    {ok, Blobs} ->
      sendBlobInfo(Sock, Blobs, 0);
    error ->
      nothingToSend
  end.

sendBlobInfo(_, [], _) ->
  blobsDone;
sendBlobInfo(Sock, [{Idx, Blob}|T], _) ->
  {ok, Type} = maps:find(type, Blob),
  {ok, X} = maps:find(x, Blob),
  {ok, Y} = maps:find(y, Blob),
  {ok, Radius} = maps:find(radius, Blob),
  gen_tcp:send(Sock, io_lib:format("B,~w,~w,~w,~w,~w~n", [Idx, Type, X, Y, Radius])),
  sendBlobInfo(Sock, T, ok);
sendBlobInfo(Sock, [H|T], Idx) ->
  {ok, Type} = maps:find(type, H),
  {ok, X} = maps:find(x, H),
  {ok, Y} = maps:find(y, H),
  {ok, Radius} = maps:find(radius, H),
  gen_tcp:send(Sock, io_lib:format("B,~w,~w,~w,~w,~w~n", [Idx, Type, X, Y, Radius])),
  sendBlobInfo(Sock, T, Idx+1).

sendScores(Sock, MatchInfo) ->
  case maps:find(scores, MatchInfo) of
    {ok, Scores} ->
      sendScore(Sock, Scores);
    error ->
      nothingToSend
  end.

sendScore(_, []) ->
  scoresDone;
sendScore(Sock, [{Username, Score}|T]) ->
  gen_tcp:send(Sock, io_lib:format("S:~s:~w~n", [Username, Score])),
  sendScore(Sock, T).