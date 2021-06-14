-module(user_manager).
-export([userAuth/1]).
-import(login_manager,[login/2,logout/1,create_account/2,close_account/2]).

%Tratar da autenticacao
userAuth(Sock) ->
  receive
    % recebe a mensagem do cliente para login/lougout/create_account/close_account
    {tcp, _, Data} ->
      Info_without_newline = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
      Info = string:split(Info_without_newline,",",all),

      case Info of
        ["create_account", User, Pass] ->
          Res = create_account(User, Pass),
          case Res of
            account_created ->
              gen_tcp:send(Sock, io_lib:format("Registered~n", [])),
              match_manager ! {newPlayer, User, self()},
              userAuth(Sock);

            user_exists ->
              gen_tcp:send(Sock, io_lib:format("UserExists~n", [])),
              userAuth(Sock)
          end;

        ["close_account", User, Pass] ->
          Res = close_account(User,Pass),
          case Res of
            account_closed ->
              gen_tcp:send(Sock, io_lib:format("AccountClosed~n", [])),
              userAuth(Sock);
            _ ->
              gen_tcp:send(Sock, io_lib:format("CloseAccountGoneWrong~n", [])),
              userAuth(Sock)
          end;


        ["login", User, Pass] ->
          Res = login(User, Pass),
          case Res of
            login_done ->
              gen_tcp:send(Sock, io_lib:format("LoginDone~n", [])),

              % quando o login é verificado chama a função para gerir o cliente enquanto está no jogo
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

        % para quando o cliente carregar no botão de pontuações no menu inicial
        % devolve as melhores pontuações
        ["Scores"] ->
          score_manager ! {getScores, self()},
          receive
            {scores, Scores} ->
              gen_tcp:send(Sock, io_lib:format("BeginScores~n", [])),
              ScoresInfo = maps:new(),
              ScoresInfo2 = maps:put(scores,Scores, ScoresInfo),
              sendScores(Sock, ScoresInfo2),
              gen_tcp:send(Sock, io_lib:format("EndScores~n", []));

            _ ->
              gen_tcp:send(Sock, io_lib:format("ScoresInvalid~n", []))
          end,
          userAuth(Sock);

        _ ->
          self() ! gen_tcp:send(Sock, io_lib:format("InvalidCommand~n", []))
      end;

    _ -> userAuth(Sock)
  end.

% Função que devolve uma partida nova
newGame(Sock, User) ->
  % enviao ao servidor que um jogador quer jogar
  match_manager ! {login, User, self()},
  receive
    % recebe informação inicial(criaturas, players, scores..) para iniciar o jogo
    {initialMatch, MatchInfo, Match, match_manager} ->
      initialInfo(Sock, MatchInfo),
      Match;

    {tcp_closed, _} ->
      io:format("User ~s disconnected~n", [User]),
      logout(User),
      match_manager ! {leaveWaitMatch, User, self()};

    {tcp_error, _, _} ->
      io:format("User ~s disconnected with error~n", [User]),
      logout(User),
      match_manager ! {leaveWaitMatch, User, self()}
  end.

%Funcao para gerir o jogo apos login validado
userInGame(Sock, Username, Match)->
  receive
    % recebe do servidor mensagem a dizer que o jogo acabou, chama a função matchover
    {matchOver, ScoreBoard, Match} ->
      matchOver(Sock,Username,ScoreBoard)

    % se não receber nenhuma informação, fica à espera de um dos padrões de baixo
  after 0 ->
      receive
        {matchOver, ScoreBoard, Match} ->
          matchOver(Sock,Username,ScoreBoard);

        % recebe do servidor mensagem com a informação atualizada do jogo e envia para o cliente
        {updateMatch, UpdateInfo, Match} ->
          sendUpdateInfo(Sock, UpdateInfo),
          userInGame(Sock,Username,Match);

        % recebe do cliente o input das teclas premidas/libertadas e envia para o servidor
        {tcp, _, Data} ->
          Info_without_newline = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
          Info = string:split(Info_without_newline,",",all),
          case Info of
          ["KeyChanged", Key, "True"] -> match_manager ! {keyChanged, Key, true, self()};
          ["KeyChanged", Key, "False"] -> match_manager ! {keyChanged, Key, false, self()}
          end,
          userInGame(Sock,Username,Match);

        {tcp_closed, _} ->
          io:format("~s has disconnected~n", [Username]),
          logout(Username),
          match_manager ! {leave, Username, self()};

        {tcp_error, _, _} ->
          io:format("~s left due to error~n", [Username]),
          logout(Username),
          match_manager ! {leave, Username, self()}
      end
  end.


% envia scores do jogo para os jogadores que estavam a jogar e não sairam.
matchOver(Sock, Username, Scoreboard) ->
  gen_tcp:send(Sock, io_lib:format("MatchOverBegin~n", [])),
  Scores = maps:new(),
  Scores1 = maps:put(scores,Scoreboard, Scores),
  sendScores(Sock, Scores1),
  gen_tcp:send(Sock, io_lib:format("MatchOverEnd~n", [])),
  match_manager ! {leaveWaitMatch, Username, self()},
  matchOverUserResponse(Sock, Username).


% verifica se um jogador que estava a jogar pretende continuar ou sair, também
matchOverUserResponse(Sock, Username) ->
  receive
    {tcp, _, Data} ->
      Info_without_newline = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
      Info = string:split(Info_without_newline,",",all),
      case Info of
        ["Continue"] ->
          match_manager ! {continue , Username, self()},
          userInGame(Sock, Username, newGame(Sock, Username));
        ["Quit"] ->
          logout(Username),
          %login_manager ! {{logout, Username},self()},
          match_manager ! {leaveWaitMatch, Username, self()},
          userAuth(Sock);
        _ ->
          gen_tcp:send(Sock,io_lib:format("UnknownResponse~n", [])),
          matchOverUserResponse(Sock,Username)
      end;

    {tcp_close, _} ->
      logout(Username),
      match_manager ! {leaveWaitMatch, Username, self()};
      %login_manager ! {{logout, Username},self()};

    {tcp_error, _, _} ->
      logout(Username),
      match_manager ! {leaveWaitMatch, Username, self()}

  end.

% envia a informação inicial para o cliente
initialInfo(Sock, MatchInfo) ->
  gen_tcp:send(Sock, io_lib:format("StartInitialMatchInfo~n", [])),
  sendPlayersInfo(Sock, MatchInfo),
  sendCreaturesInfo(Sock, MatchInfo),
  sendScores(Sock, MatchInfo),
  gen_tcp:send(Sock, io_lib:format("EndInitialMatchInfo~n", [])).


% envia a informação atualizada para o cliente
sendUpdateInfo(Sock, UpdateInfo) ->
  gen_tcp:send(Sock, io_lib:format("StartMatchInfo~n", [])),
  sendPlayersInfo(Sock, UpdateInfo),
  sendCreaturesInfo(Sock, UpdateInfo),
  sendScores(Sock, UpdateInfo),
  gen_tcp:send(Sock, io_lib:format("EndMatchInfo~n", [])).


% envia a informação sobre os jogadores para o cliente
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
  gen_tcp:send(Sock, io_lib:format("P,~s,~w,~w,~w,~w~n", [Username, X, Y, Radius, Score])),
  sendPlayerInfo(Sock, T).


% envia a informação sobre as criaturas para o cliente
sendCreaturesInfo(Sock, MatchInfo) ->
  case maps:find(creatures, MatchInfo) of
    {ok, Creatures} ->
      sendCreatureInfo(Sock, Creatures, 0);
    error ->
      nothingToSend
  end.

sendCreatureInfo(_, [], _) ->
  creaturesDone;
sendCreatureInfo(Sock, [{Idx, Creature}|T], _) ->
  {ok, Type} = maps:find(type, Creature),
  {ok, X} = maps:find(x, Creature),
  {ok, Y} = maps:find(y, Creature),
  {ok, Radius} = maps:find(radius, Creature),
  gen_tcp:send(Sock, io_lib:format("C,~w,~w,~w,~w,~w~n", [Idx, Type, X, Y, Radius])),
  sendCreatureInfo(Sock, T, ok);
sendCreatureInfo(Sock, [H|T], Idx) ->
  {ok, Type} = maps:find(type, H),
  {ok, X} = maps:find(x, H),
  {ok, Y} = maps:find(y, H),
  {ok, Radius} = maps:find(radius, H),
  gen_tcp:send(Sock, io_lib:format("C,~w,~w,~w,~w,~w~n", [Idx, Type, X, Y, Radius])),
  sendCreatureInfo(Sock, T, Idx+1).


% envia a informação sobre os scores para o cliente
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
  gen_tcp:send(Sock, io_lib:format("S,~s,~w~n", [Username, Score])),
  sendScore(Sock, T).