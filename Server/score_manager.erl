-module(score_manager).
-export([scoreBoard/2]).

%Função que gere as melhores pontuações
% Pid do match
scoreBoard(Scores, Pid) ->
  receive
    % Pid1 Pid do jogador
    %envia por mensagem os scores para o jogador
    {getScores, Pid1} ->
      Pid1 ! {scores, Scores},
      scoreBoard(Scores, Pid);

    % mensagem para adicionar um novo score
    {newScore, {User, Score}} ->
      case lists:filter(fun({Username, _}) -> Username == User end, Scores) of

        [] ->
          case length(Scores) of
            5 ->
              {_, Lowest} = lists:last(Scores),
              if
                Score > Lowest ->
                  NewScore = updateTopScoreBoard({User, Score}, lists:droplast(Scores)),
                  sendTopScoreBoard(NewScore, Pid);
                true ->
                  NewScore = Scores
              end;
            _ ->
              NewScore = updateTopScoreBoard({User, Score}, Scores),
              sendTopScoreBoard(NewScore, Pid)
          end;

        [{_, CurrentScore}] when Score =< CurrentScore ->
          NewScore = Scores;
        [{_, CurrentScore}] when Score > CurrentScore ->
          NewScore = updateTopScoreBoard({User, Score}, lists:filter(fun({Username, _}) -> Username /= User end, Scores)),
          sendTopScoreBoard(NewScore, Pid)
      end,
      scoreBoard(NewScore, Pid)
  end.


%Função que envia as novas melhores pontuações a quem pretende recebê-las
sendTopScoreBoard(Scores, Receiver) ->
  Receiver ! {scores, Scores}.

% Função que adiciona uma pontuação à lista das melhores pontuações
updateTopScoreBoard(Score, []) ->
  [Score];
updateTopScoreBoard({User, Score1}, [{_, Score2} | _] = Scores) when Score1 > Score2 ->
  [{User, Score1} | Scores];
updateTopScoreBoard({User1, Score1}, [{User2, Score2} | T] = _) when Score1 =< Score2 ->
  [{User2, Score2} | updateTopScoreBoard({User1, Score1}, T)].



