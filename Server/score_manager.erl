-module(score_manager).
-export([scoreBoard/2]).

%Função que gere as melhores pontuações
scoreBoard(Scores, Pids) ->
  receive
    {getScores, Pid} ->
      Pid ! {scores, Scores},
      scoreBoard(Scores, Pids);
    {followScores, Pid} ->
      Pid ! {scores, Scores},
      scoreBoard(Scores, [Pid|Pids]);
    {unfollowScores, Pid} ->
      scoreBoard(Scores, lists:delete(Pid, Pids));
  % verifica se a nova pontuação de um utilizador é maior que a sua anterior
  % se for, atualiza o seu novo score, senão mantem o seu score.
    {newScore, {User, Score}} ->
      case lists:filter(fun({Username, _}) -> Username == User end, Scores) of
        [] ->
          case length(Scores) of
            5 ->
              {_, Lowest} = lists:last(Scores),
              if
                Score > Lowest ->
                  NewScore = updateTopScoreBoard({User, Score}, lists:droplast(Scores)),
                  sendTopScoreBoard(NewScore, Pids);
                true ->
                  NewScore = Scores
              end;
            _ ->
              NewScore = updateTopScoreBoard({User, Score}, Scores),
              sendTopScoreBoard(NewScore, Pids)
          end;

        [{_, CurrentScore}] when Score =< CurrentScore ->
          NewScore = Scores;
        [{_, CurrentScore}] when Score >= CurrentScore ->
          NewScore = updateTopScoreBoard({User, Score}, lists:filter(fun({Username, _}) ->
            Username /= User end, Scores)),
          sendTopScoreBoard(NewScore, Pids)
      end,
      scoreBoard(NewScore, Pids)


  end.


%Função que envia as novas melhores pontuações a quem pretende recebê-las
sendTopScoreBoard(Scores, Receivers) ->
  [Receiv ! {scores, Scores} || Receiv <- Receivers].


% Função que adiciona uma pontuação à lista das melhores pontuações
updateTopScoreBoard(Score, []) ->
  [Score];

updateTopScoreBoard({User, Score1}, [_, Score2 | _] = Scores) when Score1 > Score2 ->
  [{User, Score1} | Scores];

updateTopScoreBoard({User1, Score1}, [User2, Score2 | T] = _) when Score1 =< Score2 ->
  [{User2, Score2} | updateTopScoreBoard({User1, Score1}, T)].



