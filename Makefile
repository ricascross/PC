compile:
	erlc -o Server/ Server/score_manager.erl
	erlc -o Server/ Server/login_manager.erl
	erlc -o Server/ Server/user_manager.erl
	erlc -o Server/ Server/server.erl