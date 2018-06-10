%%%-------------------------------------------------------------------
%%% @author woolfy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2018 1:15 PM
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("woolfy").

%% API
-export([start/0,init/0,loop/0]).

start() ->
  spawn(pollution_server_sup,init,[]).

init() ->
  process_flag(trap_exit,true),
  loop().

loop() ->
  register(pollution_server_gen_server,pollution_server_gen_server:start_link()),
  receive
    {'EXIT',_Pid,_Reason}->
      loop()
  end.
