%%%-------------------------------------------------------------------
%%% @author woolfy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2018 1:51 PM
%%%-------------------------------------------------------------------
-module(pollution_server_gen_server).
-author("woolfy").
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  addStation/2,
  addValue/4,
  handle_call/3,
  init/1,
  terminate/2,
  close/0,
  removeValue/3,
  getOneValue/3,
  getDailyMean/2,
  getOverLimit/1,
  getStationMean/2
]).

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% Handling calls %%

handle_call({add_station,[Name,{_,_}=Location]},_From,State) ->
  case pollution:add_station(Name,Location,State) of
    {ok, NewMonitor} ->
      {reply,NewMonitor,NewMonitor};
    {error,Reason} ->
      {reply,{error, Reason},State}
  end;

handle_call({add_value,[{_,_} = Location,Time,Type,Value]},_From,State) ->
  NewMonitor = pollution:addValue(Location,Time,Type,Value,State),
  {reply,NewMonitor,NewMonitor};

handle_call({remove_value,[{_,_}=Location,Date,Type]},_From,State) ->
  NewMonitor = pollution:removeValue(State,Location,Date,Type),
  {reply,NewMonitor,NewMonitor};

handle_call({remove_value,[Name,Date,Type]},_From,State) ->
  NewMonitor = pollution:removeValue(State,Name,Date,Type),
  {reply,NewMonitor,NewMonitor};

handle_call({get_one_value,[Type,Time,Station]},_From,State)->
  Value = pollution:getOneValue(Type,Time,Station),
  {reply,Value,{ok,State}};

handle_call({get_daily_mean,[Type,{_,_,_}=Day]},_From,State)->
  Value = pollution:getDailyMean(State,Type,Day),
  {reply,Value,{ok,State}};

handle_call({get_over_limit,[Hour]},_From,State)->
  Value = pollution:getOverLimit(State,Hour),
  {reply,Value,{ok,State}};

handle_call({get_station_mean,[Type,Station]},_From,State)->
  Value = pollution:getStationMean(State,Type,Station),
  {reply,Value,{ok,State}};

handle_call({stop,[]},_From,State) ->
  terminate(normal,State).


%% user interface %%
addStation(Name,{_,_}=Location) ->
  gen_server:call(?MODULE,{add_station,[Name,Location]}).

addValue(Location,Time,Type,Value) ->
  gen_server:call(?MODULE,{addValue,[{_,_} = Location,Time,Type,Value]}).

close() ->
  gen_server:cast(?MODULE,{stop,[]}).

init(_) ->
  pollution:create_monitor().

removeValue({_,_} = Location,Date,Type) ->
  gen_server:call(?MODULE,{remove_value,[Location,Date,Type]});

removeValue(Name,Date,Type) ->
  gen_server:call(?MODULE,{remove_value,[Name,Date,Type]}).

getOneValue(Type,Time,Station) ->
  gen_server:call(?MODULE,{get_one_value,[Type,Time,Station]}).

getDailyMean(Type,{_,_,_}=Day) ->
  gen_server:call(?MODULE,{get_daily_mean,[Type,Day]}).

getOverLimit(Hour) ->
  gen_server:call(?MODULE,{get_over_limit,[Hour]}).

getStationMean(Type,Station) ->
  gen_server:call(?MODULE,[Type,Station]).

terminate(_,State) ->
  io:format("Terminated ~B~n",[State]),
  ok.

