%%%-------------------------------------------------------------------
%%% @author woolfy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. May 2018 12:26 PM
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("woolfy").

%% API
-export([
  start/0,
  init/0,
  add_station/2,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  getStationMean/2,
  getDailyMean/2,
  getOverLimit/1,
  stop/0
]).

start() ->
  register(server,spawn(pollution_server, init, [])).

init() ->
  Monitor = pollution:create_monitor(),
  init(Monitor).

init({ok,Monitor}) ->
  run_server(Monitor);

init(Error) ->
  Error.

send_monitor(Pid,{ok,Monitor},_) ->
  Pid ! {ok,Monitor},
  run_server(Monitor);

send_monitor(Pid, {error,Monitor},PrevMonitor)->
  Pid ! {error,Monitor,PrevMonitor},
  run_server(PrevMonitor).

send_value(Pid,{ok,Value},Monitor) ->
  Pid ! {ok,Value},
  run_server(Monitor);

send_value(Pid,{error,Value},Monitor) ->
  Pid ! {error,Value,Monitor},
  run_server(Monitor).


run_server(Monitor) ->
  receive
    {add_station,Pid,[Name,{_,_}=Location]} ->
      UpdatedMonitor = pollution:add_station(Name,Location,Monitor),
      send_monitor(Pid,UpdatedMonitor,Monitor);

    {addValue,Pid,[{_,_} = Location,Time,Type,Value]} ->
      UpdatedMonitor = pollution:addValue(Location,Time,Type,Value,Monitor),
      send_monitor(Pid,UpdatedMonitor,Monitor);

    {removeValue,Pid,[Location,Date,Type]} ->
      UpdatedMonitor = pollution:removeValue(Monitor,Location,Date,Type), %location can be a name string
      send_monitor(Pid,UpdatedMonitor,Monitor);

    {getOneValue,Pid,[Type,Time,Station]} ->
      Value = pollution:getOneValue(Type,Time,Station),
      send_value(Pid,Value,Monitor);

    {getDailyMean,Pid,[Type,{_,_,_}=Day]} ->
      Value = pollution:getDailyMean(Monitor,Type,Day),
      send_value(Pid,{ok,Value},Monitor);

    {getOverLimit,Pid,[Hour]} ->
      Value = pollution:getOverLimit(Monitor,Hour),
      send_value(Pid,{ok,Value},Monitor);

    {getStationMean,Pid,[Type,Station]} ->
      Value = pollution:getStationMean(Monitor,Type,Station),
      send_value(Pid,{ok,Value},Monitor);

    {stop,Pid,[]} ->
      Pid ! {ok,Monitor};

    {_,Pid,_} ->
      Pid ! {error,"Pattern not found",""}
  end.

send_request(Type,Args) when is_list(Args) ->
  server ! {Type,self(),Args},
  receive
    Message -> Message
  after
    1000 -> {error,"Timeout"}
  end;

send_request(_,_) ->
  {error,"Wrong args"}.

add_station(Name,{_,_} = Location) ->
  send_request(add_station,[Name,Location]);

add_station(_,_) ->
  {error,"Wrong args"}.

addValue({_,_} = Location, Time, Type, Value) when is_tuple(Time) ->
  send_request(addValue,[Location,Time,Type,Value]);

addValue(_,_,_,_) ->
  {error,"wrong args"}.

removeValue({_,_}=Location,Date,Type) ->
  send_request(removeValue,[Location,Date,Type]);

removeValue(_,_,_) ->
  {error,"wrong args"}.

getOneValue(Type,Time,Station) ->
  send_request(getOneValue,[Type,Time,Station]).

getStationMean(Type,Station) -> % Station can be either name or tuple with coords
  send_request(getStationMean,[Type,Station]).

getDailyMean(Type,{_,_,_}=Day) ->
  send_request(getDailyMean,[Type,Day]);

getDailyMean(_,_) ->
  {error,"Wrong args"}.

getOverLimit(Hour) ->
  send_request(getOverLimit,[Hour]).

stop() ->
  send_request(stop,[]).