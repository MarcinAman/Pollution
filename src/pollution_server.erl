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
  stop/0,
  crash/0,
  start2/0,
  get_stations_from_database/1,
  get_measurements_from_database/1,
  get_stations_from_database/3,
  get_measurements_from_database/3,
  getMonitor/0
]).

start() ->
  register(server,spawn(pollution_server, init, [])).

start2() ->
  spawn_link(pollution_server,init,[]).

init() ->
  Monitor = pollution:create_monitor(),
  DatabaseInit = pollution_server_odbc:init(),
  init(Monitor,DatabaseInit).

init({ok,Monitor},ok) ->
  UpdatedMonitor = get_stations_from_database({ok,Monitor}),
  init(UpdatedMonitor,stations);

init({ok,Monitor},stations) ->
  UpdatedMonitor = get_measurements_from_database({ok,Monitor}),
  init(UpdatedMonitor,measurements);

init({ok,Monitor},measurements) ->
  run_server(Monitor);

init(Error,ErrorDatabase) ->
  {Error,ErrorDatabase}.

%%% Database part:
get_stations_from_database(Monitor) ->
  {ok,Ref} = pollution_server_odbc:connect(),
  UpdatedMonitor = case pollution_server_odbc:fetch_stations(Ref) of
    {ok, Columns,Values} -> get_stations_from_database(Monitor,Columns,Values);
    {error,Reason} -> {error,Reason}
  end,
  pollution_server_odbc:disconnect(Ref),
  UpdatedMonitor.

get_stations_from_database({error,Reason},_,_) ->
  {error,Reason};

get_stations_from_database({ok,Monitor},_,[]) ->
  {ok,Monitor};

get_stations_from_database({ok,Monitor},Columns,[CurrentValue|Tail]) ->
  {_,X,Y,Name} = CurrentValue,
  UpdatedMonitor = pollution:add_station(Name,{X,Y},Monitor),
  get_stations_from_database(UpdatedMonitor,Columns,Tail).

get_measurements_from_database(Monitor) ->
  {ok,Ref} = pollution_server_odbc:connect(),
  UpdatedMonitor = case pollution_server_odbc:fetch_measurements(Ref) of
              {ok, Columns,Values} -> get_measurements_from_database(Monitor,Columns,Values);
              Error -> Error
            end,
  pollution_server_odbc:disconnect(Ref),
  UpdatedMonitor.

get_measurements_from_database({error,Reason},_,_) ->
  {error,Reason};

get_measurements_from_database({ok,Monitor},_,[]) ->
  {ok,Monitor};

get_measurements_from_database({ok,Monitor},Columns,[CurrentValue|Tail]) ->
  {_,_,_,_,StationID} = CurrentValue,
  Location = pollution_server_odbc:get_station_by_id(StationID),
  get_measurements_from_database(Location,Columns,CurrentValue,Tail,Monitor).

get_measurements_from_database({error,Reason},_,_,_,Monitor)->
  {error,Reason,Monitor};

get_measurements_from_database({ok,Location},Columns,CurrentElement,Tail,Monitor)->
  {_,Type,Value,Date,_} = CurrentElement,
  ConvertedTime = pollution_server_odbc:convert_string_to_time(Date),
  UpdatedMonitor = pollution:addValue(Location,ConvertedTime,Type,Value,Monitor),
  get_measurements_from_database(UpdatedMonitor,Columns,Tail).

%%%End of database part

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
    {getMonitor,Pid,[]} ->
      send_monitor(Pid,{ok,Monitor},{ok,Monitor});

    {add_station,Pid,[Name,{_,_}=Location]} ->
      UpdatedMonitor = pollution:add_station(Name,Location,Monitor),

      {ok,Ref} = pollution_server_odbc:connect(),
      case UpdatedMonitor of
        {ok,_} -> pollution_server_odbc:add_station(Ref,Name,Location);
        _ -> error
      end,
      pollution_server_odbc:disconnect(Ref),

      send_monitor(Pid,UpdatedMonitor,Monitor);

    {addValue,Pid,[{_,_} = Location,Time,Type,Value]} ->
      UpdatedMonitor = pollution:addValue(Location,Time,Type,Value,Monitor),

      {ok,Ref} = pollution_server_odbc:connect(),
      case UpdatedMonitor of
        {ok,_} -> pollution_server_odbc:add_value(Ref,Location,Time,Type,Value);
        _ -> error
      end,
      pollution_server_odbc:disconnect(Ref),

      send_monitor(Pid,UpdatedMonitor,Monitor);

    {removeValue,Pid,[{_,_} = Location,Date,Type]} ->
      UpdatedMonitor = pollution:removeValue(Monitor,Location,Date,Type),

      {ok,Ref} = pollution_server_odbc:connect(),
      case UpdatedMonitor of
        {ok,_} -> pollution_server_odbc:remove_value(Ref,Location,Date,Type);
        _ -> error
      end,
      pollution_server_odbc:disconnect(Ref),

      send_monitor(Pid,UpdatedMonitor,Monitor);

    {removeValue,Pid,[Name,Date,Type]} ->
      UpdatedMonitor = pollution:removeValue(Monitor,Name,Date,Type),

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

    {crash,_,_} ->
      1/0;

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

getMonitor() ->
  send_request(getMonitor,[]).

stop() ->
  send_request(stop,[]).

crash() ->
  send_request(crash,[]).