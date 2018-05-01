%%%-------------------------------------------------------------------
%%% @author marcinaman
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Apr 2018 9:46 PM
%%%-------------------------------------------------------------------
-module(pollution).
-author("marcinaman").

-include("header.hrl").

-export([
  create_monitor/0,
  add_station/3,
  addValue/5,
  removeValue/4,
  getOneValue/3,
  getStationMean/3,
  getDailyMean/3,
  getOverLimit/2
]).


create_monitor() ->
  {ok, #monitor{}}.

%%Adding station

add_station(Name,{X,Y} = Location,Monitor) when is_float(X), is_float(Y)->
  CheckedIfNameUsed = check_if_ok(maps:find(Location,Monitor#monitor.locationNames)),
  CheckIfLocationUsed = check_if_ok(maps:find(Name,Monitor#monitor.locationStations)),
  add_checked_station(CheckedIfNameUsed,CheckIfLocationUsed,Monitor,Name,Location);

add_station(_,_,_) ->
  {error,"You have not filled coords with floats!"}.


check_if_ok({ok,_}) -> true;
check_if_ok(error) -> false.

add_checked_station(false,false, Monitor, Name, {_,_} = Location)->
  {ok,#monitor{
    locationNames = maps:put(Location,Name,Monitor#monitor.locationNames),
    locationStations = maps:put(Name,
      #station{name=Name,location = Location},
      Monitor#monitor.locationStations)
  }};

add_checked_station(_,_,_,_,_) ->
  {error,"Station used"}.

%%Adding value:

addValue({_,_} = Location, Time, Type, Value,Monitor) when is_tuple(Time) ->
  CheckIfLocationUsed = check_if_ok(maps:find(Location,Monitor#monitor.locationNames)),
  addValue(CheckIfLocationUsed,Location,Time,Type,Value,Monitor);

addValue(_,_,_,_,_) ->
  {error,"Wrong parameters passed to add"}.

% location was used
addValue(true,Location,Time,Type,Value,Monitor) ->
  Found_Location_Name = maps:get(Location,Monitor#monitor.locationNames,""),
  Found_Location = maps:get(Found_Location_Name,Monitor#monitor.locationStations,false),
  addValue(true,check_if_same_measurement(Type,Value,Time,Found_Location),Location,Time,Type,Value,Monitor);

addValue(false,_,_,_,_,_) ->
  {error,"Location not found"}.

addValue(true,false,Location,Time,Type,Value,Monitor) ->
  Found_Location_Name = maps:get(Location,Monitor#monitor.locationNames,""),
  Found_Location = maps:get(Found_Location_Name,Monitor#monitor.locationStations,false),
  {ok,add_value_no_check(Monitor,Found_Location,Time,Type,Value)};

addValue(true,true,_,_,_,_,_)->
  {error,"Duplicate measurements"}.

check_if_same_measurement(Type,_,Time,Monitor) ->
  check_if_ok(maps:find({Time,Type},Monitor#station.measurements)).


add_value_no_check(Monitor, Station, Time, MeasurementType, Value) ->
  Monitor#monitor{
    locationStations = maps:put(
      Station#station.name,
      Station#station{measurements = maps:put(
        {Time, MeasurementType}, #measurement{
          type = MeasurementType,
          value = Value,
          time = Time},
        Station#station.measurements)},
      Monitor#monitor.locationStations)}.



removeValue(Monitor,{_,_}=Location,Date,Type) ->
  removeValue(Monitor,maps:get(Location,Monitor#monitor.locationNames,false),Date,Type);

removeValue(_,false,_,_) ->
  {error, "No such station"};

removeValue(Monitor,Name,Date,Type) ->
  removeValue(check_if_ok(maps:find(Name,Monitor#monitor.locationStations)),
    maps:find(Name,Monitor#monitor.locationStations),Name,Date,Type,Monitor).


removeValue(true,{ok,Station},Name,Date,Type,Monitor) ->
  removeValue(true,check_if_ok(maps:find({Date,Type},Station#station.measurements)),
    Station,Name,Date,Type,Monitor);

removeValue(false,_,_,_,_,_) ->
  {error,"Station not found"}.

removeValue(true,true,Station,_,Date,Type,Monitor) ->
  {ok,Monitor#monitor{
    locationStations = maps:update(
      Station#station.name,
      Station#station{
        measurements = maps:remove(
          {Date, Type},
          Station#station.measurements)},
      Monitor#monitor.locationStations)}};


removeValue(true,false,_,_,_,_,_) ->
  {error,"No measurement found"}.

getOneValue(Type,Time,Station) ->
  checkValue(maps:get({Time,Type},Station#station.measurements,false)).

checkValue(false) -> {error,"Value not found"};

checkValue(#measurement{type = _,value = Value,time = _}) -> {ok,Value}.

getStationMean(Monitor,Type,{_,_}=StationCoords) ->
  getStationMean(Monitor,Type,maps:get(StationCoords,Monitor#monitor.locationNames));

getStationMean(Monitor,Type,StationName) ->
  getStationMean(maps:get(StationName,Monitor#monitor.locationStations),Type).

getStationMean(Station,Type) ->
  Values = maps:values(Station#station.measurements),
  Filtered = lists:filter(
    fun(Elem) -> (Elem#measurement.type == Type) end,Values),
  lists:foldl(fun(X,Sum) -> X#measurement.value + Sum end,0,Filtered)
    /length(Filtered).

getDailyMean(Monitor,Type,{_,_,_}=Day) ->
  Stations = maps:values(Monitor#monitor.locationStations),
  Measurements = lists:foldl(
    fun(X,Acc) -> maps:values(X#station.measurements)++Acc end,[],Stations),
  Sum = lists:foldl(
    fun(X,Acc) when element(1,X#measurement.time) == Day andalso X#measurement.type == Type -> X#measurement.value+Acc;
      (_,Acc) -> Acc end,0,Measurements
  ),
  Amount = lists:foldl(
    fun(X,Acc) when element(1,X#measurement.time) == Day andalso X#measurement.type == Type -> 1+Acc;
      (_,Acc) -> Acc end,0,Measurements
  ),
  Sum/Amount.

getOverLimit(Monitor,Hour) ->
  Stations = maps:values(Monitor#monitor.locationStations),
  Measurements = lists:foldl(
    fun(X,Acc) -> maps:values(X#station.measurements)++Acc end,[],Stations),

  MeasurementsFiltered = lists:filter(
    fun(Elem) ->
      (Elem#measurement.type == pm10 andalso Elem#measurement.value > 50) or
        (Elem#measurement.type == pm25 andalso Elem#measurement.value < 30) end,Measurements),

  lists:foldl(
    fun(X,Sum) when element(1,element(2,X#measurement.time)) == Hour -> Sum+1;
      (_,Acc) -> Acc end, 0, MeasurementsFiltered).