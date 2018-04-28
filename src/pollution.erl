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
  removeValue/4
%%  ,getStationMean/3
]).


create_monitor() ->
  {ok, #monitor{}}.

%%Adding station

add_station(Name,{X,Y} = Location,Monitor) when is_float(X), is_float(Y)->
  CheckedIfNameUsed = check_if_ok(maps:find(Location,Monitor#monitor.locationNames)),
  CheckIfLocationUsed = check_if_ok(maps:find(Name,Monitor#monitor.locationStations)),
%%  io:format("Name: ~s Location: ~s ~n",[CheckedIfNameUsed,CheckIfLocationUsed]),
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

check_if_same_measurement(Type,Value,Time,Monitor) ->
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
  removeValue(true,check_if_ok(maps:find({Date,Type},Station#station.measurements)),Station,Name,Date,Type,Monitor);

removeValue(false,_,_,_,_,_) ->
  {error,"Station not found"}.

removeValue(true,true,Station,Name,Date,Type,Monitor) ->
  {ok,Monitor#monitor{
    locationStations = maps:update(
      Station#station.name,
      Station#station.location,
      Station#station{
        measurements = maps:remove(
        {Date,Type},Station#station.measurements
      )
      }
    ,Monitor#monitor.locationStations)}};

removeValue(true,false,_,_,_,_,_) ->
  {error,"No measurement found"}.

getOneValue(Type,Time,Station) ->
  checkValue(maps:find({Time,Type},Station#station.measurements)).

checkValue({ok,#measurement{value = Value}}) -> Value;
checkValue(_) -> {error,"Value not found"}.