%%%-------------------------------------------------------------------
%%% @author marcinaman
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Apr 2018 10:48 PM
%%%-------------------------------------------------------------------
-module(tests).
-author("marcinaman").
-include_lib("eunit/include/eunit.hrl").
%% API
-compile(export_all).
-include("header.hrl").

create_monitor_test() ->
  {ok, {monitor, #{}, #{}}} = pollution:create_monitor().
%if error it will automatically rage at us that he cant find a pattern

add_station_test() ->
  {ok,Monitor} = pollution:create_monitor(),

  SomeStation = #station{name="SomeStation",location={10.0,20.0}},
  {ok,MonitorSome} = pollution:add_station("SomeStation",{10.0,20.0},Monitor),
  ?assertEqual(maps:get("SomeStation",MonitorSome#monitor.locationStations),SomeStation),
%Add a second one?
  OtherStation = #station{name="OtherStation",location={10.0,21.0}},
  {ok,MonitorOther} = pollution:add_station("OtherStation",{10.0,21.0},MonitorSome),
  ?assertEqual(maps:get("OtherStation",MonitorOther#monitor.locationStations),OtherStation),

%errors:
  ?assertMatch({error,"Station used"},pollution:add_station(OtherStation#station.name,OtherStation#station.location,MonitorOther)),
  ?assertMatch({error,"Station used"},pollution:add_station(OtherStation#station.name,{5.0,11.0},MonitorOther)),
  ?assertMatch({error,"Station used"},pollution:add_station("Some other name",OtherStation#station.location,MonitorOther)).


add_value_test() ->
  {ok,P} = pollution:create_monitor(),
  {ok,P1} = pollution:add_station("Some",{10.0,20.0},P),

  Time = calendar:local_time(),
  {ok,P2} = pollution:addValue({10.0,20.0},Time,pm10,10,P1),
  ?assertEqual(#monitor{
    locationNames = #{{10.0,20.0}=>"Some"},
    locationStations = #{"Some" => #station{
      name="Some",
      location = {10.0,20.0},
      measurements = #{
        {Time,pm10} => #measurement{
          type = pm10,
          value = 10,
          time = Time
        }
      }
    }}
  },P2),

  ?assertMatch({error,"Wrong parameters passed to add"},pollution:addValue(10.0,Time,pm10,10,P1)),
  ?assertMatch({error,_},pollution:addValue({10.0,21.0},Time,pm10,10,P1)),
  ?assertMatch({error,_},pollution:addValue({10.0,20.0},Time,pm10,10,P2)).

remove_value_test() ->
  {ok,P} = pollution:create_monitor(),
  {ok,P1} = pollution:add_station("Some",{10.0,20.0},P),

  Time = calendar:local_time(),
  {ok,P2} = pollution:addValue({10.0,20.0},Time,pm10,10,P1),
  {ok,P3} = pollution:removeValue(P2,{10.0,20.0},Time,pm10),
  {ok,P4} = pollution:removeValue(P2,"Some",Time,pm10),

  ?assertEqual(#monitor{
    locationNames = #{{10.0,20.0}=>"Some"},
    locationStations = #{"Some" => #station{
      name="Some",
      location = {10.0,20.0},
      measurements = #{
      }
    }}
  },P3),

  ?assertEqual(#monitor{
    locationNames = #{{10.0,20.0}=>"Some"},
    locationStations = #{"Some" => #station{
      name="Some",
      location = {10.0,20.0},
      measurements = #{
      }
    }}
  },P4).

