%%%-------------------------------------------------------------------
%%% @author woolfy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. May 2018 10:58 PM
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("marcinaman").
-include_lib("eunit/include/eunit.hrl").
%% API
-compile(export_all).
-include("header.hrl").

add_station_test() ->
  pollution_server:start(),
  P1 = {ok,{monitor,#{{10.0,20.0} => "Some"},#{"Some" => {station,"Some",{10.0,20.0},#{}}}}},
  ?assertMatch(P1,pollution_server:add_station("Some",{10.0,20.0})),
  P2 = {error,"Station used",{monitor,#{{10.0,20.0} => "Some"},#{"Some" => {station,"Some",{10.0,20.0},#{}}}}},
  ?assertMatch(P2,pollution_server:add_station("Some",{10.0,20.0})),
  P3 = {ok,{monitor,#{{10.0,20.0} => "Some",{11.0,22.0} => "Some2"},#{"Some" => {station,"Some",{10.0,20.0},#{}},"Some2" => {station,"Some2",{11.0,22.0},#{}}}}},
  ?assertMatch(P3,pollution_server:add_station("Some2",{11.0,22.0})),
  pollution_server:stop().

add_value_test() ->
  pollution_server:start(),
  pollution_server:add_station("Some",{10.0,20.0}),
  P1 = {ok,{monitor,#{{10.0,20.0} => "Some"},#{"Some" =>{station,"Some",{10.0,20.0},#{{{{2018,5,14},{12,46,59}},pm10} => {measurement,pm10,10, {{2018,5,14},{12,46,59}}}}}}}},
  ?assertMatch(P1, pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,46,59}},pm10,10)),
  P2 = {error,"Duplicate measurements",{monitor,#{{10.0,20.0} => "Some"},#{"Some" =>{station,"Some",{10.0,20.0},#{{{{2018,5,14},{12,46,59}},pm10} => {measurement,pm10,10, {{2018,5,14},{12,46,59}}}}}}}},
  ?assertMatch(P2,pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,46,59}},pm10,10)),
  pollution_server:stop().

remove_value_test() ->
  pollution_server:start(),
  pollution_server:add_station("Some",{10.0,20.0}),
  pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,46,59}},pm10,10),
  pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,47,17}},pm20,20),
  P1 = {ok,{monitor,#{{10.0,20.0} => "Some"},#{"Some" =>{station,"Some",{10.0,20.0},#{{{{2018,5,14},{12,46,59}},pm10} =>{measurement,pm10,10, {{2018,5,14},{12,46,59}}}}}}}},
  ?assertMatch(P1,pollution_server:removeValue({10.0,20.0},{{2018,5,14},{12,47,17}},pm20)),
  P2 = {error,"No measurement found",
    {monitor,
      #{{10.0,20.0} => "Some"},
      #{"Some" =>
      {station,"Some",
        {10.0,20.0},
        #{{{{2018,5,14},{12,46,59}},pm10} =>
        {measurement,pm10,10,
          {{2018,5,14},{12,46,59}}}}}}}},
    ?assertMatch(P2,pollution_server:removeValue({10.0,20.0},{{2018,5,14},{12,47,17}},pm20)),
    pollution_server:stop().

get_one_value_test() ->
  pollution_server:start(),
  pollution_server:add_station("Some",{10.0,20.0}),
  pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,46,59}},pm10,10),
  pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,47,17}},pm20,20),

  P1 = {station,"Some",
    {10.0,20.0},
    #{{{{2018,5,14},{12,46,59}},pm10} =>
    {measurement,pm10,10,
      {{2018,5,14},{12,46,59}}}}},
  ?assertMatch({ok,10},pollution_server:getOneValue(pm10,{{2018,5,14},{12,46,59}},P1)),
  ?assertMatch({error,"Value not found",_},pollution_server:getOneValue(pm10,{{2020,5,14},{12,46,59}},P1)),
  pollution_server:stop().

get_station_mean_test() ->
  pollution_server:start(),
  pollution_server:add_station("Some",{10.0,20.0}),
  pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,46,59}},pm10,10),
  pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,47,17}},pm10,20),

  ?assertMatch({ok,15.0},pollution_server:getStationMean(pm10,"Some")),
  ?assertMatch({ok,15.0},pollution_server:getStationMean(pm10,{10.0,20.0})),
  pollution_server:stop().

get_daily_mean_test() ->
  pollution_server:start(),
  pollution_server:add_station("Some",{10.0,20.0}),
  pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,46,59}},pm10,10),
  pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,47,17}},pm10,20),
  pollution_server:add_station("Other",{22.0,33.0}),
  pollution_server:addValue({22.0,33.0},{{2018,5,10},{13,43,22}},pm10,30),
  pollution_server:addValue({22.0,33.0},{{2018,5,10},{13,44,22}},pm10,60),

  ?assertMatch({ok,15.0},pollution_server:getDailyMean(pm10,{2018,5,14})),
  pollution_server:stop().

get_over_limit_test() ->
  pollution_server:start(),
  pollution_server:add_station("Some",{10.0,20.0}),
  pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,46,59}},pm10,50),
  pollution_server:addValue({10.0,20.0},{{2018,5,14},{12,47,17}},pm10,20),
  pollution_server:add_station("Other",{22.0,33.0}),
  pollution_server:addValue({22.0,33.0},{{2018,5,10},{13,43,22}},pm25,30),
  pollution_server:addValue({22.0,33.0},{{2018,5,10},{13,44,22}},pm25,10),

  ?assertMatch({ok,1},pollution_server:getOverLimit(13)),

  pollution_server:stop().

