%%%-------------------------------------------------------------------
%%% @author marci
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2018 08:36
%%%-------------------------------------------------------------------
-module(pollution_server_odbc_tests).
-author("marci").

-include_lib("eunit/include/eunit.hrl").

convert_date_to_string_test() ->
  Date = {{2018,2,14},{14,0,25}},
  ?assertEqual("2018/2/14 14/0/25",pollution_server_odbc:convert_time_to_string(Date)),

  Date2 = {{2019,12,2},{2,0,0}},
  ?assertEqual("2019/12/2 2/0/0",pollution_server_odbc:convert_time_to_string(Date2)).

convert_string_to_time_test() ->
  Date = "2019/12/2 2/0/25",
  ?assertEqual({{2019,12,2},{2,0,25}},pollution_server_odbc:convert_string_to_time(Date)),

  Date2 = "2018/2/14 14/0/25",
  ?assertEqual({{2018,2,14},{14,0,25}},pollution_server_odbc:convert_string_to_time(Date2)).

add_stations_to_monitor_test() ->
  {_,Columns,Values} = {selected,[],
    [{1,10.0,20.0,"Stacja1"},
      {2,30.0,30.1,"Stacja2"},
      {3,40.0,50.0,"Stacja3"}]},

  Monitor = {ok,{monitor,#{{10.0,20.0} => "Stacja1",
    {30.0,30.1} => "Stacja2",
    {40.0,50.0} => "Stacja3"},
    #{"Stacja1" => {station,"Stacja1",{10.0,20.0},#{}},
      "Stacja2" => {station,"Stacja2",{30.0,30.1},#{}},
      "Stacja3" => {station,"Stacja3",{40.0,50.0},#{}}}}},
  Empty_monitor = pollution:create_monitor(),

  ?assertMatch(Monitor,pollution_server:get_stations_from_database(Empty_monitor,Columns,Values)).


