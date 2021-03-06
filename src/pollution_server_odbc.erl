%%%-------------------------------------------------------------------
%%% @author Woolfy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jun 2018 11:59
%%%-------------------------------------------------------------------

%%odbc:param_query(ID2,"select id from Employees where job_title=? and gender=?",[{{sql_varchar,50},["Nurse"]},{{sql_varchar,50},["Female"]}]).
%%odbc:param_query(ID3,"select id from Employees where id=?",[{sql_integer,[50]}]).

-module(pollution_server_odbc).
-author("Woolfy").
-define(ConnectionString,"DSN=odbc-driver;UID=woolfy;PWD=1234").

-define(StationsTable,"CREATE TABLE Stations (
    id int IDENTITY(1,1) PRIMARY KEY,
    x float  NOT NULL,
    y float  NOT NULL,
    name varchar(50)  NOT NULL,
);").
-define(MeasurementsTable,"CREATE TABLE Measurements (
    id int IDENTITY(1,1) PRIMARY KEY,
    type varchar(10)  NOT NULL,
    value int  NOT NULL,
    date varchar(50)  NOT NULL,
    station_id int  NOT NULL,
);").
-define(AlterStationsMeasurements,"ALTER TABLE Measurements ADD CONSTRAINT Measurements_Stations
    FOREIGN KEY (station_id)
    REFERENCES Stations (id);").

%% API
-export([
  start/0,
  connect/0,
  create_stations/1,
  create_measurements/1,
  fetch_measurements/1,
  fetch_stations/1,
  add_value/5,
  add_station/3,
  remove_value/4,
  drop_tables_no_check/1,
  disconnect/1,
  test_all_methods/0,
  convert_time_to_string/1,
  convert_string_to_time/1,
  get_station_by_id/1,
  init/0,
  stop/0,
  setup_basic_data/0
]).

start() ->
  odbc:stop(),
  odbc:start().

init() ->
  start(),
  Ref = case connect() of
          {ok,R} -> R;
          Other -> Other
        end,
  init(Ref).

init({error,Reason}) ->
  {error,Reason};

init(Ref) ->
  init(Ref,create_stations(Ref),create_measurements(Ref)).

init(Ref,ok,ok) ->
  disconnect(Ref),
  ok;

init(Ref,{error,Reason_stations},{error,Reason_measurements}) ->
  disconnect(Ref),
  {error,Ref,Reason_stations,Reason_measurements};

init(Ref,{error,Reason},_) ->
  disconnect(Ref),
  {error,Ref,Reason};

init(Ref,_,{error,Reason})->
  disconnect(Ref),
  {error,Ref,Reason}.

connect()->
  odbc:connect(?ConnectionString,[]).

check_if_ok(ok) -> ok;

check_if_ok({updated,_}) ->
  ok;

check_if_ok(Else) ->
  Else.

create_stations(Rel) ->
  Create_result = case odbc:sql_query(Rel,"Select * from Stations") of
    {selected,_,_} -> ok;
    {error,_} -> odbc:sql_query(Rel,?StationsTable)
  end,
  check_if_ok(Create_result).

create_measurements(Rel) ->
  Create_result = case odbc:sql_query(Rel,"Select * from Measurements") of
    {selected,_,_} -> ok;
    {error,_} -> odbc:sql_query(Rel,?MeasurementsTable)
  end,
  check_if_ok(Create_result).

fetch_values(Ref,Query) ->
  case odbc:sql_query(Ref,Query) of
    {selected,Columns,Values} -> {ok,Columns,Values};
    {error,Reason} -> {error,Reason}
  end.

fetch_stations(Ref) ->
  fetch_values(Ref,"Select * from Stations").

fetch_measurements(Ref) ->
  fetch_values(Ref,"Select * from Measurements").

add_station(Ref,Name,{X,Y}) ->
  check_if_ok(
    odbc:param_query(Ref,"insert into Stations (x,y,name) values (?,?,?)",
    [
      {{sql_float,2},[X]},
      {{sql_float,2},[Y]},
      {{sql_varchar,50},[Name]}
    ])
  ).

add_value(Ref,{_,_} = Location, Time,Type,Value) ->
  case check_if_station_exists(Ref,Location) of
    {ok,none} -> {error,"No such station"};
    {ok,Index} ->  add_value_no_check(Ref,Time,Type,Value,Index);
    {error,Reason} -> {error,Reason}
  end.

add_value_no_check(Ref,Time,Type,Value,Index) ->
  case odbc:param_query(Ref,"insert into Measurements (type,date,value,station_id) values(?,?,?,?)",
    [
      {{sql_varchar,10},[Type]},
      {{sql_varchar,50},[convert_time_to_string(Time)]},
      {sql_integer,[Value]},
      {sql_integer,[Index]}
    ]) of
    {updated,_} -> ok;
    Other -> Other
  end.

check_if_station_exists(Ref,{X,Y}) ->
  case odbc:param_query(Ref,"select id from Stations where x=? and y=?",
    [
      {{sql_float,2},[X]},
      {{sql_float,2},[Y]}
    ]) of
    {selected, _ , []} -> {ok,none};
    {selected, _ , [{Index}]} -> {ok,Index};
    _ -> {error,"value error"}
  end.

remove_value(Ref,{_,_} = Location,Date,Type) ->
  case check_if_station_exists(Ref,Location) of
    {ok,none} -> {error,"No such station"};
    {ok,Index} -> remove_value_no_check(Ref,Date,Type,Index);
    Other -> Other
  end.

remove_value_no_check(Ref,Date,Type,Index) ->
  case odbc:param_query(Ref,"delete Measurements where station_id=? and date=? and type=?",
    [
      {sql_integer,[Index]},
      {{sql_varchar,50},[convert_time_to_string(Date)]},
      {{sql_varchar,10},[Type]}
    ]) of
    {updated,_} -> ok;
    Error -> Error
  end.

get_station_by_id(ID) ->
  {ok,Ref} = connect(),
  Location = case odbc:param_query(Ref,"select x,y from Stations where id=?",
    [{sql_integer,[ID]}]) of
    {selected,_,[{X,Y}]} -> {ok,{X,Y}};
    {error,Reason} -> {error,Reason}
  end,
  disconnect(Ref),
  Location.

convert_time_to_string({{YY,MM,DD},{HH,MI,SS}}) ->
  integer_to_list(YY)++"/"++integer_to_list(MM)++"/"++integer_to_list(DD)++" "++integer_to_list(HH)++"/"++integer_to_list(MI)++"/"++integer_to_list(SS).

convert_string_to_time(Value) ->
  Divided = string:tokens(Value," "),
  case lists:map(fun(A) -> parse_single_tuple(A) end,Divided) of
    [Head,Tail] -> {Head,Tail};
    _ -> error
  end.

parse_single_tuple(Value) ->
  Divided = string:tokens(Value,"/"),
  case lists:map(fun(A) -> string:to_integer(A) end,Divided) of
    [{Fst,[]},{Sec,[]},{Thr,[]}] -> {Fst,Sec,Thr};
    _ -> error
  end.

drop_tables_no_check(Ref) ->
  {
    check_if_ok(odbc:sql_query(Ref,"Drop table Measurements")),
    check_if_ok(odbc:sql_query(Ref,"Drop table Stations"))
  }.

test_all_methods() ->
  start(),
  {ok,Ref} = connect(),
  CreateStations = create_stations(Ref),
  CreateMeasurements = create_measurements(Ref),
  AddStation = add_station(Ref,"Stacja",{10.0,20.0}),
  Time = calendar:local_time(),
  AddValue = add_value(Ref,{10.0,20.0},Time,"PM10",20),
  RemoveValue = remove_value(Ref,{10.0,20.0},Time,"PM10"),
  {Ref,CreateStations,CreateMeasurements,AddStation,AddValue,RemoveValue}.

setup_basic_data() ->
  start(),
  {ok, Ref} = connect(),
  create_measurements(Ref),
  create_stations(Ref),
  S1 = add_station(Ref,"Stacja1",{10.0,20.0}),
  S2 = add_station(Ref,"Stacja2",{22.0,21.0}),
  V1 = add_value(Ref,{10.0,20.0},calendar:local_time(),"PM10",10),
  V2 = add_value(Ref,{22.0,21.0},calendar:local_time(),"PM20",20),
  disconnect(Ref),
  {S1,S2,V1,V2}.

disconnect(Ref) ->
  odbc:disconnect(Ref).

stop() ->
  odbc:stop().