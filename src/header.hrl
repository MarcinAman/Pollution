%%%-------------------------------------------------------------------
%%% @author woolfy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. kwi 2018 11:54
%%%-------------------------------------------------------------------
-author("woolfy").

-record(monitor, {
  locationNames = #{},      %Location => Name
  locationStations = #{}}   %Name => station
).
-record(station, {
  name = "",
  location,
  measurements = #{}}). %indexed with tuple {Time,Type} => measurement
-record(measurement, {
  type,
  value,
  time}).