defmodule Parser do

  def benchmark do
    {getTime(&parse/0), getTime(&runServer/0),getTime(&getStationMean/0),getTime(&getDailyMean/0),getTime(&getOverLimit/0),getOverLimit}
  end

  def getStationMean do
    :pollution_server.getStationMean(:pm10,{20.06,49.986})
  end

  def getDailyMean do
    :pollution_server.getDailyMean(:pm10,{2017,5,3})
  end

  def getOverLimit do
    :pollution_server.getOverLimit(14)
  end

  def runServer do
    :pollution_server.start()

    parse()
    |> Enum.each(&handleAddingRecord/1)
  end

  def getTime function do
    function
    |> :timer.tc
    |> elem(0)
    |> Kernel./(1_000_000)
  end

  def handleAddingRecord record do
    :pollution_server.add_station(getRandomName(20),record.location)
    :pollution_server.addValue(record.location,record.datetime,:pm10,record.pollution)
  end

  def getRandomName length do
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" <> "0123456789"

    alphabet_length = alphabet |> String.length

    1..length
    |> Enum.map_join(
         fn(_) ->
           alphabet |> String.at( :random.uniform( alphabet_length ) - 1 )
         end
       )
  end

  defp get_range(length) when length > 1, do: (1..length)
  defp get_range(_), do: [1]

  defp do_randomizer(length, lists) do
    get_range(length)
    |> Enum.reduce([], fn(_, acc) -> [Enum.random(lists) | acc] end)
    |> Enum.join("")
  end


  def getRecords do
    File.read!("./pollution.csv")
    |> String.split("\r\n")
    |> Enum.map(fn x -> String.split(x,",")  end)
  end

  def mapRecords l do
    %{:datetime => {parseDate(Enum.at(l,0)),parseHour(Enum.at(l,1))},
    :location => parseCoords(Enum.at(l,2),Enum.at(l,3)),
    :pollution => parsePollution(Enum.at(l,4))}
  end

  def parseDate record do
    record
    |> String.split("-")
    |> Enum.reverse()
    |> parseDateTime
  end

  def parseDateTime record do
    record
    |> Enum.map(fn x -> String.to_integer(x) end)
    |> Enum.reduce({},fn value,acc -> Tuple.append(acc,value) end)
  end

  def parseHour record do
    record
    |> (fn x -> x <> ":00" end).()
    |> String.split(":")
    |> Enum.map(fn x-> String.to_integer(x) end)
    |> Enum.reduce({},fn value,acc -> Tuple.append(acc,value) end)
  end

  def parseCoords fst,sec do
    {String.to_float(fst),String.to_float(sec)}
  end

  def parsePollution record do
    String.to_integer record
  end

  def parse do
    getRecords() |> Enum.map(&mapRecords/1)
  end

end
