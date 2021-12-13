defmodule Router do
  use Plug.Router
  use Plug.Debugger
  require Logger

  plug(Plug.Logger, log: :debug)
  plug(Plug.Parsers, parsers: [:urlencoded])
  plug(:match)
  plug(:dispatch)

  @usage """
  Usage: Try querying some of the following endpoints...
    GET /
    GET /help
    GET /semiprime?number=<integer>
    GET /semiprimes?numbers=<comma-separated-integers>
  """

  get "/" do
    send_resp(conn, 200, "Welcome to Semiprimes Service!\n\n#{@usage}")
  end

  get "/help" do
    send_resp(conn, 200, @usage)
  end

  get "/semiprime" do
    case conn |> Map.get(:query_params) |> Map.get("number") do
      nil ->
        send_resp(conn, 400, "You must pass an integer as a query parameter. #{@usage}")

      val ->
        case Integer.parse(val) do
          {n, ""} ->
            send_resp(conn, 200, semiprime_response(n))

          _ ->
            send_resp(conn, 400, "We could not parse the number you provided.\n\n#{@usage}")
        end
    end
  end

  get "/semiprimes" do
    case conn |> Map.get(:query_params) |> Map.get("numbers") do
      nil ->
        send_resp(
          conn,
          400,
          "You must pass a comma-separated list of integers as a query parameter.\n\n#{@usage}"
        )

      xs ->
        response =
          xs
          |> String.split(",")
          |> Stream.map(&Integer.parse/1)
          |> Stream.filter(fn
            {n, ""} -> true
            _ -> false
          end)
          |> Stream.map(fn {n, ""} -> semiprime_response(n) end)
          |> Enum.join("\n")

        send_resp(conn, 200, response)
    end
  end

  match _ do
    send_resp(conn, 404, "Not found.")
  end

  ################################################################################
  # Utils
  ################################################################################

  defp semiprime_response(n) do
    case Server.semiprime(n) do
      nil ->
        "#{n} is not a semiprime. Try another number!"

      {hit_or_miss, factors} ->
        response = "#{n} is a semiprime! Its factors are #{Enum.join(factors, " and ")}."
        "Cache #{Atom.to_string(hit_or_miss)} - #{response}"
    end
  end
end
