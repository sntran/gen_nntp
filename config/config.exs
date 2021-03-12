use Mix.Config

config :logger, :console,
  format: "$time $metadata[$level] $levelpad$message\n"

if Mix.env == :dev do
  config :mix_test_watch,
    clear: true
end
