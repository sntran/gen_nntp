defmodule GenNNTP.TestCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      import GenNNTP.TestCase
    end
  end

  setup tags do
    # So the callbacks can send message to this test process.
    Process.register(self(), :tester)

    unless tags[:async] do
    end

    :ok
  end

  def setup_articles(context) do
    articles = context[:articles] || [
      %{
        id: "<45223423@example.com>",
        headers: %{
          "Path" => "pathost!demo!whitehouse!not-for-mail",
          "From" => "'Demo User' <nobody@example.net>",
          "Newsgroups" => "misc.test",
          "Subject" => "I am just a test article",
          "Date" => "6 Oct 1998 04:38:40 -0500",
          "Organization" => "An Example Net, Uncertain, Texas",
          "Message-ID" => "<45223423@example.com>"
        },
        body: "This is just a test article."
      },
      %{
        id: "<4320003@example.com>",
        headers: %{
          "Path" => "pathost!demo!whitehouse!not-for-mail",
          "From" => "'Demo User' <nobody@example.net>",
          "Newsgroups" => "misc.test",
          "Subject" => "I am just a test article",
          "Date" => "6 Oct 1998 04:38:40 -0500",
          "Organization" => "An Example Net, Uncertain, Texas",
          "Message-ID" => "<4320003@example.com>"
        },
        body: "This is just a test article."
      }
    ]

    [articles: articles]
  end

  def setup_groups(context) do
    [groups: context[:groups] || [
        {
          "example.empty.newsgroup",
          0, # Estimated number of articles in the group
          0, # Article number of the first article in the group
          0, # Article number of the last article in the group
          [],
        },
        {
          "misc.test",
          2000, # Estimated number of articles in the group
          3000234, # Article number of the first article in the group
          3002322, # Article number of the last article in the group
          [
            3000234,
            3000237,
            3000238,
            3000239,
            3002322,
          ],
        }
      ]
    ]
  end

  def setup_group_articles(context) do
    [group_articles: context[:group_articles] || [
        {{3000234, "misc.test"}, "<4320003@example.com>"},
        {{3000237, "misc.test"}, "<7320003@example.com>"},
        {{3000238, "misc.test"}, "<8320003@example.com>"},
        {{3000239, "misc.test"}, "<45223423@example.com>"},
        {{3002322, "misc.test"}, "<2232003@example.com>"},
      ]
    ]
  end

  def setup_CAPABILITIES(context) do
    [handle_CAPABILITIES: fn(state) ->
      Kernel.send(:tester, {:called_back, :handle_CAPABILITIES, 1})
      {:ok, context[:capabilities] || [], state}
    end]
  end

  def setup_GROUP(context) do
    [handle_GROUP: fn(group, state) ->
      Kernel.send(:tester, {:called_back, :handle_GROUP, 2})

      case List.keyfind(context[:groups], group, 0, false) do
        false -> {:ok, false, state}
        # Removes the group's article numbers.
        group -> {:ok, Tuple.delete_at(group, 4), state}
      end
    end]
  end

  def setup_LISTGROUP(context) do
    [handle_LISTGROUP: fn(group, state) ->
      Kernel.send(:tester, {:called_back, :handle_LISTGROUP, 2})

      {:ok, List.keyfind(context[:groups], group, 0, false), state}
    end]
  end

  def setup_ARTICLE(context) do
    articles = context[:articles]
    group_articles = context[:group_articles]

    # Little helper
    get_article = fn(message_id, article_number) ->
      case Enum.find(articles, false, &(match_id(&1, message_id))) do
        false -> false
        article -> {article_number, article}
      end
    end

    [handle_ARTICLE: fn
      # Requests article by message_id.
      (message_id, state) when is_binary(message_id) ->
        Kernel.send(:tester, {:called_back, :handle_ARTICLE, message_id})
        {:ok, get_article.(message_id, 0), state}

      # Requests article by article number
      ({article_number, group}, state) when is_integer(article_number) ->
        Kernel.send(:tester, {:called_back, :handle_ARTICLE, article_number})

        case List.keyfind(group_articles, {article_number, group}, 0, false) do
          false ->
            {:ok, false, state}
          {_, message_id} ->
            {:ok, get_article.(message_id, article_number), state}
        end
    end]
  end

  def setup_HEAD(context) do
    articles = context[:articles]
    group_articles = context[:group_articles]

    # Little helper
    get_article = fn(message_id, article_number) ->
      case Enum.find(articles, false, &(match_id(&1, message_id))) do
        false -> false
        # Returns with the article map without the body.
        article -> {article_number,  Map.delete(article, :body)}
      end
    end

    [handle_HEAD: fn
      # Requests article by message_id.
      (message_id, state) when is_binary(message_id) ->
        Kernel.send(:tester, {:called_back, :handle_HEAD, message_id})
        {:ok, get_article.(message_id, 0), state}

      # Requests article by article number
      ({article_number, group}, state) when is_integer(article_number) ->
        Kernel.send(:tester, {:called_back, :handle_HEAD, article_number})

        case List.keyfind(group_articles, {article_number, group}, 0, false) do
          false ->
            {:ok, false, state}
          {_, message_id} ->
            {:ok, get_article.(message_id, article_number), state}
        end
    end]
  end

  def setup_BODY(context) do
    articles = context[:articles]
    group_articles = context[:group_articles]

    # Little helper
    get_article = fn(message_id, article_number) ->
      case Enum.find(articles, false, &(match_id(&1, message_id))) do
        false -> false
        # Returns with the article map without the headers.
        article -> {article_number,  Map.delete(article, :headers)}
      end
    end

    [handle_BODY: fn
      # Requests article by message_id.
      (message_id, state) when is_binary(message_id) ->
        Kernel.send(:tester, {:called_back, :handle_BODY, message_id})
        {:ok, get_article.(message_id, 0), state}

      # Requests article by article number
      ({article_number, group}, state) when is_integer(article_number) ->
        Kernel.send(:tester, {:called_back, :handle_BODY, article_number})

        case List.keyfind(group_articles, {article_number, group}, 0, false) do
          false ->
            {:ok, false, state}
          {_, message_id} ->
            {:ok, get_article.(message_id, article_number), state}
        end
    end]
  end

  def setup_STAT(context) do
    articles = context[:articles]
    group_articles = context[:group_articles]

    # Little helper
    get_article = fn(message_id, article_number) ->
      case Enum.find(articles, false, &(match_id(&1, message_id))) do
        false -> false
        # Returns with the article map without the headers and body.
        article -> {article_number,  Map.drop(article, [:headers, :body])}
      end
    end

    [handle_STAT: fn
      # Requests article by message_id.
      (message_id, state) when is_binary(message_id) ->
        Kernel.send(:tester, {:called_back, :handle_STAT, message_id})
        {:ok, get_article.(message_id, 0), state}

      # Requests article by article number
      ({article_number, group}, state) when is_integer(article_number) ->
        Kernel.send(:tester, {:called_back, :handle_STAT, article_number})

        case List.keyfind(group_articles, {article_number, group}, 0, false) do
          false ->
            {:ok, false, state}
          {_, message_id} ->
            {:ok, get_article.(message_id, article_number), state}
        end
    end]
  end

  def setup_server(context) do
    TestNNTPServer.start(context)
    :ok
  end

  def setup_socket(_context) do
    {:ok, socket, greeting} = GenNNTP.connect()

    %{socket: socket, greeting: greeting}
  end

  def match_id(%{id: id}, id), do: true
  def match_id(_, _), do: false
end
