defmodule GenNNTPTest do
  use GenNNTP.TestCase, async: true
  doctest GenNNTP

  setup do
    on_exit(fn ->
      GenNNTP.stop(TestNNTPServer)
    end)

    :ok
  end

  describe "start/3" do

    test "spawns a process" do
      {:ok, pid} = GenNNTP.start(TestNNTPServer, [], [])
      assert is_pid(pid)
    end

    test "can still be called if already started" do
      {:ok, pid} = GenNNTP.start(TestNNTPServer, [], [])
      assert is_pid(pid)
    end

  end

  describe "stop/1" do

    test "stops the server by name" do
      {:ok, pid} = GenNNTP.start(TestNNTPServer, [], [])
      assert Process.alive?(pid)
      GenNNTP.stop(TestNNTPServer)
      refute Process.alive?(pid)
    end

    test "does not raise error if server is already stopped" do
      {:ok, _pid} = GenNNTP.start(TestNNTPServer, [], [])
      GenNNTP.stop(TestNNTPServer)
      GenNNTP.stop(TestNNTPServer)
    end

  end

  describe "connect/3" do

    @port String.to_integer(System.get_env("PORT", "119"))

    test "connects to a NNTP server" do
      GenNNTP.start(TestNNTPServer, [], [])
      assert {:ok, _socket, _greeting} = GenNNTP.connect("localhost", @port, [])
    end

    test "error if fail to connect to NNTP server" do
      assert {:error, :econnrefused} = GenNNTP.connect("localhost", @port, [])
    end

    test "receives a greeting after connecting" do
      GenNNTP.start(TestNNTPServer, [], [])
      assert {:ok, _socket, greeting} = GenNNTP.connect("localhost", @port, [])
      assert greeting =~ ~r/^20[0,1] /
    end

    test "connect/2 default to empty options" do
      GenNNTP.start(TestNNTPServer, [], [])
      assert {:ok, _socket, _greeting} = GenNNTP.connect("localhost", @port)
    end

    test "connect/1 default to port in PORT or 119" do
      GenNNTP.start(TestNNTPServer, [], [])
      assert {:ok, _socket, _greeting} = GenNNTP.connect("localhost")
    end

    test "connect/0 default to localhost" do
      GenNNTP.start(TestNNTPServer, [], [])
      assert {:ok, _socket, _greeting} = GenNNTP.connect()
    end

  end

  describe "@callback init/1" do

    test "is called when a client connects to it" do
      TestNNTPServer.start(
        init: fn args ->
          Kernel.send(:tester, {:called_back, :init, 1})
          {:ok, args}
        end
      )

      refute_receive(
        {:called_back, :init, 1},
        100,
        "@callback init/1 should not be called when server starts"
      )

      {:ok, _socket, _greeting} = GenNNTP.connect()

      assert_receive(
        {:called_back, :init, 1},
        100,
        "@callback init/1 was not called"
      )
    end

  end

  describe "@callback handle_CAPABILITIES/1" do

    setup context do
      capabilities = context[:capabilities] || []

      TestNNTPServer.start(
        handle_CAPABILITIES: fn(state) ->
          Kernel.send(:tester, {:called_back, :handle_CAPABILITIES, 1})
          {:ok, capabilities, state}
        end
      )

      {:ok, socket, _greeting} = GenNNTP.connect()

      unless context[:skip_command] do
        :ok = :gen_tcp.send(socket, "CAPABILITIES\r\n")
        {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)
      end

      %{socket: socket}
    end

    @tag skip_command: true
    test "is called when the client asks for it", %{socket: socket} do

      refute_receive(
        {:called_back, :handle_CAPABILITIES, 1},
        100,
        "@callback handle_CAPABILITIES/1 should not be called when client has not asked for it"
      )

      :ok = :gen_tcp.send(socket, "CAPABILITIES\r\n")

      assert_receive(
        {:called_back, :handle_CAPABILITIES, 1},
        100,
        "@callback handle_CAPABILITIES/1 was not called"
      )
    end

    @tag skip_command: true
    test "is responded with 101 code", %{socket: socket} do
      :ok = :gen_tcp.send(socket, "CAPABILITIES\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^101 /
    end

    @tag capabilities: ["VERSION 2", "READER", "IHAVE", "POST", "NEWNEWS", "HDR", "OVER", "LIST", "MODE-READER"]
    test "is responded with capabilities returned from the callback", %{socket: socket, capabilities: capabilities} do
      for capability <- capabilities do
        {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
        assert response === "#{capability}\r\n"
      end

      # Receives the termination line.
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === ".\r\n"

      # Should not receive any other message.
      assert {:error, :timeout} = :gen_tcp.recv(socket, 0, 100)
    end

    @tag capabilities: ["READER", "IHAVE", "POST", "NEWNEWS"]
    test "prepends with VERSION if not provided", %{socket: socket, capabilities: capabilities} do
      # Asserts that "VERSION" is always first in the response.
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^VERSION \d/

      for capability <- capabilities do
        {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
        assert response === "#{capability}\r\n"
      end

      # Receives the termination line.
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === ".\r\n"
    end

    @tag capabilities: ["READER", "IHAVE", "VERSION 1", "POST", "NEWNEWS"]
    test "moves VERSION to head", %{socket: socket, capabilities: capabilities} do
      # Should respond with "VERSION 1" from the callback's return.
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === "VERSION 1\r\n"

      # Then the rest of the capabilities, without "VERSION 1".
      for capability <- capabilities, !(capability =~ ~r/^VERSION \d/) do
        {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
        assert response === "#{capability}\r\n"
      end

      # Receives the termination line.
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === ".\r\n"
    end

    @tag capabilities: ["READER", "IHAVE", "AUTOUPDATE", "POST", "NEWNEWS"]
    test "only takes actual capabilities",  %{socket: socket, capabilities: capabilities} do
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^VERSION \d/

      # Should not respond with "AUTOUPDATE" since it's not standard.
      for capability <- capabilities, capability !== "AUTOUPDATE" do
        {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
        assert response === "#{capability}\r\n"
      end

      # Receives the termination line.
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === ".\r\n"
    end

  end

  describe "@callback handle_GROUP/2" do

    setup context do
      # Default to have "READER" capability for "GROUP" to work.
      capabilities = context[:capabilities] || ["READER"]
      groups = context[:groups] || [
        {
          "misc.test",
          0, # Estimated number of articles in the group
          0, # Article number of the first article in the group
          0 # Article number of the last article in the group
        }
      ]

      TestNNTPServer.start(
        handle_CAPABILITIES: fn(state) ->
          {:ok, capabilities, state}
        end,
        handle_GROUP: fn(group, state) ->
          Kernel.send(:tester, {:called_back, :handle_GROUP, 2})

          {:ok, List.keyfind(groups, group, 0, false), state}
        end
      )

      {:ok, socket, _greeting} = GenNNTP.connect()

      unless context[:skip_command] do
        :ok = :gen_tcp.send(socket, "GROUP misc.test\r\n")
        {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)
      end

      %{socket: socket}
    end

    @tag skip_command: true
    test "is called when the client asks for it", %{socket: socket} do
      refute_receive(
        {:called_back, :handle_GROUP, 2},
        100,
        "@callback handle_GROUP/2 should not be called when client has not asked for it"
      )

      :ok = :gen_tcp.send(socket, "GROUP misc.test\r\n")

      assert_receive(
        {:called_back, :handle_GROUP, 2},
        100,
        "@callback handle_GROUP/2 was not called"
      )
    end

    @tag skip_command: true
    test "responds with `211 number low high group` when the client asks for it", %{socket: socket} do
      :ok = :gen_tcp.send(socket, "GROUP misc.test\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^211 0 0 0 misc\.test/
    end

    # The setup sets a list of capabilities with "READER" by default, so we empty it here.
    @tag skip_command: true, capabilities: []
    test "is not called when there is no READER capability", %{socket: socket} do
      :ok = :gen_tcp.send(socket, "GROUP misc.test\r\n")

      refute_receive(
        {:called_back, :handle_GROUP, 2},
        100,
        "@callback handle_GROUP/2 should not be called when the server has no READER capability"
      )
    end

    @tag skip_command: true, capabilities: []
    test "responds with 411 when there is no READER capability", %{socket: socket} do
      :ok = :gen_tcp.send(socket, "GROUP misc.test\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      # @sntran: This is unclear to me, as the specs say nothing about this case.
      assert response =~ ~r/^411 /
    end

    @tag skip_command: true
    test "responds with 411 when the group specified is not available", %{socket: socket} do
      :ok = :gen_tcp.send(socket, "GROUP example.is.sob.bradner.or.barber\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^411 /
    end

  end

  describe "@callback handle_LISTGROUP/2" do

    setup context do
      # Default to have "READER" capability for "LISTGROUP" to work.
      capabilities = context[:capabilities] || ["READER"]
      groups = context[:groups] || [
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

      TestNNTPServer.start(
        handle_CAPABILITIES: fn(state) ->
          {:ok, capabilities, state}
        end,
        handle_LISTGROUP: fn(group, state) ->
          Kernel.send(:tester, {:called_back, :handle_LISTGROUP, 2})

          {:ok, List.keyfind(groups, group, 0, false), state}
        end
      )

      {:ok, socket, _greeting} = GenNNTP.connect()

      %{socket: socket, groups: groups}
    end

    test "is called when the client asks for it", %{socket: socket} do
      refute_receive(
        {:called_back, :handle_LISTGROUP, 2},
        100,
        "@callback handle_LISTGROUP/2 should not be called when client has not asked for it"
      )

      :ok = :gen_tcp.send(socket, "LISTGROUP misc.test\r\n")

      assert_receive(
        {:called_back, :handle_LISTGROUP, 2},
        100,
        "@callback handle_LISTGROUP/2 was not called"
      )
    end

    test "responds with `211 number low high group numbers` when the client asks for it", context do
      %{socket: socket, groups: groups} = context

      group_name = "misc.test"
      {^group_name, number, low, high, article_numbers} = List.keyfind(groups, group_name, 0, false)

      :ok = :gen_tcp.send(socket, "LISTGROUP #{group_name}\r\n")

      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^211 #{number} #{low} #{high} #{group_name}/

      # Article numbers, one per line.
      Enum.each(article_numbers, fn number ->
        {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
        assert response === "#{number}\r\n"
      end)

      # Then the termination line
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === ".\r\n"
    end

    # The setup sets a list of capabilities with "READER" by default, so we empty it here.
    @tag capabilities: []
    test "is not called when there is no READER capability", %{socket: socket} do
      :ok = :gen_tcp.send(socket, "LISTGROUP misc.test\r\n")

      refute_receive(
        {:called_back, :handle_LISTGROUP, 2},
        100,
        "@callback handle_LISTGROUP/2 should not be called when the server has no READER capability"
      )
    end

    @tag capabilities: []
    test "responds with 411 when there is no READER capability", %{socket: socket} do
      :ok = :gen_tcp.send(socket, "LISTGROUP misc.test\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      # @sntran: This is unclear to me, as the specs say nothing about this case.
      assert response =~ ~r/^411 /
    end

    test "responds with 411 when the group specified is not available", %{socket: socket} do
      :ok = :gen_tcp.send(socket, "LISTGROUP example.is.sob.bradner.or.barber\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^411 /
    end

    test "responds with 412 when no group specified and the currently selected newsgroup is invalid", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "LISTGROUP\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^412 /
    end

    test "responds with `211` when no group specified but currently selected newsgroup is valid", context do
      %{socket: socket, groups: groups} = context

      group_name = "misc.test"
      {^group_name, number, low, high, article_numbers} = List.keyfind(groups, group_name, 0, false)

      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "LISTGROUP\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^211 #{number} #{low} #{high} #{group_name}/

      # Article numbers, one per line.
      Enum.each(article_numbers, fn number ->
        {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
        assert response === "#{number}\r\n"
      end)

      # Then the termination line
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === ".\r\n"
    end

  end

  describe "@callback handle_ARTICLE/2" do

    setup context do
      # Default to have "READER" capability for "ARTICLE" to work.
      capabilities = context[:capabilities] || ["READER"]
      articles = context[:articles] || [
        {
          "<45223423@example.com>", # message ID.
          # Headers
          %{
            "Path" => "pathost!demo!whitehouse!not-for-mail",
            "From" => "'Demo User' <nobody@example.net>",
            "Newsgroups" => "misc.test",
            "Subject" => "I am just a test article",
            "Date" => "6 Oct 1998 04:38:40 -0500",
            "Organization" => "An Example Net, Uncertain, Texas",
            "Message-ID" => "<45223423@example.com>"
          },
          # Body
          "This is just a test article."
        }
      ]

      TestNNTPServer.start(
        handle_CAPABILITIES: fn(state) ->
          {:ok, capabilities, state}
        end,
        handle_ARTICLE: fn(message_id, state) ->
          Kernel.send(:tester, {:called_back, :handle_ARTICLE, 2})

          case List.keyfind(articles, message_id, 0, false) do
            false ->
              {:ok, false, state}
            article ->
              {:ok, {0, article}, state}
          end
        end
      )

      {:ok, socket, _greeting} = GenNNTP.connect()

      %{socket: socket, articles: articles}
    end

    test "is called when the client asks for it", %{socket: socket} do
      refute_receive(
        {:called_back, :handle_ARTICLE, 2},
        100,
        "@callback handle_ARTICLE/2 should not be called when client has not asked for it"
      )

      :ok = :gen_tcp.send(socket, "ARTICLE <45223423@example.com>\r\n")

      assert_receive(
        {:called_back, :handle_ARTICLE, 2},
        100,
        "@callback handle_ARTICLE/2 was not called"
      )
    end

    test "responds with `220 number message_id article` when the client asks for it", context do
      %{socket: socket, articles: articles} = context

      message_id = "<45223423@example.com>"
      :ok = :gen_tcp.send(socket, "ARTICLE #{message_id}\r\n")

      # The response code with number and message_id
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === "220 0 #{message_id}\r\n"

      {^message_id, headers, body} = List.keyfind(articles, message_id, 0, false)

      # Headers, one per line.
      Enum.each(headers, fn({header, content}) ->
        {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
        assert response === "#{header}: #{content}\r\n"
      end)

      # Then an empty line
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === "\r\n"

      # Then the body
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === "#{body}\r\n"

      # Then the termination line
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === ".\r\n"

    end

    # The setup sets a list of capabilities with "READER" by default, so we empty it here.
    @tag capabilities: []
    test "is not called when there is no READER capability", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "ARTICLE <45223423@example.com>\r\n")

      refute_receive(
        {:called_back, :handle_ARTICLE, 2},
        100,
        "@callback handle_ARTICLE/2 should not be called when the server has no READER capability"
      )
    end

    # The setup sets a list of capabilities with "READER" by default, so we empty it here.
    @tag capabilities: []
    test "responds with 430 when there is no READER capability", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "ARTICLE <45223423@example.com>\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      # @sntran: This is unclear to me, as the specs say nothing about this case.
      assert response =~ ~r/^430 /
    end

    test "responds with 430 when the article specified is not available", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "ARTICLE <i.am.not.there@example.com>\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^430 /
    end

  end

  describe "command/3" do

    setup context do
      capabilities = context[:capabilities] || ["READER", "IHAVE", "POST", "NEWNEWS", "HDR", "OVER", "LIST", "MODE-READER"]

      TestNNTPServer.start(
        handle_CAPABILITIES: fn(state) ->
          {:ok, capabilities, state}
        end
      )
      {:ok, socket, _greeting} = GenNNTP.connect()

      %{socket: socket, capabilities: capabilities}
    end

    test "sends QUIT command", %{socket: socket} do
      assert {:ok, response} = GenNNTP.command(socket, "QUIT", [])
      assert response =~ ~r/^205 /
    end

    test "command/2 default to empty arguments", %{socket: socket} do
      assert {:ok, response} = GenNNTP.command(socket, "QUIT")
      assert response =~ ~r/^205 /
    end

    test "handles multi-line response", context do
      %{socket: socket, capabilities: capabilities} = context

      assert {:ok, response} = GenNNTP.command(socket, "CAPABILITIES")
      assert response === String.trim("""
      101 Capability list:\r
      VERSION 2\r
      #{ Enum.join(capabilities, "\r\n")}
      """)
    end

  end

  describe "server interaction" do
    setup do
      TestNNTPServer.start()
      {:ok, socket, greeting} = GenNNTP.connect()
      %{socket: socket, greeting: greeting}
    end

    test "MUST send a 200 greeting", %{greeting: greeting} do
      assert greeting =~ ~r/^200 /
    end

    test "MUST send a 205 if the client sends QUIT", %{socket: socket} do
      :ok = :gen_tcp.send(socket, "QUIT\r\n")
      {:ok, data} = :gen_tcp.recv(socket, 0)
      assert data =~ ~r/^205 /
    end

  end

end
