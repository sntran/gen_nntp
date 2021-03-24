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

    setup [
      :setup_CAPABILITIES,
      :setup_server, :setup_socket
    ]

    setup context do
      socket = context[:socket]

      unless context[:skip_command] do
        :ok = :gen_tcp.send(socket, "CAPABILITIES\r\n")
        {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)
      end

      :ok
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
    # Default to have "READER" capability for "GROUP" to work.
    @describetag capabilities: ["READER"]

    setup [
      :setup_groups,
      :setup_CAPABILITIES, :setup_GROUP,
      :setup_server, :setup_socket
    ]

    setup context do
      socket = context[:socket]

      unless context[:skip_command] do
        :ok = :gen_tcp.send(socket, "GROUP misc.test\r\n")
        {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)
      end

      :ok
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
    test "responds with `211 number low high group` when the client asks for it", context do
      %{socket: socket, groups: groups} = context

      group_name = "misc.test"
      {^group_name, number, low, high, _} = List.keyfind(groups, group_name, 0, false)
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^211 #{number} #{low} #{high} #{group_name}/
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
    # Default to have "READER" capability for "LISTGROUP" to work.
    @describetag capabilities: ["READER"]

    setup [
      :setup_groups,
      :setup_CAPABILITIES, :setup_LISTGROUP,
      :setup_server, :setup_socket
    ]

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

  describe "@callback handle_NEXT/2" do
    # Default to have "READER" capability for "NEXT" to work.
    @describetag capabilities: ["READER"]

    setup [
      :setup_articles, :setup_groups, :setup_group_articles,
      :setup_CAPABILITIES, :setup_GROUP, :setup_NEXT,
      :setup_server, :setup_socket
    ]

    # The setup sets a list of capabilities with "READER" by default, so we empty it here.
    @tag capabilities: []
    test "is not called when there is no READER capability", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "NEXT\r\n")

      refute_receive(
        {:called_back, :handle_NEXT, 2},
        100,
        "@callback handle_NEXT/2 should not be called when the server has no READER capability"
      )
    end

    # The setup sets a list of capabilities with "READER" by default, so we empty it here.
    @tag capabilities: []
    test "responds with 412 when there is no READER capability", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "NEXT\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      # @sntran: This is unclear to me, as the specs say nothing about this case.
      assert response =~ ~r/^412 /
    end

    test "responds with 412 if currently selected newsgroup is invalid", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "NEXT\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^412 /
    end

    test "is called when the client asks for it and currently selected newsgroup is valid", context do
      %{socket: socket} = context

      refute_receive(
        {:called_back, :handle_NEXT, 2},
        100,
        "@callback handle_NEXT/2 should not be called when client has not asked for it"
      )

      group_name = "misc.test"
      # Calling "GROUP" should set the current article number to the first article in the group
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "NEXT\r\n")

      assert_receive(
        {:called_back, :handle_NEXT, 2},
        100,
        "@callback handle_NEXT/2 was not called"
      )
    end

    test "responds with 420 if current article number is invalid", context do
      %{socket: socket, groups: groups} = context

      # This is the case where currently selected newsgroup is valid, but it's empty.
      group_name = "example.empty.newsgroup"
      {^group_name, 0, 0, 0, _} = List.keyfind(groups, group_name, 0, false)

      # Calling "GROUP" should set the current article number to the first article in the group
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "NEXT\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^420 /
    end

    test "responds with `223 n message-id` of the next article", context do
      %{socket: socket, groups: groups, group_articles: group_articles} = context

      group_name = "misc.test"
      {^group_name, _estimate, low, _high, [low, next | _]} = List.keyfind(groups, group_name, 0)
      # Get the message id of the next article number.
      {_, message_id} = List.keyfind(group_articles, {next, group_name}, 0)

      # Calling "GROUP" should set the current article number to the first article in the group
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "NEXT\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^223 #{next} #{message_id} /
    end

    test "responds with `223 n message-id` of the next article after next", context do
      %{socket: socket, groups: groups, group_articles: group_articles} = context

      group_name = "misc.test"
      {^group_name, _estimate, low, _high, [low, _next, next | _]} = List.keyfind(groups, group_name, 0)
      # Get the message id of the next article number.
      {_, message_id} = List.keyfind(group_articles, {next, group_name}, 0)

      # Calling "GROUP" should set the current article number to the first article in the group
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "NEXT\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "NEXT\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^223 #{next} #{message_id} /
    end

    test "responds with 421 when current article number is already the last article of the newsgroup", context do
      %{socket: socket, groups: groups} = context

      group_name = "misc.test"
      {^group_name, _estimate, low, _high, [low | rest]} = List.keyfind(groups, group_name, 0)

      # Calling "GROUP" should set the current article number to the first article in the group
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      # Advance to the last article.
      Enum.each(rest, fn(_number) ->
        :ok = :gen_tcp.send(socket, "NEXT\r\n")
        {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
        assert response =~ ~r/^223 /
      end)

      # Because we have just switched group, the callback should return the same article number.
      :ok = :gen_tcp.send(socket, "NEXT\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^421 /
    end

  end

  describe "@callback handle_LAST/2" do
    # Default to have "READER" capability for "LAST" to work.
    @describetag capabilities: ["READER"]

    setup [
      :setup_articles, :setup_groups, :setup_group_articles,
      :setup_CAPABILITIES, :setup_GROUP, :setup_NEXT, :setup_LAST,
      :setup_server, :setup_socket
    ]

    # The setup sets a list of capabilities with "READER" by default, so we empty it here.
    @tag capabilities: []
    test "is not called when there is no READER capability", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "LAST\r\n")

      refute_receive(
        {:called_back, :handle_LAST, 2},
        100,
        "@callback handle_LAST/2 should not be called when the server has no READER capability"
      )
    end

    # The setup sets a list of capabilities with "READER" by default, so we empty it here.
    @tag capabilities: []
    test "responds with 412 when there is no READER capability", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "LAST\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      # @sntran: This is unclear to me, as the specs say nothing about this case.
      assert response =~ ~r/^412 /
    end

    test "responds with 412 if currently selected newsgroup is invalid", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "LAST\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^412 /
    end

    test "is called when the client asks for it and currently selected newsgroup is valid", context do
      %{socket: socket} = context

      refute_receive(
        {:called_back, :handle_LAST, 2},
        100,
        "@callback handle_LAST/2 should not be called when client has not asked for it"
      )

      group_name = "misc.test"
      # Calling "GROUP" should set the current article number to the first article in the group
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "LAST\r\n")

      assert_receive(
        {:called_back, :handle_LAST, 2},
        100,
        "@callback handle_LAST/2 was not called"
      )
    end

    test "responds with 420 if current article number is invalid", context do
      %{socket: socket, groups: groups} = context

      # This is the case where currently selected newsgroup is valid, but it's empty.
      group_name = "example.empty.newsgroup"
      {^group_name, 0, 0, 0, _} = List.keyfind(groups, group_name, 0, false)

      # Calling "GROUP" should set the current article number to the first article in the group
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "LAST\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^420 /
    end

    test "responds with 422 when current article number is already the first article of the newsgroup", context do
      %{socket: socket} = context

      group_name = "misc.test"
      # Calling "GROUP" should set the current article number to the first article in the group
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      # Because we have just switched group, the callback should return the same article number.
      :ok = :gen_tcp.send(socket, "LAST\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^422 /
    end

    test "responds with `223 n message-id` of the previous article", context do
      %{socket: socket, groups: groups, group_articles: group_articles} = context

      group_name = "misc.test"
      {^group_name, _estimate, low, _high, _numbers} = List.keyfind(groups, group_name, 0)
      # Get the message id of the next article number.
      {_, message_id} = List.keyfind(group_articles, {low, group_name}, 0)

      # Calling "GROUP" should set the current article number to the first article in the group
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      # Moves to the next article.
      :ok = :gen_tcp.send(socket, "NEXT\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      # "LAST" should move back to the first article.
      :ok = :gen_tcp.send(socket, "LAST\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^223 #{low} #{message_id} /
    end

  end

  describe "@callback handle_ARTICLE/2" do
    # Default to have "READER" capability for "ARTICLE" to work.
    @describetag capabilities: ["READER"]

    setup [
      :setup_articles, :setup_groups, :setup_group_articles,
      :setup_CAPABILITIES, :setup_GROUP, :setup_ARTICLE,
      :setup_server, :setup_socket
    ]

    test "is called when the client asks for it", %{socket: socket} do
      message_id = "<45223423@example.com>"

      refute_receive(
        {:called_back, :handle_ARTICLE, ^message_id},
        100,
        "@callback handle_ARTICLE/2 should not be called when client has not asked for it"
      )

      :ok = :gen_tcp.send(socket, "ARTICLE #{message_id}\r\n")

      assert_receive(
        {:called_back, :handle_ARTICLE, ^message_id},
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

      %{headers: headers, body: body} = Enum.find(articles, &(match_id(&1, message_id)))

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

    test "responds with 412 if argument is a number and the currently selected newsgroup is invalid", context do
      %{socket: socket} = context

      article_number = 3000239

      :ok = :gen_tcp.send(socket, "ARTICLE #{article_number}\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^412 /
    end

    test "is also called with article number argument and currently selected newsgroup is valid", context do
      %{socket: socket} = context

      article_number = 3000239
      group_name = "misc.test"

      # Calling "GROUP" to set the current group.
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      refute_receive(
        {:called_back, :handle_ARTICLE, ^article_number},
        100,
        "@callback handle_ARTICLE/2 should not be called when client has not asked for it"
      )

      :ok = :gen_tcp.send(socket, "ARTICLE #{article_number}\r\n")

      assert_receive(
        {:called_back, :handle_ARTICLE, ^article_number},
        100,
        "@callback handle_ARTICLE/2 was not called"
      )
    end

    test "also responds with `220 number message_id article` when the client uses number argument", context do
      %{socket: socket, group_articles: group_articles, articles: articles} = context

      article_number = 3000239
      group_name = "misc.test"
      {_, message_id} = List.keyfind(group_articles, {article_number, group_name}, 0)

      # Calling "GROUP" to set the current group.
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "ARTICLE #{article_number}\r\n")

      # The response code with number and message_id
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === "220 #{article_number} #{message_id}\r\n"

      %{headers: headers, body: body} = Enum.find(articles, &(match_id(&1, message_id)))

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

    test "responds with 423 if argument is a number and that article does not exist in the currently selected newsgroup", context do
      %{socket: socket} = context

      article_number = 9123212
      group_name = "misc.test"

      # Calling "GROUP" to set the current group.
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "ARTICLE #{article_number}\r\n")

      # The response code with number and message_id
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^423 /
    end

    test "responds with 412 if no argument specified and the currently selected newsgroup is invalid", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "ARTICLE\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^412 /
    end

    test "responds with the article indicated by the current article number in the currently selected newsgroup if no argument specified", context do
      %{socket: socket, groups: groups, group_articles: group_articles} = context

      group_name = "misc.test"
      {^group_name, _number, low, _high, _} = List.keyfind(groups, group_name, 0, false)

      # Calling "GROUP" should set the current article number to the first article in the group
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "ARTICLE\r\n")

      {{article_number, ^group_name}, message_id} = List.keyfind(group_articles, {low, group_name}, 0)

      # The response code with number and message_id of the first article in the group.
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === "220 #{article_number} #{message_id}\r\n"
    end

    test "responds with 420 if no argument specified and the current article number is invalid", context do
      %{socket: socket, groups: groups} = context

      # This is the case where currently selected newsgroup is valid, but it's empty.
      group_name = "example.empty.newsgroup"
      {^group_name, 0, 0, 0, _} = List.keyfind(groups, group_name, 0, false)

      # Calling "GROUP" should set the current article number to the first article in the group
      :ok = :gen_tcp.send(socket, "GROUP #{group_name}\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(socket, "ARTICLE\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^420 /
    end

  end

  describe "@callback handle_HEAD/2" do
    # Default to have "READER" capability for "HEAD" to work.
    @describetag capabilities: ["READER"]

    setup [
      :setup_articles, :setup_groups, :setup_group_articles,
      :setup_CAPABILITIES, :setup_GROUP, :setup_HEAD,
      :setup_server, :setup_socket
    ]

    test "responds with `221 number message_id article`", context do
      %{socket: socket, articles: articles} = context

      message_id = "<45223423@example.com>"
      :ok = :gen_tcp.send(socket, "HEAD #{message_id}\r\n")

      # The response code with number and message_id
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === "221 0 #{message_id}\r\n"

      %{headers: headers} = Enum.find(articles, &(match_id(&1, message_id)))

      # Headers, one per line.
      Enum.each(headers, fn({header, content}) ->
        {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
        assert response === "#{header}: #{content}\r\n"
      end)

      # Then the termination line
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === ".\r\n"

    end

  end

  describe "@acllback handle_BODY/2" do
    # Default to have "READER" capability for "BODY" to work.
    @describetag capabilities: ["READER"]

    setup [
      :setup_articles, :setup_groups, :setup_group_articles,
      :setup_CAPABILITIES, :setup_GROUP, :setup_BODY,
      :setup_server, :setup_socket
    ]

    test "responds with `222 number message_id article`", context do
      %{socket: socket, articles: articles} = context

      message_id = "<45223423@example.com>"
      :ok = :gen_tcp.send(socket, "BODY #{message_id}\r\n")

      # The response code with number and message_id
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === "222 0 #{message_id}\r\n"

      %{body: body} = Enum.find(articles, &(match_id(&1, message_id)))

      # Then the body
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === "#{body}\r\n"

      # Then the termination line
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === ".\r\n"

    end

  end

  describe "@acllback handle_STAT/2" do
    # Default to have "READER" capability for "STAT" to work.
    @describetag capabilities: ["READER"]

    setup [
      :setup_articles, :setup_groups, :setup_group_articles,
      :setup_CAPABILITIES, :setup_GROUP, :setup_STAT,
      :setup_server, :setup_socket
    ]

    test "responds with `223 number message_id article`", context do
      %{socket: socket} = context

      message_id = "<45223423@example.com>"
      :ok = :gen_tcp.send(socket, "STAT #{message_id}\r\n")

      # The response code with number and message_id
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === "223 0 #{message_id}\r\n"

      # No more response since this is not a multi-line.
      assert {:error, :timeout} = :gen_tcp.recv(socket, 0, 200)
    end

  end

  describe "@callback handle_POST/2" do
    @describetag capabilities: ["POST"]

    setup [
      :setup_CAPABILITIES, :setup_POST,
      :setup_server, :setup_socket,
    ]

    test "is not called right after POST", context do
      %{socket: socket} = context

      refute_receive(
        {:called_back, :handle_POST, 2},
        100,
        "@callback handle_POST/2 should not be called when client has not asked for it"
      )

      :ok = :gen_tcp.send(socket, "POST\r\n")

      refute_receive(
        {:called_back, :handle_POST, 2},
        100,
        "@callback handle_POST/2 should not be called when client has only sent POST"
      )
    end

    test "client receives 340 from server after POST", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "POST\r\n")

      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^340 /
    end

    # Resets the capabilities to not allow posting.
    @tag capabilities: []
    test "client receives 440 when the server does not allow posting", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "POST\r\n")

      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^440 /
    end

    test "is called when the client finishes sending article", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "POST\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      :ok = :gen_tcp.send(
        socket,
        Enum.join([
          "Message-ID: <test@post>",
          "From: \"Demo User\" <nobody@example.net>",
          "Newsgroups: misc.test",
          "Subject: I am just a test article",
          "Organization: An Example Net",
          "",
          "This is just a test article.",
          ".",
          "",
        ], "\r\n")
      )

      assert_receive(
        {:called_back, :handle_POST, _},
        100,
        "@callback handle_POST/2 was not called"
      )
    end

    test "is called with an article map", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "POST\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      headers = %{
        "Message-ID" => "<test@post>",
        "From" => "\"Demo User\" <nobody@example.net>",
        "Newsgroups" => "misc.test",
        "Subject" => "I am just a test article",
        "Organization" => "An Example Net",
      }

      body = "This is just a test article."

      :ok = :gen_tcp.send(
        socket,
        Enum.join([
          to_line(headers),
          "",
          body,
          ".",
          ""
        ], "\r\n")
      )

      assert_receive(
        {:called_back, :handle_POST, %{ headers: ^headers, body: ^body }},
        100,
        "@callback handle_POST/2 was not called"
      )
    end

    test "supports empty body", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "POST\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      headers = %{
        "Message-ID" => "<test@post>",
        "From" => "\"Demo User\" <nobody@example.net>",
        "Newsgroups" => "misc.test",
        "Subject" => "I am just a test article",
        "Organization" => "An Example Net",
      }

      body = ""

      :ok = :gen_tcp.send(
        socket,
        Enum.join([
          to_line(headers),
          "",
          body,
          ".",
          ""
        ], "\r\n")
      )

      assert_receive(
        {:called_back, :handle_POST, %{ headers: ^headers, body: ^body }},
        100,
        "@callback handle_POST/2 was not called"
      )
    end

    test "supports multi-line body", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "POST\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      headers = %{
        "Message-ID" => "<test@post>",
        "From" => "\"Demo User\" <nobody@example.net>",
        "Newsgroups" => "misc.test",
        "Subject" => "I am just a test article",
        "Organization" => "An Example Net",
      }

      body_lines = [
        "This is just a test article that can ",
        "span multiple lines, as long as it ends ",
        "with a \".\"",
      ]

      :ok = :gen_tcp.send(
        socket,
        Enum.join([
          to_line(headers),
          "",
          Enum.join(body_lines, "\r\n"),
          ".",
          ""
        ], "\r\n")
      )

      assert_receive(
        {:called_back, :handle_POST, %{ headers: ^headers, body: body }},
        100,
        "@callback handle_POST/2 was not called"
      )

      assert body === Enum.join(body_lines, "")
    end

    test "responds with 240 if callback returns an ok-tuple", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "POST\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      headers = %{
        "Message-ID" => "<test@post>",
        "From" => "\"Demo User\" <nobody@example.net>",
        "Newsgroups" => "misc.test",
        "Subject" => "I am just a test article",
        "Organization" => "An Example Net",
      }

      body = ""

      :ok = :gen_tcp.send(
        socket,
        Enum.join([
          to_line(headers),
          "",
          body,
          ".",
          ""
        ], "\r\n")
      )

      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^240 /
    end

    test "responds with 441 to reject if callback returns an error-tuple", context do
      # Here we only test an example case by rejecting the article if it does
      # not have "Message-ID" header. In practive, that is totally valid, and
      # it's up to the server implementation to generate the message's ID.
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "POST\r\n")
      {:ok, _response} = :gen_tcp.recv(socket, 0, 1000)

      headers = %{
        "From" => "\"Demo User\" <nobody@example.net>",
        "Newsgroups" => "misc.test",
        "Subject" => "I am just a test article",
        "Organization" => "An Example Net",
      }

      body = ""

      :ok = :gen_tcp.send(
        socket,
        Enum.join([
          to_line(headers),
          "",
          body,
          ".",
          ""
        ], "\r\n")
      )

      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^441 /
    end

  end

  describe "DATE command" do
    @describetag capabilities: ["READER"]

    setup [
      :setup_CAPABILITIES,
      :setup_server, :setup_socket,
    ]

    test "responds with 111", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "DATE\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)

      assert response =~ ~r/^111 \d{4}\d{2}\d{2}\d{2}\d{2}\d{2}/
    end

    test "responnds with the current UTC date and time on server", context do
      %{socket: socket} = context

      :ok = :gen_tcp.send(socket, "DATE\r\n")
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)

      now = DateTime.utc_now()

      assert response === "111 #{Calendar.strftime(now, "%Y%m%d%H%M%S")}\r\n"
    end
  end

  describe "@callback handle_HELP/1" do
    setup do
      help_text = String.trim("""
      This is some help text.  There is no specific\r
      formatting requirement for this test, though\r
      it is customary for it to list the valid commands\r
      and give a brief definition of what they do.
      """)

      TestNNTPServer.start(
        handle_HELP: fn(state) ->
          Kernel.send(:tester, {:called_back, :handle_HELP, 1})

          {:ok, help_text, state}
        end
      )

      {:ok, socket, _greeting} = GenNNTP.connect()

      %{socket: socket, help_text: help_text}
    end

    test "is called when the client asks for it", %{socket: socket} do
      refute_receive(
        {:called_back, :handle_HELP, 1},
        100,
        "@callback handle_HELP/1 should not be called when client has not asked for it"
      )

      :ok = :gen_tcp.send(socket, "HELP\r\n")

      assert_receive(
        {:called_back, :handle_HELP, 1},
        100,
        "@callback handle_HELP/1 was not called"
      )
    end

    test "responds with `100` and a multi-line data block when the client asks for it", context do
      %{socket: socket, help_text: help_text} = context

      :ok = :gen_tcp.send(socket, "HELP\r\n")

      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response =~ ~r/^100 /

      # Help text line by line.
      help_text
      |> String.split("\r\n")
      |> Enum.each(fn line ->
        {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
        assert response === "#{line}\r\n"
      end)

      # Then the termination line
      {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
      assert response === ".\r\n"
    end
  end

  describe "command/3" do
    @describetag capabilities: ["READER", "IHAVE", "POST", "NEWNEWS", "HDR", "OVER", "LIST", "MODE-READER"]

    setup [
      :setup_articles, :setup_groups, :setup_group_articles,
      :setup_CAPABILITIES, :setup_GROUP, :setup_ARTICLE, :setup_POST,
      :setup_server, :setup_socket
    ]

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

    test "command/3 with arguments", context do
      %{socket: socket, articles: articles} = context

      message_id = "<45223423@example.com>"
      article_number = 3000239
      group_name = "misc.test"

      %{headers: headers, body: body} = Enum.find(articles, &(match_id(&1, message_id)))

      assert {:ok, response} = GenNNTP.command(socket, "ARTICLE", [message_id])
      assert response === String.trim("""
      220 0 #{ message_id }\r
      #{ to_line(headers) }\r
      \r
      #{ body }
      """)

      # Calling "GROUP" to set the current group.
      {:ok, _response} = GenNNTP.command(socket, "GROUP", [group_name])

      assert {:ok, response} = GenNNTP.command(socket, "ARTICLE", [article_number])
      assert response === String.trim("""
      220 #{ article_number } #{ message_id }\r
      #{ to_line(headers) }\r
      \r
      #{ body }
      """)

      article = %{
        headers: %{
          "Message-ID" => "<test@post>",
          "From" => "\"Demo User\" <nobody@example.net>",
          "Newsgroups" => "misc.test",
          "Subject" => "I am just a test article",
          "Organization" => "An Example Net",
        },
        body: "This is a test article for posting"
      }
      assert {:ok, response} = GenNNTP.command(socket, "POST", [article])
      assert response =~ ~r/^240 /
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

  # Helpers
  defp to_line(headers) when is_map(headers) do
    Enum.map_join(headers, "\r\n", &to_line/1)
  end

  defp to_line({k, v}), do: "#{ k }: #{ v }"

end
