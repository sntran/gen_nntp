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

  describe "command/3" do

    setup do
      GenNNTP.start(TestNNTPServer, [], [])
      {:ok, socket, _greeting} = GenNNTP.connect()

      %{socket: socket}
    end

    test "sends QUIT command", %{socket: socket} do
      assert {:ok, response} = GenNNTP.command(socket, "QUIT", [])
      assert response =~ ~r/^205 /
    end

    test "command/2 default to empty arguments", %{socket: socket} do
      assert {:ok, response} = GenNNTP.command(socket, "QUIT")
      assert response =~ ~r/^205 /
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
