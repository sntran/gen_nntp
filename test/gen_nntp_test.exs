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

    test "connects to a NNTP server" do
      GenNNTP.start(TestNNTPServer, [], [])
      assert {:ok, _socket, _greeting} = GenNNTP.connect("localhost", 119, [])
    end

    test "error if fail to connect to NNTP server" do
      assert {:error, :econnrefused} = GenNNTP.connect("localhost", 119, [])
    end

    test "receives a greeting after connecting" do
      GenNNTP.start(TestNNTPServer, [], [])
      assert {:ok, _socket, greeting} = GenNNTP.connect("localhost", 119, [])
      assert greeting =~ ~r/^20[0,1] /
    end

    test "connect/2 default to empty options" do
      GenNNTP.start(TestNNTPServer, [], [])
      assert {:ok, _socket, _greeting} = GenNNTP.connect("localhost", 119)
    end

    test "connect/1 default to port 119" do
      GenNNTP.start(TestNNTPServer, [], [])
      assert {:ok, _socket, _greeting} = GenNNTP.connect("localhost")
    end

    test "connect/0 default to localhost" do
      GenNNTP.start(TestNNTPServer, [], [])
      assert {:ok, _socket, _greeting} = GenNNTP.connect()
    end

  end

  describe "send/3" do

    setup do
      GenNNTP.start(TestNNTPServer, [], [])
      {:ok, socket, _greeting} = GenNNTP.connect()

      %{socket: socket}
    end

    test "sends QUIT command", %{socket: socket} do
      assert {:ok, response} = GenNNTP.send(socket, "QUIT", [])
      assert response =~ ~r/^205 /
    end

    test "send/2 default to empty arguments", %{socket: socket} do
      assert {:ok, response} = GenNNTP.send(socket, "QUIT")
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

      opts = [:binary, packet: :line, active: false]
      {:ok, _socket} = :gen_tcp.connect('localhost', 119, opts)

      assert_receive(
        {:called_back, :init, 1},
        100,
        "@callback init/1 was not called"
      )
    end

  end

  describe "server interaction" do
    setup do
      TestNNTPServer.start()

      opts = [:binary, packet: :line, active: false]
      {:ok, socket} = :gen_tcp.connect('localhost', 119, opts)
      {:ok, greeting} = :gen_tcp.recv(socket, 0, 1000)
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
