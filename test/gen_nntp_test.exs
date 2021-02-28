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
