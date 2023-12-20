defmodule TestSentants do
  use ExUnit.Case

  test "sentant" do
    sentant_definition = """
    sentant:
      name: fred
    """

    {result, pid} = Reality2.Sentants.new_sentant(sentant_definition)
    assert result == :ok
    assert Process.alive?(pid)
  end

  test "multiple sentants" do
    process_array = create_many_sentants("test", 20)
    assert check_non_zero(process_array)
  end

  test "delete sentant" do
    Reality2.Sentants.delete_sentant("fred")
    Process.sleep(1000)
    assert Process.whereis(:fred) == nil
  end

  # -------------------------------------------------------------------------------------------------------------------
  # Helper Functions
  # -------------------------------------------------------------------------------------------------------------------
  defp check_non_zero([]), do: true
  defp check_non_zero([{_, 0} | _]), do: false
  defp check_non_zero([{_, _} | tail]), do: check_non_zero(tail)

  defp create_many_sentants(name, num) do
    Enum.each(1..num, fn x ->
      sentant_definition = """
      sentant:
        name: #{name <> Integer.to_string(x)}
      """
      Reality2.Sentants.new_sentant(sentant_definition)
    end)
    Reality2.Sentants
    |> PartitionSupervisor.which_children
    |> count_processes
  end

  defp count_processes([]), do: []
  defp count_processes([{id, pid, _, _} | tail]) do
    %{active: num_children} = DynamicSupervisor.count_children(pid)
    [{id, num_children} | count_processes(tail)]
  end
end
